# ============================================================
# GitHub API utilities - Package-compatible version
# ============================================================
# 
# Two caching strategies provided:
#
# Option A: Using `memoise` (recommended for packages)
#   - Automatic memoization
#   - Cache can be cleared with `memoise::forget()`
#   - Requires: memoise package
#
# Option B: Using closure/factory pattern (no dependencies)
#   - Self-contained, no extra packages needed
#   - Cache managed via returned functions
#
# ============================================================

# ============================================================
# OPTION A: Using memoise (recommended)
# ============================================================

#' @importFrom httr GET add_headers headers status_code stop_for_status content
#' @importFrom jsonlite fromJSON
#' @import memoise
NULL

#' Get GitHub authentication token
#' 
#' Checks GITHUB_TOKEN, GITHUB_PAT, and GH_TOKEN environment variables.
#' 
#' @return Character string with token, or NULL if not found
#' @export
get_github_token <- function()
 {
  for (var in c("GITHUB_TOKEN", "GITHUB_PAT", "GH_TOKEN")) {
    token <- Sys.getenv(var)
    if (nchar(token) > 0) {
      return(token)
    }
  }
  NULL
}

#' Build GitHub API headers
#' @return httr headers object
#' @keywords internal
get_github_headers <- function() {
  token <- get_github_token()
  if (!is.null(token)) {
    httr::add_headers(
      Authorization = paste("token", token),
      Accept = "application/vnd.github.v3+json"
    )
  } else {
    httr::add_headers(Accept = "application/vnd.github.v3+json")
  }
}

#' Get default branch for a GitHub repository (non-cached version)
#' 
#' @param owner GitHub username or organization
#' @param repo Repository name
#' @return Character string with default branch name
#' @keywords internal
get_default_branch_impl <- function(owner, repo) {
  url <- sprintf("https://api.github.com/repos/%s/%s", owner, repo)
  res <- httr::GET(url, get_github_headers())
  
  # Check rate limit

  remaining <- as.numeric(httr::headers(res)$`x-ratelimit-remaining`)
  if (!is.na(remaining) && remaining < 10) {
    reset_time <- as.numeric(httr::headers(res)$`x-ratelimit-reset`)
    wait_secs <- reset_time - as.numeric(Sys.time())
    warning(sprintf("Rate limit nearly exhausted. %d remaining. Resets in %.0f seconds.", 
                    remaining, max(0, wait_secs)))
  }
  
  if (httr::status_code(res) == 403) {
    reset_time <- as.numeric(httr::headers(res)$`x-ratelimit-reset`)
    wait_secs <- reset_time - as.numeric(Sys.time())
    stop(sprintf("GitHub API rate limited (403). Resets in %.0f seconds. Set GITHUB_TOKEN for higher limits.", 
                 max(0, wait_secs)))
  }
  
  httr::stop_for_status(res)
  content <- httr::content(res)
  content$default_branch
}

#' Get default branch for a GitHub repository (memoised)
#' 
#' Results are cached in memory. Use `clear_github_cache()` to reset.
#' 
#' @param owner GitHub username or organization
#' @param repo Repository name
#' @return Character string with default branch name
#' @export
get_default_branch <- NULL

#' Clear the GitHub API cache
#' @export
clear_github_cache <- NULL

# Initialize memoised function on package load
.onLoad <- function(libname, pkgname) {
  if (requireNamespace("memoise", quietly = TRUE)) {
    # Create memoised version in package namespace
    get_default_branch <<- memoise::memoise(get_default_branch_impl)
    clear_github_cache <<- function() memoise::forget(get_default_branch)
  } else {
    # Fallback: use the non-cached version with a warning
    get_default_branch <<- function(owner, repo) {
      warning("memoise package not installed - caching disabled. Install with: install.packages('memoise')")
      get_default_branch_impl(owner, repo)
    }
    clear_github_cache <<- function() invisible(NULL)
  }
}
