#' claude-generated issue retriever
#' @import curl
#' @param owner character(1) organization name
#' @param repo character(1) repository name
#' @param token character(1) GITHUB PAT for query
#' @param state "open" or "all" or "closed"; anything other than open
#' will likely hit a rate limit
#' @param max_pages numeric
#' @return a list of outputs of api.github.com/repos with the `issues?` query
#' @export
get_issues = function(owner="bioconductor", repo="contributions", 
     token=Sys.getenv("GITHUB_TOKEN"), state = "open", max_pages = Inf) {
  base_url <- sprintf(
    "https://api.github.com/repos/%s/%s/issues?state=%s&per_page=100",
    owner, repo, state
  )
  
  all_issues <- list()
  page <- 1
  
  repeat {
    if (page > max_pages) {
      message(sprintf("Reached max_pages limit (%d)", max_pages))
      break
    }
    
    url <- paste0(base_url, "&page=", page)
    
    h <- new_handle()
    handle_setheaders(h,
      Authorization = paste("Bearer", token),
      Accept = "application/vnd.github+json",
      `X-GitHub-Api-Version` = "2022-11-28"
    )
    
    res <- curl_fetch_memory(url, handle = h)
    
    if (res$status_code >= 400) {
      stop("HTTP error: ", res$status_code, "\n", rawToChar(res$content))
    }
    
    issues <- fromJSON(rawToChar(res$content), simplifyVector = FALSE)
    
    if (length(issues) == 0) break
    
    all_issues <- c(all_issues, issues)
    
    message(sprintf(
      "Page %d: fetched %d issues (%d total)",
      page, length(issues), length(all_issues)
    ))
    
    # If we got fewer than 100, we've reached the last page
    if (length(issues) < 100) break
    
    page <- page + 1
  }
  
  message(sprintf("Done. Retrieved %d issues.", length(all_issues)))
  all_issues
}
