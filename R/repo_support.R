#' list repo folder
#' @keywords internal
list_github_dir <- function(user, repo, path) {
  branch <- get_default_branch(user, repo)
  if (is.null(branch) || nchar(branch) == 0) {
    stop(sprintf("Could not get default branch for %s/%s", user, repo))
  }
  
  api_url <- sprintf(
    "https://api.github.com/repos/%s/%s/contents/%s?ref=%s",
    user, repo, path, branch
  )
  
  res <- httr::GET(api_url, get_github_headers())
 
  if (status_code(res) == 404) {
    return(character(0))  # No vignettes directory
  }

  stop_for_status(res)
  info <- fromJSON(content(res, "text", encoding = "UTF-8"))
  return(info$name)
}

#' create path to vignette
#' @keywords internal
build_vignette_path <- function(ghuser, ghrepo, vname) {
  branch <- get_default_branch(ghuser, ghrepo)  # Will use cache
  sprintf(
    "https://raw.githubusercontent.com/%s/%s/refs/heads/%s/vignettes/%s",
    ghuser, ghrepo, branch, vname
  )
}

