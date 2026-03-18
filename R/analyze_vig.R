#' obtain information to support content analysis of a package via its vignette
#' @param repourl character URL to github repository
#' @param keyname character environment variable name for LLM-provider API key
#' @export
analyze_repo_vignette <- function(repourl, keyname = "ANTHROPIC_API_KEY") {
  stopifnot(nchar(Sys.getenv(keyname)) > 0)
  
  lk1 <- strsplit(repourl, "\\/")[[1]]
  ghuser <- lk1[4]
  ghrepo <- lk1[5]
  
  cat(sprintf("\n[%s] Processing %s/%s... ", format(Sys.time(), "%H:%M:%S"), ghuser, ghrepo))
  
  lkvigs <- list_github_dir(user = ghuser, repo = ghrepo, path = "vignettes")
  
  if (length(lkvigs) == 0) {
    cat("no vignettes directory")
    return(NULL)
  }
  
  # Check whether there is a vignette with same basename as repo
  chkmain <- grep(ghrepo, lkvigs, value = TRUE)
  if (length(chkmain) > 0 && nchar(chkmain[1]) > 0) {
    touse <- chkmain[1]
  } else {
    touse <- grep("Rmd|Rnw|qmd", lkvigs, value = TRUE)[1]
  }
  
  if (is.na(touse)) {
    cat("no Rmd, Rnw or qmd found")
    return(NULL)
  }
  
  cat(sprintf("using %s... ", touse))
  
  pa <- build_vignette_path(ghuser, ghrepo, touse)
  cur <- vig2data(pa)
  cat("done")
  cur
}

