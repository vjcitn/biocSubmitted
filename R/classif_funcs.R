
# ============================================================
# Package Classification using ellmer - Adaptive Version
# Dynamically generates categories based on package content
# ============================================================

library(ellmer)
library(jsonlite)

# ------------------------------------------------------------
# 1. Load JSON data
# ------------------------------------------------------------

#' import a json document built from a list of results of
#' analyze_repo_vignette
#' @import ellmer
#' @import jsonlite
#' @export
load_packages <- function(json_file) {
  raw_json <- fromJSON(json_file, simplifyVector = FALSE)
  
  if (is.character(raw_json[[1]])) {
    packages <- fromJSON(raw_json[[1]], simplifyVector = FALSE)
  } else {
    packages <- raw_json
  }
  cat(sprintf("Loaded %d packages\n", length(packages)))
  packages
}

# ------------------------------------------------------------
# 2. Generate categories dynamically from package content
# ------------------------------------------------------------

#' produce a vector of categories based on incoming content
#' @param packages output of `load_packages`
#' @param n_categories numeric
#' @param chatfun defaults to ellmer::chat_anthropic
#' @param model defaults to "claude-sonnet-4-20250514"
#' @return a list with components categories and descriptions
#' @export
generate_categories <- function(packages, n_categories = 10, chatfun = ellmer::chat_anthropic,
     model = "claude-sonnet-4-20250514") {
  cat("Analyzing package collection to generate categories...\n")
  
  chat <- chatfun(model = model)
  
 # Collect all topics and brief summaries
  all_topics <- character()
  all_summaries <- character()
  
  for (pkg in packages) {
    if (length(pkg$topics) > 0) {
      all_topics <- c(all_topics, unlist(pkg$topics))
    }
    if (length(pkg$focused) > 0) {
      # Take first 150 chars of each summary
      summ <- substr(pkg$focused[[1]], 1, 150)
      all_summaries <- c(all_summaries, summ)
    }
  }
  
  # Get unique topics and their frequencies
  topic_freq <- sort(table(all_topics), decreasing = TRUE)
  top_topics <- names(head(topic_freq, 50))  # Top 50 most frequent
  
  # Sample summaries (don't send all to avoid token limits)
  sampled_summaries <- if (length(all_summaries) > 15) {
    sample(all_summaries, 15)
  } else {
    all_summaries
  }
  
  prompt <- sprintf('You are analyzing a collection of %d R/Bioconductor packages in genomics and computational biology.

Based on the topics and summaries below, create exactly %d categories that:
1. Cover the major themes present in this specific collection
2. Are mutually exclusive (each package fits clearly in one category)
3. Have meaningful names appropriate for genome biology and bioinformatics
4. Are balanced (avoid categories that would only contain 1-2 packages)

FREQUENT TOPICS IN COLLECTION:
%s

SAMPLE PACKAGE SUMMARIES:
%s

Respond in this EXACT JSON format (no markdown, no backticks):
[
  {"name": "Category Name", "description": "Brief description of what belongs here, key methods/topics"},
  ...
]

Generate exactly %d categories as a JSON array:', 
    length(packages),
    n_categories,
    paste(top_topics, collapse = ", "),
    paste(sprintf("- %s", sampled_summaries), collapse = "\n"),
    n_categories
  )
  
  response <- chat$chat(prompt, echo = FALSE)
  
  # Clean and parse JSON response
  response <- trimws(response)
  response <- gsub("^```json\\s*", "", response)
  response <- gsub("^```\\s*", "", response)
  response <- gsub("\\s*```$", "", response)
  
  categories_list <- tryCatch({
    fromJSON(response, simplifyVector = FALSE)
  }, error = function(e) {
    cat("Warning: Failed to parse categories, using defaults\n")
    cat("Response was:", substr(response, 1, 500), "\n")
    # Fallback defaults
    list(
      list(name = "Single-Cell Analysis", description = "scRNA-seq, cell types, clustering"),
      list(name = "Differential Expression", description = "DE analysis, RNA-seq, gene expression"),
      list(name = "Epigenetics", description = "DNA methylation, ChIP-seq, chromatin"),
      list(name = "Data Structures", description = "GRanges, arrays, file formats"),
      list(name = "Proteomics", description = "Mass spectrometry, proteins"),
      list(name = "Imaging", description = "Microscopy, spatial analysis"),
      list(name = "Statistical Methods", description = "Models, inference, QC"),
      list(name = "Data Integration", description = "Databases, APIs, harmonization")
    )
  })
  
  # Convert to named vectors
  categories <- sapply(categories_list, `[[`, "name")
  category_desc <- setNames(
    sapply(categories_list, `[[`, "description"),
    categories
  )
  
  cat(sprintf("Generated %d categories:\n", length(categories)))
  for (i in seq_along(categories)) {
    cat(sprintf("  %d. %s: %s\n", i, categories[i], category_desc[categories[i]]))
  }
  cat("\n")
  
  list(
    categories = categories,
    descriptions = category_desc
  )
}

# ------------------------------------------------------------
# 3. Prepare package summaries
# ------------------------------------------------------------

#' internal function to manage summary
#' @keywords internal
prepare_package_info <- function(pkg, idx) {
  summary <- if (length(pkg$focused) > 0) pkg$focused[[1]] else "No summary"
  topics <- if (length(pkg$topics) > 0) paste(unlist(pkg$topics), collapse = ", ") else "None"
  authors <- if (length(pkg$author) > 0) paste(unlist(pkg$author), collapse = ", ") else "<UNKNOWN>"
  
  list(
    idx = idx,
    authors = authors,
    summary = summary,
    topics = topics
  )
}

# ------------------------------------------------------------
# 4. Batch classification function
# ------------------------------------------------------------

#' internal classification support
#' @keywords internal
classify_batch <- function(pkg_batch, categories, chat) {
  # Build the prompt with all packages in batch
  cat_list <- paste(sprintf("%d. %s", seq_along(categories), categories), collapse = "\n")
  
  pkg_descriptions <- sapply(pkg_batch, function(p) {
    sprintf("PKG_%d:\nTopics: %s\nSummary: %s", p$idx, p$topics, p$summary)
  })
  
  prompt <- sprintf('Classify each R package into exactly ONE category. For each package, respond with ONLY its number and category number.

CATEGORIES:
%s

PACKAGES:
%s

Respond in this exact format, one per line:
PKG_N: CATEGORY_NUMBER

Example response:
PKG_1: 3
PKG_2: 1
PKG_3: 5', cat_list, paste(pkg_descriptions, collapse = "\n\n"))
  
  response <- chat$chat(prompt, echo = FALSE)
  
  # Parse response
  lines <- strsplit(response, "\n")[[1]]
  results <- list()
  
  for (line in lines) {
    match <- regmatches(line, regexec("PKG_(\\d+):\\s*(\\d+)", line))[[1]]
    if (length(match) == 3) {
      pkg_idx <- as.integer(match[2])
      cat_idx <- as.integer(match[3])
      if (cat_idx >= 1 && cat_idx <= length(categories)) {
        results[[as.character(pkg_idx)]] <- categories[cat_idx]
      }
    }
  }
  
  results
}

# ------------------------------------------------------------
# 5. Run batch classification
# ------------------------------------------------------------

classify_all <- function(pkg_info, categories, batch_size = 8) {
  chat <- chat_anthropic(model = "claude-sonnet-4-20250514")
  
  n_batches <- ceiling(length(pkg_info) / batch_size)
  all_results <- list()
  
  for (b in seq_len(n_batches)) {
    start_idx <- (b - 1) * batch_size + 1
    end_idx <- min(b * batch_size, length(pkg_info))
    batch <- pkg_info[start_idx:end_idx]
    
    cat(sprintf("\rClassifying batch %d/%d (packages %d-%d)...", b, n_batches, start_idx, end_idx))
    
    batch_results <- tryCatch({
      classify_batch(batch, categories, chat)
    }, error = function(e) {
      cat(sprintf("\nError in batch %d: %s\n", b, e$message))
      list()
    })
    
    all_results <- c(all_results, batch_results)
    
    if (b < n_batches) Sys.sleep(1)
  }
  cat("\nClassification complete!\n")
  
  # Fill in any missing with "Uncategorized"
  for (p in pkg_info) {
    idx_str <- as.character(p$idx)
    if (is.null(all_results[[idx_str]])) {
      all_results[[idx_str]] <- "Uncategorized"
    }
  }
  
  all_results
}

# ------------------------------------------------------------
# 6. Main classification pipeline
# ------------------------------------------------------------

#' main tool for classifying packages
#' @param json_file path to json document produced from list of analyze_vig outputs
#' @param n_categories numeric
#' @param chatfun defaults to ellmer::chat_anthropic
#' @param model defaults to "claude-sonnet-4-20250514"
#' @param output_prefix character prefix for emitted md, html, json
#' @export
classify_packages <- function(json_file, n_categories = 10, chatfun = ellmer::chat_anthropic,
   model = "claude-sonnet-4-20250514", output_prefix = "packages_classified") {
  # Load packages
  packages <- load_packages(json_file)
  
  # Generate categories dynamically from package content
  cat_info <- generate_categories(packages, n_categories = n_categories, chatfun=chatfun,
   model=model)
  categories <- cat_info$categories
  category_desc <- cat_info$descriptions
  
  # Prepare package info
  pkg_info <- lapply(seq_along(packages), function(i) {
    prepare_package_info(packages[[i]], i)
  })
  
  # Classify all packages
  cat("Classifying packages in batches...\n")
  classifications <- classify_all(pkg_info, categories)
  
  # Build final results
  results <- lapply(pkg_info, function(p) {
    list(
      idx = p$idx,
      authors = p$authors,
      summary = p$summary,
      topics = p$topics,
      category = classifications[[as.character(p$idx)]]
    )
  })
  
  # Generate and save markdown
  md <- generate_markdown(results, category_desc)
  md_file <- paste0(output_prefix, ".md")
  writeLines(md, md_file)
  cat(sprintf("Saved: %s\n", md_file))
  
  # Generate and save HTML (collapsible)
  html <- generate_html(results, category_desc)
  html_file <- paste0(output_prefix, ".html")
  writeLines(html, html_file)
  cat(sprintf("Saved: %s\n", html_file))
  
  # Save raw results as JSON
  results_json <- toJSON(results, auto_unbox = TRUE, pretty = TRUE)
  json_out <- paste0(output_prefix, ".json")
  writeLines(results_json, json_out)
  cat(sprintf("Saved: %s\n", json_out))
  
  # Save categories for reference
  cat_json <- toJSON(
    list(categories = categories, descriptions = as.list(category_desc)), 
    auto_unbox = TRUE, pretty = TRUE
  )
  writeLines(cat_json, paste0(output_prefix, "_categories.json"))
  
  # Print summary
  cat("\n=== Classification Summary ===\n")
  tbl <- table(sapply(results, `[[`, "category"))
  for (nm in names(sort(tbl, decreasing = TRUE))) {
    cat(sprintf("  %-40s %d\n", nm, tbl[nm]))
  }
  
  invisible(list(
    results = results,
    categories = categories,
    descriptions = category_desc
  ))
}

# ------------------------------------------------------------
# 7. Generate hierarchical markdown
# ------------------------------------------------------------

#' produce markdown
#' @keywords internal
generate_markdown <- function(results, cat_desc) {
  by_cat <- split(results, sapply(results, `[[`, "category"))
  
  # Order categories
  cat_order <- c(names(cat_desc), "Uncategorized")
  cat_order <- cat_order[cat_order %in% names(by_cat)]
  
  lines <- c(
    "# R Packages for Genomics & Computational Biology",
    "",
    sprintf("*%d packages across %d categories*", length(results), length(by_cat)),
    "",
    "---",
    ""
  )
  
  # Table of contents
  lines <- c(lines, "## Contents", "")
  for (cat_name in cat_order) {
    # GitHub-compatible anchor: lowercase first, then replace non-alphanum with hyphen, trim leading/trailing hyphens
    anchor <- tolower(cat_name)
    anchor <- gsub("[^a-z0-9]+", "-", anchor)
    anchor <- gsub("^-+|-+$", "", anchor)  # trim leading/trailing hyphens
    n <- length(by_cat[[cat_name]])
    lines <- c(lines, sprintf("- [%s](#%s) (%d)", cat_name, anchor, n))
  }
  lines <- c(lines, "", "---", "")
  
  # Each category
  for (cat_name in cat_order) {
    pkgs <- by_cat[[cat_name]]
    
    lines <- c(lines, sprintf("## %s", cat_name), "")
    
    if (cat_name %in% names(cat_desc)) {
      lines <- c(lines, sprintf("*%s*", cat_desc[[cat_name]]), "")
    }
    
    lines <- c(lines, "| Package Authors | Description | Key Topics |")
    lines <- c(lines, "|:----------------|:------------|:-----------|")
    
    for (pkg in pkgs) {
      # Full description and keywords (no truncation)
      desc <- pkg$summary
      kw_str <- pkg$topics
      
      # Escape pipes for markdown table
      desc <- gsub("\\|", "–", desc)
      kw_str <- gsub("\\|", "–", kw_str)
      auth <- gsub("\\|", "–", pkg$authors)
      
      lines <- c(lines, sprintf("| %s | %s | %s |", auth, desc, kw_str))
    }
    
    lines <- c(lines, "")
  }
  
  paste(lines, collapse = "\n")
}

# ------------------------------------------------------------
# 8. Generate collapsible HTML table
# ------------------------------------------------------------

#' generate collapsible html
#' @keywords internal
generate_html <- function(results, cat_desc) {
  by_cat <- split(results, sapply(results, `[[`, "category"))
  
  cat_order <- c(names(cat_desc), "Uncategorized")
  cat_order <- cat_order[cat_order %in% names(by_cat)]
  
  html <- '<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>R Packages for Genomics &amp; Computational Biology</title>
<style>
  * { box-sizing: border-box; }
  body { 
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif; 
    max-width: 1400px; 
    margin: 0 auto; 
    padding: 20px;
    background: #fafafa;
    color: #333;
    line-height: 1.5;
  }
  h1 { 
    color: #1a1a2e; 
    border-bottom: 3px solid #4361ee;
    padding-bottom: 10px;
  }
  .subtitle {
    color: #666;
    font-style: italic;
    margin-bottom: 20px;
  }
  details { 
    margin: 12px 0; 
    border: 1px solid #ddd; 
    border-radius: 8px;
    background: white;
    box-shadow: 0 2px 4px rgba(0,0,0,0.05);
  }
  details[open] {
    box-shadow: 0 4px 12px rgba(0,0,0,0.1);
  }
  summary { 
    padding: 16px 20px; 
    background: linear-gradient(to right, #f8f9fa, #fff);
    cursor: pointer; 
    font-weight: 600; 
    font-size: 1.1em;
    border-radius: 8px;
    transition: background 0.2s;
  }
  summary:hover { 
    background: linear-gradient(to right, #e9ecef, #f8f9fa);
  }
  details[open] summary {
    border-bottom: 1px solid #eee;
    border-radius: 8px 8px 0 0;
  }
  .category-desc { 
    font-style: italic; 
    color: #666; 
    font-weight: normal; 
    font-size: 0.85em;
    display: block;
    margin-top: 4px;
  }
  .count { 
    background: #4361ee; 
    color: white; 
    padding: 3px 10px; 
    border-radius: 12px; 
    font-size: 0.8em; 
    margin-left: 10px;
    font-weight: normal;
  }
  .table-wrapper {
    overflow-x: auto;
    padding: 15px;
  }
  table { 
    width: 100%; 
    border-collapse: collapse;
    font-size: 0.9em;
  }
  th, td { 
    padding: 12px 15px; 
    text-align: left; 
    border-bottom: 1px solid #eee;
    vertical-align: top;
  }
  th { 
    background: #f8f9fa; 
    font-weight: 600;
    position: sticky;
    top: 0;
    white-space: nowrap;
  }
  tr:hover { 
    background: #f8f9fa;
  }
  .authors {
    font-weight: 500;
    color: #4361ee;
    min-width: 150px;
  }
  .description {
    color: #333;
    max-width: 600px;
  }
  .keywords { 
    font-size: 0.85em; 
    color: #666;
    max-width: 300px;
  }
  .keyword-tag {
    display: inline-block;
    background: #e9ecef;
    padding: 2px 8px;
    border-radius: 4px;
    margin: 2px;
    font-size: 0.85em;
  }
  .expand-all {
    margin-bottom: 15px;
  }
  .expand-all button {
    padding: 8px 16px;
    margin-right: 8px;
    border: 1px solid #ddd;
    border-radius: 4px;
    background: white;
    cursor: pointer;
    font-size: 0.9em;
  }
  .expand-all button:hover {
    background: #f0f0f0;
  }
</style>
</head>
<body>
<h1>R Packages for Genomics &amp; Computational Biology</h1>
'
  
  html <- paste0(html, '<p class="subtitle">', length(results), 
                 ' packages across ', length(by_cat), ' categories</p>\n')
  
  # Add expand/collapse all buttons
  html <- paste0(html, '
<div class="expand-all">
  <button onclick="document.querySelectorAll(\'details\').forEach(d => d.open = true)">Expand All</button>
  <button onclick="document.querySelectorAll(\'details\').forEach(d => d.open = false)">Collapse All</button>
</div>
')
  
  for (cat_name in cat_order) {
    pkgs <- by_cat[[cat_name]]
    n_pkgs <- length(pkgs)
    desc <- if (cat_name %in% names(cat_desc)) cat_desc[[cat_name]] else ""
    
    # Escape HTML in category name and description
    cat_name_safe <- gsub("&", "&amp;", gsub("<", "&lt;", gsub(">", "&gt;", cat_name)))
    desc_safe <- gsub("&", "&amp;", gsub("<", "&lt;", gsub(">", "&gt;", desc)))
    
    html <- paste0(html, '<details>\n<summary>', cat_name_safe, 
                   '<span class="count">', n_pkgs, '</span>',
                   if (nchar(desc) > 0) paste0('<span class="category-desc">', desc_safe, '</span>') else '',
                   '</summary>\n<div class="table-wrapper"><table>\n')
    html <- paste0(html, '<tr><th>Authors</th><th>Description</th><th>Keywords</th></tr>\n')
    
    for (pkg in pkgs) {
      # Escape HTML in content
      escape_html <- function(x) {
        x <- gsub("&", "&amp;", x)
        x <- gsub("<", "&lt;", x)
        x <- gsub(">", "&gt;", x)
        x <- gsub("\"", "&quot;", x)
        x
      }
      
      authors <- escape_html(pkg$authors)
      description <- escape_html(pkg$summary)
      
      # Format keywords as tags
      kws <- strsplit(pkg$topics, ", ")[[1]]
      kw_html <- paste(sprintf('<span class="keyword-tag">%s</span>', escape_html(kws)), collapse = " ")
      
      html <- paste0(html, '<tr>',
                     '<td class="authors">', authors, '</td>',
                     '<td class="description">', description, '</td>',
                     '<td class="keywords">', kw_html, '</td>',
                     '</tr>\n')
    }
    
    html <- paste0(html, '</table></div>\n</details>\n')
  }
  
  html <- paste0(html, '</body>\n</html>')
  html
}
