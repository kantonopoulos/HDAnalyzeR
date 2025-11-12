#' Fetch PubMed articles for disease-protein biomarker associations
#' 
#' `hd_literature_search()` fetches PubMed articles for disease-protein biomarker associations.
#' 
#' @param query_list Named list of diseases with associated proteins.
#'                   Example: list(AML = c("TCL1A", "TNFRSF9"), CLL = c("CD22", "FCRL3"))
#' @param max_results Maximum number of articles to return per protein query.
#' @param min_year Only include articles published in or after this year.
#' 
#' @return A dataframe with columns: Disease, Protein, PMID, Title, and Abstract
#' @export
#' 
#' @examples
#' queries <- list(
#'   "Acute Myeloid Leukemia" = c("TCL1A", "TNFRSF9"),
#'   "Chronic Lymphocytic Leukemia" = c("CD22")
#' )
#' 
#' res <- hd_literature_search(queries, max_results = 2, min_year = 2015)
#' head(res, 1)
hd_literature_search <- function(query_list, max_results = 20, min_year = NULL) {
  if (!requireNamespace("easyPubMed", quietly = TRUE)) {
    stop("Please install easyPubMed >= 3.0")
  }
  
  out_list <- list()
  
  for (disease in names(query_list)) {
    proteins <- query_list[[disease]]
    
    for (protein in proteins) {
      q <- paste0(disease, " AND ", protein, " AND biomarker[Title/Abstract]")
      if (!is.null(min_year)) {
        q <- paste0(q, " AND \"", min_year, "\"[PDAT] : \"3000\"[PDAT]")
      }
      
      epm_obj <- easyPubMed::epm_query(q)
      epm_obj <- easyPubMed::epm_fetch(epm_obj)
      epm_obj <- easyPubMed::epm_parse(epm_obj, max_authors = 1, include_abstract = TRUE)
      df <- easyPubMed::get_epm_data(epm_obj)
      
      if (is.null(df) || nrow(df) == 0) next
      
      df <- df[seq_len(min(nrow(df), max_results)), , drop = FALSE]
      
      # Ensure all columns exist and have same length
      safe_col <- function(x) if (length(x) == 0) rep(NA, nrow(df)) else x

      df2 <- data.frame(
        Disease      = disease,
        Protein      = protein,
        PMID         = safe_col(df$pmid),
        Title        = safe_col(df$title),
        Abstract     = safe_col(df$abstract),
        stringsAsFactors = FALSE
      )
      
      out_list[[length(out_list) + 1]] <- df2
    }
  }
  
  if (length(out_list) == 0) {
    return(data.frame())
  }
  
  result_df <- do.call(rbind, out_list)
  rownames(result_df) <- NULL
  return(result_df)
}
