#' utils::globalVariables(c("pmid", "year", "journal", "firstname", "lastname", "title", "First_author"))
#' #' Automated PubMed literature search
#' #'
#' #' `literature_search()` searches for articles for protein-disease pairs in PubMed.
#' #' A list of proteins and diseases is provided as input. The function retrieves the
#' #' articles for each protein-disease pair. The input should be in the correct format,
#' #' a list with diseases as names and protein vectors associated with each disease as
#' #' elements (see examples).
#' #'
#' #' @param prot_dis_list A list of proteins and diseases. The names of the list are the diseases and the elements are vectors of proteins.
#' #' @param max_articles The maximum number of articles to retrieve for each protein-disease pair. Default is 10.
#' #'
#' #' @return A list of tibbles. Each tibble contains the articles found for a protein-disease pair.
#' #' @export
#' #'
#' #' @details The disease and gene names should be correct in order for the query to
#' #' be successful. For example AML should be written as "acute myeloid leukemia".
#' #' @examples
#' #' # Prepare the list of protein-disease pairs
#' #' prot_dis_list <- list("acute myeloid leukemia" = c("FLT3", "EPO"),
#' #'                       "chronic lymphocytic leukemia" = c("PARP1", "FCER2"))
#' #'
#' #' # Run the literature search
#' #' lit_search_results <- literature_search(prot_dis_list, max_articles = 1)
#' #'
#' #' # Results for FLT3 in acute myeloid leukemia
#' #' lit_search_results[["acute myeloid leukemia"]][["FLT3"]]
#' literature_search <- function(prot_dis_list, max_articles = 10) {
#'
#'   diseases <- names(prot_dis_list)
#'   proteins <- prot_dis_list
#'
#'   articles_database <- list()
#'   for (disease in diseases) {
#'     articles_database[[disease]] <- list()
#'     for (protein in proteins[[disease]]) {
#'       # Collect articles from PubMed
#'       ids <- easyPubMed::get_pubmed_ids(paste0(protein, '[All Fields] AND ', disease, '[All Fields]'))
#'       message(paste0("Searching for articles on ", protein, " and ", disease))
#'       abstracts_xml <- easyPubMed::fetch_pubmed_data(pubmed_id_list = ids, retmax = max_articles)
#'
#'       # Extract data and store them in dataframe
#'       if(!is.null(abstracts_xml)) {
#'         abstracts_list <- easyPubMed::articles_to_list(abstracts_xml)
#'
#'         abstract_table <- abstracts_list |>
#'           purrr::map_df(function(abstract) {
#'             easyPubMed::article_to_df(pubmedArticle = abstract, autofill = FALSE) |>
#'               utils::head(1) |>
#'               dplyr::select(pmid, year, journal, firstname, lastname, title) |>
#'               dplyr::mutate(First_author = paste0(lastname, ", ", firstname)) |>
#'               dplyr::relocate(First_author, .before = year) |>
#'               dplyr::select(-firstname, -lastname)
#'           })
#'       }
#'       articles_database[[disease]][[protein]] <- abstract_table
#'     }
#'   }
#'   return(articles_database)
#' }
#'
