#' PubMed literature search
#'
#' `hd_literature_search()` searches for articles for gene-disease pairs in PubMed.
#' A list of genes/proteins and diseases is provided as input. The function retrieves the
#' articles for each gene-disease pair. The input should be in the correct format,
#' a list with diseases as names and gene vectors associated with each disease as
#' elements (see examples).
#'
#' @param feature_class_list A list of features (gene names) and classes (diseases). The names of the list are the classes and the elements are vectors of features. See examples.
#' @param max_articles The maximum number of articles to retrieve for each gene-disease pair. Default is 10.
#' @param keywords Additional keywords to include in the search. They are added to the query as "AND keywords". Default is NULL.
#' @param fields The fields to search for the keywords. Default is "All Fields". Other options are "Title", "Abstract", "Author", "Journal", "Affiliation", "MeSH Terms", "Other Terms".
#' @param api_key user-specific API key to increase the limit of queries per second. You can obtain your key from NCBI but not required. Default is NULL.
#' @param verbose Whether to print progress messages. Default is TRUE.
#'
#' @return A list of tibbles. Each tibble contains the articles found for a gene-disease pair.
#' @export
#'
#' @details The disease and gene names should be correct in order for the query to
#' be successful. For example AML should be written as "acute myeloid leukemia".
#' The query is constructed as "gene[field] AND disease[field] AND keywords".
#' For more details check the `easyPubMed` package documentation. For more complicated
#' queries, you can use the `easyPubMed` package directly or use the PubMed website directly.
#'
#' @examples
#' # Prepare the list of gene-disease pairs
#' feature_class_list <- list("acute myeloid leukemia" = c("FLT3", "EPO"),
#'                            "chronic lymphocytic leukemia" = c("PARP1"))
#'
#' # Run the literature search
#' lit_search_results <- hd_literature_search(feature_class_list, max_articles = 1)
#'
#' # Results for FLT3 in acute myeloid leukemia
#' lit_search_results$`acute myeloid leukemia`$FLT3
hd_literature_search <- function(feature_class_list,
                                 max_articles = 10,
                                 keywords = NULL,
                                 fields = "All Fields",
                                 api_key = NULL,
                                 verbose = TRUE) {

  # Ensure 'easyPubMed' package is loaded
  if (!requireNamespace("easyPubMed", quietly = TRUE)) {
    stop("The 'easyPubMed' package is required but not installed. Please install it using install.packages('easyPubMed').")
  }

  diseases <- names(feature_class_list)
  genes <- feature_class_list

  articles_database <- list()
  for (disease in diseases) {
    articles_database[[disease]] <- list()
    for (gene in genes[[disease]]) {

      query <- paste0(gene, '[', fields, '] AND ', disease, '[', fields, ']')
      if (!is.null(keywords)) {
        query <- paste0(query, ' AND ', keywords)
      }

      if (verbose) {
        message(paste0("Searching for articles on ", gene, " and ", disease))
      }

      ids <- easyPubMed::get_pubmed_ids(query, api_key = api_key)

      if (ids[["Count"]] == 0) {
        message(paste0("No articles found for ", gene, " and ", disease))
        next
      }

      tryCatch({
        abstracts_xml <- easyPubMed::fetch_pubmed_data(pubmed_id_list = ids, retmax = max_articles)
      }, error = function(e) {
        # Handle any errors during the query or processing
        message(paste0("An error occurred while searching for ", gene, " and ", disease, ": ", e$message))
      })

      if (is.null(abstracts_xml) || length(abstracts_xml) == 0) {
        message(paste0("No abstracts could be fetched for ", gene, " and ", disease))
        next
      }

      # Extract data and store them in a dataframe
      abstracts_list <- easyPubMed::articles_to_list(abstracts_xml)

      abstract_table <- abstracts_list |>
        purrr::map_df(function(abstract) {
          easyPubMed::article_to_df(pubmedArticle = abstract, autofill = FALSE) |>
            utils::head(1) |>
            dplyr::select(dplyr::all_of(c("pmid", "year", "journal", "firstname", "lastname", "title"))) |>
            dplyr::mutate(First_author = paste0(!!rlang::sym("lastname"), ", ", !!rlang::sym("firstname"))) |>
            dplyr::relocate(!!rlang::sym("First_author"), .before = !!rlang::sym("year")) |>
            dplyr::select(-dplyr::all_of(c("firstname", "lastname")))
        })
      articles_database[[disease]][[gene]] <- abstract_table
    }
  }
  return(articles_database)
}

