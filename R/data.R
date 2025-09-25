#' Cancer cohort Olink data
#'
#' A data subset from a synthetic cancer dataset keeping the first 100 Assays.
#' DAid, Assay_Warning, QC_Warning, and PlateID have been added as extra columns.
#' The original dataset was processed with the `process_example_data` script.
#'
#' @source <https://github.com/buenoalvezm/Pan-cancer-profiling/blob/main/data/cancer_data_synthetic.rds>
#' @format
#' A tibble with 56,142 rows and 10 columns:
#' \describe{
#'   \item{DAid}{The Disease Atlas sample ID}
#'   \item{Sample}{The Sample ID}
#'   \item{OlinkID}{The Olink Assay ID}
#'   \item{UniProt}{The UniProt Assay ID}
#'   \item{Assay}{The Assay name}
#'   \item{Panel}{The Olink Panel in which the Assay belongs to}
#'   \item{NPX}{The NPX value for each Sample and Assay}
#'   \item{Assay_Warning}{The Assay warning status for the sample}
#'   \item{QC_Warning}{The QC warning status for the sample}
#'   \item{PlateID}{The ID of the plate where the sample was processed}
#' }
#' @examples
#' \dontrun{
#' example_data
#' }
"example_data"
