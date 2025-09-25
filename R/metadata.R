#' Cancer cohort metadata
#'
#' A data subset from synthetic cancer metadata. DAid, Age, BMI, and Cohort have
#' been added as extra columns. The original dataset was processed with the
#' `process_example_metadata` script.
#'
#' @source <https://github.com/buenoalvezm/Pan-cancer-profiling/blob/main/data/cancer_metadata_synthetic.rds>
#' @format
#' A tibble with 586 rows and 9 columns:
#' \describe{
#'   \item{DAid}{The Disease Atlas sample ID}
#'   \item{Sample}{The Sample ID}
#'   \item{Disease}{The cancer type}
#'   \item{Stage}{The cancer stage}
#'   \item{Grade}{The cancer grade}
#'   \item{Sex}{The patient sex}
#'   \item{Age}{The patient age}
#'   \item{BMI}{The patient BMI}
#'   \item{Cohort}{The cohort the patient belongs to}
#' }
#' @examples
#' \dontrun{
#' example_metadata
#' }
"example_metadata"
