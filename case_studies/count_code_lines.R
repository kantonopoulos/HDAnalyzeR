library(knitr)
library(stringr)
library(dplyr)

count_code_lines <- function(rmd_file) {
  # Count lines of code
  code_chunks <- knitr::purl(rmd_file, output = tempfile(), quiet = TRUE)
  code_lines <- readLines(code_chunks)
  clean_lines <- code_lines[!str_detect(code_lines, "^\\s*$|^\\s*#")]
  line_count <- length(clean_lines)

  # Extract package imports
  pkg_calls <- str_extract(clean_lines, "(?<=library\\()[^)]+|(?<=require\\()[^)]+")
  pkg_calls <- na.omit(pkg_calls)
  pkg_calls <- trimws(pkg_calls)

  # Extract function calls (heuristic: word followed by parenthesis)
  func_calls <- unlist(str_extract_all(clean_lines, "\\b[a-zA-Z0-9_]+(?=\\()"))
  func_calls <- func_calls[!func_calls %in% c("if", "for", "while", "function")]  # remove control keywords

  summary <- list(
    "file" = basename(rmd_file),
    "lines_of_code" = line_count,
    "unique_packages" = unique(pkg_calls),
    "num_packages" = length(unique(pkg_calls)),
    "num_function_calls" = length(func_calls),
    "unique_functions" = length(unique(func_calls))
  )

  return(summary)
}

hdanalyzer_proteomics_file <- "blood_cancers_olink_case_study.Rmd"
hdanalyzer_proteomics_summary <- count_code_lines(hdanalyzer_proteomics_file)

print("Summary of HDAnalyzer Proteomics Case Study:")
print(hdanalyzer_proteomics_summary)

manual_proteomics_file <- "manual_blood_cancers_olink_case_study.Rmd"
manual_proteomics_summary <- count_code_lines(manual_proteomics_file)

print("Summary of Manual Proteomics Case Study:")
print(manual_proteomics_summary)

hdanalyzer_transcriptomics_file <- "solid_cancers_transcriptomics_case_study.Rmd"
hdanalyzer_transcriptomics_summary <- count_code_lines(hdanalyzer_transcriptomics_file)

print("Summary of HDAnalyzer Transcriptomics Case Study:")
print(hdanalyzer_transcriptomics_summary)

manual_transcriptomics_file <- "manual_solid_cancers_transcriptomics_case_study.Rmd"
manual_transcriptomics_summary <- count_code_lines(manual_transcriptomics_file)

print("Summary of Manual Transcriptomics Case Study:")
print(manual_transcriptomics_summary)