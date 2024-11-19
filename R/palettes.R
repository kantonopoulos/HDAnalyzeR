#' HPA color palettes
#'
#' `get_hpa_palettes()` returns a list of color palettes used by the Human Protein Atlas (HPA) project.
#'
#' @return List of HPA color palettes.
#' @export
#'
#' @examples
#' get_hpa_palettes()
get_hpa_palettes <- function() {

  palettes <- list(
    sex = c("F" = "red", "M" = "blue"),

    sex_hpa = c("F" = "#8a72be", "M" = "#A9D0EF"),

    diff_exp = c("not significant" = "grey",
                 "significant down" = "blue",
                 "significant up" = "red"),

    cancers12 = c("AML" = "#A6CEE3",
                  "CLL" = "#2271B5",
                  "LYMPH" = "#08585A",
                  "MYEL" = "#66C2A5",
                  "CRC" = "#B89B74",
                  "LUNGC" = "#ADC74F",
                  "GLIOM" = "#FFD321",
                  "BRC" = "#E8A29A",
                  "CVX" = "#9E0142",
                  "ENDC" = "#B195AE",
                  "OVC" = "#603479",
                  "PRC" = "#E7662B"),

    cancers15 = c("AML" = "#A6CEE3",
                  "CLL" = "#2271B5",
                  "LYMPH" = "#08585A",
                  "MYEL" = "#66C2A5",
                  "CRC" = "#B89B74",
                  "LUNGC" = "#ADC74F",
                  "GLIOM" = "#FFD321",
                  "BRC" = "#E8A29A",
                  "CVX" = "#9E0142",
                  "ENDC" = "#B195AE",
                  "OVC" = "#603479",
                  "PRC" = "#E7662B",
                  "MENI" = "#FFFF80",
                  "SI-NET" = "#504538",
                  "PIT-NET" = "#FFFF00"),

    secreted = c("Secreted to blood" = "#B30000",
                 "Secreted in brain" = "#FFDD00",
                 "Secreted to digestive system" = "#1280C4",
                 "Secreted in male reproductive system" = "#95D4F5",
                 "Secreted in female reproductive system" = "#F8BDD7",
                 "Secreted to extracellular matrix"  = "#7F6A9C",
                 "Secreted in other tissues" = "#FFD480",
                 "Secreted - unknown location" = "#A1A8AA",
                 "Intracellular and membrane" = "#F9A266",
                 "Unknown" = "grey80"),

    specificity = c( "Tissue enriched" = "#e41a1c",
                     "Group enriched" = "#FF9D00",
                     "Tissue enhanced" = "#984ea3",
                     "Low tissue specificity" = "grey40",
                     "not detected " = "grey"),

    # Disease Atlas class
    class = c("Healthy" = "#B3B3B3",
              "Cardiovascular" = "#FC8D62",
              "Metabolic" = "#E5C494",
              "Cancer" = "#8DA0CB",
              "Psychiatric" = "#66C2A5",
              "Autoimmune" = "#E78AC3",
              "Infection" = "#FFD92F",
              "Pediatric" = "#A6D854")
  )

  return(palettes)
}


#' HPA color scales
#'
#' `scale_color_hpa()` creates a ggplot2 scale for color aesthetics using the color
#' palettes from the Human Protein Atlas (HPA) project.
#'
#' @param palette The name of the palette to use. It should be one of the palettes from `get_hpa_palettes()`.
#'
#' @return A ggplot2 scale for color aesthetics.
#' @export
#'
#' @examples
#' # Create an example dataframe
#' data <- data.frame(
#'   var1 = 1:10,
#'   var2 = seq(2, 20, by = 2),
#'   Sex = rep(c("M", "F"), each = 5)
#' )
#'
#' # Create a plot
#' plot <- ggplot2::ggplot(data, ggplot2::aes(x = var1, y = var2, color = Sex)) +
#'   ggplot2::geom_point()
#' plot
#'
#' # Add a custom palette
#' plot + scale_color_hpa("sex_hpa")
scale_color_hpa <- function(palette) {
  hpa_palettes <- get_hpa_palettes()

  if (!palette %in% names(hpa_palettes)) {
    stop("Palette not found. Available palettes are: ", paste(names(hpa_palettes), collapse = ", "))
  }

  ggplot2::scale_color_manual(values = hpa_palettes[[palette]])
}


#' HPA fill scales
#'
#' `scale_fill_hpa()` creates a ggplot2 scale for fill aesthetics using the color
#' palettes from the Human Protein Atlas (HPA) project.
#'
#' @param palette The name of the palette to use. It should be one of the palettes from `get_hpa_palettes()`.
#'
#' @return A ggplot2 scale for fill aesthetics.
#' @export
#'
#' @examples
#' # Create an example dataframe
#' data <- data.frame(
#'   Sex = c("M", "F"),
#'   Count = c(60, 40)
#' )
#'
#' # Create a plot
#' plot <- ggplot2::ggplot(data, ggplot2::aes(x = Sex, y = Count, fill = Sex)) +
#'   ggplot2::geom_bar(stat = "identity", position = "dodge")
#' plot
#'
#' # Add a custom palette
#' plot + scale_fill_hpa("sex_hpa")
scale_fill_hpa <- function(palette) {
  hpa_palettes <- get_hpa_palettes()

  if (!palette %in% names(hpa_palettes)) {
    stop("Palette not found. Available palettes are: ", paste(names(hpa_palettes), collapse = ", "))
  }

  ggplot2::scale_fill_manual(values = hpa_palettes[[palette]])
}
