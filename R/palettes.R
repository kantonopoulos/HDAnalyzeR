#' HDAnalyzeR palettes
#'
#' `hd_palettes()` returns a list of palettes used by the Human Disease Blood Atlas resource of the Human Protein Atlas (HPA) project.
#'
#' @return List of HDAnalyzeR palettes.
#' @details
#' Some of the palettes are custom palettes created for the Human Disease Blood Atlas team.
#' Others are taken from the `ggsci` package, which provides palettes inspired by scientific journals.
#'
#' @export
#'
#' @examples
#' hd_palettes()
hd_palettes <- function() {

  palettes <- list(
    # Custom Palettes --------------------------------------------
    sex_basic = c("F" = "#C03830", "M" = "#317EC2"),

    sex = c("F" = "#8a72be", "M" = "#A9D0EF"),

    diff_exp = c("not significant" = "grey",
                 "significant down" = "#317EC2",
                 "significant up" = "#C03830"),

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

    # HPA Palettes -----------------------------------------------
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
              "Pediatric" = "#A6D854"),

    # ggsci palettes ----------------------------------------------
    # https://nanx.me/ggsci/
    nrc = c("Cinnabar" = "#E64B35",
            "Shakespeare" = "#4DBBD5",
            "PersianGreen" = "#00A087",
            "Chambray" = "#3C5488",
            "Apricot" = "#F39B7F",
            "WildBlueYonder" = "#8491B4",
            "MonteCarlo" = "#91D1C2",
            "Monza" = "#DC0000",
            "RomanCoffee" = "#7E6148",
            "Sandrift" = "#B09C85"),

    aaas = c("Chambray" = "#3B4992",
             "Red" = "#EE0000",
             "FunGreen" = "#008B45",
             "HoneyFlower" = "#631879",
             "Teal" = "#008280",
             "Monza" = "#BB0021",
             "ButterflyBush" = "#5F559B",
             "FreshEggplant" = "#A20056",
             "Stack" = "#808180",
             "CodGray" = "#1B1919"),

    nejm = c("TallPoppy" = "#BC3C29",
             "DeepCerulean" = "#0072B5",
             "Zest" = "#E18727",
             "Eucalyptus" = "#20854E",
             "WildBlueYonder" = "#7876B1",
             "Gothic" = "#6F99AD",
             "Salomie" = "#FFDC91",
             "FrenchRose" = "#EE4C97"),

    lancet = c("CongressBlue" = "#00468B",
               "Red" = "#ED0000",
               "Apple" = "#42B540",
               "BondiBlue" = "#0099B4",
               "TrendyPink" = "#925E9F",
               "MonaLisa" = "#FDAF91",
               "Carmine" = "#AD002A",
               "Edward" = "#ADB6B6",
               "CodGray" = "#1B1919"),

    jama = c("Limed Spruce" = "#374E55",
             "Anzac" = "#DF8F44",
             "Cerulean" = "#00A1D5",
             "Apple Blossom" = "#B24745",
             "Acapulco" = "#79AF97",
             "Kimberly" = "#6A6599",
             "Makara" = "#80796B"),

    bmj = c("Blue" = "#2A6EBB",
            "Yellow" = "#F0AB00",
            "Pink" = "#C50084",
            "Purple" = "#7D5CC6",
            "Orange" = "#E37222",
            "Green" = "#69BE28",
            "Aqua" = "#00B2A9",
            "Red" = "#CD202C",
            "Grey" = "#747678"),

    jco = c("Lochmara" = "#0073C2",
            "Corn" = "#EFC000",
            "Gray" = "#868686",
            "ChestnutRose" = "#CD534C",
            "Danube" = "#7AA6DC",
            "RegalBlue" = "#003C67",
            "Olive" = "#8F7700",
            "MineShaft" = "#3B3B3B",
            "WellRead" = "#A73030",
            "KashmirBlue" = "#4A6990"),

    igv = c("chr1" = "#5050FF",
            "chr2" = "#CE3D32",
            "chr3" = "#749B58",
            "chr4" = "#F0E685",
            "chr5" = "#466983",
            "chr6" = "#BA6338",
            "chr7" = "#5DB1DD",
            "chr8" = "#802268",
            "chr9" = "#6BD76B",
            "chr10" = "#D595A7",
            "chr11" = "#924822",
            "chr12" = "#837B8D",
            "chr13" = "#C75127",
            "chr14" = "#D58F5C",
            "chr15" = "#7A65A5",
            "chr16" = "#E4AF69",
            "chr17" = "#3B1B53",
            "chr18" = "#CDDEB7",
            "chr19" = "#612A79",
            "chr20" = "#AE1F63",
            "chr21" = "#E7C76F",
            "chr22" = "#5A655E",
            "chrX" = "#CC9900",
            "chrY" = "#99CC00",
            "chrUn" = "#A9A9A9",
            "chr23" = "#CC9900",
            "chr24" = "#99CC00",
            "chr25" = "#33CC00",
            "chr26" = "#00CC33",
            "chr27" = "#00CC99",
            "chr28" = "#0099CC",
            "chr29" = "#0A47FF",
            "chr30" = "#4775FF",
            "chr31" = "#FFC20A",
            "chr32" = "#FFD147",
            "chr33" = "#990033",
            "chr34" = "#991A00",
            "chr35" = "#996600",
            "chr36" = "#809900",
            "chr37" = "#339900",
            "chr38" = "#00991A",
            "chr39" = "#009966",
            "chr40" = "#008099",
            "chr41" = "#003399",
            "chr42" = "#1A0099",
            "chr43" = "#660099",
            "chr44" = "#990080",
            "chr45" = "#D60047",
            "chr46" = "#FF1463",
            "chr47" = "#00D68F",
            "chr48" = "#14FFB1"),

    uchicago = c("Maroon" = "#800000",
                 "DarkGray" = "#767676",
                 "Yellow" = "#FFA319",
                 "LightGreen" = "#8A9045",
                 "Blue" = "#155F83",
                 "Orange" = "#C16622",
                 "Red" = "#8F3931",
                 "DarkGreen" = "#58593F",
                 "Violet" = "#350E20")
    )

  return(palettes)
}


#' Display available palettes
#'
#' `hd_show_palettes()` displays a grid of palettes.
#'
#' @param palettes A list of palettes. Defaults to all palettes from `hd_palettes()`.
#' @param n The number of colors to show for each palette. If `NULL`, all colors are shown.
#'
#' @return A ggplot2 plot showing the palettes.
#' @export
#'
#' @examples
#' hd_show_palettes()
hd_show_palettes <- function(palettes = hd_palettes(), n = NULL) {

  # Prepare a data frame with all palette information
  palette_data <- do.call(rbind, lapply(names(palettes), function(palette_name) {
    colors <- palettes[[palette_name]]
    if (!is.null(n)) colors <- colors[seq_len(min(n, length(colors)))] # Optionally limit number of colors
    data.frame(
      palette = palette_name,
      color = as.character(colors),
      label = as.character(colors),
      position = seq_along(colors),
      stringsAsFactors = FALSE
    )
  }))

  # Plot the palettes
  palette_data |>
    ggplot2::ggplot(ggplot2::aes(x = !!rlang::sym("position"),
                                 y = !!rlang::sym("palette"),
                                 fill = !!rlang::sym("color"))) +
      ggplot2::geom_tile(color = "white", size = 1) +
      ggplot2::geom_text(ggplot2::aes(label = !!rlang::sym("label")),
                                      color = "black",
                                      size = 3,
                                      vjust = 0.5,
                                      hjust = 0.5,
                                      na.rm = TRUE,
                                      angle = 90) +
      ggplot2::scale_fill_identity() +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, face = "bold"),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     plot.margin = ggplot2::margin(5, 10, 5, 10)) +
      ggplot2::labs(x = NULL, y = NULL)
}


#' HDAnalyzeR color scales
#'
#' `scale_color_hd()` creates a ggplot2 scale for color aesthetics using the HDAnalyzeR
#' palettes.
#'
#' @param palette The name of the palette to use. It should be one of the palettes from `hd_palettes()`.
#'
#' @return A ggplot2 scale for color aesthetics.
#' @export
#'
#' @examples
#' # Create an example dataframe
#' data <- data.frame(
#'   var1 = seq_len(10),
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
#' plot + scale_color_hd("sex")
scale_color_hd <- function(palette) {
  hpa_palettes <- hd_palettes()

  if (!palette %in% names(hpa_palettes)) {
    stop("Palette not found. Available palettes are: ", paste(names(hpa_palettes), collapse = ", "))
  }

  ggplot2::scale_color_manual(values = hpa_palettes[[palette]], na.value = "grey50")
}


#' HDAnalyzeR fill scales
#'
#' `scale_fill_hd()` creates a ggplot2 scale for fill aesthetics using the HDAnalyzeR
#' palettes.
#'
#' @param palette The name of the palette to use. It should be one of the palettes from `hd_palettes()`.
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
#' plot + scale_fill_hd("sex")
scale_fill_hd <- function(palette) {
  hpa_palettes <- hd_palettes()

  if (!palette %in% names(hpa_palettes)) {
    stop("Palette not found. Available palettes are: ", paste(names(hpa_palettes), collapse = ", "))
  }

  ggplot2::scale_fill_manual(values = hpa_palettes[[palette]], na.value = "grey50")
}
