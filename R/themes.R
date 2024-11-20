#' HPA theme
#'
#' `theme_hpa()` creates a theme for ggplot2 plots that is used by the Human Protein Atlas (HPA) project.
#'
#' @param angled The angle of the x-axis text. Default is 0.
#' @param axis_x If FALSE, the x-axis is removed. Default is TRUE.
#' @param axis_y If FALSE, the y-axis is removed. Default is TRUE.
#' @param facet_title If FALSE, the facet title is removed. Default is TRUE.
#'
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' # Create a plot
#' plot <- example_metadata |>
#'   ggplot2::ggplot(ggplot2::aes(x = Sex)) +
#'   ggplot2::geom_bar()
#' plot
#'
#' # Apply the HPA theme
#' plot + theme_hd()
theme_hd <- function(angled = 0, axis_x = TRUE, axis_y = TRUE, facet_title = TRUE) {

    t <- ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
               panel.grid.minor = ggplot2::element_blank(),
               panel.spacing = ggplot2::unit(0.2, "lines"),
               panel.background = ggplot2::element_rect(fill = "white"),
               panel.border = ggplot2::element_blank(),
               plot.title = ggplot2::element_text(face = "bold",
                                         size = ggplot2::rel(1),
                                         hjust = 0.5),
               plot.subtitle = ggplot2::element_text(face = "bold",
                                            hjust = 0.5,
                                            size = ggplot2::rel(1),
                                            vjust = 1),
               axis.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1)),
               axis.ticks.length = ggplot2::unit(.25, "cm"),
               axis.line = ggplot2::element_line(linewidth = 0.5),
               axis.text = ggplot2::element_text(size = ggplot2::rel(1), color = 'black'),
               legend.key = ggplot2::element_blank(),
               legend.position = "right",
               legend.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
               legend.key.size = ggplot2::unit(0.7, "cm"),
               legend.title = ggplot2::element_text(size = ggplot2::rel(1)),
               plot.margin = ggplot2::unit(c(10, 5, 5, 5), "mm"),
               strip.background = ggplot2::element_rect(colour = "grey90", fill = "grey90"),
               strip.text = ggplot2::element_text(face = "bold")) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = angled, vjust = 0.5, hjust = 1))

    if (axis_x == F) {
      t <- t + ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.line.x = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank())
    }

    if (axis_y == F) {
      t <- t + ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     axis.line.y = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank())
    }
    if (facet_title == F) {
      t <- t + ggplot2::theme(strip.text = ggplot2::element_blank())
    }
    return(t)
  }
