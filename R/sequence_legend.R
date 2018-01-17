#' coding_region_legend
#'
#' \code{coding_region_legend} creates a very simplistic legend of the coding regions using ggplot2.
#' 
#' @importFrom ggplot2 theme_bw theme element_blank ggplot geom_rect geom_text scale_fill_manual scale_x_continuous scale_y_continuous
#' @importFrom forcats fct_reorder
#' 
#' @param data A data.frame containing gene sequence data, which must contain the following columns:
#' \itemize{
#'  \item{"label"}{ : label for the coding region}
#'  \item{"colour"}{ : colour for the coding region}
#'  }
#' @param font.size Font size of labels in the legend, default to 6.
#' 
#' @export 

coding_region_legend <- function(data, font.size = 6){
  
  max_rows <- nrow(data)
  
  data %>%
    mutate(
      index = row_number(),
      xmin = index,
      xmax = index + 1,
      ymin = 1,
      ymax = 2
    ) %>%
    mutate(label = fct_reorder(label, index),
           colour = fct_reorder(colour, index)) %>%
    ggplot() +
    geom_rect(aes(
      xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
      fill = colour
    )) +
    geom_text(aes(x = xmin + (xmax - xmin) / 2, y = ymin + (ymax - ymin) / 2, label = str_wrap(label, width = 1)), size = font.size) +
    scale_fill_manual(
      values = data$colour,
      name = "",
      breaks = data$label,
      labels = data$colour
    ) +
    scale_x_continuous(limits = c(1, max_rows + 1), expand = c(0,0)) +
    scale_y_continuous(limits = c(1, 2), expand = c(0,0)) +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()
    )
}

