#' coding_region_legend
#'
#' \code{coding_region_legend} creates a very simplistic legend of the coding regions using ggplot2.
#' 
#' @param data A data.frame containing gene sequence data, which must contain the following columns:
#' \itemize{
#'  \item{"label"}{ : label for the coding region}
#'  \item{"colour"}{ : colour for the coding region}
#'  }
#' 
#' @export 

coding_region_legend <- function(data){
  
  max_rows <- nrow(data)
  
  data %>%
    mutate(
      index = row_number(),
      xmin = index,
      xmax = index + 1,
      ymin = 1,
      ymax = 2
    ) %>%
    ggplot() +
    geom_rect(aes(
      xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
      fill = colour
    )) +
    geom_text(aes(x = xmin + (xmax - xmin) / 2, y = ymin + (ymax - ymin) / 2, label = str_wrap(label, width = 1)), size = 6) +
    scale_fill_manual(
      values = coding_region_colours$colour,
      name = "",
      breaks = coding_region_colours$label,
      labels = coding_region_colours$colour
    ) +
    scale_x_continuous(limits = c(1, max_rows), expand = c(0,0)) +
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

