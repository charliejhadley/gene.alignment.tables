#' generate_selected_col_ids
#'
#' \code{generate_selected_col_ids} generate selected col ids for selected position
#' 
#' @importFrom dplyr add_row coalesce
#' @importFrom stringr str_match_all
#'
#' @param data A data.frame with start-end pairs, needs the following columns
#' \itemize{
#'  \item{"position"}{ : sequence position, these must be sequential}
#'  \item{"colour"}{ : colour for the sequence in the gene.alignment.tables visualisation}
#'  \item{"..."}{ : any other columns will be displayed in the visualisation directly below the position number}
#'  }
#'
#' @param table.width the width of the gene.alignment.tables, defaults to 20 positions.
#' @param slice.position
#'

generate_selected_col_ids <- function(data,
                                      table.width = 15,
                                      alignment.dt.unique.id){
  
  table_width <- table.width
  table_width_1 <- table_width - 1
  
  
  lapply(seq(
    table_width,
    ceiling(max(data$position) / table_width) * table_width,
    table_width
  ), function(x){
    paste0(
      alignment.dt.unique.id,
      "_",
      x - table.width + 1,
      "_",
      x,
      "_columns_selected"
    )
  }) %>%
    as.character()
  
}

#' id_to_sequence_positon
#'
#' \code{id_to_sequence_positon} generate selected col ids for selected position
#'
#' @param id
#' @export

id_to_sequence_position <- function(id, shiny.input) {
  
  rows_in_data <-
    str_match_all(id, "[0-9]+") %>%
    unlist() %>%
    as.integer()
  
  rows_in_data <- rows_in_data[[1]]:rows_in_data[[2]]
  
  col_in_table <- shiny.input[[id]]
  
  if(is.null(col_in_table)){
    NULL
  } else {
    rows_in_data[col_in_table]
  }
  
}