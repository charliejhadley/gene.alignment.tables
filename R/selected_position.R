#' id_to_sequence_positon
#'
#' \code{id_to_sequence_positon} converts selected columns in the gene.alignment.tables visualisation to positions in the original sequence dataset.
#' 
#' @param id InputID of the format aDTXXXXX_n_m_columns_selected from the \code{datatables} generated for the gene.alignment.table visualisation
#' @param shiny.input The input object of the Shiny app
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' ## create a set of reactiveValues to store the selected column ids
#' selected_col_values <- reactiveValues() 
#' 
#' ## observe all input values matching the format aDTXXXXX_n_m_columns_selected
#' observe({
#' if (!is.null(input[[paste0(alignment.dt.unique.id,
#'                            "_1_",
#'                            table_width,
#'                            "_rows_current")]])) {
#'   selected_col_values[["previous"]] <-
#'     isolate(selected_col_values[["current"]])
#'   
#'   all_inputs <- isolate(reactiveValuesToList(input))
#'   
#'   inputs_selected_cols <-
#'     grepl(
#'       paste0(
#'         alignment.dt.unique.id,
#'         "_[0-9]{1,}_[0-9]{1,}_columns_selected"
#'       ),
#'       names(all_inputs)
#'     )
#'   
#'   inputs_with_nulls <- all_inputs[inputs_selected_cols]
#'   
#'  inputs_selected_cols <-
#'    setNames(inputs_with_nulls, names(all_inputs)[inputs_selected_cols])
#'  
#'   selected_positions <-
#'     lapply(names(inputs_selected_cols), function(id) {
#'       id_to_sequence_position(id, shiny.input = input)
#'     }) %>%
#'     unlist()
#'   
#'   selected_positions
#' } else {
#'   if (is.null(selected_col_values[["current"]])){
#'     selected_positions <- NULL
#'   }
#'   else {
#'     selected_positions <- selected_col_values[["current"]]
#'   }
#' }
#' 
#' selected_col_values[["current"]] <- selected_positions
#' })
#' 
#' }
#' 
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
