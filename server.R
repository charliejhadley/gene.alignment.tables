library("plyr")
library("shiny")
library("rhandsontable")
library("tidyverse")
library("DT")

dummy_data <- read_csv("data-raw/example_row.csv")

id_of_alignment_table <- "alignmentDT"
table_width <- 15

function(input, output, session) {
  source("alignment_DT.R", local = TRUE)$value
  
  output$programmatic_many_DT_UI <- renderUI({

    fluidPage(dummy_data %>%
                generate_dts(table.width = table_width))
    
  })
  
  selected_col_values <- reactiveValues()

  observe({
    if (!is.null(input[[paste0(id_of_alignment_table,"_1_",table_width,"_rows_current")]])) {
      selected_col_values[["previous"]] <-
        isolate(selected_col_values[["current"]])
      
      all_inputs <- isolate(reactiveValuesToList(input))
      
      inputs_selected_cols <-
        grepl(
          paste0(
            id_of_alignment_table,
            "_[0-9]{1,}_[0-9]{1,}_columns_selected"
          ),
          names(all_inputs)
        )
      
      inputs_with_nulls <- all_inputs[inputs_selected_cols]
      
      inputs_selected_cols <-
        setNames(inputs_with_nulls, names(all_inputs)[inputs_selected_cols])
      
      rows_selected <-
        lapply(names(inputs_selected_cols), function(id) {
          id_to_sequence_positon(id)
        }) %>%
        unlist()
      
      rows_selected
      
    } else {
      if (is.null(selected_col_values[["current"]]))
        rows_selected <- NULL
      else
        rows_selected <- selected_col_values[["current"]]
    }
    selected_col_values[["current"]] <- rows_selected
  })

  
  output$observe_show_inputs <- renderDataTable({
    data_rows_selected <- selected_col_values[["current"]]
    
    if (is.null(data_rows_selected)) {
      return()
    }
    
    print(dummy_data[sort(data_rows_selected), ])
    
    dummy_data[sort(data_rows_selected), ] %>%
      datatable()
    
    # print(AllInputs())
    # columns_selected()
  })
  

  
}