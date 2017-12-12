library("shiny")
library("rhandsontable")
library("tidyverse")
library("DT")
library("shinyjs")
library("gene.alignment.tables")

table_width <- 15

function(input, output, session) {
  
  alignment.dt.unique.id <- alignment_DT_unique_id()
  
  output$sequence_alignment_UI <- renderUI({
    
    the_datatables <- hbv_long_s_sequence %>%
      generate_dts(table.width = table_width,
                   alignment.table.id = alignment.dt.unique.id)
    
    fluidPage(
      the_datatables
    )
    
  })
  
  output$sequence_coding_region_legend <- renderPlot({
    
    coding_region_legend(coding_region_colours)
    
  })
  
  selected_col_values <- reactiveValues()
  
  observe({
    if (!is.null(input[[paste0(alignment.dt.unique.id,
                               "_1_",
                               table_width,
                               "_rows_current")]])) {
      selected_col_values[["previous"]] <-
        isolate(selected_col_values[["current"]])
      
      all_inputs <- isolate(reactiveValuesToList(input))
      
      inputs_selected_cols <-
        grepl(
          paste0(
            alignment.dt.unique.id,
            "_[0-9]{1,}_[0-9]{1,}_columns_selected"
          ),
          names(all_inputs)
        )

      inputs_with_nulls <- all_inputs[inputs_selected_cols]
      
      inputs_selected_cols <-
        setNames(inputs_with_nulls, names(all_inputs)[inputs_selected_cols])

      selected_positions <-
        lapply(names(inputs_selected_cols), function(id) {
          id_to_sequence_position(id, shiny.input = input)
        }) %>%
        unlist()

      selected_positions
      

    } else {
      if (is.null(selected_col_values[["current"]])){
        selected_positions <- NULL
      }
      else {
        selected_positions <- selected_col_values[["current"]]
      }
    }
    
    selected_col_values[["current"]] <- selected_positions
  })
  
  output$observe_show_inputs <- DT::renderDataTable({
    
    
    selected_positions <- selected_col_values[["current"]] %>%
      sort()
    
    if (is.null(selected_positions)) {
      
      hbv_long_s_sequence[0,] %>%
        datatable()
    } 
    
    hbv_long_s_sequence %>%
      filter(position %in% selected_positions) %>%
      datatable()

  })
  
  
  
}