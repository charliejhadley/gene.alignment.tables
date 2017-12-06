ui <- fluidPage(h1("multiple"),
                uiOutput("multiple_selected_cell"),
                uiOutput("shadowy_watchguard"),
                uiOutput("the_tables"))

iris_slice_dt <- function(
  data,
  table.width = 5,
  slice.position
){
  
  element.id <- paste0("multiple_table","_",slice.position)
  
  iris %>%
    slice({
      slice.position - table.width
    }:slice.position) %>%
    datatable(options = list(dom = "t"),
              selection = list(target = 'column', mode = 'multiple'),
              # elementId = paste0("x", "_", x)
              elementId = paste0("multiple_table","_",slice.position),
              height = "100%"
    )
}

server <- function(input, output) {

  
  table_reactive_ids <- 
  
  output$the_tables <- renderUI({
    fluidPage(lapply(c(5, 10, 15),
                     function(x) {
                       
                      iris %>%
                         iris_slice_dt(slice.position = x)
                       
                     }))
  })
  
  output$shadowy_watchguard <- renderUI({
    
    if(is.null(input$multiple_table_5_columns_selected)){
      return("Shadowy watchguard sees nothing")
    }
    
    
    print({
      
      the_names <- names(input)
      
      the_names[grepl("columns_selected", the_names)]
      
    })
    
    
    
    "SHADOWY WATCHGUARD"
    
  })
  
}

shinyApp(ui,
         server)