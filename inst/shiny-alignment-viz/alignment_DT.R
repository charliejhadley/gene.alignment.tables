alignment_data <- function(data,
                           table.width = 20,
                           slice.position,
                           return.data = FALSE) {
  if (nrow(data) < table.width) {
    
    padding_table <- data[0,]
    
    padding_table <- padding_table %>%
      add_row(position = {
        max(data$position) + 1
      }:slice.position)
    
    long_data <- data %>%
      bind_rows(padding_table) %>%
      mutate(position = formatC(
        position,
        width = 3,
        format = "d",
        flag = "0"
      ))
    
  } else {
    long_data <- data %>%
      mutate(position = formatC(
        position,
        width = 3,
        format = "d",
        flag = "0"
      ))
  }

  long_data
  
}

foo_one <- Sys.time()
foo_two <- Sys.time()
colour_timer <- foo_two - foo_one

alignment_DT <- function(data,
                         table.width = 20,
                         slice.position,
                         return.data = FALSE) {
  
  long_data <- alignment_data(data,
                              table.width,
                              slice.position,
                              return.data)
  
  col_colours <- long_data %>%
    select(colour) %>%
    mutate(col.index = row_number() + 1)
  
  tr_data <- long_data %>%
    select(-colour) %>%
    gather(., foo, val, 2:ncol(.)) %>%
    spread(position, val) %>%
    mutate_all(funs(coalesce(., "-")))
  
  colnames(tr_data) <- gsub("foo", "Position", colnames(tr_data))

  the_dt <- tr_data %>%
    datatable(
      colnames = lapply(colnames(tr_data),
                        function(x) {
                          if (x != "Position") {
                            paste0("<h6 style='transform:rotate(270deg);'>", x, "</h6>")
                          } else {
                          "Position"
                          }
                          
                          
                          
                        }) %>%
        as.character(),
      escape = FALSE,
      rownames = FALSE,
      options = list(
        dom = "t",
        ordering = F,
        autoWidth = TRUE,
        initComplete = JS(
          "function(settings, json) {",
          "var headerBorder = [0,1];",
          "var header = $(this.api().table().header()).find('tr:first > th').filter(function(index) {return $.inArray(index,headerBorder) > -1 ;}).addClass('cell-border-right');",
          "}"
        ),
        columnDefs = list(
          # list(width = '40px', targets = 1),
          list(className = "dt-right cell-border-right", targets = 1:table.width)
        )
      ),
      selection = list(target = 'column', mode = 'multiple'),
      elementId = paste0(
        "alignmentDT_",
        slice.position - table.width + 1,
        "_",
        slice.position
      ),
      height = "100%"
    )
  
  time_before_colours <- Sys.time()
  lapply(unique(col_colours$colour),
         function(col_colour){
           
           col_indices <- col_colours %>%
             filter(colour == col_colour) %>%
             select(col.index) %>%
             .[[1]]

           the_dt <<- the_dt %>%
             formatStyle(columns = col_indices,
                         backgroundColor = col_colour)
         }
  )
  time_after_colours <- Sys.time()
  
  colour_timer <<- colour_timer + {time_after_colours - time_before_colours}
  
  the_dt
  
}


generate_dts <- function(data,
                         table.width = 15){
  
  table_width <- table.width
  table_width_1 <- table_width - 1
  
  lapply(seq(
    table_width,
    ceiling(max(data$position) / table_width) * table_width,
    table_width
  ),
  function(x) {
    data %>%
      slice({
        x - table_width_1
      }:x) %>%
      alignment_DT(table.width = table_width,
                   slice.position = x)
    
  })
}





generate_selected_col_ids <- function(data,
                                table.width = 15){
  
  table_width <- table.width
  table_width_1 <- table_width - 1
  
  
  lapply(seq(
    table_width,
    ceiling(max(data$position) / table_width) * table_width,
    table_width
  ), function(x){
    paste0(
      "alignmentDT_",
      x - table.width + 1,
      "_",
      x,
      "_columns_selected"
    )
  }) %>%
    as.character()
  
}


id_to_sequence_positon <- function(id) {
  
  rows_in_data <-
    stringr::str_match_all(id, "[0-9]+") %>%
    unlist() %>%
    as.integer()
  
  rows_in_data <- rows_in_data[[1]]:rows_in_data[[2]]
  
  col_in_table <- input[[id]]
  
  if(is.null(col_in_table)){
    NULL
  } else {
    rows_in_data[col_in_table]
  }
    
}


# # 
# input <- list(alignmentDT_1_15_columns_selected = c(5,7),
#               alignmentDT_16_30_columns_selected = NULL,
#               alignmentDT_31_45_columns_selected = 2)
# 
# 
# id_to_sequence_positon("alignmentDT_16_30_columns_selected")
# 
# lapply(names(input), function(id){
#     id_to_sequence_positon(id)
# }) %>%
#   unlist()


# 
# rows_in_data <- stringr::str_match_all("alignmentDT_1_15_columns_selected", "[0-9]+") %>% 
#   unlist() %>%
#   as.integer()
# 
# dummy_data %>%
#   slice(
#     rows_in_data[[1]]:rows_in_data[[2]]
#   ) 
# 
# 
# 
# 
# 
# 
# dummy_data %>%
#   alignment_data(table.width = 15,
#                  slice.position = 15)
# 
# 
