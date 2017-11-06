alignment_data <- function(data,
                           table.width = 20,
                           slice.position,
                           return.data = FALSE) {
  if (nrow(data) < table.width) {
    padding_table <-
      tibble(
        Position = {
          max(data$Position) + 1
        }:slice.position,
        Reference = NA,
        `Geno A` = NA,
        species = NA
      )
    
    long_data <- data %>%
      bind_rows(padding_table) %>%
      mutate(Position = formatC(
        Position,
        width = 3,
        format = "d",
        flag = "0"
      )) %>%
      mutate(colour = plyr::mapvalues(
        region,
        from = c(
          "Terminal protein",
          "Spacer",
          "Reverse transcriptase",
          "RNAse H"
        ),
        to = c("red", "blue", "green", "orange")
      ))
    
    
  } else {
    long_data <- data %>%
      mutate(Position = formatC(
        Position,
        width = 3,
        format = "d",
        flag = "0"
      )) %>%
      mutate(colour = plyr::mapvalues(
        region,
        from = c(
          "Terminal protein",
          "Spacer",
          "Reverse transcriptase",
          "RNAse H"
        ),
        to = c("red", "blue", "green", "orange")
      ))
  }
  
  long_data
  
}

alignment_DT <- function(data,
                         table.width = 20,
                         slice.position,
                         return.data = FALSE) {
  
  long_data <- alignment_data(data,
                              table.width,
                              slice.position,
                              return.data)
  
  spacer_cols <- long_data %>%
    filter(region == "Spacer") %>%
    select(Position) %>%
    .[[1]]
  terminalp_cols <- long_data %>%
    filter(region == "Terminal protein") %>%
    select(Position) %>%
    .[[1]]
  reverset_cols <- long_data %>%
    filter(region == "Reverse transcriptase") %>%
    select(Position) %>%
    .[[1]]
  rnase_cols <- long_data %>%
    filter(region == "RNAse H") %>%
    select(Position) %>%
    .[[1]]
  
  tr_data <- long_data %>%
    select(-colour, -region) %>%
    gather(., foo, val, 2:ncol(.)) %>%
    spread(Position, val) %>%
    mutate_all(funs(coalesce(., "-")))

  tr_data %>%
    datatable(
      colnames = lapply(colnames(tr_data),
                        function(x) {
                          if (x != "foo") {
                            paste0("<h6 style='transform:rotate(270deg);'>", x, "</h6>")
                          } else {
                            ""
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
    ) %>%
    formatStyle(columns = spacer_cols,
                backgroundColor = "#7fc97f") %>%
    formatStyle(columns = terminalp_cols,
                backgroundColor = "#beaed4") %>%
    formatStyle(columns = reverset_cols,
                backgroundColor = "#fdc086") %>%
    formatStyle(columns = rnase_cols,
                backgroundColor = "#ffff99")
  
}


generate_dts <- function(data,
                         table.width = 15){
  
  table_width <- table.width
  table_width_1 <- table_width - 1
  
  lapply(seq(
    table_width,
    ceiling(max(dummy_data$Position) / table_width) * table_width,
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
    ceiling(max(dummy_data$Position) / table_width) * table_width,
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

dummy_data %>%
  generate_selected_col_ids(table.width = 15)


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
