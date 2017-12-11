#' alignment_DT_unique_id
#'
#' \code{alignment_DT_unique_id} generartes a unique ID for all DT::datatable outputs shown within the sequence table visualisation
#' @export
#'
alignment_DT_unique_id <- function() {
  paste0(
    "sequenceDT_",
    as.integer(Sys.time()),
    encode(
      round(rnorm(1) * 100000) + as.integer(Sys.time()),
      hashid_settings(salt = "shiny is great", min_length = 15)
    )
  )
}



#' alignment_data
#'
#' \code{alignment_data} converts a sequence dataset into the format required for the gene.alignment.tables visualisation
#'
#' @importFrom magrittr %>%
#' @importFrom tidyr gather spread
#' @importFrom dplyr row_number mutate bind_rows select mutate_all funs slice filter
#' @importFrom DT formatStyle datatable
#' @importFrom htmlwidgets JS
#' @importFrom rlang .data
#'
#' @param data A data.frame with start-end pairs, needs the following columns
#' \itemize{
#'  \item{"position"}{ : sequence position, these must be sequential}
#'  \item{"colour"}{ : colour for the sequence in the gene.alignment.tables visualisation}
#'  \item{"..."}{ : any other columns will be displayed in the visualisation directly below the position number}
#'  }
#'
#' @param table.width the width of the gene.alignment.tables, defaults to 20 positions.
#' @param slice.position fii
#' @export
alignment_data <- function(data,
                           table.width = 15,
                           slice.position,
                           return.data = FALSE) {
  if (nrow(data) < table.width) {
    padding_table <- data[0, ]

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

#' alignment_DT
#'
#' \code{alignment_DT} generates a single DT::datatable
#'
#' @param data A data.frame with start-end pairs, needs the following columns
#' \itemize{
#'  \item{"position"}{ : sequence position, these must be sequential}
#'  \item{"colour"}{ : colour for the sequence in the gene.alignment.tables visualisation}
#'  \item{"..."}{ : any other columns will be displayed in the visualisation directly below the position number}
#'  }
#'
#' @param table.width the width of the gene.alignment.tables, defaults to 20 positions.
#' @param slice.position fio
#' @export
#' 
# 
# alignment_DT <- function(data,
#                          table.width = 15,
#                          slice.position,
#                          return.data = FALSE,
#                          alignment.dt.unique.id) {
#   
#   long_data <- alignment_data(
#     data,
#     table.width,
#     slice.position,
#     return.data
#   )
# 
#   col_colours <- long_data %>%
#     select(.data$colour) %>%
#     mutate(col.index = row_number() + 1)
# 
#   tr_data <- long_data %>%
#     select(-.data$colour) %>%
#     gather(., foo, val, 2:ncol(.)) %>%
#     spread(.data$position, .data$val) %>%
#     mutate_all(funs(coalesce(., "-")))
# 
#   colnames(tr_data) <- gsub("foo", "Position", colnames(tr_data))
# 
#   the_dt <- tr_data %>%
#     datatable(
#       colnames = lapply(
#         colnames(tr_data),
#         function(x) {
#           if (x != "Position") {
#             paste0("<h6 style='transform:rotate(270deg);'>", x, "</h6>")
#           } else {
#             "Position"
#           }
#         }
#       ) %>%
#         as.character(),
#       escape = FALSE,
#       rownames = FALSE,
#       options = list(
#         dom = "t",
#         ordering = F,
#         autoWidth = TRUE,
#         initComplete = JS(
#           "function(settings, json) {",
#           "var headerBorder = [0,1];",
#           "var header = $(this.api().table().header()).find('tr:first > th').filter(function(index) {return $.inArray(index,headerBorder) > -1 ;}).addClass('cell-border-right');",
#           "}"
#         ),
#         columnDefs = list(
#           # list(width = '40px', targets = 1),
#           list(className = "dt-right cell-border-right", targets = 1:table.width)
#         )
#       ),
#       selection = list(target = "column", mode = "multiple"),
#       elementId = paste0(
#         alignment.dt.unique.id,
#         "_",
#         slice.position - table.width + 1,
#         "_",
#         slice.position
#       ),
#       height = "100%"
#     )
# 
#   
# 
#   
#   lapply(
#     unique(col_colours$colour),
#     function(col_colour) {
#       col_indices <- col_colours %>%
#         filter(.data$colour == col_colour) %>%
#         select(.data$col.index) %>%
#         .[[1]]
# 
#       the_dt <<- the_dt %>%
#         formatStyle(
#           columns = col_indices,
#           backgroundColor = col_colour
#         )
#     }
#   )
# 
#   the_dt
# }


alignment_DT <- function(data,
                         table.width = 15,
                         slice.position,
                         return.data = FALSE,
                         alignment.table.id) {
  
  long_data <- alignment_data(
    data,
    table.width,
    slice.position,
    return.data
  )
  
  col_colours <- long_data %>%
    select(.data$colour) %>%
    mutate(col.index = row_number() + 1)
  
  tr_data <- long_data %>%
    select(-.data$colour) %>%
    gather(., foo, val, 2:ncol(.)) %>%
    spread(.data$position, .data$val) %>%
    mutate_all(funs(coalesce(., "-")))
  
  colnames(tr_data) <- gsub("foo", "Position", colnames(tr_data))
  
  the_dt <- tr_data %>%
    datatable(
      colnames = lapply(
        colnames(tr_data),
        function(x) {
          if (x != "Position") {
            paste0("<h6 style='transform:rotate(270deg);'>", x, "</h6>")
          } else {
            "Position"
          }
        }
      ) %>%
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
      selection = list(target = "column", mode = "multiple"),
      elementId = paste0(
        "alignmentDT_",
        slice.position - table.width + 1,
        "_",
        slice.position
      ),
      height = "100%"
    )
  
  
  
  
  lapply(
    unique(col_colours$colour),
    function(col_colour) {
      col_indices <- col_colours %>%
        filter(.data$colour == col_colour) %>%
        select(.data$col.index) %>%
        .[[1]]
      
      the_dt <<- the_dt %>%
        formatStyle(
          columns = col_indices,
          backgroundColor = col_colour
        )
    }
  )
  
  the_dt
}

#' generate_dts
#'
#' \code{generate_dts} generates a set of DT::datatable objects.
#'
#' @param data A data.frame with start-end pairs, needs the following columns
#' \itemize{
#'  \item{"position"}{ : sequence position, these must be sequential}
#'  \item{"colour"}{ : colour for the sequence in the gene.alignment.tables visualisation}
#'  \item{"..."}{ : any other columns will be displayed in the visualisation directly below the position number}
#'  }
#'
#' @param table.width the width of the gene.alignment.tables, defaults to 20 positions.
#' @param slice.position egrger
#' @export

# generate_dts <- function(data,
#                          table.width = 15,
#                          alignment.dt.unique.id) {
#   table_width <- table.width
#   table_width_1 <- table_width - 1
# 
# 
#   
#   alignmentdt.unique.id <- alignment.dt.unique.id
#   
# 
#   
#   lapply(
#     seq(
#       table_width,
#       ceiling(max(data$position) / table_width) * table_width,
#       table_width
#     ),
#     function(x) {
#       data %>%
#         slice({
#           x - table_width_1
#         }:x) %>%
#         alignment_DT(
#           table.width = table_width,
#           slice.position = x,
#           alignment.dt.unique.id = alignmentdt.unique.id
#         )
#     }
#   )
# }

generate_dts <- function(data,
                         table.width = 15,
                         alignment.table.id) {
  table_width <- table.width
  table_width_1 <- table_width - 1
  
  
  lapply(
    seq(
      table_width,
      ceiling(max(data$position) / table_width) * table_width,
      table_width
    ),
    function(x) {
      data %>%
        slice({
          x - table_width_1
        }:x) %>%
        alignment_DT(
          table.width = table_width,
          slice.position = x,
          alignment.table.id = alignment.table.id
        )
    }
  )
}