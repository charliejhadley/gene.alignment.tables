#' alignment_DT_unique_id
#'
#' alignment_DT_unique_id generartes a unique ID for all datatable outputs shown within the sequence table visualisation
#' 
#' @importFrom magrittr %>%
#' @importFrom tidyr gather spread
#' @importFrom dplyr row_number mutate bind_rows select mutate_all funs slice filter coalesce add_row
#' @importFrom DT formatStyle datatable
#' @importFrom htmlwidgets JS
#' 
#' @export

alignment_DT_unique_id <- function() {
  paste0("aDT", paste0(sample(letters, 5, TRUE), collapse = ""), collapse = "")
}

alignment_data <- function(data,
                           table.width = 15,
                           slice.position) {
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

alignment_DT <- function(data,
                         table.width = 15,
                         slice.position,
                         alignment.table.id) {
  
  long_data <- alignment_data(
    data,
    table.width,
    slice.position
  )
  
  col_colours <- long_data %>%
    select(.data$colour) %>%
    mutate(col.index = row_number() + 1)
  
  original_column_order <- setdiff(colnames(long_data), c("position", "colour"))
  
  tr_data <- long_data %>%
    select(-.data$colour) %>%
    gather(., foo, val, 2:ncol(.)) %>%
    mutate(foo = fct_relevel(foo, original_column_order)) %>%
    spread(.data$position, .data$val) %>%
    mutate(foo = as.character(foo)) %>%
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
        alignment.table.id,
        "_",
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
#' \code{generate_dts} generates a set of \code{datatable} objects.
#'
#' @param data A data.frame containing gene sequence data, which must contain the following columns:
#' \itemize{
#'  \item{"position"}{ : sequence position, these must be sequential}
#'  \item{"colour"}{ : colour for the sequence in the gene.alignment.tables visualisation}
#'  \item{"..."}{ : any other columns will be displayed in the visualisation directly below the position number}
#'  }
#'
#' @param table.width the width of the gene.alignment.tables, defaults to 15 positions.
#' @param alignment.table.id The unique name given to datatable objects created by generate_dts, must contain ONLY alphabetical characters. Use alignment_DT_unique_id to ensure a safe name is created. 
#' @export

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