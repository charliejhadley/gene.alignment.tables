library("tidyverse")

library("DT")







alignment_DT(data = dummy_data %>%
               slice(100:103),
             slice.position = "1-20")

## Generate the tables!
lapply(
  seq(20, ceiling(max(dummy_data$Position) / 20) * 20, 20),
  function(x){
    dummy_data %>%
      slice({x-19}:x) %>%
      alignment_DT(slice.position = paste0(x-19, "-", x))
  }
)



tr_dummy_data <- dummy_data %>%
  slice(1:20) %>%
  gather(., the.position, val, 2:ncol(.)) %>%
  spread(Position, val)

the_x <- 120


short_table <- dummy_data %>%
  slice(100:120)

short_table %>%
  add_row(Position = c("-", "-"),
          Reference = c("-", "-"),
          `Geno A` = c("-", "-"),
          species = c("-", "-"))



padding_table <- 
  tibble(
    Position = {max(short_table$Position)+1}:the_x,
    Reference = NA,
    `Geno A` = NA,
    species = NA
  )


short_table %>%
  bind_rows(padding_table) %>%
  gather(., the.position, val, 2:ncol(.)) %>%
  spread(Position, val) %>%
  mutate_all(funs(coalesce(., "")))



tr_dummy_data %>%
  datatable(
    colnames = lapply(colnames(tr_dummy_data),
                      function(x) {
                        paste0("<h6 style='transform:rotate(270deg);'>", x, "</h6>")
                      }) %>%
      as.character(),
    escape = FALSE,
    options = list(
      dom = "t",
      ordering = F,
      autoWidth = TRUE,
      columnDefs = list(list(
        width = '21px', targets = 2:20
      ))
    ),
    selection = list(target = "cell")
  )
