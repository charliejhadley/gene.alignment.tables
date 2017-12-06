hbv_sequence_data <- read_csv("data-raw/hbv-pol-data.csv")

sequence_region_colours <- list(
  "Terminal protein" = "#ccfecc",
  "Spacer" = "#f9fd74",
  "Reverse transcriptase" = "#f4b084",
  "RNAse H" = "#bdd7ee",
  "Pre-S2" = "#fe6600",
  "Africa" = "#00b0f0",
  "Surface antigen" = "#a9d08e"
)

as.character(sequence_region_colours)


hbv_table_data <- hbv_sequence_data %>%
  mutate(colour = plyr::mapvalues(label, from = names(sequence_region_colours),
                                  to = as.character(sequence_region_colours))) %>%
  select(-label)
