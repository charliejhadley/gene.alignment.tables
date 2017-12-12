library("tidyverse")

hbv_pol_data <- read_csv("data-raw/hbv-pol-data.csv")

sequence_region_colours <- list(
  "Terminal protein" = "#ccfecc",
  "Spacer" = "#f9fd74",
  "Reverse transcriptase" = "#f4b084",
  "RNAse H" = "#bdd7ee",
  "Pre-S2" = "#fe6600",
  "Surface antigen" = "#a9d08e"
)

coding_region_colours <- as_tibble(sequence_region_colours) %>%
  gather(coding.region, colour)

save(coding_region_colours, file = "data/coding_region_colours.rdata")

hbv_pol_sequence <- hbv_pol_data %>%
  filter(sheet == "HBV Pol") %>%
  select(-sheet) %>%
  mutate(colour = plyr::mapvalues(label, from = names(sequence_region_colours),
                                  to = as.character(sequence_region_colours))) %>%
  select(-label) %>%
  replace_na(list(colour = NA))

save(hbv_pol_sequence, file = "data/hbv_pol_sequence.rdata")

hbv_long_s_sequence <- hbv_pol_data %>%
  filter(sheet == "HBV long S") %>%
  select(-sheet) %>%
  mutate(colour = plyr::mapvalues(label, from = names(sequence_region_colours),
                                  to = as.character(sequence_region_colours))) %>%
  select(-label) %>%
  replace_na(list(colour = NA))

save(hbv_long_s_sequence, file = "data/hbv_long_s_sequence.rdata")
