library("shiny")
library("shinyjs")

fluidPage(
  wellPanel(
    tabsetPanel(
      tabPanel(
        "Selected positions",
        fluidPage(
          "Select a column in the gene sequence alignment below, and the table below will be populated with details about the selected positions. Switch to the \"Coding Region Legend\" tab for a legend of the colours in the gene sequence alignment viz.",
          DT::dataTableOutput("observe_show_inputs")
        )
      ),
      tabPanel(
        "Coding Region Legend",
        fluidPage(
          "Simple legend generated using `coding_region_legend`.",
          plotOutput("sequence_coding_region_legend", height = "150px")
        )
      )
    )
  ),
  fluidPage(
    uiOutput("sequence_alignment_UI"),
    style = "overflow-y:scroll; max-height: 600px"
  )
)