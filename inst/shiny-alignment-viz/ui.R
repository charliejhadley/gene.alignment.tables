library("rhandsontable")
library("shiny")
library("DT")
library("shinyjs")

appCSS <- "
#loading-content {
position: absolute;
background: #FFFFFF;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #000000;
}
"

navbarPage(
  "HBV Alignment Shiny App",
  tabPanel(
    "Visualisation",
    fluidPage(
      theme = "animate.min.css",
      useShinyjs(),
      inlineCSS(appCSS),
      tags$head(tags$style(
        HTML(".cell-border-right{border-right: 1px solid #000}")
      )),
      wellPanel(h4("Dear reviewer, this is a beta version of our sequence alignment visualisation tool for drug resistance in HBV. The tool is still under development but you are welcome to test out the user interface. Please note the code lives here: https://github.com/martinjhnhadley/hbv-alignment-viz")),
      wellPanel(
        selectInput(
          "selected_species",
          "Selected Species",
          choices = c("HBV Pol",
                      "HBV long S")
        ),
        "Scroll through the sequence visualisation below and select sequences of interest. A table will appear below with information about each of the selected rows.",
        dataTableOutput("observe_show_inputs"),
        uiOutput("column_in_programmaticDT_UI")
      ),
      fluidPage(
        h2("Sequence Visualisation"),
        div(id = "loading-content",
            fluidPage(
              h2(class = "animated infinite pulse", "Loading data...")
              # HTML("<img src=images/cruk-logo.png width='50%'></img>")
            )),
        uiOutput("programmatic_many_DT_UI"),
        style = "overflow-y:scroll; max-height: 600px"
      )
      
    )
  ),
  tabPanel(
    HTML(
      '<span class="glyphicon glyphicon-info-sign" aria-hidden="true""></span>'
    ),
    fluidPage(includeMarkdown(knitr::knit("tab_about.Rmd")))
  )
)