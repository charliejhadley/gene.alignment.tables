library("rhandsontable")
library("shiny")
library("DT")

fluidPage(
  tags$head(
    tags$style(HTML(".cell-border-right{border-right: 1px solid #000}"))),
  wellPanel(
    "Selected columns in the sequence visualisation below to see the information about the sequence here!",
    tags$script(
      "$(document).on('shiny:inputchanged', function(event) {
      if (event.name === 'a') {
      $('#valueA').text(event.value);
      }
      });
      "
  ),
    dataTableOutput("observe_show_inputs"),
    uiOutput("column_in_programmaticDT_UI")
  ),
  h2("Sequence Visualisation"),
  uiOutput("programmatic_many_DT_UI")
)