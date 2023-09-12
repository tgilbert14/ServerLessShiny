library(shiny)
library(shinyWidgets)
library(DT)

name<-c("John","Jack","Bill")
value1<-c(2,4,6)
add<-c("SDF","GHK","FGH")
value2<-c(3,4,5)
dt<-data.frame(name,value1,add,value2)

# ui object
ui <- fluidPage(
  titlePanel(p("Spatial app", style = "color:#3474A7")),
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        inputId = "p1",
        label = "Select Column headers",
        choices = colnames( dt),
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      ),
      tags$div(id = "add_ui_here")
      
      
    ),
    
    mainPanel(
    )
  )
)

# server()
server <- function(input, output) {
  
  # store currently selected columns
  selected_columns <- c()
  
  observeEvent(input$p1, {
    
    # determine pickerInputs to remove
    input_remove <- !selected_columns %in% input$p1
    input_remove <- selected_columns[input_remove]
    
    # remove inputs
    if (!is.null(input_remove) && length(input_remove) > 0) {
      for (input_element in input_remove) {
        removeUI(selector = paste0("#", input_element, "_remove_id"))
      }
    }
    
    # determine pickerInputs to add
    input_add <- !input$p1 %in% selected_columns
    input_add <- input$p1[input_add]
    
    # add inputs
    if (length(input_add) > 0) {
      for (input_element in input_add) {
        insertUI(
          selector = "#add_ui_here",
          where = "afterEnd",
          ui = tags$div(id = paste0(input_element, "_remove_id"),
                        pickerInput(
                          inputId = input_element
                          ,
                          label = input_element
                          ,
                          choices = dt[, input_element]
                          ,
                          multiple = TRUE,
                          options = list(`actions-box` = TRUE)
                        ))
        )
      }
    }
    
    # update the currently stored column variable
    selected_columns <<- input$p1
  },
  ignoreNULL = FALSE)
  
  
  
  
}

# shinyApp()
shinyApp(ui = ui, server = server)