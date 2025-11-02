#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DT)
#library(rsconnect)
store_data <- read_excel("../US Superstore data.xls",
                         .name_repair = function(nm) str_replace_all(nm, "[ -]", "_"))
store_data <- store_data |>
  mutate(Ship_Mode = as.factor(Ship_Mode),
         Segment = as.factor(Segment),
         City = as.factor(City),
         State = as.factor(State),
         Region = as.factor(Region),
         Category = as.factor(Category),
         Sub_Category = as.factor(Sub_Category)) |>
  rename(Sell_Price = Sales)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("US Superstore Sales Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      h2("Select data "),
      selectizeInput(inputId = "city", "City",
                     choices= store_data$City,
                     selected = store_data$City[1]),
      radioButtons(inputId = "seg",
                   label = "Segment",
                   choices = c("All", levels(store_data$Segment))
      ),
      radioButtons(inputId = "category",
                   label = "Cagetory",
                   choices = c("All", levels(store_data$Category))
      ),

      selectizeInput(inputId = "sel_num_var", label = "Select a numeric variable",
                     selected = "",
                     choices = c("", "Sell price", "Discount", "Profit")
      ),
      conditionalPanel("input.sel_num_var != '' ",
                       sliderInput(inputId = "num_var1", label = "",
                                   min = 0, max = 1, value = 0)
                       ),
      selectizeInput(inputId = "sel_num_var2", label = "Select a numeric variable",
                     selected = "",
                     choices = c("", "Sell price", "Discount", "Profit")
      ),
      conditionalPanel("input.sel_num_var2 != '' ",
                       sliderInput(inputId = "num_var2", label = "",
                                   min = 0, max = 1, value = 0
                       )
      ),
      actionButton(inputId = "btn",
                   label = "Subset Data")
    ),

    mainPanel(
tabsetPanel(
  tablPanel("About", "Purpose: "  ),
  tabPanel("Data Download",
            DT::dataTableOutput("table")
           ),
  tabPanel("Data Exploration")
  )

)

slider_min_max <- function(selection) {
  #min_val <- max_val <- NULL
  if(selection == "Sell price") {
    min_val <- min(store_data$Sell_Price)
    max_val <- max(store_data$Sell_Price)
  }else if(selection == "Discount") {
    min_val <- min(store_data$Discount)
    max_val <- max(store_data$Discount)
  } else {
    min_val <- min(store_data$Profit)
    max_val <- max(store_data$Profit)
  }
  return(c(min_val, max_val))
}


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  observeEvent(input$sel_num_var, {
    min_max <- slider_min_max(input$sel_num_var)
    choices <-  c("", "Sell price", "Discount", "Profit")
    if(input$sel_num_var2 != input$sel_num_var) {
      updateSelectizeInput(session,
                           "sel_num_var2",
                           choices = choices[-which(choices == input$sel_num_var)],
                           selected = input$sel_num_var2)
    }
    updateSliderInput(session,
                      "num_var1",
                      #label = "",
                      min = min_max[1],
                      max = min_max[2],
                      value = min_max[1])
  })
  observeEvent(input$sel_num_var2, {
    min_max <- slider_min_max(input$sel_num_var2)
    choices <-  c("", "Sell price", "Discount", "Profit")
    if(input$sel_num_var2 != input$sel_num_var) {
      updateSelectizeInput(session,
                         "sel_num_var",
                         choices = choices[-which(choices == input$sel_num_var2)],
                         selected = input$sel_num_var)
    }
    updateSliderInput(session,
                      "num_var2",
                      #label = "",
                      min = min_max[1],
                      max = min_max[2],
                      value = min_max[1])
  })

  #Action button event
  observeEvent(input$btn, {
  #   if(input$city != "" ) {
  #     city <- input$city
  #   }
  #   if(input$seg != "All") {
  #     segment <- input$seg
  #   }
  #   if(input$)
  }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
