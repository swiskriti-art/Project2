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
library(shinyalert)
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
    sidebarPanel( width = 3,
      h3("Choose columns to filter data"),
      selectizeInput(inputId = "state", "State",
                     choices= c("All", levels(store_data$State)),
                     selected = "All"),
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
        tabPanel("About", "Purpose: "  ),
        tabPanel("Data Download",
                 downloadButton(outputId = "download", label= "Download Table"),
                 DT::dataTableOutput("table")
                 ),
        tabPanel("Data Exploration",
                 br(),
                 tabsetPanel(
                   tabPanel("Box Plot",
                              fluidRow(
                                column(3,
                                       selectizeInput(inputId = "bx_cat_var",
                                                      label = "Select a categorical variable",
                                                      choices = c("Segment", "Category", "Sub Category"),
                                                      selected = "Segment"
                                       )
                                      ),
                                column(3,
                                       selectizeInput(inputId = "bx_num_var",
                                                      label = "Select a numeric variable",
                                                      selected = "Sell Price",
                                                      choices = c("Sell Price", "Discount", "Profit")
                                       )
                                ),
                                column(3,
                                       br(),
                                       actionButton(inputId = "disp_box_plot",
                                                    label = "Display")
                                )
                              ),
                            br(),
                            plotOutput(outputId = "box_plot")
                   ),
                   tabPanel("Bar Chart",
                            fluidRow(
                              column(3,
                                     selectizeInput(inputId = "bar_cat_var",
                                                    label = "Select a categorical variable",
                                                    choices = c("Segment", "Category", "Sub Category"),
                                                    selected = "Segment"
                                     )
                              ),
                              column(3,
                                     selectizeInput(inputId = "bar_num_var",
                                                    label = "Select a numeric variable",
                                                    selected = "Sell Price",
                                                    choices = c("Frequency","Sell Price", "Discount", "Profit")
                                     )
                              ),
                              column(3,
                                     selectizeInput(inputId = "bar_group_var",
                                                    label = "Select a numeric variable",
                                                    selected = "Sell Price",
                                                    choices = c("Year", "Discount")
                                     )
                              ),
                              column(3,
                                     br(),
                                     actionButton(inputId = "disp_bar_plot",
                                                  label = "Display")
                              ),
                            ),
                            br(),
                            plotOutput(outputId = "bar_plot")
                   ),
                )
          )
      )
    )
  )
)

#Function to get min and max for dynamic slider on selection
slider_min_max <- function(selection) {
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
  #num_vars = c("Sell Price" = "Sell_Price", "" )
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
  plot_data <- reactiveVal()
  #Action button event
  observeEvent(input$btn, {
    state <- input$state
    if(input$state == "All") {
      state <- levels(store_data$State)
    }
    segment <- input$seg
    if(input$seg == "All") {
      segment <- levels(store_data$Segment)
    }
    category <- input$category
    if(input$category == "All") {
      category <- levels(store_data$Category)
    }
    num_var1 <- num_var2 <- NULL
    if(input$sel_num_var != "") {
      num_var1 <- input$sel_num_var
      if(num_var1 == "Sell price") num_var1 <- "Sell_Price"
      num_var1_value <- input$num_var1
    }
    if(input$sel_num_var2 != "" ) {
      num_var2 <- input$sel_num_var2
      if(num_var2 == "Sell price") num_var2 <- "Sell_Price"
      num_var2_value <- input$num_var2
    }
    data_subset <- store_data |>
      filter(
        State %in% state,
        Segment %in% segment,
        Category %in% category,
      ) %>%
      {if (!is.null(num_var1)) filter(., !!sym(num_var1) <= num_var1_value) else .} %>%
      {if (!is.null(num_var2)) filter(., !!sym(num_var2) <= num_var2_value) else .}
    plot_data(data_subset)
    output$table = DT::renderDataTable(data_subset)
    output$download <- downloadHandler(
      filename = function() {
        paste0('StoreData-', Sys.Date(), '.csv')
      },
      content = function(con) {
        write.csv(data_subset, con)
      }
    )
  }
  )
  observeEvent(input$disp_box_plot, {
    output$box_plot <- renderPlot({
      cat_var <- str_replace(input$bx_cat_var, " ", "_")
      num_var <- str_replace(input$bx_num_var, " ", "_")
      if(is.null(plot_data())) {
        shinyalert(title = "Data Not Available", "Please select data set using the Subset button")
      } else {
        ggplot(data = plot_data(),
               aes_string(x = cat_var, y = num_var))+
          geom_boxplot() +
          #coord_cartesian(ylim = c(-2000, 2000)) +
          labs(title = paste("Boxplot for", input$bx_num_var, "per",input$bx_cat_var)
               , x = input$bx_cat_var ) +
          theme(axis.text.x = element_text(angle = 45, vjust = 0.6))
      }
    })
  })
  observeEvent(input$disp_bar_plot, {
    output$bar_plot <- renderPlot({
      cat_var <- str_replace(input$bar_cat_var, " ", "_")
      num_var <- str_replace(input$bar_num_var, " ", "_")
      if(is.null(plot_data())) {
        shinyalert(title = "Data Not Available", "Please select data set using the Subset button")
      } else {
        if (num_var == "Frequency") {
          ggplot(data = plot_data(),
                 aes(x = year(Order_Date), fill = !!sym(cat_var)) +
            geom_bar(width = 0.5) +
            labs(x = "Order Year", y = "Count", title = "Segment count by year")
          #theme(axis.text.x = element_text(angle = 45, vjust = 0.6)
          )
        } else {
          ggplot(data = plot_data(),
                 aes(x = year(Order_Date), y = !!sym(num_var), fill = !!sym(cat_var)) +
                   geom_col(width = 0.5) +
                   labs(x = "Order Year", y = num_var, title = "T")
          )
        }
      }
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
