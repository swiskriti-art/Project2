
#loading libraries
library(shiny)
library(DT)
library(shinyalert)
library (readxl)
library(tidyverse)
library(ggplot2)

# Read the excel data, using a name repair function to handle spaces in column names
store_data <- read_excel("US Superstore data.xls",
                         .name_repair = function(nm) str_replace_all(nm, "[ -]", "_"))
# Create factors for the categorical variables
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
      actionButton(inputId = "subset_btn",
                   label = "Subset Data")
    ),
    # Creating a panel set for each of the panels.
    # Exploratory data panel uses sub panels
    mainPanel(
      tabsetPanel(
        tabPanel("About",
                  br(),
                  h4("This Shiny App provides analysis of US Superstore data  from year 2014 to 2018."),
                  "Purpose:", br(),
                  "Users can select, download and visualize the data using data tables and plots.", br(),
                  "To begin, users should select the dataset to analyze on using the left panel that provides some of the key columns to subset on",br(),
                  "Press the", em("Subset"), "button to generate the dataset to analyze",
                  br(),
                  "After selecting the data users can use the tab panels on the right to visualize the data. Some key columns help in selecting approprite plots",
                  br(),
                  "Visualization appear after the", em("Display")," button is clicked",
                  br(), br(),
                  "Data Source:",a("https://www.kaggle.com/datasets/juhi1994/superstore?resource=download"),
                 ),
        tabPanel("Data Download",
                 downloadButton(outputId = "download", label= "Download Table"),
                 DT::dataTableOutput("table")
                 ),
        tabPanel("Data Exploration",
                 br(),
                 #Add panels for each of the different types of charts and graphs
                 tabsetPanel(
                   tabPanel("Summary Chart",
                              br(),
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
                   tabPanel("Relationship Chart",
                            br(),
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
                                     radioButtons(inputId = "bar_group",
                                                    label = "Select Grouping",
                                                    selected = "Year",
                                                    choices = c("Year", "Month", "Month per Year")
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
                   tabPanel("Trend Chart",
                            br(),
                            fluidRow(
                              column(3,
                                     selectizeInput(inputId = "line_num_var1",
                                                    label = "Select a numeric variable",
                                                    choices = c("Sell Price", "Discount", "Profit"),
                                                    selected = "Profit"
                                     )
                              ),
                              column(3,
                                     selectizeInput(inputId = "line_num_var2",
                                                    label = "Select a numeric variable",
                                                    selected = "Sell Price",
                                                    choices = c("Sell Price", "Discount", "Profit")
                                     )
                              ),
                              column(3,
                                     radioButtons(inputId = "line_group",
                                                  label = "Select Grouping",
                                                  selected = "Category",
                                                  choices = c("Segment", "Category", "Region")
                                     )
                              ),
                              column(3,
                                     br(),
                                     actionButton(inputId = "disp_line_plot",
                                                  label = "Display")
                              ),
                            ),
                            br(),
                            plotOutput(outputId = "line_plot")
                   ),
                   tabPanel("Key Numerical Summary",
                            br(),
                            DT::dataTableOutput("summ")

                   )
                ),
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
  plot_data <- reactiveVal()
  # Subset data on button press
  observeEvent(input$subset_btn, {
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
    # subsetting data
    data_subset <- store_data |>
      filter(
        State %in% state,
        Segment %in% segment,
        Category %in% category,
      ) %>%
      {if (!is.null(num_var1)) filter(., !!sym(num_var1) <= num_var1_value) else .} %>%
      {if (!is.null(num_var2)) filter(., !!sym(num_var2) <= num_var2_value) else .}
    plot_data(data_subset)
    summary <- plot_data() |>
      group_by(year(Order_Date), State, Segment, Category) |>
      summarize(across(c(Profit, Sell_Price, Discount), .fns = list( "sum" = sum, "mean" = mean,
                                                                     "median" = median,
                                                                     "min" = min ,
                                                                     "max" = max),
                       .names = "{.fn}_{.col}"))

    output$summ <- DT::renderDataTable(summary)
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
  # Event to render box plots
  observeEvent(input$disp_box_plot, {
    output$box_plot <- renderPlot({
      isolate({
        cat_var <- str_replace(input$bx_cat_var, " ", "_")
        num_var <- str_replace(input$bx_num_var, " ", "_")
        if(is.null(plot_data())) {
          shinyalert(title = "Data Not Available", "Please select data set using the Subset button")
        } else {
          ggplot(data = plot_data(),
                 aes_string(x = cat_var, y = num_var))+
            geom_boxplot() +
            {if (num_var == "Profit") coord_cartesian(ylim = c(-2000, 2000))}+
            labs(title = paste("Boxplot for", input$bx_num_var, "per",input$bx_cat_var)
                 , x = input$bx_cat_var ) +
            theme(axis.text.x = element_text(angle = 45, vjust = 0.6))
        }
      })
    })
  })
  # Event for bar plots display button
  observeEvent(input$disp_bar_plot, {
    output$bar_plot <- renderPlot({
      isolate({
        cat_var <- str_replace(input$bar_cat_var, " ", "_")
        num_var <- str_replace(input$bar_num_var, " ", "_")
        group <- input$bar_group
        if(is.null(plot_data())) {
          shinyalert(title = "Data Not Available", "Please select data set using the Subset button")
        } else {
          if (num_var == "Frequency") {
            ggplot(data = plot_data(),
                   aes(x = year(Order_Date), fill = !!sym(cat_var))) +
              geom_bar(width = 0.5) +
              labs(x = "Order Year", y = "Count", title = "Segment count by year")
          } else {
            if(group == "Year") {
              ggplot(data = plot_data(),
                   aes(x = year(Order_Date), y = !!sym(num_var), fill = !!sym(cat_var))) +
              geom_col(width = 0.5) +
              labs(x = "Order Year", y = num_var,
                   title = paste(input$bar_num_var, "per Year for", input$bar_cat_var))
            } else if(group == "Month") {
              ggplot(data = plot_data(),
                     aes(x = month(Order_Date, label = TRUE), y = !!sym(num_var), fill = !!sym(cat_var))) +
                geom_col(width = 0.5) +
                labs(x = "Order Month", y = num_var,
                     title = paste(input$bar_num_var, "per Month for", input$bar_cat_var))
            } else {
              ggplot(data = plot_data(),
                     aes(x = month(Order_Date, label = TRUE), y = !!sym(num_var), fill = !!sym(cat_var))) +
                geom_col(width = 0.5) +
                facet_wrap(vars(year(Order_Date))) +
                labs(x = "Order Month", y = num_var,
                     title = paste(input$bar_num_var, "per Month for", input$bar_cat_var))
            }
          }
        }
      })
    })
  })
  # Handling events for display of line plots
  observeEvent(input$disp_line_plot, {
    output$line_plot <- renderPlot({
      isolate({
        num_var1 <- str_replace(input$line_num_var1, " ", "_")
        num_var2 <- str_replace(input$line_num_var2, " ", "_")
        validate(
          need(input$line_num_var1 != input$line_num_var2, "Choose different numerical variables visualize")
        )
        if(is.null(plot_data())) {
          shinyalert(title = "Data Not Available", "Please select data set using the Subset button")
        } else {
          dt <- plot_data() |>
            group_by(year(Order_Date), !!sym(input$line_group)) |>
            summarize(across(c(!!sym(num_var1), !!sym(num_var2) ), .fns = list( "sum" = sum),
                             .names = "{.fn}_{.col}")) |>
            rename(Year = `year(Order_Date)`)
          sum_num1 <- paste0("sum_", num_var1)
          sum_num2 <- paste0("sum_", num_var2)
          ggplot(data= dt, aes(x =Year)) +
            geom_line(aes(y = !!sym(sum_num1)/100, col = paste("Total", input$line_num_var1))) +
            geom_line(aes(y = !!sym(sum_num2)/100, col = paste("Total", input$line_num_var2))) +
            geom_point(aes(y = !!sym(sum_num1)/100)) +
            geom_point(aes(y = !!sym(sum_num2)/100)) +
            facet_grid(vars(!!sym(input$line_group))) +
            labs(x = "Year", y = "USD(x100)",
                 title=paste("Trend for", input$line_num_var1, "and", input$line_num_var2))
        }
      })
    })
  })

}

# Run the application
shinyApp(ui = ui, server = server)
