# Load the necessary packages
library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)

# Load the data from the Excel file
data <- read_excel("ConcCampDeathsEthnicity.xlsx")

# Define UI for application
ui <- fluidPage(
  titlePanel("Holocaust Victims by Nationality"),
  sidebarLayout(
    sidebarPanel(
      selectInput("nationality", "Select Nationality:", choices = c("All", unique(data$Nationality)))
    ),
    mainPanel(
      plotOutput("plot"),
      tableOutput("table")
    )
  )
)

# Define server logic
server <- function(input, output) {
  filtered_data <- reactive({
    if (input$nationality == "All") {
      data
    } else {
      filter(data, Nationality == input$nationality)
    }
  })
  
  output$plot <- renderPlot({
    ggplot(data, aes(x = Nationality, y = NumberKilledPerNationality, fill = Nationality)) +
      geom_bar(stat = "identity", aes(fill = ifelse(Nationality == input$nationality, "selected", "other"))) +
      labs(x = "Nationality", y = "Number of Victims Killed",
           title = "Number of Holocaust Victims Killed by Nationality") +
      scale_fill_manual(name = "Legend", values = c("selected" = "#FFCCCC", "other" = "gray"), guide = "none")
  })
  
  output$table <- renderTable({
    selected_data <- filtered_data()
    selected_data["People Killed"] <- format(selected_data$NumberKilledPerNationality, scientific = FALSE, trim = TRUE)
    selected_data[, c("Nationality", "People Killed")]
  })
}

# Run the application
shinyApp(ui = ui, server = server)
