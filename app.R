library(shiny)
library(ggplot2)
library(dplyr)

# Generate data
set.seed(123)
data <- data.frame(
  region = sample(c("North", "South", "East", "West"), 200, replace = TRUE),
  product = sample(c("Gadget", "Appliance", "Furniture"), 200, replace = TRUE),
  price = runif(200, 50, 500),
  quantity = sample(1:100, 200, replace = TRUE)
)

data$sales <- data$price * data$quantity

# UI
ui <- fluidPage(
  titlePanel("📊 Interactive Sales Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Select Region:", choices = unique(data$region), multiple = TRUE),
      selectInput("product", "Select Product:", choices = unique(data$product), multiple = TRUE),
      sliderInput("price", "Price Range:", min = 50, max = 500, value = c(50, 500))
    ),
    
    mainPanel(
      plotOutput("barPlot"),
      plotOutput("scatterPlot"),
      plotOutput("pieChart")
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    data %>%
      filter(region %in% input$region,
             product %in% input$product,
             price >= input$price[1],
             price <= input$price[2])
  })
  
  output$barPlot <- renderPlot({
    df <- filtered_data() %>%
      group_by(region) %>%
      summarise(total_sales = sum(sales))
    
    ggplot(df, aes(x = region, y = total_sales, fill = region)) +
      geom_bar(stat = "identity") +
      ggtitle("Sales by Region")
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = price, y = quantity, color = product)) +
      geom_point() +
      ggtitle("Price vs Quantity")
  })
  
  output$pieChart <- renderPlot({
    df <- filtered_data() %>%
      group_by(product) %>%
      summarise(total_sales = sum(sales))
    
    ggplot(df, aes(x = "", y = total_sales, fill = product)) +
      geom_bar(stat = "identity") +
      coord_polar("y") +
      ggtitle("Sales Distribution")
  })
}

# Run app
shinyApp(ui, server)