#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Jumlah Ulangan RAL"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of treatmenats ----
      sliderInput(inputId = "perlakuan",
                  label = "Jumlah perlakuan:",
                  min = 1,
                  max = 15,
                  value = 6)
      
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Verbatim text for data summary ----
      htmlOutput("jSampel")
    )
  )
)
server <- function(input, output) {
  output$jSampel <- renderText(
    {paste("Perlu","<font color=\"#e67e00\"><b>",ceiling(15/input$perlakuan), 
           "</b></font> ulangan.") })
}
shinyApp(ui,server)
