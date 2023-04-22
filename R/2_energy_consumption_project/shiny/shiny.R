
library(tidyverse)
library(shiny)
library(sf)
library(spData)
library(rsconnect)

setwd("/Users/chenjunzhuo/Documents/GitHub/final-project-xiaoqi-ellie-lee/shiny/")

ui <- fluidPage(
  titlePanel(h1("Overview of U.S. Household Energy Consumption", align = "center")), 
  
    fluidRow(
    column(width = 3, 
           offset = 2,
           align = "center", 
           tags$h4(tags$h4("Lee Ko, Xiaoqi Zhou, Ellie Chen"))
    )
  ),
  fluidRow(
    column(width = 12, 
           align = "left", 
           tags$h3("Energy Usage in U.S. by Division and Energy Types")
    )
  ),

  sidebarLayout(
    
    # Sidebar panel for inputs 
    sidebarPanel(
      
      # First input: choose to display whichever division
      selectInput(inputId = "division",
                  label = "Division Type",
                  choices = c("Division", "Climate Type")), 
      
      # Second input: choose to display whichever energy type
      selectInput(inputId = "energy",
                  label = "Energy Type",
                  choices = c("Oil", "Fossil Fuel", "Natural Gas", "Electricity")),
    ),
    
    # Main panel for map plotting
    mainPanel(
      plotOutput("graphs")
    )
  ),
  fluidRow(
    column(width = 12, 
           align = "left", 
           tags$h3("More Information by Division")
    )
  ),
  # for lee's maps
  sidebarLayout(

    sidebarPanel(
      # Input: choose to display whichever division
      selectInput(inputId = "characteristic",
                  label = "More Information by Division",
                  choices = c("AC System type", "Housing built year", 
                              "Division Distribution", "Elecricity Consumption Bill", "Percentage of Energy Efficient Household")), 
    ),
    # Main panel for map plotting
    mainPanel(
      plotOutput("maps")
    )
  ),
  fluidRow(
    column(width = 12, 
           align = "left", 
           tags$h3("Sentiment Analysis for Annual Energy Outlooks (2013-2022)")
    )
  ),
  # for sentiments
  sidebarLayout(
    
    # Sidebar panel for inputs 
    sidebarPanel(
      
      # Input: choose to display whichever division
      selectInput(inputId = "sentiment",
                  label = "Sentiment Type",
                  choices = c("AFFIN", "NRC")), 
    ),
    # Main panel for map plotting
    mainPanel(
      plotOutput("senti_plots")
    )
  ),
  fluidRow(
    column(width = 12, 
           align = "left", 
           tags$h3("House Built Year: Distribution and Energy Consumption")
    )
  ), 
  # for line charts
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "build",
                  label = "Chart",
                  choices = c("Count of house built year by division", 
                              "House built year and electricity consumption")), 
    ),
    # Main panel for map plotting
    mainPanel(
      plotOutput("lines")
    )
  )
)


server <- function(input, output) {
  output$graphs <- renderImage({
    
    if(input$division == "Division" & input$energy == "Oil"){            
      list(src <- "division_oil.png", height = 200, width = 250)}  
    else if(input$division == "Division" & input$energy == "Fossil Fuel"){
      src <- "division_propane.png"}
    else if(input$division == "Division" & input$energy == "Electricity"){
      src <- "division_electricity.png"}
    else if(input$division == "Division" & input$energy == "Natural Gas"){
      src <- "division_gas.png"}
    else if(input$division == "Climate Type" & input$energy == "Oil"){
      src <- "climate_oil.png"}
    else if(input$division == "Climate Type" & input$energy == "Electricity"){
      src <- "climate_electricity.png"}
    else if(input$division == "Climate Type" & input$energy == "Natural Gas"){
      src <- "climate_gas.png"}
    else if(input$division == "Climate Type" & input$energy == "Fossil Fuel"){
      src <- "climate_propane.png"}
    list(src = src,
         width = "60%")
  }, deleteFile = FALSE)
  
  output$senti_plots <- renderImage({
    if(input$sentiment == "AFFIN"){            
      src <- "sentiment_affin.png"}  
    else if(input$sentiment == "NRC"){
      src <- "sentiment_bing.png"}
    list(src = src,
         width = "60%")
  }, deleteFile = FALSE)

  output$maps <- renderImage({
    if(input$characteristic == "AC System type"){            
      src <- "map_ac.png"}  
    else if(input$characteristic == "Housing built year"){
      src <- "map_builtyear.png"}
    else if(input$characteristic == "Division Distribution"){
      src <- "map_division.png"}
    else if(input$characteristic == "Elecricity Consumption Bill"){
      src <- "map_elec.png"}
    else if(input$characteristic == "Percentage of Energy Efficient Household"){
      src <- "map_energy_efficient_home.png"}
    list(src = src,
         width = "60%")
  }, deleteFile = FALSE)
  
  output$lines <- renderImage({
    if(input$build == "Count of house built year by division"){            
      src <- "builtyear_line.png"}  
    else if(input$build == "House built year and electricity consumption"){
      src <- "reg_builtyear_elect.png"}
    list(src = src,
         width = "60%")
  }, deleteFile = FALSE)
}
    


shinyApp(ui = ui, server = server)
  