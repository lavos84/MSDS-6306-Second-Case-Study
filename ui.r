library(shiny)
library(openxlsx)
library(plotly)

# Load in the data
setwd("C:/Users/Andrew/Documents/Data Science Masters/Doing Data Science/Case Study 2/Case Study 2 Project/")
casestudy <- read.xlsx("Data/CaseStudy2-data.xlsx", sheet= 1, colNames= T)

# Columns 9, 10, 22, and 27 have no meaningful data.  Remove them.
casestudy <- casestudy[,-c(9, 10, 22, 27)]
parameters <- colnames(casestudy) # This will be used for the plot

# ui.R definition
fluidPage(
  h2("Exploratory Data Analysis"),
  h4("Change the vertical and horizontal axes to compare different parameters"),
  tags$ol(
    tags$li("The first dropdown is the vertical axis"),
    tags$li("The second dropdown is for the horizontal axis")
  ),
  
  # Vertical space
  tags$hr(),
  
  # Feature selection
  fixedRow(
    column(3, selectInput(inputId = "featureInput1", label = "Vertical axis", choices = parameters, selected = "MonthlyIncome")),
    column(4, selectInput(inputId = "featureInput2", label = "Horizontal axis", choices = parameters, selected = "JobRole"))),
  
  # First row
  fixedRow(
    column(6, plotlyOutput("Plot1", height = "500px", width= "500px"))))