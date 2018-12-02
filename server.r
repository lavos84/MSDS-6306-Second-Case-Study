library(shiny)

# Load in the data
setwd("C:/Users/Andrew/Documents/Data Science Masters/Doing Data Science/Case Study 2/Case Study 2 Project/")
casestudy <- read.xlsx("Data/CaseStudy2-data.xlsx", sheet= 1, colNames= T)

# Columns 9, 10, 22, and 27 have no meaningful data.  Remove them.
casestudy <- casestudy[,-c(9, 10, 22, 27)]
parameters <- colnames(casestudy) # This will be used for the plot

# server.R definition
function(input, output){
  
  # Observes the second feature input for a change
  observeEvent(input$featureInput1,{
    
    # Create a convenience data.frame which can be used for charting
    plot.df <- data.frame(casestudy[,input$featureInput1],
                          casestudy[,input$featureInput2],
                          attrite <- casestudy$Attrition)
    # Add column names
    colnames(plot.df) <- c("vertical", "horizontal", "Attrition")
    
    # Define the color scheme
    pal <- c("blue", "red")
    pal <- setNames(pal, c("No", "Yes"))
    
    # Do a plotly contour plot to visualize the two featres with
    # the number of malignant cases as size
    # Note the use of 'source' argument
    output$Plot1 <- renderPlotly({
      plot_ly(plot.df, x = ~horizontal, y = ~vertical, mode = "markers",
              type = "scatter", color = ~attrite, colors= pal, source = "subset",
              marker = list(size = 5)) %>%
        config(displayModeBar = F) %>% # this removes the interactive toolbar
        layout(title = paste(input$featureInput1, "vs ", input$featureInput2),
               yaxis = list(title = input$featureInput1),
               xaxis = list(title = input$featureInput2))
    })

    
    # Assign to parent environment
    plot.df <<- plot.df
  })
  observeEvent(input$featureInput2,{
    
    # Create a convenience data.frame which can be used for charting
    plot.df <- data.frame(casestudy[,input$featureInput1],
                          casestudy[,input$featureInput2],
                          attrite <- casestudy$Attrition)
    
    # Add column names
    colnames(plot.df) <- c("vertical", "horizontal", "Attrition")
    
    # Define the color scheme
    pal <- c("blue", "red")
    pal <- setNames(pal, c("No", "Yes"))
    
    # Do a plotly contour plot to visualize the two featres with
    # the number of malignant cases as size
    # Note the use of 'source' argument
    output$Plot1 <- renderPlotly({
      plot_ly(plot.df, x = ~horizontal, y = ~vertical, mode = "markers",
              type = "scatter", color = ~attrite, colors= pal, source = "subset",
              marker = list(size = 5)) %>%
        config(displayModeBar = F) %>% # this removes the interactive toolbar
        layout(title = paste(input$featureInput1, "vs ", input$featureInput2),
               yaxis = list(title = input$featureInput1),
               xaxis = list(title = input$featureInput2))
    })
  })
}