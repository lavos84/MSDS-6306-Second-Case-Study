library(shiny)
library(ggplot2)

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
    
    # Identify what kind of data is being requested
    vertclass <- class(input$featureInput1)
    horclass <- class(input$featureInput2)
    # Then use that to determine what kind of plot to make
    # Same on both axis --> Histogram
    if ( (identical(input$featureInput1, input$featureInput2)) ){
      output$Plot1 <- renderPlotly({
        plot_ly(plot.df, x = ~horizontal,
                type = "histogram", color = ~attrite, colors= pal, 
                source = "subset") %>%
          config(displayModeBar = F) %>% # this removes the interactive toolbar
          layout(title = paste(input$featureInput2, "vs ", input$featureInput1),
                 yaxis = list(title = input$featureInput1),
                 xaxis = list(title = input$featureInput2))
      })
    } 
    # Both axes are numeric --> Scatterplot
    else if ( (vertclass == "numeric") & (horclass == "numeric") ){
      output$Plot1 <- renderPlotly({
        plot_ly(plot.df, x = ~horizontal, y = ~vertical, mode = "markers",
                type = "scatter", color = ~attrite, colors= pal, source = "subset",
                marker = list(size = 5)) %>%
          config(displayModeBar = F) %>% # this removes the interactive toolbar
          layout(title = paste(input$featureInput2, "vs ", input$featureInput1),
                 yaxis = list(title = input$featureInput1),
                 xaxis = list(title = input$featureInput2))
      })
    }
    # Vertical is categorical --> Jitter on vertical axis
    else if ( (vertclass == "character") & (horclass == "numeric") ){
      output$Plot1 <- renderPlotly({
        print(
          ggplotly(
            ggplot(plot.df, aes(x= horizontal, y= vertical, 
                                fill = attrite)) + 
              scale_fill_manual(values = pal) +
              geom_point() + 
              geom_jitter(height = 0.25) + 
              labs(title= paste(input$featureInput2,"vs",input$featureInput1),
                   x = input$featureInput2, y = input$featureInput1) +
              ggtitle(paste(input$featureInput2, "vs ", input$featureInput1))) %>%
            config(displayModeBar = F))
      })
    }
    # Horizontal is categorical --> Jitter on horizontal axis
    else if ( (vertclass == "numeric") & (horclass == "character") ){
      output$Plot1 <- renderPlotly({
        print(
          ggplotly(
            ggplot(plot.df, aes(x= horizontal, y= vertical, 
                                fill = attrite)) + 
              scale_fill_manual(values = pal) +
              geom_point() + 
              geom_jitter(width = 0.25) + 
              labs(title= paste(input$featureInput2,"vs",input$featureInput1),
                   x = input$featureInput2, y = input$featureInput1) +
              theme(axis.text.x= element_text(size= 8, angle= 90)) +
              ggtitle(paste(input$featureInput2, "vs ", input$featureInput1))) %>%
            config(displayModeBar = F))
      })
    }
    # Both are categorical --> Double Jitter
    else {
      output$Plot1 <- renderPlotly({
        print(
          ggplotly(
            ggplot(plot.df, aes(x= horizontal, y= vertical, 
                                fill = attrite)) + 
              scale_fill_manual(values = pal) +
              geom_point() + 
              geom_jitter(width = 0.25, height = 0.25) + 
              labs(title= paste(input$featureInput2,"vs",input$featureInput1),
                   x = input$featureInput2, y = input$featureInput1) +
              theme(axis.text.x= element_text(size= 8, angle= 90)) +
              ggtitle(paste(input$featureInput2, "vs ", input$featureInput1))) %>%
            config(displayModeBar = F))
      })
    }
 
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
    
    # Identify what kind of data is being requested
    vertclass <- class(input$featureInput1)
    horclass <- class(input$featureInput2)
    # Then use that to determine what kind of plot to make
    # Same on both axis --> Histogram
    if ( (identical(input$featureInput1, input$featureInput2)) ){
      output$Plot1 <- renderPlotly({
        plot_ly(plot.df, x = ~horizontal,
                type = "histogram", color = ~attrite, colors= pal, 
                source = "subset") %>%
          config(displayModeBar = F) %>% # this removes the interactive toolbar
          layout(title = paste(input$featureInput2, "vs ", input$featureInput1),
                 yaxis = list(title = input$featureInput1),
                 xaxis = list(title = input$featureInput2))
      })
    } 
    # Both axes are numeric --> Scatterplot
    else if ( (vertclass == "numeric") & (horclass == "numeric") ){
      output$Plot1 <- renderPlotly({
        plot_ly(plot.df, x = ~horizontal, y = ~vertical, mode = "markers",
                type = "scatter", color = ~attrite, colors= pal, source = "subset",
                marker = list(size = 5)) %>%
          config(displayModeBar = F) %>% # this removes the interactive toolbar
          layout(title = paste(input$featureInput2, "vs ", input$featureInput1),
                 yaxis = list(title = input$featureInput1),
                 xaxis = list(title = input$featureInput2))
      })
    }
    # Vertical is categorical --> Jitter on vertical axis
    else if ( (vertclass == "character") & (horclass == "numeric") ){
      output$Plot1 <- renderPlotly({
        print(
          ggplotly(
            ggplot(plot.df, aes(x= horizontal, y= vertical, 
                                fill = attrite)) + 
              scale_fill_manual(values = pal) +
              geom_point() + 
              geom_jitter(height = 0.25) + 
              labs(title= paste(input$featureInput2,"vs",input$featureInput1),
                   x = input$featureInput2, y = input$featureInput1) +
              ggtitle(paste(input$featureInput2, "vs ", input$featureInput1))) %>%
            config(displayModeBar = F))
      })
    }
    # Horizontal is categorical --> Jitter on horizontal axis
    else if ( (vertclass == "numeric") & (horclass == "character") ){
      output$Plot1 <- renderPlotly({
        print(
          ggplotly(
            ggplot(plot.df, aes(x= horizontal, y= vertical, 
                                fill = attrite)) + 
              scale_fill_manual(values = pal) +
              geom_point() + 
              geom_jitter(width = 0.25) + 
              labs(title= paste(input$featureInput2,"vs",input$featureInput1),
                   x = input$featureInput2, y = input$featureInput1) +
              theme(axis.text.x= element_text(size= 8, angle= 90)) +
              ggtitle(paste(input$featureInput2, "vs ", input$featureInput1))) %>%
            config(displayModeBar = F))
      })
    }
    # Both are categorical --> Double Jitter
    else {
      output$Plot1 <- renderPlotly({
        print(
          ggplotly(
            ggplot(plot.df, aes(x= horizontal, y= vertical, 
                                fill = attrite)) + 
              scale_fill_manual(values = pal) +
              geom_point() + 
              geom_jitter(width = 0.25, height = 0.25) +
              labs(title= paste(input$featureInput2,"vs",input$featureInput1),
                   x = input$featureInput2, y = input$featureInput1) +
              theme(axis.text.x= element_text(size= 8, angle= 90)) +
              ggtitle(paste(input$featureInput2, "vs ", input$featureInput1))) %>%
            config(displayModeBar = F))
      })
    }
    
    # Assign to parent environment
    plot.df <<- plot.df
})
}