#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  titlePanel("Word Prediction Algorithm--Trevor A"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("phrase","Enter a Phrase:",value="") 

       
    ),
    
    mainPanel(
       h3("Predicted Next Word:"),
       textOutput("predictionFinal")
       
    )
  )
))
