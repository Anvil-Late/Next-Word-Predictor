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

    # Application title
    titlePanel("Next Word Predictor"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h2("Enter the start of your sentence here"),
            br("Example : 'New York' or 'Make sure to eat fruits and'"),
            textInput("usertext", ""),
            submitButton("Predict !")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h2("Predictions :"),
            textOutput("predictedword1"),
            textOutput("predictedword2"),
            textOutput("predictedword3"),
            textOutput("predictedword4"),
            textOutput("predictedword5"),
            
        )
    )
))
