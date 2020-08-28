library(shiny)


shinyUI(fluidPage(

    # Application title
    titlePanel("Next Word Predictor"),

    # Sidebar input
    sidebarLayout(
        sidebarPanel(
            h2("Enter the start of your sentence here"),
            br("Example : \"New York\" or \"Make sure to eat fruits and\""),
            textInput("usertext", ""),
            submitButton("Predict !")
        ),

        # Main panel output
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Prediction App", br(),
                                 h2("Predictions :"),
                                 textOutput("predictedword1"),
                                 textOutput("predictedword2"),
                                 textOutput("predictedword3"),
                                 textOutput("predictedword4"),
                                 textOutput("predictedword5")),
                        tabPanel("How was this predictor built ?", br(),
                                 br("This predictor was built using Katz' backoff
                                    model along with Good-Turing smoothing"),
                                 br("You will find here a step-by-step description
                                    of how it was built :"),
                                 uiOutput("pitchlink")),
                        tabPanel("UI Code", br(),
                                 uiOutput("uicodelink")),
                        tabPanel("Server & computation code", br(),
                                 uiOutput("servercodelink"))
                            
                        )
            
            
        )
    )
))

