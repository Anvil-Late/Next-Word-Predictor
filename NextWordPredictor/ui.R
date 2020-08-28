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
                        tabPanel("UI Code",
                                 uiOutput("uicodelink")),
                        tabPanel("Server & computation code",
                                 uiOutput("servercodelink"))
                            
                        )
            
            
        )
    )
))

