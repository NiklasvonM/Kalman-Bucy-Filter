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
    titlePanel("Kalman-Bucy-Filter"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("example",
                        "Beispiel:",
                        choices = c(
                            "noisy observations of a constant process",
                            "noisy observations of a Brownian motion",
                            "wrong model",
                            "noisy observations of population growth"
                        ),
                        selected = "noisy observations of a constant process"),
            sliderInput("seed",
                        "Seed:",
                        min = 1,
                        max = 50,
                        value = 1),
            sliderInput(
                "m",
                label = "m",
                min = 0.1,
                max = 10,
                step = 0.1,
                value = 1
            ),
            sliderInput(
                "sigma",
                label = HTML("&sigma;:"),
                min = 0.1,
                max = 10,
                step = 0.1,
                value = 1
            ),
            sliderInput(
                "T1",
                label = "Endzeitpunkt:",
                min = 1,
                max = 100,
                step = 1,
                value = 5
            ),
            checkboxInput(
                "showObservations",
                "Show observations",
                value = TRUE
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("distPlot"),
            uiOutput(
                "formula"
            )
        )
    )
))
