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
    titlePanel("Propagacion del Spam Adams"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("inicial",
                        "Población Suceptible:",
                        min = 1,
                        max = 100,
                        value = 30),
            sliderInput("infectados",
                        "Población infectada Inicial:",
                        min = 1,
                        max = 100,
                        value = 30),
            sliderInput("datos",
                        "Numero de Datos:",
                        min = 1,
                        max = 100,
                        value = 52),
            sliderInput("tasa",
                        "Tasa de Infeccion:",
                        min = 0,
                        max = 0.0205,
                        value = 0.0205),
            sliderInput("recuperacion",
                        "Tasa de recuperacion:",
                        min = 0,
                        max = 0.0165,
                        value = 0.0165)
        ),
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
        
    )
    
))
