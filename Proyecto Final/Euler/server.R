#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$distPlot <- renderPlot({
        
        #tamaño poblacional
        N = input$inicial
        
        #estado inicial de los compartimentos
        init <- c(S = N-input$infectados,
                  I = input$infectados,
                  R = 0)
        #parámetros del modelo (coeficientes de las variables)
        param <- c(beta = input$tasa,
                   gamma = input$recuperacion)
        #crear la función con las ODE
        sir <- function(times, init, param) {
            with(as.list(c(init, param)), {
                #ecuaciones diferenciales   
                dS <- -beta * S * I
                dI <-  beta * S * I - gamma * I
                dR <-                 gamma * I
                #resultados de las tasas de cambio    
                return(list(c(dS, dI, dR)))
            })
        }
        #intervalo de tiempo y resolución
        times <- seq(0, input$datos, by = 1)
        #resolver el sistema de ecuaciones con función 'ode'
        out <- ode(y = init, times = times, func = sir, parms = param, method = "euler")
        #cambiar out a un data.frame
        out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
        #eliminar la variable 'time' en out
        out$time <- NULL
        #mostrar todos los datos
        out
        
        
        # generate bins based on input$bins from ui.R
        x    <- out[, 2]
        bins <- seq(min(x), max(x), length.out = input$inicial)
        
        #gráfica
        matplot(x = times, y = out, type = "l",
                xlab = "Tiempo", ylab = "I, S, R", main = "Modelo SIR básico",
                lwd = 4, lty = 1, bty = "l", col = 2:4)
        #añadir leyenda de líneas
        legend(input$datos, 0.2, c("No Infectados", "Infectados", "Recuperados"), 
               pch = 1, col = 2:4, bty = "n", cex = 1)
        
    })
    
})
