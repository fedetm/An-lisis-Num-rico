puntofijo <- function(g, x0, tol=1e-8, maxIteraciones=100){
  k=0
  r2 = 1.638528422
  array1 <- c()
  dx = 0
  array2 <- c(dx)
  # iteración hasta que abs(x1 - x0) <= tol o se alcance maxIteraciones
  repeat{
    x1 = g(x0)
    da = dx
    dx = abs(x1 - x0)
    x0 =x1
    array1 <- c(array1,da)
    array2 <- c(array2,dx)
    #Imprimir estado
    cat("x_", k, "= ", x1, "error i = ",da, "error i+1 = ",dx,"\n ")
    k = k+1
    #until
    if(dx< tol|| k > maxIteraciones) break;
  }
  # Mensaje de salida
  if( dx > tol ){
    cat("No hubo convergencia   ")
    #return(NULL)
  } else{
    cat("x* es aproximadamente ", x1," y ",r2, "con error de ", dx)
    array1 <- c(array1,dx)
    plot(array2,array1,
         pch = 19,
         main = "Error i vs Error i+1",
         xlab = "Error i",
         ylab = "Error i+1",
         type = "o")
  } }

g <- function(x) (exp(x)/pi)
puntofijo(g, 0, 1e-8)
