#1e-9 = 0.000000001
puntofijo <- function(g, x0, tol=1e-8, maxIteraciones=100){
k=0
array1 <- c()
# iteración hasta que abs(x1 - x0) <= tol o se alcance maxIteraciones
repeat{
  x1 = g(x0)
  dx = abs(x1 - x0)
  x0 =x1
  array1 <- c(array1,x1)
  #Imprimir estado
  cat("x_", k, "= ", x1, "\n")
  k = k+1
  #until
  if(dx< tol|| k > maxIteraciones) break;
}
# Mensaje de salida
if( dx > tol ){
  cat("No hubo convergencia   ")
  #return(NULL)
} else{
  cat("x* es aproximadamente ", x1, " con error menor que ", tol)
  x <- seq(1, k, by=1)
  plot(x,array1,
       pch = 19,
       main = "Iteraciones vs Error",
       xlab = "Iteraciones",
       ylab = "Error",
       type = "l")
} }

g <- function(x) ((x^3-1)/5)
f <- function(x) (x^3+5*x-1)
plot(f,-1, 1, col="red" , xlab="", ylab="")
par(new=TRUE)
plot(g, -1, 1, col="blue" , xlab="", ylab="",  axes=FALSE) 
title(main="Gráficas f(x) y g(x)")
abline(a=0, b=0)
puntofijo(g, 0.5, 1e-8) 

