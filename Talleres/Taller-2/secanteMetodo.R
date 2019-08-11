secante = function(f, x0, x1, tol, maxiter = 100){ 
f0 = f(x0)
f1 = f(x1)
k=0
array1 <- c()
dx = 0
array2 <- c(abs(f1-f0))
cat("---------------------------------------\n")
cat("#",formatC( c("Convergencia","Error est."), width = -15, format = "f", flag = " "), "\n") 
cat("---------------------------------------\n")
while (abs(x1 - x0) > tol && k <= maxiter ) { 
  k = k+1
  pendiente = (f1 - f0)/(x1 - x0)
  if (pendiente == 0) return( cero = NA, f.cero = NA, iter = k, ErrorEst = NA) 
  x2 = x1 - f1/pendiente
  f2 = f(x2)
  x0 = x1; f0 = f1
  x1 = x2; f1 = f2
  array1 <- c(array1,abs(x1-x0))
  array2 <- c(array2,abs(x1-x0))
  # Imprimir iteraciones
  cat(k,formatC( c(x1,abs(x1-x0)), digits=9, width = -15, format = "f", flag = " "), "\n") 
}
array1 <- c(array1,abs(x1-x0))
plot(array1,array2,
     pch = 15,
     main = "Error i vs Error i+1",
     xlab = "Error i",
     ylab = "Error i+1",
     type = "o")
cat("error: ",abs(x1-x0),"\n" )

if (k > maxiter) {
  warning("No se alcanzó el número de iteraciones")
}
#return(list(cero=x2, f.cero=f2, iter=k, ErrorEst =abs(x2-x1)))
}
##--- Pruebas
f =  function(x) exp(x)-pi*x
plot(f,0, 2)
secante(f, 0, 1, 1e-8, 50)
secante(f, 1, 2, 1e-8, 50)
