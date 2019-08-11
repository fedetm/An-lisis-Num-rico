newton1 = function(f, fp, x0, tol, maxiter){ 
  k=0
  r2 = 1.638528422
  array1 <- c()
  dx = 0
  array2 <- c(dx)
# Imprimir estado
cat("---------------------------------------------------------------------------\n") 
cat(formatC( c("x_k"," f(x_k)","Error est."), width = -20, format = "f", flag = " "), "\n") 
cat("---------------------------------------------------------------------------\n")
  repeat{
    correccion = f(x0)/fp(x0)
    x1 = x0 - correccion
    da = dx
    dx = abs(x1-x0)
    array1 <- c(array1,da)
    array2 <- c(array2,dx)
    # Imprimir iteraciones
    cat(formatC( c(x1 ,f(x1), dx), digits=15, width = -15, format = "f", flag = " "), "\n") 
    x0 = x1
    k = k+1
    # until
    if(dx <= tol || k > maxiter ) break;
  }
  cat("---------------------------------------------------------------------------\n") 
  if(k > maxiter){
    cat("Se alcanzó el máximo número de iteraciones.\n")
    cat("k = ", k, "Estado: x = ", x1, "Error estimado <= ", correccion) } 
  else {
      cat("k = ", k, " x = ", x1, " f(x) = ", f(x1), " Error estimado <= ", correccion) 
      array1 <- c(array1,dx)
      plot(array2,array1,
           pch = 19,
           main = "Error i vs Error i+1",
           xlab = "Error i",
           ylab = "Error i+1",
           type = "o")
  } 
}
## --- Pruebas
f  = function(x) exp(x)-pi*x
fp = function(x) exp(x)-pi
options(digits = 15)
newton1(f,fp, 0, 0.00000008, 30)
newton1(f,fp, 2, 0.00000008, 30)
