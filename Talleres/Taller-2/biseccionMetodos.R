biseccion <- function(f, xa, xb, tol){
  cont = 1
  array1 <- c()
  er = (xb-xa)/2
  array2 <- c(er)
  if( sign(f(xa)) == sign(f(xb)) ){ 
    stop("f(xa) y f(xb) tienen el mismo signo") }
  # a = min(xa,xb)
  # b = max(xa,xb)
  a = xa; b = xb
  k=0
  #Par imprimir estado
  cat("----------------------------------------------------------\n")
  cat(formatC( c("a","b","m","Error est."), width = -15, format = "f", flag = " "), "\n") 
  cat("----------------------------------------------------------\n")
  repeat{
    cont = cont+1
    m = a + 0.5*(b-a)
    if( f(m)==0 ){ 
      cat("Cero de f en [",xa,",",xb,"] es: ", m ) }
    if( sign(f(a)) != sign(f(m)) ){
      b=m
    } else { a = m }
    dx = (b-a)/2
    array1 <- c(array1,dx)
    array2 <- c(array2,dx)
    # imprimir estado
    cat(formatC( c(a,b,m,dx), digits=7, width = -15, format = "f", flag = " "), "\n") 
    k = k+1
    #until
    if( dx < tol ){
      cat("----------------------------------------------------------\n\n") 
      cat("Cero de f en [",xa,",",xb,"] es approx: ", m, "con error <=", dx) 
      cat("\n iteraciones", cont)
      x <- seq(1, cont, by=1)
      array1 <- c(array1,0)
      plot(array1,array2,
           pch = 19,
           main = "Error i vs Error i+1",
           xlab = "Error i",
           ylab = "Error i+1",
           type = "o")
      break;
    }
  } #repeat
}
## Pruebas

b = function(x) (exp(x)-pi*x)
curve(b, 0,3); abline(h=0, v=0) #gráfico para decidir un intervalo
biseccion(b, 0, 1, 0.00000001)
biseccion(b, 1, 2, 0.00000001)