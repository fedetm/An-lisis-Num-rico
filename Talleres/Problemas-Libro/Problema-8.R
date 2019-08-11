biseccion <- function(f, xa, xb, tol){
  cont = 0
  array1 <- c()
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
    if( f(m)==0 ){ cat("Cero de f en [",xa,",",xb,"] es: ", m ) }
    if( sign(f(a)) != sign(f(m)) ){
      b=m
    } else { a = m }
    dx = (b-a)/2
    array1 <- c(array1,dx)
    # imprimir estado
    cat(formatC( c(a,b,m,dx), digits=7, width = -15, format = "f", flag = " "), "\n") 
    k = k+1
    #until
    if( dx < tol ){
      cat("----------------------------------------------------------\n\n") 
      cat("Cero de f en [",xa,",",xb,"] es approx: ", m, "con error <=", dx) 
      cat("\n iteraciones", cont)
      x <- seq(1, cont, by=1)
      plot(x,array1,
           pch = 19,
           main = "Iteraciones vs Error est ",
           xlab = "Iteraciones",
           ylab = "Error est",
           type = "l")
      break;
    }
  } #repeat
}
## Pruebas
b = function(x) sin(x) - log(x)
curve(b, 0, 4); abline(h=0, v=0) #grafico para decidir un intervalo
biseccion(b, 2, 3, 0.00000001)

