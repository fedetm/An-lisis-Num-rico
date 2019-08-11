# Remueve todos los objetos creados
rm(list=ls())
Fx <- function(x) exp(x) - x*pi
F1x <- function(x) exp(x)-pi
# Halla la raiz de Fx
falsaPosicion <- function(a,b) {
  k = 0
  array1 <- c()
  y<-(Fx(b)*a-Fx(a)*b)/(Fx(b)-Fx(a))
  dx = abs(Fx(y)/F1x(y))
  array2 <- c(dx)
  #x<-b
  #d<-(Fx(b)*a-Fx(a)*b)/(Fx(b)-Fx(a))
  error<-1
  while (error > 1.e-8) {
    x<-(Fx(b)*a-Fx(a)*b)/(Fx(b)-Fx(a))
    if (Fx(x) == 0) break
    if (Fx(x)*Fx(a) < 0) {b <- x}
    else {a <- x}
    error<-abs(Fx(x)/F1x(x))
    array1 <- c(array1,error)
    array2 <- c(array2,error)
    points(rbind(c(x,0)),pch=19,cex=0.7,col="red")
    cat("X=",x,"\t","E=",error,"\n")
  }
  array1 <- c(array1,error)
  plot(array1,array2,
       pch = 15,
       main = "Error i vs Error i+1",
       xlab = "Error i",
       ylab = "Error i+1",
       type = "o")
}
falsaPosicion(0,1)
falsaPosicion(1,2)
