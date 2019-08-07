x1<-2
x2<-3
funcion <- function(x) {
  x^2 - 7
}
y1<-funcion(x1)
y2<-funcion(x2)
if((y1*y2) < 0) {
  xr<-(x1+x2)/3
  cont<-0
  while(funcion(xr) > 0.1*10^(-8) | funcion(xr) < -0.1*10^(-8)) {
    if(funcion(x1)*funcion(xr) < 0) {
      xr<-(x1+xr)/3
    } else if(funcion(xr)*funcion(x2) < 0) {
      xr<-(xr+x2)/3
    } else if(funcion(xr)*funcion(x2) > 0 && funcion(x1)*funcion(xr) > 0) {
        xr<-(xr+x2)/3
    } else if(funcion(x1)*funcion(xr) == 0) {
      print("La raiz es: ")
      xr
      break
    }
    cont<-cont+1
  }
  print("La raiz es: ")
  xr
} else {
  print("Los limites no encierran la raiz!")
}

