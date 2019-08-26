#Punto 2
#a
f = function(x) sin(x)
g = function(x) log(x+2)
plot(f, -2, 3, col="red")
par(new=TRUE)
plot(g, -2, 3, col="blue")
h = function(x) sin(x)-log(x+2)
#Interseccion
x0 = -1.8
x1 = -1.1
iter = 1
x2 = (x0-((f(x0)*(x0-x1))/(f(x0)-f(x1))))
e = abs(x2-x1)
x0 = x1
x1 = x2
datos = c(e)
while(e > 1e-8){
  iter = iter+1
  x2 = (x0-((f(x0)*(x0-x1))/(f(x0)-f(x1))))
  e = abs(x2-x1)
  datos = c(datos,e)
  x0 = x1
  x1 = x2
}
x = seq(1, iter, by=1)
cat("Iteraciones: ",iter,"  Resultado:",x2)
