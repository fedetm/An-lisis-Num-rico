e = exp(1)
f = function(u) 5 - e^u
plot(f, 0, 0.01,
     lwd = 2,
     main = "Gráfico de f", col = "red", xlab = "x", ylab="f(u)", axes = TRUE, n = 100)
tol = 0.01
x = 0
repeat{
  x = x + 0.00001
  dx = x/100
  base = seq(0,x, by=dx)
  altura = f(base)
  area = f(base)*base
  suma = sum(area)
  if(suma > 2-tol & suma < 2+tol) break;
}
i = 0
u = dx
while(u < x){
  rect(0+i, 0, u, f(u), border="blue")
  i = i + dx
  u = u + dx
}
x #Valor de x
suma #Resultado sumas de Riemann para calcular la integral
tol #Nivel de tolerancia
abs (2 - suma) #Error