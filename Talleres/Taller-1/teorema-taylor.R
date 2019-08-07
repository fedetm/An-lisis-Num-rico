require(pracma)
e = exp(1)
f = function(x) e^(x)
p = taylor(f, 0, 4) # Polinomio de Taylor de orden 4, alrededor de a=0.
p # Coeficientes
# Evaluar en x=0.5
aprox = polyval(p, 0.5)
round(aprox,4)
