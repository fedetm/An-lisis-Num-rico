raiznesima = function(n, num, pinicial, tol) {
  p = pinicial
  intervalo = 1
  iter = 0
  while (abs(intervalo) > tol){
    intervalo = ((num/(p^(n-1))) - p)/n
    p = p + intervalo
    iter = iter + 1
  }
  iter #Numero de iteraciones
  return (p) # Raiz n-ésima
}
raiznesima(4,247,2,0.01)