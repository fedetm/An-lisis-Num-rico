#Punto 1
#c
n = sample(1:10,1); n
numeros = seq(from = 0, to = n)
suma = 0
for(i in numeros){
  suma = suma + i^2
}; suma
