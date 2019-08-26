#Punto 1
#b
n = sample(1:10,1); n
sample(1:20,n,replace = TRUE)
A =matrix(c(runif(n*n, min=3, max=21)), nrow = n, ncol = n); A
suma = 0
for(i in A){
  suma = suma + i
}; suma
