#Punto 1
#a
n = sample(1:10,1); n
sample(1:20,n,replace = TRUE)
A =matrix(c(runif(n*n, min=3, max=21)), nrow = n, ncol = n); A
LA = A
LA[col(LA) >= row(LA)] = 0; LA
suma = 0
for(i in A){
  suma = suma + i
}; suma