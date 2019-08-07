import math

def calcularRaiz(x,n):
    y = 0.5*(x + (n / x))
    e = 0.0000000001
    while True :
        if math.fabs(x-y)<e:
            print"La raiz aproximada de", n, "es:", y, "con un error de", e
            break
        x = y
        y = 0.5 * (x + (n / x))

calcularRaiz(3,7)
