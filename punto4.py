def operacionAritmetica(v,t):
    errorAbsoluto = (v*0.1)*(t*0.1)
    errorRelativo = (0.1/v)*(0.1/t)
    print "el error relativo es:", errorRelativo, "y el error obsoluto es:", errorAbsoluto, "en una distancia de", v*t ,"m"
operacionAritmetica(4,5)