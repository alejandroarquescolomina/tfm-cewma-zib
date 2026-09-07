from random import random
import numpy as np



def ZIBsim(n,p,phi,N):
    simulaciones = []
    for i in range(N):
        rand = random()
        if rand > phi:
            simulaciones.append(int(np.random.binomial(n,p,1)))
        else:
            simulaciones.append(0)
    return simulaciones



n = int(input("Introduce el número de repeticiones:"))
p = float(input("Introduce la probabilidad de defecto:"))
phi = float(input("Introduce el parámetro de inflamiento:"))
N = int(input("Introduce el número de simulaciones:"))

simulacion = ZIBsim(n,p,phi,N)

print(simulacion)

frecuencias = {}
for nfallos in simulacion:
    if nfallos in frecuencias:
        frecuencias[nfallos] += 1
    else:
        frecuencias[nfallos] = 1
frecuenciasorden = sorted(frecuencias.items())

for fallos,frec in frecuenciasorden:
    print(fallos,frec)
