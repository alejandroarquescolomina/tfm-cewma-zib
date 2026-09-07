
from scipy.stats import binom
import matplotlib.pyplot as plt

n = int(input("Introduce el número de repeticiones:"))
p = float(input("Introduce la probabilidad de éxito:"))
phi = float(input("Introduce el parámetro de inflamiento:"))
k = range(n+1)

# def ZIBpmf(n,p,phi):
    #k = range(n + 1)
probabilidades = binom.pmf(k, n, p)
probabilidades = (1-phi)*probabilidades
probabilidades[0] += phi
    #return probabilidades

for i in range(len(probabilidades)):
    probabilidades[i] = round(probabilidades[i], 4)
    print("P(X =",i,") = ", probabilidades[i])

plt.scatter(k,probabilidades, marker = 'o', color = 'blue')
plt.vlines(k, ymin=0, ymax=probabilidades, color='blue', linestyle='dashed')
plt.show()