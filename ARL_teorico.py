import numpy as np
from scipy.stats import binom

def ZIBpmf(n,p,phi):
    k = range(n + 1)
    probabilidades = binom.pmf(k, n, p)
    probabilidades = (1-phi)*probabilidades
    probabilidades[0] += phi
    return probabilidades

def calculQ(n,p,phi,gammax,gammay,UCL):
    probabilidadesZIB = ZIBpmf(n,p,phi)
    bmin = 0
    bmax = gammax + gammay*(UCL + 1) - 1
    transistentes = bmax - bmin + 1
    Q = np.zeros([transistentes,transistentes])
    for i in range(bmin,bmax + 1):
        for x in range(n+1):
            Y = int((gammax * x + i)/(gammax + gammay))
            if Y <= UCL:
                R = gammax*x + i - (gammax + gammay)*Y
                j = gammay*Y + R
                Q[i,j] = Q[i,j] + probabilidadesZIB[x]
    return Q

def ARL_teoric(n,p0,phi0,delta,tau,gammax,gammay,UCL):
    EY = n * p0 * (1 - phi0)
    Q = calculQ(n,p0*delta,phi0*tau,gammax,gammay,UCL)
    d = len(Q)
    q = np.zeros(d)
    B0 = gammay*int(EY)
    q[B0] = 1
    unos = np.ones(d)
    I = np.identity(d)
    A = np.linalg.inv(I - Q)
    ARL = q.T @ A @ unos
    return ARL

#ARL = ARL_teoric(1000, 0.02, 0.8, 2, 8, 10)
#ARL = ARL_teoric(10,0.5,0.1,1,1,1,11,5)
#ARL = ARL_teoric(100,0.5,0.1,4,7,55)
#ARL = ARL_teoric(10,0.5,0.1,1,11,2)
ARL = ARL_teoric(100,0.02,0.8,1.1,1.1,1,2,2)
print(ARL)