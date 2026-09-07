import numpy as np
from scipy.stats import binom

from scipy.sparse import csr_matrix

from scipy.sparse import identity, csc_matrix
from scipy.sparse.linalg import spsolve


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


def ARL_teoric(n,p,phi,gammax,gammay,UCL):
    EY = n * p * (1 - phi)
    Q = calculQ(n,p,phi,gammax,gammay,UCL)
    d = len(Q)
    q = np.zeros(d)
    B0 = gammay*int(EY)
    q[B0] = 1
    unos = np.ones(d)
    Qsp = csc_matrix(Q)
    A = identity(d, format='csc') - Qsp
    Asp = csr_matrix(A)
    M = spsolve(Asp, unos)
    ARL = q @ M
    return ARL


#ARL = ARL_teoric(10,0.5,0.1,1,11,5)
ARL = ARL_teoric(1000,0.02,0.8,2,8,5)
print(ARL)