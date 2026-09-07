import numpy as np
from scipy.stats import binom
from math import gcd

from scipy.sparse import csr_matrix
from scipy.sparse import identity, csc_matrix
from scipy.sparse.linalg import spsolve


def ZIBpmf(n,p,phi):
    k = range(n + 1)
    probabilidades = binom.pmf(k, n, p)
    probabilidades = (1-phi)*probabilidades
    probabilidades[0] += phi
    return probabilidades

def calculQ(n,p,phi,gammax,gammay,UCL, probabilidadesZIB):
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

def ARL_teoric(n,p0,phi0,delta,tau,gammax,gammay,UCL, probabilidades):
    EY = n * p0 * (1 - phi0)
    Q = calculQ(n,p0*delta,phi0*tau,gammax,gammay,UCL, probabilidades)
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

def opt_param(n,p0,phi0,delta,tau,ARL0min = 370.4):
    probabilidadesZIB0 = ZIBpmf(n, p0, phi0)
    probabilidadesZIB1 = ZIBpmf(n,p0*delta, phi0*tau)
    gammamax = 25
    gammax_opt = 0
    gammay_opt = 0
    UCL_opt = 0
    UCLmax = n + 1
    ARL1_opt = float('inf')
    for gammax in range(1,gammamax+1):
        for gammay in range(1,gammamax+1):
            if gcd(gammax,gammay) == 1:
                for UCL in range(1, UCLmax+1):
                    try:
                        ARL0 = ARL_teoric(n,p0,phi0,1,1,gammax,gammay,UCL,probabilidadesZIB0)
                        #print(gammax,gammay,UCL,ARL0)
                    except IndexError:
                        pass
                    else:
                        if ARL0 >= ARL0min:
                            ARL1 = ARL_teoric(n, p0, phi0,delta,tau, gammax, gammay, UCL, probabilidadesZIB1)
                            if ARL1 < ARL1_opt:
                                ARL1_opt = ARL1
                                gammax_opt = gammax
                                gammay_opt = gammay
                                UCL_opt = UCL
                                ARL0_opt = ARL0
                            break
    return gammax_opt, gammay_opt, UCL_opt, ARL1_opt, ARL0_opt

#gammax_opt, gammay_opt, UCL_opt, ARL1_opt = opt_param(10,0.5,0.1,1.1,1.1)

import time

for i in [1.1,1.2,1.3,1.5,1.7,2]:

    n = 288
    p = 0.007
    phi = 0.4
    delta = i
    tau = 1

    start = time.process_time()

    gammax_opt, gammay_opt, UCL_opt, ARL1_opt, ARL0_opt = opt_param(n,p,phi,delta,tau)  #(4,7,55,12.929036752517904) #2199.008049 s

    end = time.process_time()
    tiempo = end - start

    desv = (ARL1_opt-ARL0_opt)/ARL0_opt

    print(gammax_opt, gammay_opt, UCL_opt, ARL0_opt, ARL1_opt,desv)

    print("Tiempo de ejecución:", tiempo)
    escritura = [n,p,phi,delta,tau,':',gammax_opt, gammay_opt, UCL_opt, ARL0_opt, ARL1_opt,desv,tiempo]

    with open('resultatsextra.txt', 'a') as archivo:
        archivo.write(' '.join(str(i) for i in escritura) + '\n')

