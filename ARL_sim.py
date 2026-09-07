from random import random
import numpy as np

def ZIBsim(n,p,phi,N = 1):
    simulaciones = []
    for i in range(N):
        rand = random()
        if rand > phi:
            simulaciones.append(int(np.random.binomial(n,p,1)))
        else:
            simulaciones.append(0)
    return simulaciones


def CEWMA(n,p0,phi0,delta,tau,gammax,gammay,UCL):
    EY = n*p0*(1-phi0)
    Y = [EY]
    R = [0]
    B0 = gammay*int(Y[0]) + R[0]
    B = [B0]
    t = 1
    while t > 0:
        X = int(ZIBsim(n,p0*delta,phi0*tau)[0])
        Yt = int((gammax*X + B[t-1])/(gammax + gammay))
        Y.append(Yt)
        if Yt > UCL:
            #print("¡PROCESO FUERA DE CONTROL! tras", t, "pasos")
            break
        Rt = (gammax*X + B[t-1]) - (gammax + gammay)*Yt
        R.append(Rt)
        Bt = gammay*Yt + Rt
        B.append(Bt)
        t += 1
    return Y,R,B,t

for j in range(30):
    T = []
    for i in range(10000):
        #Y, R, B, t = CEWMA(1000, 0.02, 0.8, 2, 8, 10)
        Y, R, B, t = CEWMA(10, 0.5, 0.1,1,1, 1, 11, 5)
        T.append(t)

    media = 0
    for i in range(len(T)):
        media += T[i]

    print(media/len(T))