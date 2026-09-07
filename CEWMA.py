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


def CEWMA(n,p0,phi,gammax,gammay,UCL):
    EY = n*p0*(1-phi)
    Y = [EY]
    R = [0]
    B0 = gammay*Y[0] + R[0]
    B = [B0]
    t = 1
    while t > 0:
        X = int(ZIBsim(n,p0,phi)[0])
        Yt = int((gammax*X + B[t-1])/(gammax + gammay))
        Y.append(Yt)
        if Yt > UCL:
            print("¡PROCESO FUERA DE CONTROL! tras", t, "pasos")
            break
        Rt = (gammax*X + B[t-1]) - (gammax + gammay)*Yt
        R.append(Rt)
        Bt = gammay*Yt + Rt
        B.append(Bt)
        t += 1
    return Y,R,B,t

Y,R,B,t = CEWMA(100,0.05,0.7,2,1,5)
print(Y)
print(R)
print(B)
print(t)