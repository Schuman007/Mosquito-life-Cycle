# -*- coding: utf-8 -*-
"""
Created on Thu Feb 23 11:34:19 2023

@author: sumanb
"""

import sympy as sym

E, L, P, A, AB, AEn, AEl = sym.symbols('E, L, P, A, AB, AEn, AEl')
alphaA, z, gammaL, betaE, betaL, beta1, gammaP, betaP = sym.symbols('alphaA, z, gammaL, betaE, betaL, beta1, gammaP, betaP')
gammaA, sigmaA, gammaem, KP, betaA, gammaB = sym.symbols('gammaA, sigmaA, gammaem, KP, betaA, gammaB')
muB, gammaEn, gammaEl, KL =  sym.symbols('muB, gammaEn, gammaEl, KL')

F1 = alphaA*AEl
F2 = F3 = F4 = F5 = F6 = F7 = 0

V1 = -(z*gammaL*E+betaE*E)
V2 = -z*gammaL*E+betaL*L+(beta1*L*L)/KL+gammaP*L
V3 = -gammaP*L+betaP*P+gammaA*P
V4 = -gammaA*sigmaA*P*sym.exp(-gammaem*(1+P/KP))+betaA*A+gammaB*A
V5 = -gammaB*A+betaA*AB+muB*AB+gammaEn*AB
V6 = -gammaEn*AB+betaA*AEn+gammaEl*AEn
V7 = -gammaEl*AEn+betaA*AEl



X = sym.Matrix([F1, F2, F3, F4, F5, F6, F7])
Y = sym.Matrix([V1, V2, V3, V4, V5, V6, V7])
Z = sym.Matrix([E, L, P, A, AB, AEn, AEl])

F = X.jacobian(Z)
V = Y.jacobian(Z)

NGMos = -F*V.inv()

R0 = NGMos[0,0]
R01 = R0.subs([(E, 0), (L, 0), (P, 0), (A, 0), (AB, 0), (AEn, 0), (AEl, 0)])
Offspring_Number=sym.factor(R01)
print(sym.latex(Offspring_Number))