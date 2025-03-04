#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jan 28 10:03:25 2025

@author: sumanbhowmick
"""

import sympy as sym

E, L, P, A, AB, AEn, AEl = sym.symbols('E, L, P, A, AB, AEn, AEl')

alphaA, betaE, gammaL, gammaP, betaL, beta1, betaW, KL = sym.symbols('alphaA, betaE, gammaL, gammaP, betaL, beta1, betaW, KL')
gammaA, betaP, KP, betaA, gammaB, muB, gammaEn = sym.symbols('gammaA, betaP, KP, betaA, gammaB, muB, gammaEn')
gammaEl, sigmaA, gammaem, gammaA0, zeta0 = sym.symbols('gammaEl, sigmaA, gammaem, gammaA0, zeta0')
psi= sym.symbols('psi')

F1 = gammaA0*alphaA*AEl-psi*gammaL*E-betaE*E
F2 = psi*gammaL*E-betaL*L-(beta1*L*L)/KL-gammaP*L-betaW*L
F3 = gammaP*L-betaP*P-gammaA*P
F4 = gammaA*sigmaA*P*sym.exp(-gammaem*(1+P/KP))-betaA*A-gammaB*A
F5 = gammaB*A-betaA*AB-muB*AB-gammaEn*AB
F6 = gammaEn*AB-betaA*AEn-gammaEl*AEn
F7 = gammaEl*AEn-betaA*AEl

X = sym.Matrix([F1, F2, F3, F4, F5, F6, F7])
Z = sym.Matrix([E, L, P, A, AB, AEn, AEl])

F = X.jacobian(Z)

#print(F.eigenvals())
sym.solve([F1, F2, F3, F4, F5, F6, F7], E, L, P, A, AB, AEn, AEl, dict=True)