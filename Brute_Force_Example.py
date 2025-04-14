#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Apr 14 13:55:57 2025

@author: sumanbhowmick
"""

import numpy as np
from scipy.integrate import solve_ivp
import matplotlib.pyplot as plt

# Simulated "True" data for the simple ODE model
true_k = 0.3
t_obs = np.linspace(0, 10, 100)
y0 = [1.0]

def true_model(t, y):
    return [-true_k * y[0]]

true_solution = solve_ivp(true_model, [t_obs[0], t_obs[-1]], y0, t_eval=t_obs)
D = true_solution.y[0] + 0.05 * np.random.randn(len(t_obs))  # Add noise with the solution

# Brute force fitting and parameter space exploration
param_grid = np.linspace(0.1, 0.5, 50)
best_loss = np.inf
best_k = None
best_s = None
best_fit = None

for k in param_grid:
    def ode(t, y): return [-k * y[0]]
    sol = solve_ivp(ode, [t_obs[0], t_obs[-1]], y0, t_eval=t_obs)
    M = sol.y[0]

    # Optimal scaling factor s*
    s_opt = np.dot(D, M) / np.dot(M, M)

    # Compute error
    error = np.sum((D - s_opt * M) ** 2)

    if error < best_loss:
        best_loss = error
        best_k = k
        best_s = s_opt
        best_fit = s_opt * M

# Results
print(f"Best k: {best_k:.3f}, Scaling factor: {best_s:.3f}, Loss:{best_loss:.4f}")

# Plot
plt.plot(t_obs, D, 'o', label='Data', markersize=4)
plt.plot(t_obs, best_fit, label='Fitted ODE (scaled)')
plt.xlabel('Time')
plt.ylabel('y(t)')
plt.title('An example of brute force ODE fit with a scaling factor')
plt.legend()
plt.grid(True)
plt.show()