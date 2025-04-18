# -*- coding: utf-8 -*-
"""
Created on Tue Jan 30 10:17:41 2024

@author: sumanb
"""

"""
Sobol Sensitivity Analysis: Bar Plot

"""

import numpy as np
import seaborn as sns
import pandas as pd
import matplotlib.pyplot as plt
from SALib.sample import saltelli
from SALib.analyze import sobol
z= 1
sigmaA = 0.5

def R0(X): 
    return(
        (z*X[:,0]*X[:,1]*X[:,2]*X[:,3]*X[:,4]*X[:,5]*X[:,6]*X[:,7]*sigmaA*(-1-X[:,8]))/(X[:,9]*(X[:,9]+X[:,3])*(X[:,9]+X[:,4])*(X[:,10]+X[:,6])*(X[:,11]+X[:,7])*(X[:,12]+X[:,2])*(X[:,9]+X[:,5]+X[:,13]))
         )

problem = {'num_vars': 14,
           'names': ['gammaA0', 'alphaA', 'gammaA', 'gammaB', 'gammaEl', 'gammaEn', 'gammaL', 'gammaP', 'gammaem',
                     'betaA', 'betaE', 'betaL', 'betaP', 'muB'],
           'bounds': [
                     [0.01, 0.8],
                     [100, 350],
                     [0.01, 0.6],
                     [0.01, 0.8],
                     [0.01, 0.8],
                     [0.01, 0.8],
                     [0.01, 0.8],
                     [0.01, 0.89],
                     [0.1,0.8],
                     [1/43, 0.8],
                     [0.0262, 0.8],
                     [0.0304, 0.8],
                     [0.0146, 0.8],
                     [0.01, 0.8]
                     ]  
        }

# Generate samples
nSamples = 100000; #typically a large number of samples are required (think 100,000)
param_values = saltelli.sample(problem, nSamples, calc_second_order=True)

# Run model (example)
Y = R0(param_values)

# Perform analysis
Si = sobol.analyze(problem, Y,calc_second_order=True)

Si_filter = {k:Si[k] for k in ['ST','ST_conf','S1','S1_conf']}
Si_df = pd.DataFrame(Si_filter, index=problem['names'])
sns.set(font_scale = 1)
sns.set_style("white")
fig, ax = plt.subplots(1)

indices = Si_df[['S1','ST']]
err = Si_df[['S1_conf','ST_conf']]

indices.plot.bar(yerr=err.values.T,ax=ax)
fig.set_size_inches(8,4)
ax.set_xticklabels(['$\\gamma_{A_{0}}$','$\\alpha_A$','$\\gamma_A$','$\\gamma_B$', '$\\gamma_{El}$', '$\\gamma_{En}$',
                    '$\\gamma_L$','$\\gamma_P$','$\\gamma_{em}$','$\\beta_A$','$\\beta_E$','$\\beta_L$', '$\\beta_P$',
                    '$\\mu_B$'])
plt.savefig('Sobol.eps', dpi=300)
plt.show()








