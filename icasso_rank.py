# -*- coding: utf-8 -*-
"""
With credit to
https://github.com/Teekuningas/icasso
for the implementation and the sample code
which was modified to do this

"""

""" 

.. _tut_icasso_fastica:

Extend scikit-learn FastICA example to use Icasso
=================================================

FastICA is fit multiple times to simple example data and performance can be visually inspected.
"""
# Authors: Erkka Heinila <erkka.heinila@jyu.fi>
#
# License BSD (3-clause)

from itertools import cycle

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

from scipy import signal

from sklearn.decomposition import FastICA
from sklearn.decomposition import PCA

# from icasso import Icasso


##############################################################################
# Define functions for extracting bootstraps and unmixing matrices from ica object
def bootstrap_fun(data, generator): 
    return data[generator.choice(range(data.shape[0]), size=data.shape[0]), :]

def unmixing_fun(ica): 
    return ica.components_


##############################################################################

# For replicability
random_state = 50
distance = 0.12

##############################################################################

# Rank Data

ica_params = {
    'n_components': 6,
    'algorithm':'deflation',
    'whiten':True, 
    'fun':'exp'
}


# MONDAY AUTOENCODER
rankPC = pd.read_csv("F:/PCData.csv")

icassoRankPC = Icasso(FastICA, ica_params=ica_params, iterations=30, bootstrap=False,
                vary_init=True) 
icassoRankPC.fit(data=rankPC, fit_params={}, random_state=random_state, 
           unmixing_fun=unmixing_fun)

centro = icassoRankPC.get_centrotype_unmixing(distance = 0.00001)
centrotype_weights = pd.DataFrame(centro[0])
centrotype_weights.to_csv("F:/Rank_PC_Weights.csv")
centrotype_scores = pd.DataFrame(centro[1])
centrotype_scores.to_csv("F:/Rank_PC_Scores.csv")

rankPCSources = np.dot(centrotype_weights, rankPC.T).T
rPC_sources_df = pd.DataFrame(rankPCSources)
rPC_sources_df.to_csv('F:/Rank_PC_Sources.csv')

icassoRankPC.plot_dendrogram()
icassoRankPC.plot_mds(distance=0.5)



