# -*- coding: utf-8 -*-
# Libraries
import nltk
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import gensim
from gensim.models import Word2Vec
from sklearn.manifold import TSNE
import re
import itertools
import operator
import random
from gensim.models import KeyedVectors

# Functions

def createSentences(fileName, header=False):
    resultList = list()

    with open(fileName, "r") as csvFile:
        lineCount = 0
        line = csvFile.readline()

        # Skip the header!
        if header:
            line = csvFile.readline()

        while line:
            data = line.strip().split(",")
            lineCount+=1
            # print(", ".join(data))
            resultList.append(data)
            line = csvFile.readline()

    return resultList



###############################################################################

# Read in the source data, which includes Source/Destination IP/Port, and Protocol 

res_full = createSentences(fileName = "F:/Storage/Thesis/Data/CICIDS2017/CICIDS_IPs.csv")

# Parameters based on the original paper were 10 epochs (iter) and hidden layer size of 32.
# One might suggest a hidden layer of 128 based on IP address construction
# but here we will try the one used in the paper. 
source_model = Word2Vec(res_full, workers=8, window=4, min_count=1, max_vocab_size=None, 
                     min_alpha=0.0001, sg=1, hs=0, negative=5, ns_exponent=1.0, iter=10, size=32)

# To extract the word vectors: 
source_wv = source_model.wv

# Insert vectors into data frame
df = []
for word in source_wv.vocab:
    vector=source_wv.get_vector(word)
    df.append(vector)
voc=source_wv.vocab
df = pd.DataFrame(df, index=voc)

df.to_csv("F:/Storage/Thesis/Data/CICIDS2017/CICIDS_WordVectors.csv")



