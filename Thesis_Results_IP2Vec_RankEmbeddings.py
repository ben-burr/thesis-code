# -*- coding: utf-8 -*-

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

res_full = createSentences(fileName = "F:/RankEmbeddingData.csv")

# Parameters based on the original paper were 10 epochs (iter) and hidden layer size of 32.
# One might suggest a hidden layer of 128 based on IP address construction
# but here we will try the one used in the paper. 
source_model = Word2Vec(res_full, workers=8, window=17, min_count=1, max_vocab_size=None, 
                     min_alpha=0.0001, sg=1, hs=0, negative=5, ns_exponent=1.0, iter=10, size=32)

ordered_vocab = [(term, voc.index, voc.count) for term, voc in source_model.wv.vocab.items()]
ordered_vocab = sorted(ordered_vocab, key=lambda k: k[2])
ordered_terms, term_indices, term_counts = zip(*ordered_vocab)
word_vectors = pd.DataFrame(source_model.wv.vectors[term_indices, :], index=ordered_terms)

word_vectors.to_csv("F:/RankWvs.csv")