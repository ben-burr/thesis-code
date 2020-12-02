# -*- coding: utf-8 -*-
import hdbscan
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.metrics.pairwise import pairwise_distances
from scipy.spatial.distance import euclidean
import seaborn as sns
%matplotlib inline

############################################################
# Tuesday PCA
tues = pd.read_csv("F:/Storage/Thesis/Results/Data_PCA/PCA_Tues.csv")
tuesLabs = pd.read_csv("F:/Storage/Thesis/Results/Data_Base/tuesLabs.csv")
del tues['labels']
clusterer = hdbscan.HDBSCAN(min_cluster_size=1000, min_samples=15, gen_min_span_tree=True, allow_single_cluster=True, cluster_selection_epsilon=0.01)
clusterer.fit(tues)
cluster_labels = pd.DataFrame(clusterer.labels_)
cluster_labels.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_ActualPCA_ClusterLabels.csv")
clusterer.labels_.max()
cluster_probs = pd.DataFrame(clusterer.probabilities_)
cluster_probs.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_ActualPCA_ClusterProbs.csv")
cluster_outlier_scores = pd.DataFrame(clusterer.outlier_scores_)
cluster_outlier_scores.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_ActualPCA_Cluster_Anomaly_Scores.csv")

# Add Cluster Labels and Original Labels
tues = tues.assign(labels_orig=tuesLabs)
tues = tues.assign(labels_clust=cluster_labels)
del tues['labels_orig']
del tues['labels_clust']
tues.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_ActualPCA_Clustered.csv")

# Plots
clusterer.condensed_tree_.plot(select_clusters=True,
                               selection_palette=sns.color_palette('deep', 8))
sns.distplot(clusterer.outlier_scores_[np.isfinite(clusterer.outlier_scores_)], rug=True)
sns.scatterplot(data=tues, x="PC1", y="PC2", style='labels_clust', hue='labels_orig')

# Convert to pandas table
tree = clusterer.condensed_tree_.to_pandas()
cluster_tree = tree[tree.child_size > 1]
cluster_tree.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_ActualPCA_Condensed_Tree.csv")

############################################################
# Tuesday RPCA

tues = pd.read_csv("F:/Storage/Thesis/Results/Data_RPCA/RPCA_Tues.csv")
del tues['label']
del tues['H0']
del tues['H1']
clusterer = hdbscan.HDBSCAN(min_cluster_size=1000, min_samples=15, gen_min_span_tree=True, allow_single_cluster=True, cluster_selection_epsilon=0.02)
clusterer.fit(tues)
cluster_labels = pd.DataFrame(clusterer.labels_)
cluster_labels.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_ActualRPCA_ClusterLabels.csv")
clusterer.labels_.max()
cluster_probs = pd.DataFrame(clusterer.probabilities_)
cluster_probs.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_ActualRPCA_ClusterProbs.csv")
cluster_outlier_scores = pd.DataFrame(clusterer.outlier_scores_)
cluster_outlier_scores.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_ActualRPCA_Cluster_Anomaly_Scores.csv")

# Add Cluster Labels and Original Labels
tues = tues.assign(labels_orig=tuesLabs)
tues = tues.assign(labels_clust=cluster_labels)
del tues['labels_orig']
del tues['labels_clust']
tues.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_ActualRPCA_Clustered.csv")

# Plots
clusterer.condensed_tree_.plot(select_clusters=True,
                               selection_palette=sns.color_palette('deep', 8))
sns.distplot(clusterer.outlier_scores_[np.isfinite(clusterer.outlier_scores_)], rug=True)
sns.scatterplot(data=tues, x="PC1", y="PC2", style='labels_clust', hue='labels_orig')

# Convert to pandas table
tree = clusterer.condensed_tree_.to_pandas()
cluster_tree = tree[tree.child_size > 1]
cluster_tree.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_ActualRPCA_Condensed_Tree.csv")

############################################################
# Tues AE

tues = pd.read_csv("F:/Storage/Thesis/Results/Data_AE/AE_Tues.csv")
del tues['labels']
clusterer = hdbscan.HDBSCAN(min_cluster_size=1000, min_samples=15, gen_min_span_tree=True, allow_single_cluster=True, cluster_selection_epsilon=0.02)
clusterer.fit(tues)
cluster_labels = pd.DataFrame(clusterer.labels_)
cluster_labels.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_ActualAE_ClusterLabels.csv")
clusterer.labels_.max()
cluster_probs = pd.DataFrame(clusterer.probabilities_)
cluster_probs.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_ActualAE_ClusterProbs.csv")
cluster_outlier_scores = pd.DataFrame(clusterer.outlier_scores_)
cluster_outlier_scores.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_ActualAE_Cluster_Anomaly_Scores.csv")
pers = pd.DataFrame(clusterer.cluster_persistence_)
pers.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_ActualAE_Persistence_Scores.csv")

# Plots
clusterer.condensed_tree_.plot(select_clusters=True,
                               selection_palette=sns.color_palette('deep', 8))
sns.distplot(clusterer.outlier_scores_[np.isfinite(clusterer.outlier_scores_)], rug=True)

# Add Cluster Labels and Original Labels
tues = tues.assign(labels_orig=tuesLabs)
tues = tues.assign(labels_clust=cluster_labels)
# del tues['labels_orig']
# del tues['labels_clust']
tues.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_ActualAE_Clustered.csv")
sns.scatterplot(data=tues, x="DF.L1.C1", y="DF.L1.C2", style='labels_clust', hue='labels_orig')

# Convert to pandas table
tree = clusterer.condensed_tree_.to_pandas()
cluster_tree = tree[tree.child_size > 1]
cluster_tree.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_AE_Condensed_Tree.csv")


############################################################
# Tuesday PCA-ICA
tues = pd.read_csv("F:/Storage/Thesis/Results/Data_Clustering/Tues_PCA_AvgICs.csv")
tuesLabs = pd.read_csv("F:/Storage/Thesis/Results/Data_Base/tuesLabs.csv")
clusterer = hdbscan.HDBSCAN(min_cluster_size=1000, min_samples=15, gen_min_span_tree=True, allow_single_cluster=True, cluster_selection_epsilon=0.0002)
clusterer.fit(tues)
cluster_labels = pd.DataFrame(clusterer.labels_)
cluster_labels.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_PCA_ClusterLabels.csv")
clusterer.labels_.max()
cluster_probs = pd.DataFrame(clusterer.probabilities_)
cluster_probs.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_PCA_ClusterProbs.csv")
cluster_outlier_scores = pd.DataFrame(clusterer.outlier_scores_)
cluster_outlier_scores.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_PCA_Cluster_Anomaly_Scores.csv")

# Add Cluster Labels and Original Labels
tues = tues.assign(labels_orig=tuesLabs)
tues = tues.assign(labels_clust=cluster_labels)
del tues['labels_orig']
del tues['labels_clust']
tues.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_PCA_Clustered.csv")

# Plots
clusterer.condensed_tree_.plot(select_clusters=True,
                               selection_palette=sns.color_palette('deep', 8))
sns.distplot(clusterer.outlier_scores_[np.isfinite(clusterer.outlier_scores_)], rug=True)
sns.scatterplot(data=tues, x="V2", y="V8", style='labels_clust', hue='labels_orig')

# Convert to pandas table
tree = clusterer.condensed_tree_.to_pandas()
cluster_tree = tree[tree.child_size > 1]
cluster_tree.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_PCA_Condensed_Tree.csv")


############################################################
# Tuesday RPCA-ICA

tues = pd.read_csv("F:/Storage/Thesis/Results/Data_Clustering/Tues_RPCA_AvgICs.csv")
# del tues['labels']
clusterer = hdbscan.HDBSCAN(min_cluster_size=1000, min_samples=15, gen_min_span_tree=True, allow_single_cluster=True, cluster_selection_epsilon=0.000005)
clusterer.fit(tues)
cluster_labels = pd.DataFrame(clusterer.labels_)
cluster_labels.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_RPCA_ClusterLabels.csv")
clusterer.labels_.max()
cluster_probs = pd.DataFrame(clusterer.probabilities_)
cluster_probs.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_RPCA_ClusterProbs.csv")
cluster_outlier_scores = pd.DataFrame(clusterer.outlier_scores_)
cluster_outlier_scores.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_RPCA_Cluster_Anomaly_Scores.csv")

# Add Cluster Labels and Original Labels
tues = tues.assign(labels_orig=tuesLabs)
tues = tues.assign(labels_clust=cluster_labels)
del tues['labels_orig']
del tues['labels_clust']
tues.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_RPCA_Clustered.csv")

# Plots
clusterer.condensed_tree_.plot(select_clusters=True,
                               selection_palette=sns.color_palette('deep', 8))
sns.distplot(clusterer.outlier_scores_[np.isfinite(clusterer.outlier_scores_)], rug=True)
sns.scatterplot(data=tues, x="V4", y="V6", style='labels_clust', hue='labels_orig')

# Convert to pandas table
tree = clusterer.condensed_tree_.to_pandas()
cluster_tree = tree[tree.child_size > 1]
cluster_tree.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_RPCA_Condensed_Tree.csv")

############################################################
# Tues AE-ICA

tues = pd.read_csv("F:/Storage/Thesis/Results/Data_Clustering/Tues_AE_AvgICs.csv")
clusterer = hdbscan.HDBSCAN(min_cluster_size=1000, min_samples=15, gen_min_span_tree=True, allow_single_cluster=True, cluster_selection_epsilon=0.0000001)
clusterer.fit(tues)
cluster_labels = pd.DataFrame(clusterer.labels_)
cluster_labels.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_AE_ClusterLabels.csv")
clusterer.labels_.max()
cluster_probs = pd.DataFrame(clusterer.probabilities_)
cluster_probs.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_AE_ClusterProbs.csv")
cluster_outlier_scores = pd.DataFrame(clusterer.outlier_scores_)
cluster_outlier_scores.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_AE_Cluster_Anomaly_Scores.csv")
pers = pd.DataFrame(clusterer.cluster_persistence_)
pers.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_AE_Persistence_Scores.csv")

# Plots
clusterer.condensed_tree_.plot(select_clusters=True,
                               selection_palette=sns.color_palette('deep', 8))
sns.distplot(clusterer.outlier_scores_[np.isfinite(clusterer.outlier_scores_)], rug=True)

# Add Cluster Labels and Original Labels
tues = tues.assign(labels_orig=tuesLabs)
tues = tues.assign(labels_clust=cluster_labels)
# del tues['labels_orig']
# del tues['labels_clust']
tues.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_AE_Clustered.csv")
sns.scatterplot(data=tues, x="V4", y="V6", style='labels_clust', hue='labels_orig')

# Convert to pandas table
tree = clusterer.condensed_tree_.to_pandas()
cluster_tree = tree[tree.child_size > 1]
cluster_tree.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_AE_Condensed_Tree.csv")


############################################################
# Rank AE-ICA

rankDat = pd.read_csv("F:/Storage/Thesis/Results/Rank_Output/Rank_PCA_Avg_ICs.csv")
clusterer = hdbscan.HDBSCAN(min_cluster_size=1000, min_samples=15, gen_min_span_tree=True, allow_single_cluster=True, cluster_selection_epsilon=0.00005)
clusterer.fit(rankDat)
cluster_labels = pd.DataFrame(clusterer.labels_)
cluster_labels.to_csv("F:/Storage/Thesis/Results/Rank_Output/Rank_Labels.csv")
clusterer.labels_.max()
cluster_probs = pd.DataFrame(clusterer.probabilities_)
cluster_probs.to_csv("F:/Storage/Thesis/Results/Rank_Output/Rank_ClusterProbs.csv")
cluster_outlier_scores = pd.DataFrame(clusterer.outlier_scores_)
cluster_outlier_scores.to_csv("F:/Storage/Thesis/Results/Rank_Output/Rank_Cluster_Anomaly_Scores.csv")
pers = pd.DataFrame(clusterer.cluster_persistence_)
pers.to_csv("F:/Storage/Thesis/Results/Rank_Output/Rank_Persistence_Scores.csv")

# Plots
clusterer.condensed_tree_.plot(select_clusters=True,
                               selection_palette=sns.color_palette('deep', 8))
sns.distplot(clusterer.outlier_scores_[np.isfinite(clusterer.outlier_scores_)], rug=True)

rankDat = rankDat.assign(labels_clust=cluster_labels)

rankDat.to_csv("F:/Storage/Thesis/Results/Rank_Output/Rank_Clustered.csv")
sns.scatterplot(data=rankDat, x="V1", y="V2", style='labels_clust', hue='labels_clust')

# Convert to pandas table
tree = clusterer.condensed_tree_.to_pandas()
cluster_tree = tree[tree.child_size > 1]
cluster_tree.to_csv("F:/Storage/Thesis/Results/Cluster_Results/Tues_AE_Condensed_Tree.csv")
