setwd("F:/Storage/Thesis/Data/CICIDS2017/")
library(data.table)
library(moments)

# Read in the Data
cic <- data.table(fread("cic2017.csv"))

# Remove binary variables
# cic <- cic[, c(8:36, 41:49, 58:84)]
cic <- cic[!is.na(cic$source_port), ]
cic <- janitor::remove_constant(cic)
cic[, flow_id := NULL]
cic[, source_ip := NULL]
cic[, source_port := NULL]
cic[, destination_ip := NULL]
cic[, destination_port := NULL]
cic[, protocol := NULL]
cic[, timestamp := NULL]
cic[, label := NULL]

cic <- cic[, c(1:29, 32:40, 49:68)]

# NAs
for(i in 1:dim(cic)[2]){
  print(paste0(names(cic)[i], ": ", table(is.na(cic[[i]]))))
}

# Correcting
min(cic$flow_bytes_s[!is.na(cic$flow_bytes_s)])
cic$flow_bytes_s[is.na(cic$flow_bytes_s)] <- -2.61e+08

# Infinites
for(i in 1:dim(cic)[2]){
  print(paste0(names(cic)[i], ": ", table(is.infinite(cic[[i]]))))
}

# Correcting
max(cic$flow_packets_s[!is.infinite(cic$flow_packets_s)])
cic[flow_packets_s[cic$flow_packets_s %in% Inf], flow_packets_s := 4e+06]
max(cic$flow_packets_s)

max(cic$flow_bytes_s[!is.infinite(cic$flow_bytes_s)], na.rm = TRUE)
cic[flow_bytes_s %in% Inf, flow_bytes_s := 2.071e+09]

cic$fwd_header_length[cic$fwd_header_length < 0] <- 0


# First Moments
# Mean
for(i in 1:dim(cic)[2]){
  print(paste0(names(cic)[i], ": ", mean(cic[[i]], na.rm = TRUE)))
}
mean(cic$flow_bytes_s)
mean(cic$flow_packets_s)

# Median
for(i in 1:dim(cic)[2]){
  print(paste0(names(cic)[i], ": ", median(cic[[i]], na.rm = TRUE)))
}


# Second Moments
# Variance
for(i in 1:dim(cic)[2]){
  print(paste0(names(cic)[i], ": ", var(cic[[i]], na.rm = TRUE)))
}

# Median Absolute Deviation
for(i in 1:dim(cic)[2]){
  print(paste0(names(cic)[i], ": ", mad(cic[[i]], na.rm = TRUE)))
}

# Third Moments
for(i in 1:dim(cic)[2]){
  print(paste0(names(cic)[i], ": ", skewness(cic[[i]], na.rm = TRUE)))
}
summary(cic$fwd_header_length)
skewness(cic$fwd_header_length)

# Fourth Moments
for(i in 1:dim(cic)[2]){
  print(paste0(names(cic)[i], ": ", kurtosis(cic[[i]], na.rm = TRUE)))
}


# Correlations ------------------------------------------------------------

library(Hmisc)

# http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software#a-simple-function-to-format-the-correlation-matrix
flattenCorrMatrix <- function(cormat, pmat){
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

# Remove variables that cause corr algorithm to fail
cic[, flow_bytes_s := NULL][, flow_packets_s := NULL][, fwd_header_length := NULL]
# Remove binary variables if still present
# cic <- cic[, c(1:27, 30:37, 46:64)]

corrs <- rcorr(as.matrix(cic))
flatCorrs <- flattenCorrMatrix(corrs$r, corrs$P)
flatCorrs$cor <- round(flatCorrs$cor, 5)
flatCorrs$p <- round(flatCorrs$p, 5)
fwrite(flatCorrs, "Corrs_Flattened.csv")
corrplot::corrplot(corrs$r, method = "square", type = "upper", diag = FALSE,
                   hclust.method = "complete", cl.cex = 0.5, tl.cex = 0.5, tl.offset = 0.45,
                   tl.col = "black", order = "alphabet", p.mat = corrs2$P, sig.level = 0.01,
                   insig = "blank")
# Cluster Them
dissim <- 1 - corrs$r
d <- as.dist(dissim)
plot(hclust(d, method = "average"), main = "Dissimilarity", xlab = "")
