# Load Packages -----------------------------------------------------------

library(data.table)
library(stringr)
library(PCDimension)
library(plotly)
library(rgl)


# Load Data ---------------------------------------------------------------

# The first 84 columns are as provided in the dataset
# The remaining 160 columns are 5 variables of 32-dimension Word2Vec embeddings
setwd("F:/Storage/Thesis/Data/CICIDS2017/")
cic <- data.table(fread("cicids2017_full_with_WordVectors.csv"))

# Remove the embedded variables
cic[, flow_id := NULL]
cic[, source_ip := NULL]
cic[, source_port := NULL]
cic[, destination_ip := NULL]
cic[, destination_port := NULL]
cic[, protocol := NULL]

# Remove time variables
cic[, timestamp := NULL]
cic[, time := NULL]
cic[, hour := NULL]
cic[, date := NULL]

# Extract labels for later
labels <- cic$label
cic[, label := NULL]

# Remove binary variables
cic <- cic[, c(1:29, 32:40, 49:228)]

# First 58 Variables
cic <- cic[, 1:58]


# Standardize -----------------------------------------------------------------


cicstd <- as.data.table(lapply(cic, scale, center = TRUE, scale = TRUE))


# PCA ---------------------------------------------------------------------


cicpcs <- prcomp(cicstd[, 1:58], scale. = FALSE, center = FALSE, retx = TRUE)

# Extract rotated data (principal components)
PCDat <- as.data.table(cicpcs$x)

# Eigenvalues and Cutoff Values
PCTab <- data.table()
PCTab <- as.data.table(matrix(NA, nrow = dim(PCDat)[2]))
PCTab[, eigenvalues := round(cicpcs$sdev^2, 5)]
eigenSum <- sum(PCTab$eigenvalues) # or dim(PCTab)[1] because correlations
PCTab[, explained := round(PCTab$eigenvalues/eigenSum, 5)]
PCTab[, meancutoff := (eigenvalues > mean(eigenvalues))]
PCTab[, scaledcutoff := (eigenvalues > 0.7 * mean(eigenvalues))]
bscut <- bsDimension(lambda = PCTab$eigenvalues)
PCTab[, bscutoff := c(rep(TRUE, bscut), rep(FALSE, dim(PCTab)[1] - bscut))]
PCTab[, bsModel :=  brokenStick(1:dim(PCDat)[2], dim(PCDat)[2])]
pExpl <- vector(length = dim(PCTab)[1])
for(i in 1:dim(PCTab)[1]){
  pExpl[i] <- sum(PCTab$explained[1:i])
}
PCTab[, propnExpl := round(pExpl * 100, 3)]
PCTab[, V1 := NULL]
rm(bscut, i, pExpl, cicpcs, eigenSum)
fwrite(PCTab, "CPCA_Table.csv")

# Screeplot
bsVals <- brokenStick(1:58, 58)
plot(PCTab$explained, lty = 1, pch = 20, type = "b",
     main = "Broken Stick Model vs. Eigenvalues",
     xlab = "Component Number",
     ylab = "Variation by Model")
xtick <- seq(1, 58, by = 1)
axis(side = 1, at = xtick, labels = FALSE)
lines(bsVals, lty = 2, pch = 18, col = "red", type = "b")
PCTab[14, ]
PCTab[19, ]
PCTab[5, ]
symbols(x = c(14, 19, 5), y = c(0.01754, 0.01292, 0.05139),
        circles = c(0.3, 0.3, 0.3), bg = "orange",
        add = TRUE, inches = FALSE, fg = "orange")
text(x = 10, y = 0.07, "Broken Stick = 5")
text(x = 15, y = 0.045, "Mean Eigenvalue = 14")
text(x = 20, y = 0.03, "Scaled Mean Eigenvalue = 19")

# Extract resulting PCs
cicPCAAvg <- as.data.table(PCDat[, 1:sum(PCTab$meancutoff)])
cicPCAAvg[, label := labels]
cicPCABS <- as.data.table(PCDat[, 1:sum(PCTab$bscutoff)])
cicPCABS[, label := labels]
PCAsd <- as.data.table(cicpcs$sdev)
PCARotation <- as.data.table(cicpcs$rotation)
PCACenter <- as.data.table(cicpcs$center)
PCAScale <- as.data.table(cicpcs$scale)

# Write all to file for future use: Standard Scaling/58 Vars
fwrite(PCDat, "~/PCA_58Vars_StdScaling.csv")
fwrite(cicPCAAvg, "~/PCA_58Vars_StdScaling_AvgCutoff.csv")
fwrite(cicPCABS, "~/PCA_58Vars_StdScaling_BSCutoff.csv")
fwrite(PCAsd, "~/PCA_58Vars_StdScaling_sd.csv")
fwrite(PCARotation, "~/PCA_58Vars_StdScaling_Rotation.csv")
fwrite(PCACenter, "~/PCA_58Vars_StdScaling_Center.csv")
fwrite(PCAScale, "~/PCA_58Vars_StdScaling_Scale.csv")

# Clear Environment of extra items
# rm(cic, cicstd, cicpcs, cicPCAFull, vt, kAvg, pExpl, kBS, cicPCAAvg,
#    cicPCABS, PCAsd, PCARotation, PCACenter, PCAScale)


# Word Embeddings Version ---------------------------------------------------------

# Start over with augmented (embeddings) and subset the data (daily)
cic <- data.table(fread("cicids2017_full_with_WordVectors.csv"))

cic[, flow_id := NULL]
cic[, source_ip := NULL]
cic[, source_port := NULL]
cic[, destination_ip := NULL]
cic[, destination_port := NULL]
cic[, protocol := NULL]

# Remove binary variables
cic <- cic[, c(1:29, 32:40, 49:228)]


# Extract Daily Data ------------------------------------------------------


# Classical Standardization


# Monday ------------------------------------------------------------------

cic_mon <- cic[as.character(cic$date) %in% "2017-07-03"]
monLabs <- cic_mon$label
cic_mon[, date := NULL][, time := NULL][, timestamp := NULL][, hour := NULL]
cic_mon[, label := NULL][, fwd_header_length := NULL]
cic_mon <- janitor::remove_constant(cic_mon)

# Tuesday ------------------------------------------------------------------

cic_tues <- cic[as.character(cic$date) %in% "2017-07-04"]
tuesLabs <- cic_tues$label
cic_tues[, date := NULL][, time := NULL][, timestamp := NULL][, hour := NULL]
cic_tues[, label := NULL][, fwd_header_length := NULL]
cic_tues <- janitor::remove_constant(cic_tues)

# Wednesday ------------------------------------------------------------------

cic_wed <- cic[as.character(cic$date) %in% "2017-07-05"]
wedLabs <- cic_wed$label
cic_wed[, date := NULL][, time := NULL][, timestamp := NULL][, hour := NULL]
cic_wed[, label := NULL][, fwd_header_length := NULL]
cic_wed <- janitor::remove_constant(cic_wed)

# Thursday ------------------------------------------------------------------

cic_thurs <- cic[as.character(cic$date) %in% "2017-07-06"]
thursLabs <- cic_thurs$label
cic_thurs[, date := NULL][, time := NULL][, timestamp := NULL][, hour := NULL]
cic_thurs[, label := NULL][, fwd_header_length := NULL]
cic_thurs <- janitor::remove_constant(cic_thurs)

# Friday ------------------------------------------------------------------

cic_fri <- cic[as.character(cic$date) %in% "2017-07-07"]
friLabs <- cic_fri$label
cic_fri[, date := NULL][, time := NULL][, timestamp := NULL][, hour := NULL]
cic_fri[, label := NULL][, fwd_header_length := NULL]
cic_fri <- janitor::remove_constant(cic_fri)

# Write Subsets to File ---------------------------------------------------

fwrite(cic_mon, "cic_mon.csv")
fwrite(cic_tues, "cic_tues.csv")
fwrite(cic_wed, "cic_wed.csv")
fwrite(cic_thurs, "cic_thurs.csv")
fwrite(cic_fri, "cic_fri.csv")
fwrite(as.data.table(monLabs), "monLabs.csv")
fwrite(as.data.table(tuesLabs), "tuesLabs.csv")
fwrite(as.data.table(wedLabs), "wedLabs.csv")
fwrite(as.data.table(thursLabs), "thursLabs.csv")
fwrite(as.data.table(friLabs), "friLabs.csv")


# Classical PCA: Daily Version --------------------------------------------

monpcs <- prcomp(cic_mon, scale. = TRUE, center = TRUE, retx = TRUE)
tuespcs <- prcomp(cic_tues, scale. = TRUE, center = TRUE, retx = TRUE)
wedpcs <- prcomp(cic_wed, scale. = TRUE, center = TRUE, retx = TRUE)
thurpcs <- prcomp(cic_thurs, scale. = TRUE, center = TRUE, retx = TRUE)
fripcs <- prcomp(cic_fri, scale. = TRUE, center = TRUE, retx = TRUE)
# rm(cic_mon, cic_tues, cic_wed, cic_thurs, cic_fri)

# Extract rotated data (principal components)
mPCDat <- as.data.table(monpcs$x)
tuPCDat <- as.data.table(tuespcs$x)
wPCDat <- as.data.table(wedpcs$x)
thPCDat <- as.data.table(thurpcs$x)
fPCDat <- as.data.table(fripcs$x)


# PCA Dimension Reduction Values ------------------------------------------

# Monday ------------------------------------------------------------------
mPCTab <- data.table()
mPCTab <- as.data.table(matrix(NA, nrow = dim(mPCDat)[2]))
mPCTab[, eigenvalues := round(monpcs$sdev^2, 5)]
eigenSum <- sum(mPCTab$eigenvalues) # or dim(mPCTab)[1] because correlations
mPCTab[, explained := round(mPCTab$eigenvalues/eigenSum, 5)]
mPCTab[, meancutoff := (eigenvalues > mean(eigenvalues))]
mPCTab[, scaledcutoff := (eigenvalues > 0.7 * mean(eigenvalues))]
bscut <- bsDimension(lambda = mPCTab$eigenvalues)
mPCTab[, bscutoff := c(rep(TRUE, bscut), rep(FALSE, dim(mPCTab)[1] - bscut))]
mPCTab[, bsModel :=  brokenStick(1:dim(mPCDat)[2], dim(mPCDat)[2])]
pExpl <- vector(length = dim(mPCTab)[1])
for(i in 1:dim(mPCTab)[1]){
  pExpl[i] <- sum(mPCTab$explained[1:i])
}
mPCTab[, propnExpl := round(pExpl * 100, 3)]
mPCTab[, V1 := NULL]
rm(bscut, i, pExpl, monpcs, eigenSum)
fwrite(mPCTab, "MPCA_Table.csv")

# Tuesday -----------------------------------------------------------------
tuPCTab <- data.table()
tuPCTab <- as.data.table(matrix(NA, nrow = dim(tuPCDat)[2]))
tuPCTab[, eigenvalues := round(tuespcs$sdev^2, 5)]
eigenSum <- sum(tuPCTab$eigenvalues) # or dim(tuPCTab)[1] because correlations
tuPCTab[, explained := round(tuPCTab$eigenvalues/eigenSum, 5)]
tuPCTab[, meancutoff := (eigenvalues > mean(eigenvalues))]
tuPCTab[, scaledcutoff := (eigenvalues > 0.7 * mean(eigenvalues))]
bscut <- bsDimension(tuPCTab$eigenvalues)
tuPCTab[, bscutoff := c(rep(TRUE, bscut), rep(FALSE, dim(tuPCTab)[1] - bscut))]
tuPCTab[, bsModel :=  brokenStick(1:dim(tuPCDat)[2], dim(tuPCDat)[2])]
pExpl <- vector(length = dim(tuPCTab)[1])
for(i in 1:dim(tuPCTab)[1]){
  pExpl[i] <- sum(tuPCTab$explained[1:i])
}
tuPCTab[, propnExpl := round(pExpl * 100, 3)]
tuPCTab[, V1 := NULL]
rm(bscut, i, pExpl, tuespcs)
fwrite(tuPCTab, "TuPCA_Table.csv")

# Wednesday ---------------------------------------------------------------
wPCTab <- data.table()
wPCTab <- as.data.table(matrix(NA, nrow = dim(wPCDat)[2]))
wPCTab[, eigenvalues := round(wedpcs$sdev^2, 5)]
eigenSum <- sum(wPCTab$eigenvalues) # or dim(wPCTab)[1] because correlations
wPCTab[, explained := round(wPCTab$eigenvalues/eigenSum, 5)]
wPCTab[, meancutoff := (eigenvalues > mean(eigenvalues))]
wPCTab[, scaledcutoff := (eigenvalues > 0.7 * mean(eigenvalues))]
bscut <- bsDimension(wPCTab$eigenvalues)
wPCTab[, bscutoff := c(rep(TRUE, bscut), rep(FALSE, dim(wPCTab)[1] - bscut))]
pExpl <- vector(length = dim(wPCTab)[1])
for(i in 1:dim(wPCTab)[1]){
  pExpl[i] <- sum(wPCTab$explained[1:i])
}
wPCTab[, propnExpl := round(pExpl * 100, 3)]
wPCTab[, V1 := NULL]
rm(bscut, i, pExpl, wedpcs)
fwrite(wPCTab, "WedPCA_Table.csv")

# Thursday ----------------------------------------------------------------
thPCTab <- data.table()
thPCTab <- as.data.table(matrix(NA, nrow = dim(thPCDat)[2]))
thPCTab[, eigenvalues := round(thurpcs$sdev^2, 5)]
eigenSum <- sum(thPCTab$eigenvalues) # or dim(thPCTab)[1] because correlations
thPCTab[, explained := round(thPCTab$eigenvalues/eigenSum, 5)]
thPCTab[, meancutoff := (eigenvalues > mean(eigenvalues))]
thPCTab[, scaledcutoff := (eigenvalues > 0.7 * mean(eigenvalues))]
bscut <- bsDimension(thPCTab$eigenvalues)
thPCTab[, bscutoff := c(rep(TRUE, bscut), rep(FALSE, dim(thPCTab)[1] - bscut))]
pExpl <- vector(length = dim(thPCTab)[1])
for(i in 1:dim(thPCTab)[1]){
  pExpl[i] <- sum(thPCTab$explained[1:i])
}
thPCTab[, propnExpl := round(pExpl * 100, 3)]
thPCTab[, V1 := NULL]
rm(bscut, i, pExpl, thurpcs)
fwrite(thPCTab, "ThursPCA_Table.csv")

# Friday ------------------------------------------------------------------
fPCTab <- data.table()
fPCTab <- as.data.table(matrix(NA, nrow = dim(fPCDat)[2]))
fPCTab[, eigenvalues := round(fripcs$sdev^2, 5)]
eigenSum <- sum(fPCTab$eigenvalues) # or dim(fPCTab)[1] because correlations
fPCTab[, explained := round(fPCTab$eigenvalues/eigenSum, 5)]
fPCTab[, meancutoff := (eigenvalues > mean(eigenvalues))]
fPCTab[, scaledcutoff := (eigenvalues > 0.7 * mean(eigenvalues))]
bscut <- bsDimension(fPCTab$eigenvalues)
fPCTab[, bscutoff := c(rep(TRUE, bscut), rep(FALSE, dim(fPCTab)[1] - bscut))]
pExpl <- vector(length = dim(fPCTab)[1])
for(i in 1:dim(fPCTab)[1]){
  pExpl[i] <- sum(fPCTab$explained[1:i])
}
fPCTab[, propnExpl := round(pExpl * 100, 3)]
fPCTab[, V1 := NULL]
rm(bscut, i, pExpl, fripcs)
fwrite(fPCTab, "FriPCA_Table.csv")

# Screeplots --------------------------------------------------------------
bsVals <- brokenStick(1:211, 211)

# Monday

plot(mPCTab$explained, lty = 1, pch = 20, type = "b",
     main = "Broken Stick Model vs. Eigenvalues",
     xlab = "Component Number",
     ylab = "Variation by Model")
xtick <- seq(1, 211, by = 1)
axis(side = 1, at = xtick, labels = FALSE)
lines(bsVals, lty = 2, pch = 18, col = "red", type = "b")
mPCTab[32, ]
mPCTab[44, ]
mPCTab[11, ]
symbols(x = c(32, 44, 11), y = c(0.00495, 0.00346, 0.01948),
        circles = rep(1, 2, 3), bg = "orange",
        add = TRUE, inches = FALSE, fg = "orange")
text(x = 25, y = 0.04, "Broken Stick = 11")
text(x = 35, y = 0.03, "Mean Eigenvalue = 32")
text(x = 45, y = 0.02, "Scaled Mean Eigenvalue = 44")

# Tuesday
plot(tuPCTab$explained, lty = 1, pch = 20, type = "b",
     main = "Broken Stick Model vs. Eigenvalues",
     xlab = "Component Number",
     ylab = "Variation by Model")
xtick <- seq(1, 211, by = 1)
axis(side = 1, at = xtick, labels = FALSE)
lines(bsVals, lty = 2, pch = 18, col = "red", type = "b")
tuPCTab[36, ]
tuPCTab[45, ]
tuPCTab[12, ]
symbols(x = c(36, 45, 12), y = c(0.00476, 0.00339, 0.02040),
        circles = rep(1, 2, 3), bg = "orange",
        add = TRUE, inches = FALSE, fg = "orange")
text(x = 25, y = 0.04, "Broken Stick = 12")
text(x = 35, y = 0.03, "Mean Eigenvalue = 36")
text(x = 45, y = 0.02, "Scaled Mean Eigenvalue = 45")

# Wednesday
plot(wPCTab$explained, lty = 1, pch = 20, type = "b",
     main = "Broken Stick Model vs. Eigenvalues",
     xlab = "Component Number",
     ylab = "Variation by Model")
xtick <- seq(1, 211, by = 1)
axis(side = 1, at = xtick, labels = FALSE)
lines(bsVals, lty = 2, pch = 18, col = "red", type = "b")
wPCTab[33, ]
wPCTab[45, ]
wPCTab[9, ]
symbols(x = c(33, 45, 9), y = c(0.00482, 0.00350, 0.02254),
        circles = rep(1, 2, 3), bg = "orange",
        add = TRUE, inches = FALSE, fg = "orange")
text(x = 25, y = 0.04, "Broken Stick = 9")
text(x = 35, y = 0.03, "Mean Eigenvalue = 33")
text(x = 45, y = 0.02, "Scaled Mean Eigenvalue = 45")

# Thursday
bsValsTh <- brokenStick(1:212, 212)
plot(thPCTab$explained, lty = 1, pch = 20, type = "b",
     main = "Broken Stick Model vs. Eigenvalues",
     xlab = "Component Number",
     ylab = "Variation by Model")
xtick <- seq(1, 212, by = 1)
axis(side = 1, at = xtick, labels = FALSE)
lines(bsValsTh, lty = 2, pch = 18, col = "red", type = "b")
thPCTab[35, ]
thPCTab[46, ]
thPCTab[12, ]
symbols(x = c(35, 46, 12), y = c(0.00485, 0.00333, 0.01918),
        circles = rep(1, 2, 3), bg = "orange",
        add = TRUE, inches = FALSE, fg = "orange")
text(x = 25, y = 0.04, "Broken Stick = 12")
text(x = 35, y = 0.03, "Mean Eigenvalue = 35")
text(x = 45, y = 0.02, "Scaled Mean Eigenvalue = 46")

# Friday
plot(fPCTab$explained, lty = 1, pch = 20, type = "b",
     main = "Broken Stick Model vs. Eigenvalues",
     xlab = "Component Number",
     ylab = "Variation by Model")
xtick <- seq(1, 211, by = 1)
axis(side = 1, at = xtick, labels = FALSE)
lines(bsVals, lty = 2, pch = 18, col = "red", type = "b")
fPCTab[33, ]
fPCTab[43, ]
fPCTab[10, ]
symbols(x = c(33, 43, 10), y = c(0.00482, 0.00334, 0.02209),
        circles = rep(1, 2, 3), bg = "orange",
        add = TRUE, inches = FALSE, fg = "orange")
text(x = 25, y = 0.04, "Broken Stick = 10")
text(x = 35, y = 0.03, "Mean Eigenvalue = 33")
text(x = 45, y = 0.02, "Scaled Mean Eigenvalue = 43")

# Dimension Reduction -----------------------------------------------------
mPCDat <- mPCDat[ , 1:44]
mPCDat[, labels := monLabs]
tuPCDat <- tuPCDat[, 1:45]
tuPCDat[, labels := tuesLabs]
wPCDat <- wPCDat[, 1:45]
wPCDat[, labels := wedLabs]
thPCDat <- thPCDat[, 1:46]
thPCDat[, labels := thursLabs]
fPCDat <- fPCDat[, 1:43]
fPCDat[, labels := friLabs]

# Robust Centering --------------------------------------------------------

# Monday ------------------------------------------------------------------
rstd_mon <- cic_mon
for(i in 1:dim(cic_mon)[2]){
  rstd_mon[[i]] <- robustHD::robStandardize(rstd_mon[[i]],
                                            centerFun = median,
                                            scaleFun = mad)
}
nas <- list()
for(i in 1:dim(cic_mon)[2]){
  nas[[i]] <- table(is.na(rstd_mon[[i]]))
}
# Many variables were unstable when trying this.
# Hereafter, Robust Centering was discarded
rm(rstd_mon, nas)

# Robust PCA --------------------------------------------------------------
library(rospca)
library(robustHD)

# For reproducibility on PP
set.seed(13579)

# Monday ------------------------------------------------------------------

# Standardize
for(i in 1:dim(cic_mon)[2]){
  cic_mon[[i]] <- scale(cic_mon[[i]], center = TRUE, scale = TRUE)
}

# ROBPCA requires a matrix.
# The bigmemory package may be used to make it a big.matrix if needed
cic_mon <- as.matrix(cic_mon)

monRPCs <- robpca(cic_mon, kmax = dim(cic_mon)[2], ndir = 1000, skew = TRUE)
monRPCData <- as.data.table(monRPCs$scores)
monRPCData[, label := monLabs]
monRPCData[, H0 := monRPCs$H0]
monRPCData[, H1 := monRPCs$H1]
monREigen <- as.data.table(monRPCs$eigenvalues)
monREigen
mean(monRPCs$sd)
mean(monRPCs$od)
var(monRPCs$sd)
var(monRPCs$od)
monRPCs$cutoff.sd
monRPCs$cutoff.od
table(monRPCData$label)
table(monRPCData$H0)
table(monRPCData$H1)
table(monRPCData$label, monRPCData$H0)
table(monRPCData$label, monRPCData$H1)
table(monRPCData$label, monRPCData$H0, monRPCData$H1)
fwrite(monRPCData, "Mon_RobustPCs.csv")
fwrite(monREigen, "Mon_REigen.csv")
rm(monRPCData, cic_mon, monREigen, monRPCs, monLabs, monH1, i)

# Tuesday ------------------------------------------------------------------

for(i in 1:dim(cic_tues)[2]){
  cic_tues[[i]] <- scale(cic_tues[[i]], center = TRUE, scale = TRUE)
}
cic_tues <- as.matrix(cic_tues)
tuesRPCs <- robpca(cic_tues, kmax = dim(cic_tues)[2], ndir = 1000, skew = TRUE)
tuesRPCData <- as.data.table(tuesRPCs$scores)
tuesRPCData[, label := tuesLabs]
tuesRPCData[, H0 := tuesRPCs$H0]
tuesRPCData[, H1 := tuesRPCs$H1]
tuesREigen <- as.data.table(tuesRPCs$eigenvalues)
tuesREigen
mean(tuesRPCs$sd)
mean(tuesRPCs$od)
var(tuesRPCs$sd)
var(tuesRPCs$od)
tuesRPCs$cutoff.sd
tuesRPCs$cutoff.od
table(tuesRPCData$label)
table(tuesRPCData$H0)
table(tuesRPCData$H1)
table(tuesRPCData$label, tuesRPCData$H0)
table(tuesRPCData$label, tuesRPCData$H1)
table(tuesRPCData$label, tuesRPCData$H0, tuesRPCData$H1)
fwrite(tuesRPCData, "Tues_RobustPCs.csv")
fwrite(tuesREigen, "Tues_REigen.csv")
rm(tuesRPCData, cic_tues, tuesREigen, tuesRPCs, tuesLabs, tuesH1, i)

# Wednesday ------------------------------------------------------------------
for(i in 1:dim(cic_wed)[2]){
  cic_wed[[i]] <- scale(cic_wed[[i]], center = TRUE, scale = TRUE)
}
cic_wed <- as.matrix(cic_wed)
wedRPCs <- robpca(cic_wed, kmax = dim(cic_wed)[2], ndir = 1000, skew = TRUE)
wedRPCData <- as.data.table(wedRPCs$scores)
wedRPCData[, label := wedLabs]
wedRPCData[, H0 := wedRPCs$H0]
wedRPCData[, H1 := wedRPCs$H1]
wedREigen <- as.data.table(wedRPCs$eigenvalues)
wedREigen
mean(wedRPCs$sd)
mean(wedRPCs$od)
wedRPCs$cutoff.sd
wedRPCs$cutoff.od
var(wedRPCs$sd)
var(wedRPCs$od)
table(wedRPCData$label)
table(wedRPCData$H0)
table(wedRPCData$H1)
table(wedRPCData$label, wedRPCData$H0)
table(wedRPCData$label, wedRPCData$H1)
table(wedRPCData$label, wedRPCData$H0, wedRPCData$H1)
fwrite(wedRPCData, "Wed_RobustPCs.csv")
fwrite(wedREigen, "Wed_REigen.csv")
# rm(wedRPCData, cic_wed, wedREigen, wedRPCs, wedLabs, wedH1, i)

# Thursday ------------------------------------------------------------------
for(i in 1:dim(cic_wed)[2]){
  cic_thurs[[i]] <- scale(cic_thurs[[i]], center = TRUE, scale = TRUE)
}
cic_thurs <- as.matrix(cic_thurs)
thursRPCs <- robpca(cic_thurs, kmax = dim(cic_thurs)[2], ndir = 1000, skew = TRUE)
thursRPCData <- as.data.table(thursRPCs$scores)
thursRPCData[, label := thursLabs]
thursRPCData[, H0 := thursRPCs$H0]
thursRPCData[, H1 := thursRPCs$H1]
thursREigen <- as.data.table(thursRPCs$eigenvalues)
thursREigen
mean(thursRPCs$sd)
mean(thursRPCs$od)
thursRPCs$cutoff.sd
thursRPCs$cutoff.od
var(thursRPCs$sd)
var(thursRPCs$od)
table(thursRPCData$label)
table(thursRPCData$H0)
table(thursRPCData$H1)
table(thursRPCData$label, thursRPCData$H0)
table(thursRPCData$label, thursRPCData$H1)
table(thursRPCData$label, thursRPCData$H0, thursRPCData$H1)
fwrite(thursRPCData, "Thurs_RobustPCs.csv")
fwrite(thursREigen, "Thurs_REigen.csv")
# rm(thursRPCData, cic_thurs, thursREigen, thursRPCs, thursLabs, thursH1, i)

# Friday ------------------------------------------------------------------
for(i in 1:dim(cic_fri)[2]){
  cic_fri[[i]] <- scale(cic_fri[[i]], center = TRUE, scale = TRUE)
}
cic_fri <- as.matrix(cic_fri)
friRPCs <- robpca(cic_fri, kmax = dim(cic_fri)[2], ndir = 1000, skew = TRUE)
friRPCData <- as.data.table(friRPCs$scores)
friRPCData[, label := friLabs]
friRPCData[, H0 := friRPCs$H0]
friRPCData[, H1 := friRPCs$H1]
friREigen <- as.data.table(friRPCs$eigenvalues)
friREigen
mean(friRPCs$sd)
mean(friRPCs$od)
var(friRPCs$sd)
var(friRPCs$od)
friRPCs$cutoff.sd
friRPCs$cutoff.od
table(friRPCData$label)
table(friRPCData$H0)
table(friRPCData$H1)
table(friRPCData$label, friRPCData$H0)
table(friRPCData$label, friRPCData$H1)
table(friRPCData$label, friRPCData$H0, friRPCData$H1)
fwrite(friRPCData, "Fri_RobustPCs.csv")
fwrite(friREigen, "Fri_REigen.csv")

# RPCA Density Plots ------------------------------------------------------
library(ggplot2)
library(ggridges)
library(ggthemes)

# Monday
ggplot(monRPCData,
       aes(x = monRPCData$PC1,
           y = label,
           fill = label)) +
  ggridges::geom_density_ridges() +
  ggtitle("Density of PC1 for Monday CICIDS2017 Dataset") +
  labs(y = "Traffic Type", x = "PC1", fill = "Traffic Type") +
  ggthemes::theme_tufte()

# Tuesday
ggplot(tuesRPCData,
       aes(x = tuesRPCData$PC1,
           y = label,
           fill = label)) +
  ggridges::geom_density_ridges() +
  ggtitle("Density of PC1 for Tuesday CICIDS2017 Dataset") +
  labs(y = "Traffic Type", x = "PC1", fill = "Traffic Type") +
  ggthemes::theme_tufte()

# Wednesday
ggplot(wedRPCData,
       aes(x = wedRPCData$PC1,
           y = label,
           fill = label)) +
  ggridges::geom_density_ridges() +
  ggtitle("Density of PC1 for Wednesday CICIDS2017 Dataset") +
  labs(y = "Traffic Type", x = "PC1", fill = "Traffic Type") +
  ggthemes::theme_tufte()

# Thursday
thursRPCData <- data.table(fread("~/Thurs_RobustPCs.csv"))
thursRPCData$label <- thursRPCData[, fifelse(label %in% "Web Attack \x96 Brute Force", "Web Attack x96 Brute Force",
                                             fifelse(label %in% "Web Attack \x96 Sql Injection", "Web Attack x96 Sql Injection",
                                                     fifelse(label %in% "Web Attack \x96 XSS", "Web Attack x96 XSS", label)))]
ggplot(thursRPCData,
       aes(x = thursRPCData$PC1,
           y = label,
           fill = label)) +
  ggridges::geom_density_ridges() +
  ggtitle("Density of PC1 for Thursday CICIDS2017 Dataset") +
  labs(y = "Traffic Type", x = "PC1", fill = "Traffic Type") +
  ggthemes::theme_tufte()

# Friday
ggplot(friRPCData,
       aes(x = friRPCData$PC1,
           y = label,
           fill = label)) +
  ggridges::geom_density_ridges() +
  ggtitle("Density of PC1 for Friday CICIDS2017 Dataset") +
  labs(y = "Traffic Type", x = "PC1", fill = "Traffic Type") +
  ggthemes::theme_tufte()

# Autoencoder -------------------------------------------------------------

library(h2o)


# Initialize H2O

localH2o <- h2o.init(ip = "localhost", port = 54321, nthreads = -1, max_mem_size = "8g")


# Monday

# Convert Data to H2O dataframe
mh2odat <- as.h2o(cic_mon)
# Create Feature Set
mfeatures <- colnames(mh2odat)
# Set Size of Hidden Layer
mhide <- 12
# Run Model
mh2oModel <- h2o.deeplearning(x = mfeatures,
                              training_frame = mh2odat,
                              autoencoder = TRUE,
                              reproducible = TRUE,
                              seed = 321,
                              hidden = c(mhide),
                              epochs = 200,
                              activation = "Tanh")
# Save Model
h2o.saveModel(mh2oModel, path = "MonH2OModel", force = TRUE)
# Extract Hidden Layer
hlMon <- h2o.deepfeatures(mh2oModel, mh2odat, layer = 1) %>% as.data.table()
# Add Labels
hlMon[, labels := monLabs]
# Export Hidden Layer to File
fwrite(hlMon, "AE_MonHL.csv")


# Repeat for Tuesday

tuh2odat <- as.h2o(cic_tues)
tufeatures <- colnames(tuh2odat)
tuhide <- 13
tuh2oModel <- h2o.deeplearning(x = tufeatures,
                               training_frame = tuh2odat,
                               autoencoder = TRUE,
                               reproducible = TRUE,
                               seed = 321,
                               hidden = c(tuhide),
                               epochs = 200,
                               activation = "Tanh")
h2o.saveModel(tuh2oModel, path = "TuesH2OModel", force = TRUE)
hlTues <- h2o.deepfeatures(tuh2oModel, tuh2odat, layer = 1) %>% as.data.table()
hlTues[, labels := tuesLabs]
fwrite(hlTues, "AE_TuesHL.csv")


# Repeat for Wednesday

wh2odat <- as.h2o(cic_wed)
wfeatures <- colnames(wh2odat)
whide <- 14
wh2oModel <- h2o.deeplearning(x = wfeatures,
                              training_frame = wh2odat,
                              autoencoder = TRUE,
                              reproducible = TRUE,
                              seed = 321,
                              hidden = c(whide),
                              epochs = 200,
                              activation = "Tanh")
h2o.saveModel(wh2oModel, path = "WedH2OModel", force = TRUE)
hlWed <- h2o.deepfeatures(wh2oModel, wh2odat, layer = 1) %>% as.data.table()
hlWed[, labels := wedLabs]
fwrite(hlWed, "AE_WedHL.csv")


# Repeat for Thursday

thh2odat <- as.h2o(cic_thurs)
thfeatures <- colnames(thh2odat)
thhide <- 13
thh2oModel <- h2o.deeplearning(x = thfeatures,
                               training_frame = thh2odat,
                               autoencoder = TRUE,
                               reproducible = TRUE,
                               seed = 321,
                               hidden = c(thhide),
                               epochs = 200,
                               activation = "Tanh")
h2o.saveModel(thh2oModel, path = "ThursH2OModel", force = TRUE)
hlThurs <- h2o.deepfeatures(thh2oModel, thh2odat, layer = 1) %>% as.data.table()
hlThurs[, labels := thursLabs]
fwrite(hlThurs, "AE_ThursHL.csv")


# Repeat for Friday

fh2odat <- as.h2o(cic_fri)
ffeatures <- colnames(fh2odat)
fhide <- 12
fh2oModel <- h2o.deeplearning(x = ffeatures,
                              training_frame = fh2odat,
                              autoencoder = TRUE,
                              reproducible = TRUE,
                              seed = 321,
                              hidden = c(fhide),
                              epochs = 200,
                              activation = "Tanh")
h2o.saveModel(fh2oModel, path = "FriH2OModel", force = TRUE)
hlFri <- h2o.deepfeatures(fh2oModel, fh2odat, layer = 1) %>% as.data.table()
hlFri[, labels := friLabs]
fwrite(hlFri, "AE_FriHL.csv")


# Shutdown H2O

h2o.shutdown()


# Plot Hidden Layers ------------------------------------------------------

ggplot(hlMon, aes(x = DF.L1.C1, y = DF.L1.C2, color = labels)) +
  geom_point(alpha = 0.1, size = 0.1) + theme_bw() +
  scale_fill_brewer(palette = "Accent") +
  guides(color = guide_legend(override.aes = list(size = 5)))

ggplot(hlTues, aes(x = DF.L1.C1, y = DF.L1.C2, color = labels)) +
  geom_point(alpha = 0.1, size = 0.1) + theme_bw() +
  scale_fill_brewer(palette = "Accent") +
  guides(color = guide_legend(override.aes = list(size = 5)))

ggplot(hlWed, aes(x = DF.L1.C1, y = DF.L1.C2, color = labels)) +
  geom_point(alpha = 0.1, size = 0.1) + theme_bw() +
  scale_fill_brewer(palette = "Accent") +
  guides(color = guide_legend(override.aes = list(size = 5)))

ggplot(hlThurs, aes(x = DF.L1.C2, y = DF.L1.C2, color = labels)) +
  geom_point(alpha = 0.1, size = 0.1) + theme_bw() +
  scale_fill_brewer(palette = "Accent") +
  guides(color = guide_legend(override.aes = list(size = 5)))

ggplot(hlFri, aes(x = DF.L1.C1, y = DF.L1.C2, color = labels)) +
  geom_point(alpha = 0.1, size = 0.1) + theme_bw() +
  scale_fill_brewer(palette = "Accent") +
  guides(color = guide_legend(override.aes = list(size = 5)))


# 3D Plots of Hidden Layers -----------------------------------------------

library(rgl)

plot3d(hlMon$DF.L1.C1, hlMon$DF.L1.C2, hlMon$DF.L1.C3,
       col = as.numeric(as.factor(hlMon$labels))+1)

plot3d(hlTues$DF.L1.C1, hlTues$DF.L1.C2, hlTues$DF.L1.C3,
       col = as.numeric(as.factor(hlTues$labels))+1)

plot3d(hlWed$DF.L1.C1, hlWed$DF.L1.C2, hlWed$DF.L1.C3,
       col = as.numeric(as.factor(hlWed$labels))+1)

plot3d(hlThurs$DF.L1.C1, hlThurs$DF.L1.C2, hlThurs$DF.L1.C3,
       col = as.numeric(as.factor(hlThurs$labels))+1)

plot3d(hlFri$DF.L1.C1, hlFri$DF.L1.C2, hlFri$DF.L1.C3,
       col = as.numeric(as.factor(hlFri$labels))+1)

mfrow3d(3, 3)
for(i in 3:12){
  plot3d(hlTues[[i]], hlTues$DF.L1.C1, hlTues$DF.L1.C2,
         col = as.numeric(as.factor(hlTues$labels))+1)
}

mfrow3d(3, 3)
for(i in 3:13){
  plot3d(hlTues[[i]], hlTues$DF.L1.C1, hlTues$DF.L1.C2,
         col = as.numeric(as.factor(hlTues$labels))+1)
}

mfrow3d(4, 4)
for(i in 3:14){
  plot3d(hlWed[[i]], hlWed$DF.L1.C1, hlWed$DF.L1.C2,
         col = as.numeric(as.factor(hlWed$labels))+1)
}

mfrow3d(4, 4)
for(i in 3:13){
  plot3d(hlThurs[[i]], hlThurs$DF.L1.C1, hlThurs$DF.L1.C2,
         col = as.numeric(as.factor(hlThurs$labels))+1)
}

mfrow3d(3, 4)
for(i in 3:12){
  plot3d(hlFri[[i]], hlThurs$DF.L1.C1, hlThurs$DF.L1.C2,
         col = as.numeric(as.factor(hlThurs$labels))+1)
}


# Autoencoder Scoring -----------------------------------------------------


library(h2o)
setwd("F:/Storage/Thesis/Results/Data_Base/")


# Monday Anomaly Scores

localh2o <- h2o.init(ip = "localhost", port = 54321, nthreads = -1, max_mem_size = "8g")
cic_mon <- data.table(fread("cic_mon.csv"))
monLabs <- data.table(fread("monLabs.csv"))
monModel <- h2o.loadModel("F:/Storage/Thesis/Results/Data_AE/MonH2OModel/DeepLearning_model_R_1604941890435_1")
mh2odat <- as.h2o(cic_mon)
monAnomaly = h2o.anomaly(monModel, mh2odat, per_feature = FALSE)
errMon <- as.data.table(monAnomaly)
errMon[, labels := monLabs]
fwrite(errMon, "F:/Storage/Thesis/Results/Mon_AE_ErrorScores_byObs.csv")
h2o.shutdown()
plot(sort(errMon$Reconstruction.MSE),
     main = "Monday Reconstruction Error",
     xlab = "Index", ylab = "Reconstruction MSE",
     pch = 20,
     col = as.numeric(as.factor(errMon$labels))+1)


# Tuesday Anomaly Scores

localh2o <- h2o.init(ip = "localhost", port = 54321, nthreads = -1, max_mem_size = "8g")
cic_tues <- data.table(fread("cic_tues.csv"))
tuesLabs <- data.table(fread("tuesLabs.csv"))
tuesModel <- h2o.loadModel("F:/Storage/Thesis/Results/Data_AE/TuesH2OModel/DeepLearning_model_R_1604972054202_1")
tuh2odat <- as.h2o(cic_tues)
tuesAnomaly <- h2o.anomaly(tuesModel, tuh2odat, per_feature = FALSE)
errTues <- as.data.table(tuesAnomaly)
errTues[, labels := tuesLabs]
fwrite(errTues, "F:/Storage/Thesis/Results/Tues_AE_ErrorScores_byObs.csv")
plot(sort(errTues$Reconstruction.MSE),
     main = 'Tuesday Reconstruction Error',
     xlab = "Index", ylab = "Reconstruction MSE",
     pch = 20,
     col = as.numeric(as.factor(errTues$labels))+1,
     cex = 0.5)
h2o.shutdown()


# Wednesday Anomaly Scores

localh2o <- h2o.init(ip = "localhost", port = 54321, nthreads = -1, max_mem_size = "8g")
cic_wed <- data.table(fread("cic_wed.csv"))
wedLabs <- data.table(fread("wedLabs.csv"))
wedModel <- h2o.loadModel("F:/Storage/Thesis/Results/Data_AE/WedH2OModel/DeepLearning_model_R_1604972054202_2")
wh2odat <- as.h2o(cic_wed)
wedAnomaly <- h2o.anomaly(wedModel, wh2odat, per_feature = FALSE)
errWed <- as.data.table(wedAnomaly)
errWed[, labels := wedLabs]
fwrite(errWed, "F:/Storage/Thesis/Results/Wed_AE_ErrorScores_byObs.csv")
h2o.shutdown()
plot(sort(errWed$Reconstruction.MSE),
     main = "Wednesday Reconstruction Error",
     xlab = "Index", ylab = "Reconstruction MSE",
     pch = 20,
     col = as.numeric(as.factor(errWed$labels))+1)


# Thursday Anomaly Scores

localh2o <- h2o.init(ip = "localhost", port = 54321, nthreads = -1, max_mem_size = "8g")
cic_thurs <- data.table(fread("cic_thurs.csv"))
thursLabs <- data.table(fread("thursLabs.csv"))
thursModel <- h2o.loadModel("F:/Storage/Thesis/Results/Data_AE/ThursH2OModel/DeepLearning_model_R_1604972054202_3")
thh2odat <- as.h2o(cic_thurs)
thursAnomaly <- h2o.anomaly(thursModel, thh2odat, per_feature = FALSE)
errThurs <- as.data.table(thursAnomaly)
errThurs[, labels := thursLabs]
fwrite(errThurs, "F:/Storage/Thesis/Results/Thurs_AE_ErrorScores_byObs.csv")
plot(sort(errThurs$Reconstruction.MSE),
     main = "Thursday Reconstruction Error",
     xlab = "Index", ylab = "Reconstruction MSE",
     pch = 20,
     col = as.numeric(as.factor(errThurs$labels))+1)
h2o.shutdown()


# Friday Anomaly Scores

localh2o <- h2o.init(ip = "localhost", port = 54321, nthreads = -1, max_mem_size = "8g")
cic_fri <- data.table(fread("cic_fri.csv"))
friLabs <- data.table(fread("friLabs.csv"))
friModel <- h2o.loadModel("F:/Storage/Thesis/Results/Data_AE/FriH2OModel/DeepLearning_model_R_1604972054202_4")
fh2odat <- as.h2o(cic_fri)
friAnomaly <- h2o.anomaly(friModel, fh2odat, per_feature = FALSE)
errFri <- as.data.table(friAnomaly)
errFri[, labels := friLabs]
fwrite(errFri, "F:/Storage/Thesis/Results/Fri_AE_ErrorScores_byObs.csv")
h2o.shutdown()
plot(sort(errFri$Reconstruction.MSE),
     main = "Friday Reconstruction Error",
     xlab = "Index", ylab = "Reconstruction MSE",
     pch = 20,
     col = as.numeric(as.factor(errFri$labels))+1)


# ICA ---------------------------------------------------------------------


# Import the source files, cluster the components, extract IC axes
library(cluster)
setwd("F:/Storage/Thesis/Results/Data_ICA")


# Tuesday PCA-ICA --------------------------------------------------------

tuesICA <- data.table(fread("Tues_PCA_Sources.csv"))
tuesICA <- tuesICA[-1, -1]
compCorrs <- cor(tuesICA)
dissim <- sqrt(1 - abs(compCorrs))
hclTues <- agnes(dissim, diss = TRUE, method = "average")
pltree(hclTues, cex = 0.6, hang = -1,
       main = "Dendrogram of Tuesday ICs")

# Generate Cluster Labels for ICs
clTues <- dendextend::cutree(hclTues, h = 0.88)
table(clTues)
rm(compCorrs, dissim, hclTues)

# Generate Component Sets
tuesICA <- as.data.frame(rbind(t(clTues), tuesICA))
comps <- which(clTues %in% 1)
comp1 <- tuesICA[, comps]
comp1 <- as.data.table(comp1)
comp1 <- comp1[-1, ]
comp1[, IC1 := rowMeans(comp1)]

comps <- which(clTues %in% 2)
comp2 <- tuesICA[, comps]
comp2 <- as.data.table(comp2)
comp2 <- comp2[-1, ]
comp2[, IC2 := rowMeans(comp2)]

comps <- which(clTues %in% 3)
comp3 <- tuesICA[, comps]
comp3 <- as.data.table(comp3)
comp3 <- comp3[-1, ]
comp3[, IC3 := rowMeans(comp3)]

comps <- which(clTues %in% 4)
comp4 <- tuesICA[, comps]
comp4 <- as.data.table(comp4)
comp4 <- comp4[-1, ]
comp4[, IC4 := rowMeans(comp4)]

comps <- which(clTues %in% 5)
comp5 <- tuesICA[, comps]
comp5 <- as.data.table(comp5)
comp5 <- comp5[-1, ]
comp5[, IC5 := rowMeans(comp5)]

comps <- which(clTues %in% 6)
comp6 <- tuesICA[, comps]
comp6 <- as.data.table(comp6)
comp6 <- comp6[-1, ]
comp6[, IC6 := rowMeans(comp6)]

comps <- which(clTues %in% 7)
comp7 <- tuesICA[, comps]
comp7 <- as.data.table(comp7)
comp7 <- comp7[-1, ]
comp7[, IC7 := rowMeans(comp7)]

comps <- which(clTues %in% 8)
comp8 <- tuesICA[, comps]
comp8 <- as.data.table(comp8)
comp8 <- comp8[-1, ]
comp8[, IC8 := rowMeans(comp8)]
rm(tuesICA, clTues)

# Average components based on cutoff
tuesICs <- as.data.table(cbind(comp1$IC1, comp2$IC2, comp3$IC3, comp4$IC4, comp5$IC5, comp6$IC6,
                               comp7$IC7, comp8$IC8))
rm(comp1, comp2, comp3, comp4, comp5, comp6, comp7, comp8, comps)

# Add labels and write to file
# To avoid Python annoyances, don't add labels
tuesLabs <- data.table(fread("F:/Storage/Thesis/Results/Data_Base/tuesLabs.csv"))
tuesICs[, labels := tuesLabs]
fwrite(tuesICs, "F:/Storage/Thesis/Results/Data_Clustering/Tues_PCA_AvgICs.csv")

# Plot
plot3d(x = tuesICs$V3, y = tuesICs$V4, z = tuesICs$V6,
       col = as.numeric(as.factor(tuesICs$labels)), axes = FALSE)
plot3d(x = tuesICs$V2, y = tuesICs$V3, z = tuesICs$V5,
       col = as.numeric(as.factor(tuesICs$labels)), axes = FALSE)
plot3d(x = tuesICs$V1, y = tuesICs$V2, z = tuesICs$V4,
       col = as.numeric(as.factor(tuesICs$labels)), axes = FALSE)
hist(tuesICs$V1, breaks = 100,
     col = as.numeric(as.factor(tuesICs$labels)) + 1)
hist(tuesICs$V2, breaks = 1000,
     col = as.numeric(as.factor(tuesICs$labels)) + 1)
plot(tuesICs$V1, tuesICs$V2, col = as.numeric(as.factor(tuesICs$labels)) + 1)
plot(tuesICs$V1[!(tuesICs$label %in% "BENIGN")], tuesICs$V2[!(tuesICs$label %in% "BENIGN")],
     col = as.numeric(as.factor(tuesICs$labels)) + 1)


# Tuesday RPCA-ICA --------------------------------------------------------

tuesICA <- data.table(fread("Tues_RPCA_Sources.csv"))
tuesICA <- tuesICA[-1, -1]
compCorrs <- cor(tuesICA)
dissim <- sqrt(1 - abs(compCorrs))
hclTues <- agnes(dissim, diss = TRUE, method = "average")
pltree(hclTues, cex = 0.6, hang = -1,
       main = "Dendrogram of Tuesday ICs")

# Generate Cluster Labels for ICs
clTues <- dendextend::cutree(hclTues, h = 0.88)
table(clTues)
rm(compCorrs, dissim, hclTues)

# Generate Component Sets
tuesICA <- as.data.frame(rbind(t(clTues), tuesICA))
comps <- which(clTues %in% 1)
comp1 <- tuesICA[, comps]
comp1 <- as.data.table(comp1)
comp1 <- comp1[-1, ]
comp1[, IC1 := rowMeans(comp1)]

comps <- which(clTues %in% 2)
comp2 <- tuesICA[, comps]
comp2 <- as.data.table(comp2)
comp2 <- comp2[-1, ]
comp2[, IC2 := rowMeans(comp2)]

comps <- which(clTues %in% 3)
comp3 <- tuesICA[, comps]
comp3 <- as.data.table(comp3)
comp3 <- comp3[-1, ]
comp3[, IC3 := rowMeans(comp3)]

comps <- which(clTues %in% 4)
comp4 <- tuesICA[, comps]
comp4 <- as.data.table(comp4)
comp4 <- comp4[-1, ]
comp4[, IC4 := rowMeans(comp4)]

comps <- which(clTues %in% 5)
comp5 <- tuesICA[, comps]
comp5 <- as.data.table(comp5)
comp5 <- comp5[-1, ]
comp5[, IC5 := rowMeans(comp5)]

comps <- which(clTues %in% 6)
comp6 <- tuesICA[, comps]
comp6 <- as.data.table(comp6)
comp6 <- comp6[-1, ]
comp6[, IC6 := rowMeans(comp6)]

comps <- which(clTues %in% 7)
comp7 <- tuesICA[, comps]
comp7 <- as.data.table(comp7)
comp7 <- comp7[-1, ]
comp7[, IC7 := rowMeans(comp7)]

comps <- which(clTues %in% 8)
comp8 <- tuesICA[, comps]
comp8 <- as.data.table(comp8)
comp8 <- comp8[-1, ]
comp8[, IC8 := rowMeans(comp8)]

comps <- which(clTues %in% 9)
comp9 <- tuesICA[, comps]
comp9 <- as.data.table(comp9)
comp9 <- comp9[-1, ]
comp9[, IC9 := rowMeans(comp9)]
rm(tuesICA, clTues)

# Average components based on cutoff
tuesICs <- as.data.table(cbind(comp1$IC1, comp2$IC2, comp3$IC3, comp4$IC4, comp5$IC5, comp6$IC6,
                               comp7$IC7, comp8$IC8, comp9$IC9))
rm(comp1, comp2, comp3, comp4, comp5, comp6, comp7, comp8, comp9, comps)

fwrite(tuesICs, "F:/Storage/Thesis/Results/Data_Clustering/Tues_RPCA_AvgICs.csv")

# Plot
# Add labels
tuesLabs <- data.table(fread("F:/Storage/Thesis/Results/Data_Base/tuesLabs.csv"))
tuesICs[, labels := tuesLabs]

plot3d(x = tuesICs$V3, y = tuesICs$V4, z = tuesICs$V6,
       col = as.numeric(as.factor(tuesICs$labels)), axes = FALSE)
plot3d(x = tuesICs$V2, y = tuesICs$V3, z = tuesICs$V5,
       col = as.numeric(as.factor(tuesICs$labels)), axes = FALSE)
plot3d(x = tuesICs$V1, y = tuesICs$V2, z = tuesICs$V4,
       col = as.numeric(as.factor(tuesICs$labels)), axes = FALSE)


# Tuesday Autoencoder-ICA -------------------------------------------------

tuesICA <- data.table(fread("Tues_AE_Sources.csv"))
tuesICA <- tuesICA[-1, -1]
compCorrs <- cor(tuesICA)
dissim <- sqrt(1 - abs(compCorrs))
hclTues <- agnes(dissim, diss = TRUE, method = "average")
pltree(hclTues, cex = 0.6, hang = -1,
       main = "Dendrogram of Tuesday ICs")

# Generate Cluster Labels for ICs
clTues <- dendextend::cutree(hclTues, h = 0.88)
table(clTues)
rm(compCorrs, dissim, hclTues)

# Generate Component Sets
tuesICA <- as.data.frame(rbind(t(clTues), tuesICA))
comps <- which(clTues %in% 1)
comp1 <- tuesICA[, comps]
comp1 <- as.data.table(comp1)
comp1 <- comp1[-1, ]
comp1[, IC1 := rowMeans(comp1)]

comps <- which(clTues %in% 2)
comp2 <- tuesICA[, comps]
comp2 <- as.data.table(comp2)
comp2 <- comp2[-1, ]
comp2[, IC2 := rowMeans(comp2)]

comps <- which(clTues %in% 3)
comp3 <- tuesICA[, comps]
comp3 <- as.data.table(comp3)
comp3 <- comp3[-1, ]
comp3[, IC3 := rowMeans(comp3)]

comps <- which(clTues %in% 4)
comp4 <- tuesICA[, comps]
comp4 <- as.data.table(comp4)
comp4 <- comp4[-1, ]
comp4[, IC4 := rowMeans(comp4)]

comps <- which(clTues %in% 5)
comp5 <- tuesICA[, comps]
comp5 <- as.data.table(comp5)
comp5 <- comp5[-1, ]
comp5[, IC5 := rowMeans(comp5)]

comps <- which(clTues %in% 6)
comp6 <- tuesICA[, comps]
comp6 <- as.data.table(comp6)
comp6 <- comp6[-1, ]
comp6[, IC6 := rowMeans(comp6)]

comps <- which(clTues %in% 7)
comp7 <- tuesICA[, comps]
comp7 <- as.data.table(comp7)
comp7 <- comp7[-1, ]
comp7[, IC7 := rowMeans(comp7)]

comps <- which(clTues %in% 8)
comp8 <- tuesICA[, comps]
comp8 <- as.data.table(comp8)
comp8 <- comp8[-1, ]
comp8[, IC8 := rowMeans(comp8)]
rm(tuesICA, clTues)

# Average components based on cutoff
tuesICs <- as.data.table(cbind(comp1$IC1, comp2$IC2, comp3$IC3, comp4$IC4, comp5$IC5, comp6$IC6,
                               comp7$IC7, comp8$IC8))
rm(comp1, comp2, comp3, comp4, comp5, comp6, comp7, comp8, comps)

# Add labels
# tuesLabs <- data.table(fread("F:/Storage/Thesis/Results/Data_Base/tuesLabs.csv"))
# tuesICs[, labels := tuesLabs]
fwrite(tuesICs, "F:/Storage/Thesis/Results/Data_Clustering/Tues_AE_AvgICs.csv")

# Plot
plot3d(x = tuesICs$V1, y = tuesICs$V2, z = tuesICs$V6,
       col = as.numeric(as.factor(tuesICs$labels)), axes = FALSE)
plot3d(x = tuesICs$V1, y = tuesICs$V2, z = tuesICs$V9,
       col = as.numeric(as.factor(tuesICs$labels)), axes = FALSE)
plot3d(x = tuesICs$V3, y = tuesICs$V2, z = tuesICs$V5,
       col = as.numeric(as.factor(tuesICs$labels)), axes = FALSE)


# HDBSCAN ------------------------------------------------------------------

setwd("F:/Storage/Thesis/Results/Cluster_Results")


# PCA-ICA-Clustering ----------------------------------------------------------


pcaClust <- data.table(fread("Tues_PCA_Clustered.csv"))
pcaClust <- pcaClust[, -1]
pcaProbs <- data.table(fread("Tues_PCA_ClusterProbs.csv"))
pcaProbs <- pcaProbs[-1, -1]
pcaOutliers <- data.table(fread("Tues_PCA_Cluster_Anomaly_Scores.csv"))
pcaOutliers <- pcaOutliers[-1, -1]
pcaTree <- data.table(fread("Tues_PCA_Condensed_Tree.csv"))

# Plots of Malicious Traffic
mfrow3d(3,3)
for(i in 3:8){
  plot3d(x = pcaClust$V1[!(pcaClust$labels_orig %in% "BENIGN")], y = pcaClust$V2[!(pcaClust$labels_orig %in% "BENIGN")], z = pcaClust[!(pcaClust$labels_orig %in% "BENIGN")][[i]],
         col = as.numeric(as.factor(pcaClust$labels_clust[!(pcaClust$labels_orig %in% "BENIGN")])), axes = FALSE, xlab = "", ylab = "", zlab = "")
}
plot3d(x = pcaClust$V3, y = pcaClust$V2, z = pcaClust$V4,
       col = as.numeric(as.factor(pcaClust$labels_clust)), axes = FALSE,
       xlab = "", ylab = "", zlab = "")

plot3d(x = pcaClust$V3[!(pcaClust$labels_orig %in% "BENIGN")],
       y = pcaClust$V2[!(pcaClust$labels_orig %in% "BENIGN")],
       z = pcaClust$V7[!(pcaClust$labels_orig %in% "BENIGN")],
       col = as.numeric(as.factor(pcaClust$labels_clust)), axes = FALSE,
       xlab = "", ylab = "", zlab = "")

plot3d(x = pcaClust$V3[!(pcaClust$labels_orig %in% "BENIGN")],
       y = pcaClust$V2[!(pcaClust$labels_orig %in% "BENIGN")],
       z = pcaClust$V4[!(pcaClust$labels_orig %in% "BENIGN")],
       col = as.numeric(as.factor(pcaClust$labels_clust)), axes = FALSE,
       xlab = "", ylab = "", zlab = "")

# Table of Accuracy
table(pcaClust$labels_orig, pcaClust$labels_clust)

# Add Variables
pcaClust[, outliers := pcaOutliers]
pcaClust[, Probs := pcaProbs]

# Clustering Quality Checks
table(pcaClust$outliers < 0.7, pcaClust$labels_orig)
table(pcaClust$Probs > 0.95, pcaClust$labels_orig)

# RPCA-ICA-Clustering ---------------------------------------------------------

rpcaClust <- data.table(fread("Tues_RPCA_Clustered.csv"))
rpcaClust <- rpcaClust[, -1]
rpcaProbs <- data.table(fread("Tues_RPCA_ClusterProbs.csv"))
rpcaProbs <- rpcaProbs[-1, -1]
rpcaOutliers <- data.table(fread("Tues_RPCA_Cluster_Anomaly_Scores.csv"))
rpcaOutliers <- rpcaOutliers[-1, -1]

# Plots of Malicious Traffic
rpcaClust[, cols := fifelse(as.numeric(rpcaClust$labels_clust) %in% c(3, 4), "red",
                       fifelse(as.numeric(rpcaClust$labels_clust) %in% c(6, 7), "green",
                               fifelse(as.numeric(rpcaClust$labels_clust) %in% c(-1), "black", "gray")))]
plot3d(x = rpcaClust$V2,
       y = rpcaClust$V4,
       z = rpcaClust$V6,
       col = rpcaClust$cols, axes = FALSE,
       xlab = "", ylab = "", zlab = "")

# Accuracy Table
table(rpcaClust$labels_orig, rpcaClust$labels_clust)

# Add Variables
rpcaClust[, outliers := rpcaOutliers]
rpcaClust[, Probs := rpcaProbs]

# Clustering Quality Check
table(rpcaClust$outliers < 0.2, rpcaClust$labels_orig)
table(rpcaClust$Probs > 0.95, rpcaClust$labels_orig)
table(rpcaClust$Probs > 0.5, rpcaClust$labels_orig)

# AE-ICA-Clustering -----------------------------------------------------------

aeClust <- data.table(fread("Tues_AE_Clustered.csv"))
aeClust <- aeClust[, -1]
aeProbs <- data.table(fread("Tues_AE_ClusterProbs.csv"))
aeProbs <- aeProbs[-1, -1]
aeOutliers <- data.table(fread("Tues_AE_Cluster_Anomaly_Scores.csv"))
aeOutliers <- aeOutliers[-1, -1]

# Accuracy Table
table(aeClust$labels_orig, aeClust$labels_clust)

# Plots of Malicious Traffic
aeClust[, cols := fifelse(as.numeric(aeClust$labels_clust) %in% c(4, 5), "red",
                          fifelse(as.numeric(aeClust$labels_clust) %in% c(6, 7), "green",
                                  fifelse(as.numeric(aeClust$labels_clust) %in% c(29, 28, 18), "orange",
                                          fifelse(as.numeric(aeClust$labels_clust) %in% c(-1), "black", "gray"))))]
plot3d(x = aeClust$V2,
       y = aeClust$V4,
       z = aeClust$V6,
       col = aeClust$cols, axes = FALSE,
       xlab = "", ylab = "", zlab = "")

# Add Variables
aeClust[, outliers := aeOutliers]
aeClust[, Probs := aeProbs]

# Clustering Quality Check
table(aeClust$outliers < 0.7, aeClust$labels_orig)
table(aeClust$Probs > 0.95, aeClust$labels_orig)
table(aeClust$Probs > 0.5, aeClust$labels_orig)

# Cluster Comparison ---------------------------------------------------------------

# Distances with PCA Clustering --------------------------------------------

pcaClust <- data.table(fread("F:/Storage/Thesis/Results/Cluster_Results/Tues_PCA_Clustered.csv"))
pcaClust <- pcaClust[ , -1]

# Centroid Vectors
pcaCentroids <- aggregate(pcaClust[ , 1:8], list(pcaClust$labels_clust), mean)
pcaPairs <- as.data.table(t(combn(dim(table(pcaClust$labels_clust)), 2)))
pcaPairs[ , V1 := V1 - 2]
pcaPairs[ , V2 := V2 - 2]
pcaPairs[ , clDist := dist(pcaCentroids[ , 2:9]), ]
pcaPairs[ , cols := fifelse(as.numeric(pcaPairs$V1) %in% c(0) | as.numeric(pcaPairs$V2) %in% c(0), "red",
                            fifelse(as.numeric(pcaPairs$V1) %in% c(-1), "black",
                                    fifelse(as.numeric(pcaPairs$V2) %in% c(-1), "black", "gray")))]
pcaPairs <- pcaPairs[order(pcaPairs$clDist), ]
plot(pcaPairs$clDist, col = pcaPairs$cols, ylab = "Distance", pch = 16, xlab = "Index of Pair Group")
pcaPairsSub <- pcaPairs[!(V1 %in% c(-1)) & !(V2 %in% c(-1))]
plot(pcaPairsSub$clDist, col = pcaPairsSub$cols, ylab = "Distance", pch = 16)


# Distances with RPCA Clustering -------------------------------------------

rpcaClust <- data.table(fread("F:/Storage/Thesis/Results/Cluster_Results/Tues_RPCA_Clustered.csv"))
rpcaClust <- rpcaClust[ , -1]

# Centroid Vectors
rpcaCentroids <- aggregate(rpcaClust[ , 1:8], list(rpcaClust$labels_clust), mean)
rpcaPairs <- as.data.table(t(combn(dim(table(rpcaClust$labels_clust)), 2)))
rpcaPairs[ , V1 := V1 - 2]
rpcaPairs[ , V2 := V2 - 2]
rpcaPairs[ , clDist := dist(rpcaCentroids[ , 2:9], method = "euclidean"), ]
table(rpcaClust$labels_orig, rpcaClust$labels_clust)
rpcaPairs[ , cols := fifelse(as.numeric(rpcaPairs$V1) %in% c(3, 4, 6, 7) |
                               as.numeric(rpcaPairs$V2) %in% c(3, 4, 6, 7), "red",
                             fifelse(as.numeric(rpcaPairs$V1) %in% c(2) |
                                       as.numeric(rpcaPairs$V2) %in% c(2), "green",
                                     fifelse(as.numeric(rpcaPairs$V1) %in% c(-1), "black",
                                             fifelse(as.numeric(rpcaPairs$V2) %in% c(-1), "black", "gray"))))]
rpcaPairs <- rpcaPairs[order(rpcaPairs$clDist), ]
plot(rpcaPairs$clDist, col = rpcaPairs$cols, pch = 16, cex = 0.8, ylab = "Distance")
View(rpcaPairs[rpcaPairs$clDist > 0.0018])
rpcaSubPairs <- rpcaPairs[V1 %in% c(3, 4, 6, 7)]
rpcaSubPairs <- rpcaSubPairs[order(rpcaSubPairs$clDist), ]
plot(rpcaSubPairs$clDist, col = rpcaSubPairs$cols, pch = 5)

rpcaSubPairs2 <- rpcaPairs[!((V1 %in% c(3, 4, 6, 7)) & (V2 %in% c(3, 4, 6, 7)))]
rpcaSubPairs2 <- rpcaSubPairs2[!(V1 %in% c(-1)) & !(V2 %in% c(-1))]
plot(rpcaSubPairs2$clDist, col = rpcaSubPairs2$cols, pch = 16, ylab = "Distance", xlab = "Index of Pair Group")

View(rpcaSubPairs2[cols %in% "red"])

# Try plotting the closest clusters with colour
rpcaClust[ , cols := fifelse(as.numeric(rpcaClust$labels_clust) %in% c(3, 4, 6, 7) | as.numeric(rpcaClust$labels_clust) %in% c(3, 4, 6, 7), "red",
                             fifelse(as.numeric(rpcaClust$labels_clust) %in% c(-1), "black",
                                     fifelse(as.numeric(rpcaClust$labels_clust) %in% c(2, 8), "green",
                                             fifelse(as.numeric(rpcaClust$labels_clust) %in% c(-1), "black", "gray"))))]
table(rpcaClust$labels_orig, rpcaClust$labels_clust)
plot3d(rpcaClust$V2, rpcaClust$V4, rpcaClust$V6,
       col = rpcaClust$cols,
       axes = FALSE,
       xlab = "", ylab = "", zlab = "")

cs <- c(1, 3, 5, 6, 7, 8)
cs <- as.data.table(combn(8, 3))
mfrow3d(5, 5)
for(i in 31:56){
  plot3d(rpcaClust[[cs[[i]][1]]], rpcaClust[[cs[[i]][2]]], rpcaClust[[cs[[i]][3]]],
         col = rpcaClust$cols,
         axes = FALSE,
         xlab = "", ylab = "", zlab = "")
}

# Distances with Autoencoder-ICA -------------------------------------

aeICAClust <- data.table(fread("F:/Storage/Thesis/Results/Cluster_Results/Tues_AE_Clustered.csv"))
aeICAClust <- aeICAClust[ , -1]

# Centroid Vectors
aeICACentroids <- aggregate(aeICAClust[ , 1:8], list(aeICAClust$labels_clust), mean)
aeICAPairs <- as.data.table(t(combn(dim(table(aeICAClust$labels_clust)), 2)))
aeICAPairs[ , V1 := V1 - 2]
aeICAPairs[ , V2 := V2 - 2]
aeICAPairs[ , clDist := dist(aeICACentroids[ , 2:9])]
table(aeICAClust$labels_orig, aeICAClust$labels_clust)
aeICAPairs[ , cols := fifelse(as.numeric(aeICAPairs$V1) %in% c(4, 5, 6, 7) | as.numeric(aeICAPairs$V2) %in% c(4, 5, 6, 7), "red",
                              fifelse(as.numeric(aeICAPairs$V1) %in% c(-1), "black",
                                      fifelse(as.numeric(aeICAPairs$V2) %in% c(-1), "black", "gray")))]
aeICAPairs <- aeICAPairs[order(aeICAPairs$clDist), ]
plot(aeICAPairs$clDist, col = aeICAPairs$cols)

# Find the closest clusters
View(aeICAPairs[aeICAPairs$clDist < 0.0002])
View(aeICAPairs[aeICAPairs$clDist < 0.0025])
aeICASubPairs <- aeICAPairs[V1 %in% c(4, 5, 6, 7) & !(V2 %in% c(4, 5, 6, 7))]
View(aeICASubPairs)
aeICASubPairs <- aeICASubPairs[order(aeICASubPairs$clDist), ]
aeICASubPairs[ , cols := fifelse(as.numeric(aeICASubPairs$V2) %in% c(4, 5, 6, 7), "red", "grey")]
plot(aeICASubPairs$clDist, col = aeICASubPairs$cols)

# Try plotting the closest clusters with colour
View(aeICAPairs)
aeICAClust[ , cols := fifelse(as.numeric(aeICAClust$V1) %in% c(4, 5, 6, 7) | as.numeric(aeICAClust$V2) %in% c(4, 5, 6, 7), "red",
                              fifelse(as.numeric(aeICAClust$V1) %in% c(-1), "black",
                                      fifelse(as.numeric(aeICAClust$labels_clust) %in% c(28, 29, 18), "green",
                                              fifelse(as.numeric(aeICAClust$V2) %in% c(-1), "black", "gray"))))]
plot3d(aeICAClust$V1, aeICAClust$V2, aeICAClust$V3,
       col = aeICAClust$cols,
       axes = FALSE,
       xlab = "", ylab = "", zlab = "")



# Rank --------------------------------------------------------------------

library(data.table)
library(stringr)
library(PCDimension)
library(plotly)
library(rgl)
setwd("F:/Storage/Thesis/Data/Rank")

# Load Events Data
rankd1 <- data.table(fread("F:/Storage/Thesis/Data/Rank/Exports/Parquet_Merge_19708_to_68/Events_Parquet_Merge_1.csv"))

# Pre-processing -----------------------------------------------------

rankd1[ , datetime := as.POSIXct(as.numeric(datetime)/1000, origin = "1970-01-01")]
rankd1[ , source_assets_id := fifelse(source_assets_id %in% "[]", "0", source_assets_id)] # Mostly blank
rankd1 <- janitor::remove_constant(rankd1)
rankd1[ , source_reputation := fifelse(source_reputation %in% "[]", "0", source_reputation)] # Mostly blank
rankd1[ , destination_assets_id := fifelse(destination_assets_id %in% "[]", "0", destination_assets_id)] # Mostly blank
rankd1[ , destination_assets_id := fifelse(destination_assets_id %in% "", "0", destination_assets_id)]
rankd1[ , destination_name := NULL] # Same as destination_ip
rankd1[ , destination_reputation := fifelse(destination_reputation %in% "[]", "0", destination_reputation)] # Mostly blank
names(rankd1)[33] <- "observedProtocolPortProtocol"
names(rankd1)[32] <- "observedProtocolHasKnownPort"
names(rankd1)[31] <- "observedProtocolName"
rankd1[, observedProtocolPortProtocol := fifelse(observedProtocolPortProtocol %in% "", "0", observedProtocolPortProtocol)]
rankd1[, observedProtocolName := fifelse(observedProtocolName %in% "", "0", observedProtocolName)]
rankd1[, observedProtocolHasKnownPort := fifelse(is.na(observedProtocolHasKnownPort), FALSE, observedProtocolHasKnownPort)]
nom <- names(rankd1)
nom <- str_replace_all(nom, "data_conn_", "")
names(rankd1) <- nom
rm(nom)
rankd1[ , source_name := NULL] # Same as source_ip
rankd1[ , source_assets_id := fifelse(source_assets_id %in% "", "0", source_assets_id)]
rankd1[ , destination_asn := fifelse(is.na(destination_asn), 0, destination_asn)]
rankd1[ , source_asn := fifelse(is.na(source_asn), 0, source_asn)]
rankd1[ , destination_country := fifelse(destination_country %in% "", "NA", destination_country)]
rankd1[ , source_country := fifelse(source_country %in% "", "NA", source_country)]
rankd1[ , source_internal := fifelse(source_internal == TRUE, 1, 0)]
rankd1[ , destination_internal := fifelse(destination_internal == TRUE, 1, 0)]
rankd1[ , status := fifelse(status %in% "fail", 0, 1)]
rankd1[ , observedProtocolHasKnownPort := fifelse(observedProtocolHasKnownPort == TRUE, 1, 0)]
rankd1 <- janitor::remove_constant(rankd1)

# Extract Embedding Vars and Numeric Columns ---------------------------------------------
rankNom <- rankd1[, c("source_ip", "source_port", "source_assets_id", "source_reputation",
                      "destination_ip", "destination_port", "destination_assets_id",
                      "destination_routingMode", "destination_reputation", "protocol_service",
                      "protocol_udpOrTcp", "state", "history", "source_country",
                      "destination_country", "observedProtocolName", "observedProtocolPortProtocol")]
rankNom <- janitor::remove_constant(rankNom)
rankNum <- rankd1[, c("respBytes", "reqBytes", "duration", "source_asn", "destination_asn", "bytes", "reqPackets",
                      "respPackets", "reqIPBytes", "respIPBytes", "observedProtocolHasKnownPort", "source_internal",
                      "destination_internal", "status")]
rankNum <- janitor::remove_constant(rankNum)

# Set Categorical variables to Character type for embedding with Word2Vec
rankNom <- as.data.table(apply(rankNom, 2, as.character))

# Write to file
fwrite(rankNom, "F:/RankEmbeddingData.csv", row.names = FALSE, col.names = FALSE)

# Load the Python embeddings
embeds <- data.table(fread("F:/RankWVs.csv"))
# The first column is the associated word
blankrank <- data.table(matrix(data = NA, nrow = dim(rankNum)[1], ncol = 32*2))

# Append Blank Columns
rankNum <- as.data.table(cbind(rankNum, blankrank))
rankNum <- as.data.table(cbind(rankNum, rankNom))
rm(blankrank, rankNom, rankd1)

rankNum[, 15:46] <- embeds[match(rankNum$source_ip, embeds$V1), 2:33]
rankNum[, 47:78] <- embeds[match(rankNum$destination_ip, embeds$V1), 2:33]
rankNum[ , respBytes := as.numeric(respBytes)]
rankNum[ , reqBytes := as.numeric(reqBytes)]
rankNum[ , bytes := as.numeric(bytes)]
rankNum[ , reqIPBytes := as.numeric(reqIPBytes)]
rankNum[ , respIPBytes := as.numeric(respIPBytes)]
rankNum[ , respPackets := as.numeric(respPackets)]
rankNum[ , reqPackets := as.numeric(reqPackets)]
rankNum <- rankNum[ , -c(79:95)]

rm(embeds)
fwrite(rankNum, "F:/Rank_Fully_Embedded.csv")
rankNum <- data.table(fread("F:/Storage/Thesis/Results/Rank_Output/Rank_Fully_Embedded.csv"))

# Rank PCA ----------------------------------------------------------------

# Test PCA for Dimension Cutoff
for(i in 1:dim(rankNum)[2]){
  rankNum[[i]] <- scale(rankNum[[i]], center = TRUE, scale = TRUE)
}
for(i in 1:78){
  print(i)
  print(table(is.na(rankNum[[i]])))
}

pcs <- prcomp(rankNum, scale. = FALSE, center = FALSE)

plot(pcs)
bsDimension(pcs$sdev^2)
pcRank <- pcs$x[ , 1:6]
fwrite(pcRank, "F:/PCData.csv")
rm(pcRank)

PCDat <- as.data.table(pcs$x)
rPCTab <- data.table()
rPCTab <- as.data.table(matrix(NA, nrow = dim(PCDat)[2]))
rPCTab[, eigenvalues := round(pcs$sdev^2, 5)]
eigenSum <- sum(rPCTab$eigenvalues) # or dim(rPCTab)[1] because correlations
rPCTab[, explained := round(rPCTab$eigenvalues/eigenSum, 5)]
rPCTab[, meancutoff := (eigenvalues > mean(eigenvalues))]
rPCTab[, scaledcutoff := (eigenvalues > 0.7 * mean(eigenvalues))]
bscut <- bsDimension(lambda = rPCTab$eigenvalues)
rPCTab[, bscutoff := c(rep(TRUE, bscut), rep(FALSE, dim(rPCTab)[1] - bscut))]
pExpl <- vector(length = dim(rPCTab)[1])
for(i in 1:dim(rPCTab)[1]){
  pExpl[i] <- sum(rPCTab$explained[1:i])
}
rPCTab[, explained := round(explained * 100, 3)]
rPCTab[, propnExpl := round(pExpl * 100, 3)]
rPCTab[, bsModel :=  round(brokenStick(1:dim(PCDat)[2], dim(PCDat)[2]) * 100, 3)]
rPCTab[, V1 := NULL]
rm(bscut, i, pExpl, monpcs, eigenSum)
fwrite(rPCTab, "F:/Storage/Thesis/Results/Rank_Output/Rank_PCA_Table.csv")

# Rank PCA-ICA --------------------------------------------------------

rankICA <- data.table(fread("F:/Rank_PC_Sources.csv"))
rankICA <- rankICA[-1, -1]
compCorrs <- cor(rankICA)
dissim <- sqrt(1 - abs(compCorrs))
hclRank <- agnes(dissim, diss = TRUE, method = "average")
pltree(hclRank, cex = 0.6, hang = -1,
       main = "Dendrogram of Tuesday ICs")

# Generate Cluster Labels for ICs
clRank <- dendextend::cutree(hclRank, h = 0.8)
table(clRank)
rm(compCorrs, dissim, hclRank)

# Generate Component Sets
rankICA <- as.data.frame(rbind(t(clRank), rankICA))
comps <- which(clRank %in% 1)
comp1 <- rankICA[, comps]
comp1 <- as.data.table(comp1)
comp1 <- comp1[-1, ]
comp1[, IC1 := rowMeans(comp1)]

comps <- which(clRank %in% 2)
comp2 <- rankICA[, comps]
comp2 <- as.data.table(comp2)
comp2 <- comp2[-1, ]
comp2[, IC2 := rowMeans(comp2)]

comps <- which(clRank %in% 3)
comp3 <- rankICA[, comps]
comp3 <- as.data.table(comp3)
comp3 <- comp3[-1, ]
comp3[, IC3 := rowMeans(comp3)]

comps <- which(clRank %in% 4)
comp4 <- rankICA[, comps]
comp4 <- as.data.table(comp4)
comp4 <- comp4[-1, ]
comp4[, IC4 := rowMeans(comp4)]

comps <- which(clRank %in% 5)
comp5 <- rankICA[, comps]
comp5 <- as.data.table(comp5)
comp5 <- comp5[-1, ]
comp5[, IC5 := rowMeans(comp5)]
rm(rankICA, clRank)

# Average components based on cutoff
rankICs <- as.data.table(cbind(comp1$IC1, comp2$IC2, comp3$IC3, comp4$IC4, comp5$IC5))
rm(comp1, comp2, comp3, comp4, comp5, comps)

fwrite(rankICs, "F:/Rank_PCA_Avg_ICs.csv")

# Plot
plot3d(x = rankICs$V3, y = rankICs$V4, z = rankICs$V5, axes = FALSE)


# Find IQR for Clustering -------------------------------------------------

iqs <- {}
for(i in 1:5){
  iqs[i] <- IQR(rankICs[[i]])
}
max(iqs)
min(iqs)

# Rank HDBSCAN -------------------------------------------------------------

rankClust <- data.table(fread(file.choose()))

# Plots
rankClust <- rankClust[, -1]
rankClust[, cols := fifelse(as.numeric(rankClust$labels_clust) %in% c(13, 22, 24, 63, 66, 73, 75, 76, 81, 86), "red",
                            fifelse(as.numeric(rankClust$labels_clust) %in% c(-1), "black", "gray"))]
plot3d(x = rankClust$V1,
       y = rankClust$V2,
       z = rankClust$V3,
       col = as.numeric(as.factor(rankClust$labels_clust)), axes = FALSE,
       xlab = "", ylab = "", zlab = "")

mfrow3d(4, 3)
combs <- as.data.table(t(combn(5, 3)))
for(i in 1:10){
  plot3d(x = rankClust[[combs[[1]][i]]],
         y = rankClust[[combs[[2]][i]]],
         z = rankClust[[combs[[3]][i]]],
         col = as.numeric(as.factor(rankClust$labels_clust)), axes = FALSE,
         xlab = "", ylab = "", zlab = "")
}


# Rank Cluster Distances --------------------------------------------------

rank_clust <- rank_clust[ , -1]

# Centroid Vectors
rankCentroids <- aggregate(rank_clust[ , 1:5], list(rank_clust$labels_clust), mean)
rankPairs <- as.data.table(t(combn(dim(table(rank_clust$labels_clust)), 2)))
rankPairs[ , V1 := V1 - 2]
rankPairs[ , V2 := V2 - 2]
rankPairs[ , clDist := dist(rankCentroids[ , 2:6]), ]
rankPairs[ , cols := fifelse(as.numeric(rankPairs$V1) %in% c(55, 74, 75), "red",
                             fifelse(as.numeric(rankPairs$V1) %in% c(94, 95, 96, 97, 98, 122, 131, 132, 154), "green",
                                     fifelse(as.numeric(rankPairs$V1) %in% c(-1), "black",
                                             fifelse(as.numeric(rankPairs$V2) %in% c(-1), "black", "gray"))))]
rankPairs <- rankPairs[order(rankPairs$clDist, decreasing = TRUE), ]
plot(rankPairs$clDist, col = rankPairs$cols, ylab = "Distance", pch = 16, xlab = "Index of Pair Group")

rank_clust[ , cols := fifelse(as.numeric(rank_clust$labels_clust) %in% c(55, 74, 75), "red",
                             fifelse(as.numeric(rank_clust$labels_clust) %in% c(94, 95, 96, 97, 98, 122, 131, 132, 154), "green",
                                     fifelse(as.numeric(rank_clust$labels_clust) %in% c(-1), "black", "gray")))]

plot3d(x = rank_clust$V2,
       y = rank_clust$V3,
       z = rank_clust$V4,
       col = rank_clust$cols, axes = FALSE,
       xlab = "", ylab = "", zlab = "")

rankPairs[, cols := NULL]
print(xtable(rankPairs, digits = 5, type = "latex", tabular.environment="longtable"),
      file = "F:/Storage/Thesis/Results/Rank_Output/Rank_Pairwise_Dists.tex")
print(xtable(rankPairs, digits = 8, type = "latex", tabular.environment="longtable"),
      file = "F:/Storage/Thesis/Results/Rank_Output/Rank_Pairwise_Dists.tex")



# Writeup ---------------------------------------------------------------------

library(data.table)
library(stringr)
library(PCDimension)
library(plotly)
library(rgl)
library(moments)
library(xtable)
setwd("F:/Storage/Thesis/Data/Rank")

# Load Events Data
rankd1 <- data.table(fread("F:/Storage/Thesis/Results/Rank_Output/Events_Parquet_Merge_1.csv"))
rank_embedded <- data.table(fread("F:/Storage/Thesis/Results/Rank_Output/Rank_Fully_Embedded.csv"))
rank_pcs <- data.table(fread("F:/Storage/Thesis/Results/Rank_Output/Rank_PCA_Data.csv"))
rank_ics <- data.table(fread("F:/Storage/Thesis/Results/Rank_Output/Rank_PCA_Avg_ICs.csv"))
rank_clust <- data.table(fread("F:/Storage/Thesis/Results/Rank_Output/Rank_Clustered.csv"))

# Run Summary Statistics
sumStatNum <- data.table(matrix(nrow = dim(rankNum)[2], ncol = 7, dimnames = list(NULL, c("Variable", "Mean", "Variance", "Median", "MAD", "Skewness", "Kurtosis"))))
for(i in 1:14){
  rankNum[[i]] <- as.numeric(rankNum[[i]])
}
sumStatNum$Variable <- as.character(names(rankNum))
rankMeans <- apply(rankNum, 2, mean)
sumStatNum$Mean <- apply(rankNum, 2, mean)
sumStatNum$Variance <- apply(rankNum, 2, var)
sumStatNum$Median <- apply(rankNum, 2, median)
sumStatNum$MAD <- apply(rankNum, 2, mad)
sumStatNum$Skewness <- apply(rankNum, 2, skewness)
sumStatNum$Kurtosis <- apply(rankNum, 2, kurtosis)
print(xtable(sumStatNum, type = "latex", tabular.environment="longtable"), file = "F:/Storage/Thesis/Results/Rank_Output/Summary_Statistics.tex")

iqs <- {}
for(i in 1:5){
  iqs[i] <- IQR(rank_ics[[i]])
}
max(iqs)
min(iqs)

# PCA Distances
pcaPairs[ , cols := NULL]
print(xtable(pcaPairs, digits = 6, type = "latex", tabular.environment="longtable"), file = "F:/Storage/Thesis/Results/PCA_Pairwise_Comparison.tex")
# RPCA Distances
rpcaPairs <- rpcaPairs[order(rpcaPairs$clDist, decreasing = TRUE), ]
print(xtable(rpcaPairs, digits = 7, type = "latex", tabular.environment="longtable"), file = "F:/Storage/Thesis/Results/RPCA_Pairwise_Comparison.tex")
