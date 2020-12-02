library(data.table)
cicTues <- fread("cicTues.csv")

tuesLabs <- cic$label[as.character(cic$date) == "2017-07-04"]
rm(cic)

library(fastICA)
tuesICA <- fastICA(cicTues,
                   alg.typ = "deflation",
                   fun = "exp",
                   row.norm = FALSE,
                   method = "C",
                   verbose = TRUE,
                   n.comp = 5)

ics <- tuesICA$S
ics <- as.data.table(ics)

library(subspace)
cl1 <- subspace::SubClu(ics$V1, epsilon = 0.05)

ics$label <- tuesLabs

ics[, clustLab := NA]
ics[, clustLab := fifelse(as.numeric(rownames(ics)) %in% cl[[1]]$objects, 1,
                          fifelse(as.numeric(rownames(ics)) %in% cl[[2]]$objects, 2,
                                             fifelse(as.numeric(rownames(ics)) %in% cl[[3]]$objects, 3,
                                                                fifelse(as.numeric(rownames(ics)) %in% cl[[4]]$objects, 4, 0))))]

library(rgl)
combos <- combn(5, 3)
comb1 <- combos[1, ]
comb2 <- combos[2, ]
comb3 <- combos[3, ]
mfrow3d(4, 3)
# Plot the 3D PCA data in the window
for(i in 1:12){
  plot3d(x = ics$V1,
         y = ics[ , comb2[i]],
         z = ics[ , comb3[i]],
         col = as.numeric(as.factor(ics$label)) + 1,
         # size = 1.5,
         #type = "s",
         xlab = combos[1, i], ylab = combos[2, i], combos[3, i],
         axes = FALSE)
  box3d()
}
plot3d(ics$V1, ics$V2, ics$V3, col = as.numeric(as.factor(ics$clustLab)) + 1, pch = as.numeric(as.factor(ics$label)) + 1)
hist(ics$V1[ics$V1 < -0.05 & ics$V1 > -0.07], breaks = 300, col = as.numeric(as.factor(ics$clustLab)))
hist(ics$V2[ics$V2 < 0.175 & ics$V2 > 0.15], breaks = 300, col = as.numeric(as.factor(ics$clustLab)))
hist(ics$V3[ics$V3 < -0.01 & ics$V3 > -0.02], breaks = 300, col = as.numeric(as.factor(ics$clustLab)))
hist(ics$V4[ics$V4 < -0.32 & ics$V4 > -0.33], breaks = 300, col = as.numeric(as.factor(ics$clustLab)))
hist(ics$V5[ics$V5 < -0.015 & ics$V5 > -0.025], breaks = 300, col = as.numeric(as.factor(ics$clustLab)))
plot3d(ics$V1, ics$V2, col = as.numeric(as.factor(ics$clustLab)), pch = as.numeric(as.factor(ics$label)), cex = 0.7)



# Manual Subspace Clustering ----------------------------------------------

library(dbscan)
mancl <- SubClu(ics[, c(1, 3)], epsilon = 0.01, minSupport = 500)
ics[, mancl1 := as.numeric(as.factor(fifelse(as.numeric(rownames(ics)) %in% mancl[[1]]$objects, 1, 0)))]
ics[, mancl2 := as.numeric(as.factor(fifelse(as.numeric(rownames(ics)) %in% mancl[[2]]$objects, 1, 0)))]
ics[, mancl3 := as.numeric(as.factor(fifelse(as.numeric(rownames(ics)) %in% mancl[[3]]$objects, 1, 0)))]
ics[, mancl4 := as.numeric(as.factor(fifelse(as.numeric(rownames(ics)) %in% mancl[[4]]$objects, 1, 0)))]

mfrow3d(3, 4)
plot3d(ics$V1, ics$V2, ics$V3, col = ics$mancl1 + 1)
plot3d(ics$V1, ics$V2, ics$V4, col = ics$mancl1 + 1)
plot3d(ics$V1, ics$V2, ics$V5, col = ics$mancl1 + 1)
plot3d(ics$V1, ics$V3, ics$V4, col = ics$mancl1 + 1)
plot3d(ics$V1, ics$V3, ics$V5, col = ics$mancl1 + 1)
plot3d(ics$V1, ics$V4, ics$V5, col = ics$mancl1 + 1)
plot3d(ics$V2, ics$V3, ics$V4, col = ics$mancl1 + 1)
plot3d(ics$V2, ics$V3, ics$V5, col = ics$mancl1 + 1)
plot3d(ics$V2, ics$V4, ics$V5, col = ics$mancl1 + 1)
plot3d(ics$V3, ics$V4, ics$V5, col = ics$mancl1 + 1)

mfrow3d(3, 4)
plot3d(ics$V1, ics$V2, ics$V3, col = as.numeric(as.factor(ics$label))+1)
plot3d(ics$V1, ics$V2, ics$V4, col = as.numeric(as.factor(ics$label))+1)
plot3d(ics$V1, ics$V2, ics$V5, col = as.numeric(as.factor(ics$label))+1)
plot3d(ics$V1, ics$V3, ics$V4, col = as.numeric(as.factor(ics$label))+1)
plot3d(ics$V1, ics$V3, ics$V5, col = as.numeric(as.factor(ics$label))+1)
plot3d(ics$V1, ics$V4, ics$V5, col = as.numeric(as.factor(ics$label))+1)
plot3d(ics$V2, ics$V3, ics$V4, col = as.numeric(as.factor(ics$label))+1)
plot3d(ics$V2, ics$V3, ics$V5, col = as.numeric(as.factor(ics$label))+1)
plot3d(ics$V2, ics$V4, ics$V5, col = as.numeric(as.factor(ics$label))+1)


mfrow3d(2, 4)
plot3d(ics$V1, ics$V2, ics$V4, col = as.numeric(as.factor(ics$label))+1)
plot3d(ics$V1, ics$V2, ics$V4, col = ics$mancl1 + 1)
plot3d(ics$V2, ics$V3, ics$V4, col = as.numeric(as.factor(ics$label))+1)
plot3d(ics$V2, ics$V3, ics$V5, col = ics$mancl1 + 1)
plot3d(ics$V2, ics$V3, ics$V5, col = as.numeric(as.factor(ics$label))+1)
plot3d(ics$V2, ics$V4, ics$V5, col = ics$mancl1 + 1)
plot3d(ics$V2, ics$V4, ics$V5, col = as.numeric(as.factor(ics$label))+1)
plot3d(ics$V3, ics$V4, ics$V5, col = ics$mancl1 + 1)


table(ics$label, ics$mancl1, ics$V5)
table(ics$label, ics$mancl2)
table(ics$label, ics$mancl3)
table(ics$label, ics$mancl4)


clfull <- SubClu(cicTues, epsilon = 0.01, minSupport = 5000)
plot(clfull, data = cicTues, color_by = "mix")
# We get 2, 6, 2, 2 relevant dimensions in the 4 output list elements of clfull
# These fall in dimensions (50, 51), (3, 15, 31, 32, 38, 48), (7, 33), (7, 33)
cicSubspace <- cicTues[, c(3, 7, 15, 31, 32, 33, 38, 48, 50, 51)]
cicSubspace[, label := ics$label]
cicSubspace[, cl3 := as.numeric(as.factor(fifelse(as.numeric(rownames(cicSubspace)) %in% clfull[[3]]$objects, 1, 0)))]
cicSubspace[, cl2 := as.numeric(as.factor(fifelse(as.numeric(rownames(cicSubspace)) %in% clfull[[2]]$objects, 1, 0)))]
plot3d(cicSubspace$total_backward_packets, cicSubspace$fwd_packet_length_min, cicSubspace$min_packet_length, col = as.numeric(as.factor(cicSubspace$cl2))+1)
plot3d(cicSubspace$total_backward_packets, cicSubspace$fwd_packet_length_min, cicSubspace$flow_packets_s, col = as.numeric(as.factor(cicSubspace$cl2))+1)
plot3d(cicSubspace$fwd_packets_s, cicSubspace$bwd_packets_s, cicSubspace$min_packet_length, col = as.numeric(as.factor(cicSubspace$cl2))+1)
plot3d(cicSubspace$act_data_pkt_fwd, cicSubspace$active_mean, cicSubspace$active_std, col = as.numeric(as.factor(cicSubspace$cl2))+1)
