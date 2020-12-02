setwd("F:/Storage/Thesis/Data/CICIDS2017/")
library(data.table)
library(stringr)
library(lubridate)
library(dplyr)
library(dplyr)

# Preprocessing Steps
cic <- data.table(fread("cic2017.csv"))
cic <- cic[!is.na(cic$source_port), ]
cic <- cic[, 2:6]
cic[ , source_port := as.character(source_port)]
cic[ , destination_port := as.character(destination_port)]
cic[ , protocol := as.character(protocol)]
fwrite(cic, "CICIDS_IPs.csv", row.names = FALSE, col.names = FALSE)

srcip <- as.vector(cic$source_ip)
srcip <- unique(srcip)
srcip <- paste0("srcip_", srcip)
destip <- as.vector(cic$destination_ip)
destip <- unique(destip)
destip <- paste0("destip_", destip)
srcport <- as.vector(cic$source_port)
srcport <- unique(srcport)
srcport <- paste0("srcport_", srcport)
destport <- as.vector(cic$destination_port)
destport <- unique(destport)
destport <- paste0("destport_", destport)
prot <- as.vector(cic$protocol)
prot <- unique(prot)
prot <- paste0("prot_", prot)

vocabs <- vector(length = 154565, mode = "character")
for(i in 1:length(srcip)){
  vocabs[i] <- srcip[i]
}
for(i in 1:length(destip)){
  vocabs[length(srcip) + i] <- destip[i]
}
for(i in 1:length(srcport)){
  vocabs[length(srcip) + length(destip) + i] <- srcport[i]
}
for(i in 1:length(destport)){
  vocabs[length(srcip) + length(destip) + length(srcport) + i] <- destport[i]
}
for(i in 1:length(prot)){
  vocabs[length(srcip) + length(destip) + length(srcport) + length(destport) + i] <- prot[i]
}
length(vocabs)
vocabs <- unique(vocabs)

fwrite(as.data.table(vocabs), "cic_vocab.csv")


# Load Word Vectors -------------------------------------------------

cicwv <- data.frame(fread("CICIDS_WordVectors.csv"))
cicwv <- cicwv[-1, ]


# Re-Load full Dataset ----------------------------------------------------

# Preprocessing Steps
cic <- data.table(fread("cic2017.csv"))
# cic[, "source_ip_num" := iptools::ip_to_numeric(cic$source_ip)]
# cic[, "destination_ip_num" := iptools::ip_to_numeric(cic$destination_ip)]
cic <- cic[!is.na(cic$source_port), ]

# Some duration values are less than zero
cic$flow_duration[cic$flow_duration < 0] <- 0
cic$flow_iat_max[cic$flow_iat_max < 0] <- 0
cic$flow_iat_mean[cic$flow_iat_mean < 0] <- 0
cic$fwd_header_length[cic$fwd_header_length < 0] <- 0
cic$bwd_header_length[cic$bwd_header_length < 0] <- 0
cic$min_seg_size_forward[cic$min_seg_size_forward < 0] <- 0
cic$flow_bytes_s[is.na(cic$flow_bytes_s)] <- 0
cic$flow_packets_s[cic$flow_packets_s < 0] <- 0
cic$flow_bytes_s[cic$flow_bytes_s < 0] <- 0
cic$flow_bytes_s[is.na(cic$flow_bytes_s)] <- 0

# Some duration values are infinite
cic$flow_packets_s[cic$flow_packets_s %in% Inf] <- 4e+06
cic$flow_bytes_s[cic$flow_bytes_s %in% Inf] <- 2.071e+09

# Remove constants
cic <- janitor::remove_constant(cic)

cic$date <- as.Date.character(str_trunc(cic$timestamp, width = 10, side = "right", ellipsis = ""), format = "%d/%m/%Y")
cic$time <- sapply(strsplit(as.character(cic$timestamp), " "), "[", 2)
cic[, hour := ifelse(nchar(cic$time) == 4, str_trunc(cic$time, width = 1, side = "right", ellipsis = ""), str_trunc(cic$time, width = 2, side = "right", ellipsis = ""))]
cic[, hour := ifelse(cic$hour == "1", 13,
                     ifelse(cic$hour == "2", 14,
                            ifelse(cic$hour == "3", 15,
                                   ifelse(cic$hour == "4", 16,
                                          ifelse(cic$hour == "5", 17,
                                                 ifelse(cic$hour == "8", 20,
                                                        ifelse(cic$hour == "9", 21, cic$hour)))))))]

# Word Vectors have been learned contextually -----------------------------
# This means that they will apply to multiple items
# Let's say a flow has particular Source & Destination IP addresses, Ports, and a protocol
# This implies that the vector for each of these items applies to this flow,
# which may enrich it by many columns

# Create a blank 32-column table
blankcic <- data.table(matrix(data = NA, nrow = dim(cic)[1], ncol = (dim(cicwv)[2])-1))
# Attach it 5 times to the dataframe, once per variable
cic2 <- bind_cols(cic, blankcic, blankcic, blankcic, blankcic, blankcic)
# Insert names for the new variables
nomvec <- vector(mode = "character", length = 32)
for(i in 1:32){
  nomvec[i] <- paste0("sip_V", i)
}
names(cic2)[80:111] <- nomvec
for(i in 1:32){
  nomvec[i] <- paste0("dip_V", i)
}
names(cic2)[112:143] <- nomvec
for(i in 1:32){
  nomvec[i] <- paste0("spo_V", i)
}
names(cic2)[144:175] <- nomvec
for(i in 1:32){
  nomvec[i] <- paste0("dpo_V", i)
}
names(cic2)[176:207] <- nomvec
for(i in 1:32){
  nomvec[i] <- paste0("pro_V", i)
}
names(cic2)[208:239] <- nomvec

# Turn the word vectors into a data table
cicwv <- as.data.table(cicwv)

# Insert word vectors based on vocabulary item
# Recall that each vocabulary item has its own 32-dimensional vector
# So a shared pair (say Source and Destination IPs)
# will have two 32-dimensional vectors associated with them
cic2[, 80:111] <- cicwv[match(cic2$source_ip, cicwv$source_ip), 2:33]
cic2[, 112:143] <- cicwv[match(cic2$destination_ip, cicwv$source_ip), 2:33]
cic2[, 144:175] <- cicwv[match(cic2$source_port, cicwv$source_ip), 2:33]
cic2[, 176:207] <- cicwv[match(cic2$destination_port, cicwv$source_ip), 2:33]
cic2[, 208:239] <- cicwv[match(cic2$protocol, cicwv$source_ip), 2:33]


# Export File -------------------------------------------------------------

fwrite(cic2, "cicids2017_full_with_WordVectors.csv")
cic2 <- unique(cic)
fwrite(cic, "cicids2017_full_with_WordVectors_unique.csv")
rm(cic2)


# Extract Labels ----------------------------------------------------------

cic <- data.table(fread("cic2017.csv"))
labels <- as.data.table(cic$label)
tuesLabs <- as.data.table(cic$label[as.character(cic$date) %in% "2017-07-04"])
wedLabs <- as.data.table(cic$label[as.character(cic$date) %in% "2017-07-05"])
thursLabs <- as.data.table(cic$label[as.character(cic$date) %in% "2017-07-06"])
friLabs <- as.data.table(cic$label[as.character(cic$date) %in% "2017-07-07"])
fwrite(labels, "cic_wv_labels.csv")
fwrite(tuesLabs, "cic_wv_labels_tues.csv")
fwrite(wedLabs, "cic_wv_labels_wed.csv")
fwrite(thursLabs, "cic_wv_labels_thurs.csv")
fwrite(friLabs, "cic_wv_labels_fri.csv")
