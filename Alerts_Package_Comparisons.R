# jsonlite-ndjson Package Comparison

library(data.table)
library(dplyr)
library(janitor)
library(tidyr)
library(purrr)
library(tidyverse)
library(stringr)
library(waldo)

# For the purposes of this work, assume we have pasted all
# JSON rows into a single larger JSON file

# Stream in the entire dataset as a large nested list
alertsData <- jsonlite::stream_in(file(file.choose(), open = "r"))

# We know that ndjson::stream_in() will automatically unnest everything
# We want to know if it does so correctly
aD <- ndjson::stream_in(file.choose(), cls = "dt")


# jsonlite Cleanup --------------------------------------------------------

# "correlationIds" Feature ------------------------------------------------
correlationIds <- alertsData$`_source`$correlationIds
correlationIds <- t(sapply(correlationIds, '[', seq(max(sapply(correlationIds, length)))))
correlationIds <- as.data.table(correlationIds)
# The first column in subAD is the empty header column from the original nesting
# We drop it. The tables now appear to be transpositions of each other in size
# Pull out the names from the ndjson version, which are not in order
# Clean up the names and sort the columns in order
subAD <- aD %>% dplyr::select(grep("correlationIds", names(aD)), )
subAD <- subAD[, -1]
subAD <- clean_names(subAD)
nom <- names(subAD)
nom <- sub("source_correlation_", "", nom)
nom <- sub("ids_", "", nom)
nom <- ifelse(nchar(nom) == 1, paste0("000", nom),
              ifelse(nchar(nom) == 2, paste0("00", nom),
                     ifelse(nchar(nom) == 3, paste0("0", nom), nom)))
nom <- paste0("correlation_id_", nom)
names(subAD) <- nom
subAD <- subAD %>% select(sort(names(.)))

# Name the columns of the jsonlite version to match
names(correlationIds) <- names(subAD)

# Test a few columns for matching. The first 10 entries of row 6 match by visual inspection
# Now we select a random column to compare
# Choosing a random column index, we get 5744
# Choosing 10 random indices, we get
# c(1626, 4454, 7593, 6380, 2227, 1313, 2122, 8197, 6892, 2609)
identical(subAD[, 6], correlationIds[, 6])
set.seed(3859)
rndInd <- sample(1:8735, 1)
identical(subAD[, 5744], correlationIds[, 5744])
rndInds <- sample(1:8735, 10)
identical(subAD[, c(1626, 4454, 7593, 6380, 2227, 1313, 2122, 8197, 6892, 2609)],
          correlationIds[, c(1626, 4454, 7593, 6380, 2227, 1313, 2122, 8197, 6892, 2609)])
identical(subAD[, 1], correlationIds[, 1])
identical(subAD[, 8735], correlationIds[, 8735])
# We have matches on all! Success!
rm(correlationIds, subAD, nom, rndInd, rndInds)

# Convert jsonlite version to wider data.table ----------------------------
# This immediately unnests the "_source" column-list into 76 columns
str(alertsData, max.level = 1)
alertsData <- as.data.table(alertsData)

# "structuredMessage" Feature ---------------------------------------------
# This is empty
structuredMessage <- alertsData[, "X_source.data.rank_alert.structuredMessage"]
structMessCheck <- unnest(structuredMessage,
                          cols = c(X_source.data.rank_alert.structuredMessage),
                          keep_empty = T)
table(structMessCheck$X_source.data.rank_alert.structuredMessage)
structMessCheck <- unlist(structMessCheck$X_source.data.rank_alert.structuredMessage)
View(aD$`_source.data.rank_alert.structuredMessage`)
table(aD$`_source.data.rank_alert.structuredMessage`)
rm(structMessCheck, structuredMessage, structMessage)

# "justification" Feature -------------------------------------------------
# This is empty
justifData <- as.data.table(unlist(srcData$data.rank_alert.justification))
justification <- alertsData[, "X_source.data.rank_alert.justification"]
justifCheck <- justification[[1]]
justifCheck <- unlist(justifCheck)
str(justifCheck)
View(aD$`_source.data.rank_alert.justification`)
table(aD$`_source.data.rank_alert.justification`)
rm(justifCheck, justification, justifData)

# "source.Reputation" Feature ---------------------------------------------
# Produces two columns with key-value pairs in 8,115 rows
sourceReputation <- alertsData[, "X_source.source.reputation"]
srcRepCheck <- as_tibble(sourceReputation)
srcRepCheck$ID <- alertsData$X_id
srcRepCheck <- srcRepCheck %>% unnest_wider(col = "X_source.source.reputation")
srcRepCheck$reputation <- unlist(srcRepCheck$reputation)
alertsData$source <- srcRepCheck$source[srcRepCheck$ID == alertsData$X_id]
alertsData$reputation <- srcRepCheck$reputation[srcRepCheck$ID == alertsData$X_id]
View(aD$`_source.source.reputation.0.reputation.0`)
View(aD$`_source.source.reputation.0.source`)
table(aD$`_source.source.reputation.0.source`)
rm(srcRepCheck, sourceReputation)

# "destination.Reputation" Feature ---------------------------------------------
# There are only two observations of alien-vault/Malicious Host, so
# we'll ignore this feature, as it does not provide enough to do anything with it
destinationReputation <- alertsData[, "X_source.destination.reputation"]
dRep <- unlist(destinationReputation)
destRepCheck <- as_tibble(destinationReputation)
destRepCheck$ID <- alertsData$X_id
destRepCheck <- destRepCheck %>% unnest_wider(col = "X_source.destination.reputation")
rm(destinationReputation, dRep, destRepCheck)

# "feedbackHistoryClosed" Feature -----------------------------------------
# Produces 12 columns, all character vectors, with 4/37082 observations
# These are 4 false positives from the IDS.
# With such a limited number there are not enough to do anything
feedbackHistoryClosed <- alertsData[, "X_source.feedbackData.history.closed"]
fHistClosedCheck <- as_tibble(feedbackHistoryClosed)
fHistClosedCheck$ID <- alertsData$X_id
fHistClosedCheck <- fHistClosedCheck %>% unnest_wider(col = "X_source.feedbackData.history.closed")
table(is.na(fHistClosedCheck$name))
View(fHistClosedCheck[c(29745, 30383, 31047, 31794),])
rm(feedbackHistoryClosed, fHistClosedCheck)

# "feedbackHistoryComment" Feature ----------------------------------------
# Produces 12 columns with 4 observations in list structure
# The observations are not relevant to any kind of analysis we'll be doing
# since they are simply test comments added to the data by security team
# As such, we will not retain these in the dataset
feedbackHistoryComment <- alertsData[, "X_source.feedbackData.history.comment"]
fHistCommentCheck <- as_tibble(feedbackHistoryComment)
fHistCommentCheck$ID <- alertsData$X_id
fHistCommentCheck <- fHistCommentCheck %>% unnest_wider(col = "X_source.feedbackData.history.comment")
fHistCommentCheckExpand <- apply(fHistCommentCheck[, 1:12], 2, unlist)
View(!is.na(fHistCommentCheckExpand))
View(fHistCommentCheckExpand[c(30263, 35883, 36639, 36640),])
rm(feedbackHistoryComment, fHistCommentCheck, fHistCommentCheckExpand)

# "feedbackHistoryDefault" Feature ----------------------------------------
# Produces 12 columns, all character vectors, with 4/37082 entries
# The resulting three columns are 132 "suppressed" messages and timestamps from a security analyst
# These are not helpful to this analysis and are eliminated
feedbackHistoryDefault <- alertsData[, "X_source.feedbackData.history.default"]
fHistDefaultCheck <- as_tibble(feedbackHistoryDefault)
fHistDefaultCheck$ID <- alertsData$X_id
fHistDefaultCheck$X_source.feedbackData.history.default <- as.list(fHistDefaultCheck$X_source.feedbackData.history.default)
fHistDefaultCheck <- fHistDefaultCheck %>% unnest_wider(col = "X_source.feedbackData.history.default")
fHistDefaultCheckExpand <- apply(fHistDefaultCheck[, 1:3], 2, unlist)
rm(fHistDefaultCheck, fHistDefaultCheckExpand, feedbackHistoryDefault)

# "feedbackHistoryTag" Feature --------------------------------------------
# Again, the resulting 12 columns are messages about added and removed tags
# These are not helpful here, so they are eliminated
feedbackHistoryTag <- alertsData[, "X_source.feedbackData.history.tag"]
fHistTagCheck <- as_tibble(feedbackHistoryTag)
fHistTagCheck$ID <- alertsData$X_id
fHistTagCheck$X_source.feedbackData.history.tag <- as.list(fHistTagCheck$X_source.feedbackData.history.tag)
fHistTagCheck <- fHistTagCheck %>% unnest_wider(col = "X_source.feedbackData.history.tag")
fHistTagCheckExpand <- apply(fHistTagCheck[, 1:12], 2, unlist)
rm(fHistTagCheck, fHistTagCheckExpand, feedbackHistoryTag)

# "feedbackHistoryOpen" Feature -------------------------------------------
# The columns do not unlist properly using the method from earlier
# However, unlisting the entire original column yields better information
# This is a set of notes on whether an alert is or should be re-opened
# with analyst information, timestamps, and so on.
# There is no particularly useful information for our work here,
# so we eliminate these values as well.
feedbackHistoryOpen <- alertsData[, "X_source.feedbackData.history.open"]
fHistOpenCheck <- as_tibble(feedbackHistoryOpen)
fHistOpenCheck$ID <- alertsData$X_id
fHistOpenCheck$X_source.feedbackData.history.open <- as.list(fHistOpenCheck$X_source.feedbackData.history.open)
fHistOpenCheck <- fHistOpenCheck %>% unnest_wider(col = "X_source.feedbackData.history.open")
fHistOpenCheckExpand <- apply(fHistOpenCheck[, 1:12], 2, unlist)
fHistUnlist <- unlist(feedbackHistoryOpen)
rm(fHistOpenCheck, fHistOpenCheckExpand, feedbackHistoryOpen, fHistUnlist)

# "feedbackTags" Feature --------------------------------------------------
# This results in 10 "test tag" values. We eliminate this as well
feedbackTags <- alertsData[, "X_source.feedbackData.tags"]
fTagsCheck <- as_tibble(feedbackTags)
fTagsCheck$ID <- alertsData$X_id
fTagsCheck$X_source.feedbackData.tags <- as.list(fTagsCheck$X_source.feedbackData.tags)
fTagsUnlist <- unlist(fTagsCheck$X_source.feedbackData.tags)
rm(fTagsCheck, fTagsUnlist, feedbackTags)

# "anomalyAssets" Feature -------------------------------------------------
# The resulting `@type` column is constant
# 11688 unique assets
anomalyAssets <- alertsData[, "X_source.data.rank_alert.additionalDetails.AnomalyReason.anomaly_assets"]
anomalyAssets <- anomalyAssets %>% data.table::rbindlist(fill = T, idcol = T)
anomalyAssets[, `@type`:= NULL]
names(anomalyAssets) <- c("id", "type", "value")
n_distinct(anomalyAssets$id)
# Resulting in 11688 rows
anomalyAssets <- anomalyAssets %>% pivot_wider(id_cols = "id", names_from = "type", values_fill = NA)
# Fill in the remaining rows with blanks
anomalyAssets2 <- as.data.table(seq(from = 1, to = 37082, by = 1))
names(anomalyAssets2) <- "id"
# Merge back in
anomalyAssets2 <- merge(x = anomalyAssets2, y = anomalyAssets, by = "id", all.x = TRUE, )
alertsData <- as.data.table(alertsData)
alertsData[, "id" := seq(from = 1, to = 37082, by = 1)]
alertsData <- merge(x = alertsData, y = anomalyAssets, by = "id", all.x = T)
rm(anomalyAssets, anomalyAssets2)

# "interesting" Feature ---------------------------------------------------
# This feature has quite a bit of deeply nested data
# Not all of it may be relevant
interesting <- alertsData[, "X_source.data.rank_alert.interesting"]
interesting <- interesting$X_source.data.rank_alert.interesting
# interesting2 <- as.data.table(sapply(interesting[[1]], '[', (seq(max(sapply(interesting[[1]], length))))))
intCheck <- as.data.table(interesting)
intCheck$ID <- alertsData$X_id
intCheck <- intCheck %>% unnest_wider(col = "X_source.data.rank_alert.interesting")
intCheck <- intCheck %>% unnest_wider(col = "additionalInfo")

# The ID column (not to be confused with the id column) is a single value per cell
# Hence, we'll retain this column and need to do nothing further with it
intID <- as.data.table(t(sapply(intCheck$ID, '[', seq(max(sapply(intCheck$ID, length))))))
rm(intID)

# There is a "type" feature. This contains possible values
#   anomaly, domain, file, ip, machine, program signature, user
# 36,894/37082 alerts have at least one interesting feature associated with them (99.49%)
# One of them has 203 interesting features associated with it. This will be a recurring theme
intType <- as.data.table(t(sapply(intCheck$type, '[', seq(max(sapply(intCheck$type, length))))))
names(intType) <- c(paste0("type_", as.character(seq(1, 203, 1))))
intCheck <- bind_cols(intCheck, intType)
intCheck$type <- NULL
rm(intType)

# We can find matching "value" entries for each "interesting:type" entry
# Since this is one of the famous deeply nested JSON key-value pairs nested within pairs
# nested within pairs nested within pairs
# For example, an anomaly value could be "periodicity.conn-bytes:192.168.2.9:ip:1567296000:3.9358220718978E-4"
# and may be associated with the individual information found within that string
# as well as a large amount of other information
intValue <- as.data.table(t(sapply(intCheck$value, '[', seq(max(sapply(intCheck$value, length))))))
# We can find matching values in the ndjson data columns by visually scanning through the jsonlite columns
View(interesting[32161])
View((interesting[32161])[[1]][[1]])
View(intCheck[32161, ])
View(aD[32161, ])
intCheck <- bind_cols(intCheck, intValue)
intCheck$value <- NULL
rm(intValue)
valCols <- c(paste0("value_", as.character(seq(1, 203, 1))))
names(intCheck[215:417]) <- valCols
rm(valCols)

# The coordinates are latitude and longitude values
# There is a maximum of 203 in a single alert
intCoords <- rbindlist(intCheck$coordinates, idcol = T)
names(intCoords)[1] <- "id"
idCol <- as.data.table(seq(1, 37082, 1))
names(idCol) <- "id"
intCoords <- left_join(idCol, intCoords,  by = "id")
intCoords[, CaseCount := rowid(id)]
intCoords <- data.table::dcast(setDT(intCoords), id ~ CaseCount, sep = "_", value.var = names(intCoords)[2:3])
intCheck <- bind_cols(intCheck, intCoords[, 2:407])
rm(intCoords, idCol)
intCheck$coordinates <- NULL

# We should find that the asn, country, city, and region are similar
# The country feature is very similar
intCountry <- sapply(intCheck$country, '[', seq(max(sapply(intCheck$country, length))))
intCountryInd <- lapply(intCheck$country, is.null)
intCountry2 <- intCountry[intCountryInd %in% FALSE]
intCountry2 <- as.data.table(intCountry2)
intCountry2 <- as.data.table(t(intCountry2))
intCountryRowNames <- unlist(intCountryInd)
intCountryRowNames <- as.data.table(intCountryRowNames)
intCountryRowNames$intCountryRowNames <- as.numeric(intCountryRowNames$intCountryRowNames)
intCountryRowNames[, "rowID" := seq(1, 37082, 1)]
intCountryRowNames <- intCountryRowNames[intCountryRowNames %in% 0]
intCountry2$id <- intCountryRowNames$rowID
rm(intCountryRowNames, intCountryInd)
names(intCountry2)[1:203] <- c(paste0("country_", as.character(seq(1, 203, 1))))
idCol <- as.data.table(seq(1, 37082, 1))
names(idCol) <- "id"
intCountry2 <- left_join(idCol, intCountry2,  by = "id")
intCheck <- bind_cols(intCheck, intCountry2[, 1:203])
intCheck$country <- NULL
rm(idCol, intCountry, intCountry2)

# The region feature matches with the country feature
intRegion <- as.data.table(intCheck$region)
intRegion <- as.data.table(t(intRegion))
intRegionRows <- lapply(intCheck$region, is.null)
intRegionRows <- as.data.table(unlist(intRegionRows))
intRegionRows$V1 <- as.numeric(intRegionRows$V1)
intRegionRows[, "id" := seq(1, 37082, 1)]
intRegionRows <- intRegionRows[V1 %in% 0]
intRegion$id <- intRegionRows$id
names(intRegion)[1:203] <- c(paste0("region_", as.character(seq(1, 203, 1))))
idCol <- as.data.table(seq(1, 37082, 1))
names(idCol) <- "id"
intRegion <- left_join(idCol, intRegion,  by = "id")
intCheck <- bind_cols(intCheck, intRegion[, 1:203])
intCheck$region <- NULL
rm(idCol, intRegion, intRegionRows)

# The city feature matches with the country feature
intCity <- as.data.table(intCheck$city)
intCity <- as.data.table(t(intCity))
intCityRows <- lapply(intCheck$city, is.null)
intCityRows <- as.data.table(unlist(intCityRows))
intCityRows$V1 <- as.numeric(intCityRows$V1)
intCityRows[, "id" := seq(1, 37082, 1)]
intCityRows <- intCityRows[V1 %in% 0]
intCity$id <- intCityRows$id
names(intCity)[1:203] <- c(paste0("city_", as.character(seq(1, 203, 1))))
idCol <- as.data.table(seq(1, 37082, 1))
names(idCol) <- "id"
intCity <- left_join(idCol, intCity,  by = "id")
intCheck <- bind_cols(intCheck, intCity[, 1:203])
intCheck$city <- NULL
rm(idCol, intCity, intCityRows)

# The owner feature matches the country feature
intOwner <- as.data.table(intCheck$owner)
intOwner <- as.data.table(t(intOwner))
intOwnerRows <- lapply(intCheck$owner, is.null)
intOwnerRows <- as.data.table(unlist(intOwnerRows))
intOwnerRows$V1 <- as.numeric(intOwnerRows$V1)
intOwnerRows[, "id" := seq(1, 37082, 1)]
intOwnerRows <- intOwnerRows[V1 %in% 0]
intOwner$id <- intOwnerRows$id
names(intOwner)[1:203] <- c(paste0("owner_", as.character(seq(1, 203, 1))))
idCol <- as.data.table(seq(1, 37082, 1))
names(idCol) <- "id"
intOwner <- left_join(idCol, intOwner,  by = "id")
intCheck <- bind_cols(intCheck, intOwner[, 1:203])
intCheck$owner <- NULL
rm(idCol, intOwner, intOwnerRows)

# The asn feature matches the country feature
intAsn <- as.data.table(intCheck$asn)
intAsn <- as.data.table(t(intAsn))
intAsnRows <- lapply(intCheck$asn, is.null)
intAsnRows <- as.data.table(unlist(intAsnRows))
intAsnRows$V1 <- as.numeric(intAsnRows$V1)
intAsnRows[, "id" := seq(1, 37082, 1)]
intAsnRows <- intAsnRows[V1 %in% 0]
intAsn$id <- intAsnRows$id
names(intAsn)[1:203] <- c(paste0("asn_", as.character(seq(1, 203, 1))))
idCol <- as.data.table(seq(1, 37082, 1))
names(idCol) <- "id"
intAsn <- left_join(idCol, intAsn,  by = "id")
intCheck <- bind_cols(intCheck, intAsn[, 1:203])
intCheck$asn <- NULL
rm(idCol, intAsn, intAsnRows)

# The "interesting:: Internal" feature
intInternal <- as.data.table(intCheck$internal)
intInternal <- as.data.table(t(intInternal))
intInternalRows <- lapply(intCheck$internal, is.null)
intInternalRows <- as.data.table(unlist(intInternalRows))
intInternalRows$V1 <- as.numeric(intInternalRows$V1)
intInternalRows[, "id" := seq(1, 37082, 1)]
intInternalRows <- intInternalRows[V1 %in% 0]
intInternal$id <- intInternalRows$id
names(intInternal)[1:203] <- c(paste0("internal_", as.character(seq(1, 203, 1))))
idCol <- as.data.table(seq(1, 37082, 1))
names(idCol) <- "id"
intInternal <- left_join(idCol, intInternal,  by = "id")
intCheck <- bind_cols(intCheck, intInternal[, 1:203])
intCheck$internal <- NULL
rm(idCol, intInternal, intInternalRows)


compare(alertsData$X_source.data.rank_alert.interesting[[1]]$relatedInterestingObjects, list(aD[, 21:34]))

# Reputation
intReputation <- intCheck$reputation
elemsRows <- sapply(intReputation, lengths)
intReputation <- as.data.table(intReputation)
intReputation <- as.data.table(t(intReputation))
rownames(intReputation) <- elemsRows[which(elemsRows != 0)]
intRep <- rbindlist(intReputation)
# intRep <- rlist::list.clean(intReputation[, 1] ,recursive = T)

# tempCol1 <- rlist::list.clean(intReputation[, 1] ,recursive = T)

# elems captures the nonempty list elements within the list

elems <- as.data.table(elems)
elems[, "id" := seq(1, 37082, 1)]



bigTemp <- intReputation$V1
bigTemp <- compact(bigTemp)
bigTemp <- rbindlist(bigTemp) # Same
tempCol2 <- as.data.table(rbindlist(intReputation$V1)) # Same


elems2 <- sapply(intReputation$V1, lengths)

empties <- lapply(intReputation$V1, sjmisc::is_empty, first.only = FALSE)
intRepCol <- as.data.table(intReputation[, 1])
intRepRows <- lapply(intReputation[, 1], sjmisc::is_empty, first.only = FALSE)
tempCol2 <- compact(intReputation[, 1])




intReputation <- intCheck$reputation
elemsRows <- sapply(intReputation, lengths)
intReputation <- as.data.table(intReputation)
intReputation <- as.data.table(t(intReputation))
# intReputation <- map_dfr(intReputation, rbindlist)
intRep <- map_dfr(intReputation[, 1], as.data.table)
intRep <- as.data.table(t(intRep))
intRep[, "id" := seq(1, nrow(intRep), 1)]
for(i in 1:5) {
  tempIntRep <- map_dfc(intReputation[, c()], as.data.table)
  tempIntRep <- as.data.table(t(tempIntRep))
  tempIntRep[, "id" := seq(1, nrow(tempIntRep), 1)]
  intRep <- intRep[tempIntRep, on = .(id = id)]
}

tempIntRep <- map_dfc(intReputation[, c(1)], as.data.table)
tempIntRep <- as.data.table(t(tempIntRep))
tempIntRep[, "id" := seq(1, nrow(tempIntRep), 1)]
intRep <- intRep[tempIntRep, on = .(id = id)]
tempIntRep <- map_dfc(intReputation[, c(2)], as.data.table)
tempIntRep <- as.data.table(t(tempIntRep))
tempIntRep[, "id" := seq(1, nrow(tempIntRep), 1)]
intRep <- intRep[tempIntRep, on = .(id = id)]



# Nullify the columns checked
alertsData2 <- alertsData[, c("X_source.data.rank_alert.structuredMessage",
                              "X_source.data.rank_alert.additionalDetails.AnomalyReason.anomaly_assets",
                              "X_source.data.rank_alert.justification",
                              "X_source.data.rank_alert.interesting",
                              "X_source.correlationIds",
                              "X_source.feedbackData.history.default",
                              "X_source.feedbackData.history.open",
                              "X_source.feedbackData.history.tag",
                              "X_source.feedbackData.history.closed",
                              "X_source.feedbackData.history.comment",
                              "X_source.feedbackData.tags",
                              "X_source.source.reputation",
                              "X_source.destination.reputation") := NULL]

# Remove any column remaining that is a constant, which eliminates 3 columns
alertsData2 <- janitor::remove_constant(alertsData2)
