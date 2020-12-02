# This script requires the following packages:
# install.packages("data.table")
# install.packages("janitor")
# install.packages("ndjson")

# Stream in the Alerts data -----------------------------------------------

# This section completely unnests the data into a very
# wide and sparse data.table

# Set the working directory to the folder where the data is extracted
setwd("directory/FolderOfJSONs")

# Creates a list of JSON files in the directory
filenames <- list.files(getwd(), include.dirs = TRUE, recursive = TRUE,
                        pattern = "*.json", full.names = TRUE)

# Use the ndjson package's stream_in() function to read the data and unnest it automatically
# This generates a list of data tables called alertData
alertData <- lapply(filenames, function(x) {ndjson::stream_in(x, cls = "dt")})

# Name the datasets
for (i in 1:length(alertData)) {
  names(alertData)[[i]] <- paste0("alerts_", i)
}

# Remove the empty columns of these datasets
alertData <- lapply(alertData, janitor::remove_empty, which = "cols")

# Write the datasets to file in csv format
sapply(names(alertData), function(x) data.table::fwrite(alertData[[x]], file = paste(x, "csv", sep = ".")))

# Extract the individual datasets to the Global environment if desired
# list2env(alertData, envir = .GlobalEnv)

# Alternative Data Streaming -----------------------------------------------

# If we have pasted all of the data into a single file
# we may stream it in to a data.frame that contains several
# other data.frames and lists in several of the columns.

alertsData <- jsonlite::stream_in(file(file.choose(), open="r"))
data.table::fwrite(alertsData, "alertsData.csv")
