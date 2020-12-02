library(data.table)
library(dplyr)

# Set the working directory to the folder where the data is extracted
setwd("F:/Storage/Thesis/Rank/Data/Parquet")

# Creates a list of JSON files in the directory
filenames <- list.files(getwd(), include.dirs = TRUE, recursive = TRUE,
                        pattern = "*.parquet", full.names = TRUE)

# Use the ndjson package's stream_in() function to read the data and unnest it automatically
# This generates a list of data tables called alertData
events <- data.table()
for(i in 1:1000){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
for(i in 1001:1100){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}

for(i in 1101:1500){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}

for(i in 1501:2000){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_2.csv")
events <- data.table()

for(i in 2001:3000){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_3.csv")

events <- data.table()
for(i in 3001:3500){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_4.csv")

events <- data.table()
for(i in 3501:3600){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
for(i in 3601:3700){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
for(i in 3701:3800){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
for(i in 3801:3900){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
for(i in 3901:4000){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_5.csv")

events <- data.table()
for(i in 4001:4500){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_6.csv")

events <- data.table()
for(i in 4501:4600){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
for(i in 4601:4700){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
for(i in 4701:4800){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
for(i in 4801:4900){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
for(i in 4901:5000){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_7.csv")

events <- data.table()
for(i in 5001:5250){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
for(i in 5251:5500){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_8.csv")

events <- data.table()
for(i in 5501:5750){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
for(i in 5751:6000){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_9.csv")

events <- data.table()
for(i in 6001:6250){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
for(i in 6251:6500){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
for(i in 6501:6750){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
for(i in 6751:7000){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_10.csv")

events <- data.table()
for(i in 7001:7250){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_7000.csv")
events <- data.table()
for(i in 7251:7500){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_7250.csv")
events <- data.table()
for(i in 7501:7750){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_7500.csv")
events <- data.table()
for(i in 7751:8000){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_7750.csv")

events <- data.table()
for(i in 8001:8250){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_8000.csv")
events <- data.table()
for(i in 8251:8500){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_8250.csv")
events <- data.table()
for(i in 8501:8750){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_8500.csv")
events <- data.table()
for(i in 8751:9000){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_8750.csv")

events <- data.table()
for(i in 9001:9250){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_9000.csv")
events <- data.table()
for(i in 9251:9500){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_9250.csv")
events <- data.table()
for(i in 9501:9750){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_9500.csv")
events <- data.table()
for(i in 9751:10000){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_9750.csv")

events <- data.table()
for(i in 10001:10250){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_10000.csv")
events <- data.table()
for(i in 10251:10350){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_10250.csv")
events <- data.table()
for(i in 10351:10500){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_10350.csv")
events <- data.table()
for(i in 10501:10750){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_10500.csv")
events <- data.table()
for(i in 10751:11000){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_10750.csv")

events <- data.table()
for(i in 11001:11250){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_11000.csv")
events <- data.table()
for(i in 11251:11300){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
for(i in 11301:11350){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
for(i in 11351:11400){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
for(i in 11401:11500){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_11250.csv")
events <- data.table()
for(i in 11501:11750){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_11500.csv")
events <- data.table()
for(i in 11751:12000){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_11750.csv")


events <- data.table()
for(i in 12001:12250){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_12000.csv")
events <- NULL
events <- data.table()
for(i in 12251:12500){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_12250.csv")
events <- NULL
i <- NULL
events <- data.table()
for(i in 12501:12750){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_12500.csv")
events <- NULL
i <- NULL
events <- data.table()
for(i in 12751:13000){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_12750.csv")

events <- NULL
i <- NULL
events <- data.table()
for(i in 13001:13250){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_13000.csv")
events <- NULL
i <- NULL
events <- data.table()
for(i in 13251:13500){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_13250.csv")
events <- NULL
i <- NULL
events <- data.table()
for(i in 13501:13600){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
for(i in 13601:13700){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
for(i in 13701:13750){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_13500.csv")
events <- NULL
i <- NULL
events <- data.table()
for(i in 13751:14000){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_13750.csv")

events <- NULL
i <- NULL
events <- data.table()
for(i in 14001:14250){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_14000.csv")
events <- NULL
events <- data.table()
for(i in 14251:14500){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_14250.csv")
events <- NULL
i <- NULL
events <- data.table()
for(i in 14501:14600){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
for(i in 14601:14700){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
for(i in 14701:14750){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_14500.csv")
events <- NULL
i <- NULL
eventData <- NULL
events <- data.table()
for(i in 14751:15000){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_14750.csv")


events <- NULL
i <- NULL
events <- data.table()
for(i in 15001:15250){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_15000.csv")
events <- NULL
events <- data.table()
for(i in 15251:15300){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
for(i in 15301:15400){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
for(i in 15401:15500){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_15250.csv")
events <- NULL
i <- NULL
events <- data.table()
for(i in 15501:15750){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_15500.csv")
events <- NULL
i <- NULL
events <- data.table()
for(i in 15751:16000){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_15750.csv")


events <- NULL
i <- NULL
events <- data.table()
for(i in 16001:16250){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_16000.csv")
events <- NULL
i <- NULL
events <- data.table()
for(i in 16251:16500){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_16250.csv")
events <- NULL
i <- NULL
events <- data.table()
for(i in 16501:16750){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_16500.csv")
events <- NULL
i <- NULL
events <- data.table()
for(i in 16751:17000){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_16750.csv")


events <- NULL
i <- NULL
events <- data.table()
for(i in 17001:17250){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_17000.csv")
i <- NULL
events <- NULL
events <- data.table()
for(i in 17251:17500){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_17250.csv")
events <- NULL
i <- NULL
events <- data.table()
for(i in 17501:17750){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_17500.csv")
events <- NULL
i <- NULL
events <- data.table()
for(i in 17751:18000){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_17750.csv")

events <- NULL
i <- NULL
events <- data.table()
for(i in 18001:18100){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
for(i in 18101:18200){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
for(i in 18201:18250){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_18000.csv")
i <- NULL
events <- NULL
events <- data.table()
for(i in 18251:18500){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_18250.csv")
events <- NULL
i <- NULL
events <- data.table()
for(i in 18501:18750){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_18500.csv")
events <- NULL
i <- NULL
events <- data.table()
for(i in 18751:19000){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_18750.csv")



events <- NULL
i <- NULL
events <- data.table()
for(i in 19001:19250){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_19000.csv")
i <- NULL
events <- NULL
events <- data.table()
for(i in 19251:19500){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_19250.csv")
events <- NULL
i <- NULL
events <- data.table()
for(i in 19501:length(filenames)){
  eventData <- lapply(filenames[i], function(x) {arrow::read_parquet(x)})
  events <- bind_rows(events, eventData[[1]])
}
fwrite(events, "F:/Storage/Thesis/Rank/Data/Exports/Events_Parquet_Merge_19500.csv")


