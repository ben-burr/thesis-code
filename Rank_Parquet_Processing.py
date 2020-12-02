# -*- coding: utf-8 -*-
"""
Created on Sat Nov 23 12:38:57 2019

@author: Shelly
"""

import os
import re

import pandas as pd

# where you want to put the output files
output_path = "F:\\Storage\\Thesis\\Rank\\Data\\Exports\\Parquet_Output_Full\\"

# the program will iterate through all of the file in the given directory, find all the parquet files
# and process it
# if you are running this on your own computer, extract the zip in a folder, don't move the paquet
# files and just point the data_path variable to it (since the program search for that file path
data_path = "F:\\Storage\\Thesis\\Rank\\Data\\Parquet\\" # path to where the data is (on the server all the paquet files are in /data/rank/raw/)

# modify this array for the fields that you want to extract
training_values = ["datetime", "source_ip", "source_port", "source_assets_id", 
                   "source_name", "source_internal", "source_routingMode", 
                   "source_reputation", "source_asn", "source_country", 
                   "destination_ip", "destination_port", 
                   "destination_assets_id","destination_name", 
                   "destination_internal", "destination_routingMode", 
                   "destination_reputation", "destination_asn", 
                   "destination_country", "protocol_service", 
                   "protocol_udpOrTcp", "data_conn_duration", 
                   "data_conn_reqBytes", "data_conn_bytes", 
                   "data_conn_reqPackets", "data_conn_respPackets", 
                   "data_conn_reqIPBytes", "data_conn_respIPBytes", 
                   "data_conn_respBytes", "data_conn_reqBytes", 
                   "data_conn_duration", "data_conn_state", 
                   "data_conn_status", "data_conn_history", 
                   "data_conn_bytes", "data_conn_reqPackets", 
                   "data_conn_respPackets", "data_conn_reqIPBytes", 
                   "data_conn_respIPBytes", 
                   "data_conn_info_observedProtocol_name", 
                   "data_conn_info_observedProtocol_hasKnownPort", 
                   "data_conn_info_observedProtocol_portProtocol"]


def writeIPToFile(inputFile, outputFile):
    outFile = open(outputFile, "a", newline='\n')
    try:
        df = pd.read_parquet(inputFile, engine="pyarrow")
        newdf = df[training_values]
        newdf["source_port"] = df["source_port"].astype(int)
        newdf["destination_port"] = df["destination_port"].astype(int)
        newdf.to_csv(outFile, header=False, index=False)
    except:
        print("error reading parquet file...")
    outFile.close()


for subdir, dirs, files in os.walk(data_path):
    for file in files:
        fileName = os.path.join(subdir, file)

        if "conn" in fileName and "parquet" in fileName:
            m = re.search("conn.*day=[0-9]+", fileName)
            # print(m.group(0))
            # the output file names would be of the form conn-YYYY-MM-day=DD-vector_training.csv
            outputFileName = output_path + m.group(0).replace("\\", "-") + "-vector_training.csv"
            print("output to file: " + outputFileName)

            writeIPToFile(fileName, outputFileName)
