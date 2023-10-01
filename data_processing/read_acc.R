################################################ 
### 1. READ ACCELEROMETER DATA FROM FOLDERS ####
################################################
# This script reads all the raw csv files and merge them into one data set
# It can be run standalone but it is already called in the data processing script (processing.R)
library(data.table) # For speed instead of data.frames we are using data.tables
library(lubridate)
# Set data folder
dir.data = paste0(getwd(),"/01_data/activity_raw")
# Get all files in data folder
files = list.files(path = dir.data, pattern = "*\\.csv$", full.names = F)
# Disconsider animal OCT02 - It was predated and collar found 1km from the tunnel malfunctiong.
files = files[files != "OCT02.csv"]

tuco_acc = vector("list", length(files)) # Initialize a empty list
for (i in 1:length(files)) {
    # 1. READ FILE
    message("\nReading file:", files[i], "...")
    tuco_acc[[i]] = fread(paste0(dir.data,"/",files[i]),
                          header = T,
                          sep = "auto", # set sep to auto. Some files use comma, others space
                          fill = T,
                          select = c("Timestamp","X","Y","Z","Temp. (?C)"),
                          col.names = c("datetime", "X", "Y","Z","temp"),
                          showProgress = T,
                          data.table = T)
    
    # 2. FIX ID AND PARSE DATETIME
    tuco_acc[[i]][, ID := strsplit(files[i], ".", fixed = T)[[1]][1]]
    tuco_acc[[i]][, datetime := parse_date_time(datetime, "dmYHMOS", tz = "America/Argentina/La_Rioja")]
    
    # DELETE FIRST AND LAST DAY OF RECORDING
    tuco_acc[[i]][, day_number := frank(as_date(datetime), ties.method = "dense")]
    tuco_acc[[i]] = tuco_acc[[i]][day_number > 1 & day_number < last(day_number)] 
    tuco_acc[[i]][, day_number := frank(as_date(datetime), ties.method = "dense")]
    
    # DELETE PARTICULAR DATA
    # delete last 5 days of #FEV05 because it was brought to the lab and continued recording for a week
    # delete last 2 days of #JUL23 due to recapture efforts
    # delete last 5 days of #JUL16 due to recapture efforts
    if(files[i] == "FEV05.csv"){
        tuco_acc[[i]] = tuco_acc[[i]][day_number < last(day_number) - 5]
    }
    if(files[i] == "JUL23.csv"){
        tuco_acc[[i]] = tuco_acc[[i]][day_number < last(day_number) - 2]
    }
    if(files[i] == "JUL16.csv"){
        tuco_acc[[i]] = tuco_acc[[i]][day_number < last(day_number) - 5]
    }
}
# Bind the whole list to create a unique data.table with all animals' data
tuco_acc = rbindlist(tuco_acc)
setcolorder(tuco_acc, c("ID", "day_number", "datetime", "temp", "X", "Y", "Z"))
tuco_acc[,ID := factor(ID, levels = c("MAR01", "MAR02",
                                      "JUL15", "JUL16", "JUL17", "JUL18", "JUL19", "JUL20", "JUL21", "JUL23",
                                      "OCT01", "OCT08", "OCT09", "OCT10", "OCT13", "OCT14",
                                      "FEV01", "FEV02", "FEV03", "FEV05", "FEV06"))]

#Save created data.frame as a .rds file
#saveRDS(tuco_acc, "data_rds/activity/tuco_10hz_raw.rds")
