library(data.table) # For speed instead of data.frames we are using data.tables
library(lubridate)

################################################ 
### 1. READ ACCELEROMETER DATA FROM FOLDERS ####
################################################
# Set data folder
dir.data = paste0(getwd(),"/01_data/lux_raw/")
# Get all files in data folder
files = list.files(path = dir.data, pattern = "*\\.lux$", full.names = F)
# Disconsider animal OCT02 - It was predated and collar found 1km from the tunnel
files = files[files != "OCT02.csv"]

tuco_lux = vector("list", length(files)) # Initialize a empty list
for (i in 1:length(files)) {
    # 1. READ FILE
    message("\nReading file:", files[i], "...")
    tuco_lux[[i]] = fread(paste0(dir.data,"/",files[i]),
                          header = F,   # ignore file's header
                          skip = 20 ,    # skip first line corresponding to header
                          sep = "auto", # set sep to auto. Some files use comma, others space
                          fill = T,
                          #select = 2:5, # ignore id column
                          col.names = c("datetime", "lux"),
                          showProgress = T,
                          data.table = T)
    
    # 2. FIX ID AND PARSE DATETIME
    tuco_lux[[i]][, ID := strsplit(files[i], ".", fixed = T)[[1]][1]]
    tuco_lux[[i]][, datetime := parse_date_time(datetime, "dmYHMS", tz = "America/Argentina/La_Rioja")]
}
# Bind the whole list to create a unique data.table with all animals' data
tuco_lux = rbindlist(tuco_lux)
setcolorder(tuco_lux, c("ID", "datetime", "lux"))

# Save created data.frame as a .rds file
#saveRDS(tuco_lux, "data_rds/lux/tuco_lux.rds")
