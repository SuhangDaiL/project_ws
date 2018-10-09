# Setting Arguments -------------------------------------------------------

# Define file path saving all data
args.DATA.PATH <- "D:/Applications/R/william stobart/ws_data"

# subfolders: all week data: 'traffic_data' all map
# data: sysdata

# update data or map in the folder?
args.TEMP.DATA.UPDATE <- F
args.TEMP.MAP.UPDATE <- F
args.TEMP.DATASORT.UPDATE <- F

# Traffic Area and PostCode: Area, Distriction,
# Sector, City Names
args.POSTAREA.TRAFFICAREA <- read.csv(file.path(args.DATA.PATH, 
    "sysdata", "postarea_and_trafficearea.csv"))
args.POSTATEA.CITY <- read.csv(file.path(args.DATA.PATH, 
    "sysdata", "postarea_and_city.csv"))

# installed packages
args.PKG <- c("tidyverse", "readxl", "feather", "leaflet", "shiny",
              "dplyr", "ggplot2", "plotly", "igraph", "heatmaply")
pkgStatus <- suppressWarnings(lapply(args.PKG, require, 
    quietly = T, character = T))
if (!all(as.logical(pkgStatus))) {
    stop(paste0("Packages not loaded:", pkg[!as.logical(pkgStatus)]))
    # Print not loaded packages
}


# Load Data ---------------------------------------------------------------

# Read EXCEL Files or Write Feather Files
tempFun.writeFeather <- function(args.DATA.PATH) {
    traffic.Data.Path <- file.path(args.DATA.PATH, "traffic_data")
    filenames <- list.files(traffic.Data.Path)
    filenamesf <- filenames[grepl(".*.feather", filenames)]
    filenames <- filenames[grepl(".*.xlsx", filenames)]
    
    trafficData <- lapply(file.path(traffic.Data.Path, 
        filenames), readxl::read_excel)
    trafficData.feather <- as.data.frame(bind_rows(trafficData, 
        .id = "id"))
    
    f <- file.remove(file.path(traffic.Data.Path, filenamesf))
    feather::write_feather(trafficData.feather, file.path(traffic.Data.Path, 
        "trafficData.feather"))
}

if (args.TEMP.DATA.UPDATE) {
    tempFun.writeFeather(args.DATA.PATH, "traffic_data")  # update dataset
}
data.Traffic <- feather::read_feather(file.path(args.DATA.PATH, 
    "traffic_data", "trafficData.feather"))

# Read Map Files or Write Files
if (args.TEMP.DATA.UPDATE) {
    source(file.path(args.DATA.PATH, "sysdata", "readMap.R"))  # update dataset
    source(file.path(args.DATA.PATH, "sysdata", "updateMap.R"))
}
load(file = file.path(args.DATA.PATH, "sysdata", "ukmaps.rds"))

# Data Transformation -----------------------------------------------------

full.data.Traffic <- data.Traffic
tempData.Traffic <- data.Traffic[c("Start Date", "FinishDate", 
    "Cust Code", "Coll Dpt", "Del Dpt", "Coll PCode", 
    "Del PCode", "Distance", "Job Type", "Revenue")]

# Date, y,w,m,d PCode: trafficarea, postarea,
# postdist, x, y, city Cumulative value: cost,
# distance, price Countable value: Job Type, Cust Code
tempFun.splitPostCode <- function(pc, matchPart = NULL, data = NULL) {
    temp <- str_trim(pc)
    temp <- str_split_fixed(temp, " ", n = 2)
    temp.p1 <- temp[, 1]
    temp.p2 <- temp[, 2]
    temp.l1 <- str_locate(temp.p1, "\\d")[, 1]
    temp.l2 <- str_locate(temp.p2, "\\d")[, 2]
    temp.p1s <- str_sub(temp.p1, end = temp.l1 - 1)
    temp.p1d <- str_sub(temp.p1, start = temp.l1)
    temp.p1s[is.na(temp.p1s)] <- temp.p1[is.na(temp.p1s)]
    temp.p1d[is.na(temp.p1s)] <- ""
    temp.p2d <- str_sub(temp.p2, end = temp.l2)
    temp.p2s <- str_sub(temp.p2, start = temp.l2 + 1)
    temp.p2s[is.na(temp.p2s)] <- temp.p2[is.na(temp.p2s)]
    temp.p2d[is.na(temp.p2s)] <- ""
    miss.index <- seq(1, length(pc))[temp.p1d == "" | 
        temp.p1s == "" | temp.p2d == "" | temp.p2s == 
        ""]
    return(list(dist = str_c(temp.p1s, temp.p1d), area = str_c(temp.p1s), 
        sect = str_c(temp.p1s, temp.p1d, " ", temp.p2d), 
        warning = miss.index))
}

tempData <- tempData.Traffic
temp.cp <- tempFun.splitPostCode(tempData$`Coll PCode`)
temp.dp <- tempFun.splitPostCode(tempData$`Del PCode`)
# Remove the unnormal variables
tempData <- mutate(tempData, Col_Area = temp.cp$area, 
    Col_Dist = temp.cp$dist, Col_Sect = temp.cp$sect) %>% 
    mutate(Del_Area = temp.dp$area, Del_Dist = temp.dp$dist, 
        Del_Sect = temp.dp$sect)
temp.colpt <- data.frame(Col_Area = args.POSTAREA.TRAFFICAREA$postarea, 
    Col_Traffic_Area = args.POSTAREA.TRAFFICAREA$trafficarea)
temp.delpt <- data.frame(Del_Area = args.POSTAREA.TRAFFICAREA$postarea, 
    Del_Traffic_Area = args.POSTAREA.TRAFFICAREA$trafficarea)
tempData <- tempData[!seq(1, nrow(tempData)) %in% c(temp.cp$warning, 
    temp.dp$warning), ] %>% left_join(temp.colpt, by = "Col_Area") %>% 
    left_join(temp.delpt, by = "Del_Area")
temp.colpt <- data.frame(Col_Area = args.POSTATEA.CITY$postarea, 
    Col_City = args.POSTATEA.CITY$city)
temp.delpt <- data.frame(Del_Area = args.POSTATEA.CITY$postarea, 
    Del_City = args.POSTATEA.CITY$city)
tempData <- tempData %>% left_join(temp.colpt, by = "Col_Area") %>% 
    left_join(temp.delpt, by = "Del_Area")

# add map marker
temp.marker <- rgeos::gCentroid(args.MAP.SECT.MARKERONLY, 
    byid = T)
temp.col <- data.frame(temp.marker@coords, Col_Sect = args.MAP.SECT.MARKERONLY@data$name)
colnames(temp.col) <- c("Col_long", "Col_lat", "Col_Sect")
temp.del <- data.frame(temp.marker@coords, Dol_Sect = args.MAP.SECT.MARKERONLY@data$name)
colnames(temp.del) <- c("Del_long", "Del_lat", "Del_Sect")
tempData <- tempData %>% left_join(temp.col, by = "Col_Sect") %>% 
    left_join(temp.del, by = "Del_Sect")

# add map circle
temp.circle <- rgeos::gCentroid(args.MAP.DIST, byid = T)
temp.col <- data.frame(temp.circle@coords, Col_Dist = args.MAP.DIST@data$name)
colnames(temp.col) <- c("Col_long_Dist", "Col_lat_Dist", 
    "Col_Dist")
temp.del <- data.frame(temp.circle@coords, Dol_Dist = args.MAP.DIST@data$name)
colnames(temp.del) <- c("Del_long_Dist", "Del_lat_Dist", 
    "Del_Dist")
tempData <- tempData %>% left_join(temp.col, by = "Col_Dist") %>% 
    left_join(temp.del, by = "Del_Dist")

data.Traffic <- tempData

data.Traffic$Del_Area <- factor(data.Traffic$Del_Area, 
    levels = args.MAP.AREA@data$name)
data.Traffic$Col_Area <- factor(data.Traffic$Col_Area, 
    levels = args.MAP.AREA@data$name)

data.Traffic$Del_Traffic_Area <- factor(data.Traffic$Del_Traffic_Area, 
    levels = args.MAP.TRAFFICAREA@data$trafficarea)
data.Traffic$Col_Traffic_Area <- factor(data.Traffic$Col_Traffic_Area, 
    levels = args.MAP.TRAFFICAREA@data$trafficarea)

data.Traffic$Del_Sect <- factor(data.Traffic$Del_Sect, 
    levels = args.MAP.SECT.MARKERONLY@data$name)
data.Traffic$Col_Sect <- factor(data.Traffic$Col_Sect, 
    levels = args.MAP.SECT.MARKERONLY@data$name)

data.Traffic$Del_Dist <- factor(data.Traffic$Del_Dist, 
    levels = args.MAP.DIST@data$name)
data.Traffic$Col_Dist <- factor(data.Traffic$Col_Dist, 
    levels = args.MAP.DIST@data$name)


data.Traffic <- na.omit(data.Traffic)


# Time!
tempFun.changeDateToWeek <- function(x) {
    days <- as.numeric(format(x, "%j"))
    year <- as.numeric(format(x, "%y"))
    temp <- as.Date(paste0(year, "/01/01"), "%y/%m/%d")
    temp0 <- as.Date(paste0(year - 1, "/01/01"), "%y/%m/%d")
    dif <- as.numeric(format(temp, "%w"))
    dif <- ifelse(dif > 0, dif, 7)
    dif0 <- as.numeric(format(temp0, "%w"))
    dif0 <- ifelse(dif0 > 0, dif, 7)
    dif00 <- as.numeric(format(temp - 1, "%j"))
    output <- floor((days - dif)/7) + 1
    output <- ifelse(output == 0, floor((dif00 - dif0)/7) + 
        1, output)
    return(output)
}

data.Traffic["Col_Date"] <- data.Traffic$`Start Date`
data.Traffic["Del_Date"] <- data.Traffic$FinishDate
data.Traffic["Col_D"] <- format(data.Traffic$Col_Date, 
    "%A")
data.Traffic["Del_D"] <- format(data.Traffic$Del_Date, 
    "%A")
data.Traffic["Col_Y"] <- as.numeric(format(data.Traffic$Col_Date, 
    "%y"))
data.Traffic["Del_Y"] <- as.numeric(format(data.Traffic$Del_Date, 
    "%y"))
data.Traffic["Col_M"] <- format(data.Traffic$Col_Date, 
    "%B")
data.Traffic["Del_M"] <- format(data.Traffic$Del_Date, 
    "%B")
data.Traffic["Col_W"] <- tempFun.changeDateToWeek(data.Traffic$Col_Date)
data.Traffic["Del_W"] <- tempFun.changeDateToWeek(data.Traffic$Del_Date)
data.Traffic["Col_d"] <- as.numeric(data.Traffic$Col_Date - 
    min(data.Traffic$Col_Date))/3600/24 + 1
data.Traffic["Del_d"] <- as.numeric(data.Traffic$Del_Date - 
    min(data.Traffic$Del_Date))/3600/24 + 1

# Map Names Sequence based on sequence --------------------------------
if (args.TEMP.DATASORT.UPDATE){
  tempData.x = data.Traffic[,c("Col_Area", "Del_Area")] %>%
    setNames(c("V1", "V2"))
  tempData.sortArea <- ws_genTable(tempData.x, "V1", "V2")
  tempData.x = data.Traffic[,c("Col_Dist", "Del_Dist")] %>%
    setNames(c("V1", "V2"))
  tempData.sortDist <- ws_genTable(tempData.x,"V1", "V2")
  tempData.x = data.Traffic[,c("Col_Sect", "Del_Sect")] %>%
    setNames(c("V1", "V2"))
  tempData.sortSect <- ws_genTable(tempData.x,"V1", "V2")
  
  # Sort by area!
  tempData.sort <- data.frame(area = ws_sort(tempData.sortArea)) %>%
    left_join(setNames(args.POSTAREA.TRAFFICAREA, c("a", "area", "trafficarea")), by = "area") %>%
    arrange(trafficarea) %>%
    setNames(c("Area", "X", "Traffic.Area"))
  tempVar <- ws_sort(tempData.sortDist)
  for (i in 1:length(tempData.sort$Area)){
    tempVar0 <- tempVar[tempData.sort[i,1] == 
                          tempFun.splitPostCode(tempVar)$area]
    if (i == 1){
      tempData.sort0 <- data.frame(Dist = ifelse(length(tempVar0) == 0,
                                                 NA,
                                                 tempVar0),
                                   Area = tempData.sort[i,1],
                                   Traffic.Area = tempData.sort[i,3],
                                   stringsAsFactors = F)
    }
    tempData.sort0 <- tempData.sort0 %>%
      rbind(data.frame(Dist = tempVar0,
                       Area = tempData.sort[i,1],
                       Traffic.Area = tempData.sort[i,3],
                       stringsAsFactors = F))
  }
  tempVar <- sort(unique(c(as.character(tempData.sortSect$V1[tempData.sortSect$count != 0]),
                           as.character(tempData.sortSect$V2[tempData.sortSect$count != 0]))))
  for (i in 1:length(tempData.sort0$Dist)){
    tempVar0 <- tempVar[tempData.sort0$Dist[i] == 
                          tempFun.splitPostCode(tempVar)$dist]
    if (i == 1){
      tempData.sort1 <- data.frame(Sect = ifelse(length(tempVar0) == 0,
                                                        NA,
                                                        tempVar0),
                                   Dist = tempData.sort0[i,1],
                                   Area = tempData.sort0[i,2],
                                   Traffic.Area = tempData.sort0[i,3],
                                   stringsAsFactors = F)
    }
    tempData.sort1 <- tempData.sort1 %>%
      rbind(data.frame(Sect = ifelse(length(tempVar0) == 0,
                                     NA,
                                     tempVar0),
                       Dist = tempData.sort0[i,1],
                       Area = tempData.sort0[i,2],
                       Traffic.Area = tempData.sort0[i,3],
                       stringsAsFactors = F))
  }
  args.DATASEQ <- tempData.sort1
  save(args.DATASEQ, file = file.path(args.DATA.PATH, "sysdata", "datasequence.rds"))
}
load(file = file.path(args.DATA.PATH, "sysdata", "datasequence.rds"))

remove(list = ls(pattern = "^temp"))
