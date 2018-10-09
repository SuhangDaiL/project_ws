ws_genTable <- function(data, v1, v2, value = NULL, fun = sum, na.rm = TRUE) {
  
  # Check! ------------------------
  
  # check v1, v2 length
  if (length(v1) != 1 | length(v2) != 1) {
    stop("check v1 and v2  length = 1?")
  }
  # v1 and v2 are two column's of data
  if (!v1 %in% names(data) | !v2 %in% names(data) | v1 == v2) {
    stop("check your names")
  }
  # data has to be data.frame
  if (!is.data.frame(data)) {
    warning("data is not dataframe and we change it")
  }
  data <- data.frame(data)
  # v1, v2 has to be factor
  if (!is.factor(data[, v1])) {
    warning("v1 is not a factor and we change it to the factor")
    data[, v1] <- factor(data[, v2])
  }
  if (!is.factor(data[, v2])) {
    warning("v2 is not a factor and we change it to the factor")
    data[, v2] <- factor(data[, v2])
  }
  v1.levels <- levels(data[, v1])
  v2.levels <- levels(data[, v2])
  
  # Default value! -----------------
  
  # if na.rm is true, function will remove the rows value is null
  if (na.rm) {
    data <- na.omit(data)
  }
  # value = null
  if (is.null(value)) {
    value <- "count"
    data[, value] <- 1
  }
  
  # Outputs ----------------------
  eval(parse(text = paste0("table <- data %>%", "group_by(", v1, "=", v1, ",", v2, "=", v2, ") %>%", 
                           "summarise(", value, "=", "fun(", value, "))", sep = "")))
  output <- data.frame(rep(v1.levels, each = length(v2.levels)), rep(v2.levels, length(v1.levels)))
  names(output) <- c(v1, v2)
  output <- dplyr::left_join(output, data.frame(table), by = c(v1, v2))
  levels(output[, v1]) <- v1.levels
  levels(output[, v2]) <- v2.levels
  output[is.na(output[,value]), value] = 0
  return(output)
}

ws_combineTable <- function(data, v1, v2, value = NULL,
                         name = c("V1", "V2", "value", "G1", "G2")) {
  data = data.frame(data)
  output <- data.frame()
  for (i in 1:length(v1)){
    for (j in 1:length(v2)){
      x <- data.frame(genTable(data, v1[i], v2[j], value), v1[i],v2[j]) %>%
        mutate_if(.predicate = is.factor, .funs = as.character)
      output <- bind_rows(output, setNames(x, name))
    }
  }
  return(output)
}

# sort sequence of area, district and sectors for better visualization for
# heatmap
ws_sort <- function(q){
  q0 <- setNames(q, c("V1", "V2", "value"))  %>%
    mutate( V1 = as.character(V1),
            V2 = as.character(V2),
            value = ifelse(value == 0, NA, value)) %>%
    na.omit()
  q0 <- graph_from_data_frame(q0) %>%
    as_adj(attr = "value")
  eigen <-  eigen(q0)
  eigenvec2_sort <- data.frame(eigen = eigen$vectors[, length(eigen$values) - 1]) %>%
    mutate(row = row_number(),
           names = q0@Dimnames[[1]]) %>%
    arrange(eigen)
  eigenvec2_sort %>% pull(names)
}

ws_heatmap <- function(data, ticktext,
                       gridname = NULL, 
                       h = sqrt(nrow(data))*20,
                       axisfontsize = 8){
  # first column is V1
  # second column is V2
  # value is for color
  # string is for label
  seq.sortV <- setNames(seq(1, length(ticktext)), ticktext)
  data <- data %>%
    setNames(c("V1", "V2", "zcolor", "info")) %>%
    mutate(V1 = seq.sortV[as.character(V1)],
          V2 = seq.sortV[as.character(V2)])
  tickvals = seq(1, length(ticktext) ,length.out = length(ticktext))
  l1 <- list( tickmode = "array",
              tickvals = tickvals, 
              ticktext = ticktext,
              tickfont = list(size = axisfontsize),
              title = "COLL")
  l2 <- list( tickmode = "array",
              tickvals = tickvals, 
              ticktext = ticktext,
              tickfont = list(size = axisfontsize),
              title = "DEL")
  p <- plot_ly(height = h, 
               width = h,
               showscale=FALSE, showlegend = FALSE
  ) %>%
    add_trace(data = data, x = ~V1, y = ~V2, 
              z = ~ zcolor, 
              type = "heatmap",
              text = ~ info,
              hoverinfo = "text",
              colors =  "YlOrRd") 
  if (length(ticktext) > 0){
    p <- p %>%
      layout(xaxis = l1,
             yaxis = l2)
  } 
  if (length(gridname)>0) {
    gridn <- c(0)
    for (i in seq(1, length(gridname)-1)){
      if (gridname[i] != gridname[i+1]){
        gridn <- c(gridn, i+1)
      }
    }
    gridn <- c(gridn, length(gridname))
    for (i in gridn){
      p <- p %>% add_lines(data = NULL, x = c(i, i)-0.5, y = c(0,length(ticktext))-0.5, color = I("black"), 
                           size = I(1), linetype = I("dotted"))
      p <- p %>% add_lines(data = NULL, x = c(0,length(ticktext))-0.5, y = c(i, i)-0.5, color = I("black"), 
                           size = I(1), linetype = I("dotted"))
    }
  }
  
  return(p)
}


ws_genHeatmapData <- function(data, xnames, seqData){
  tempData.x <- seqData[, c("Dist", "Area", "Traffic.Area")] %>%
    mutate(Dist = as.character(Dist),
           Area = as.character(Area),
           Traffic.Area = as.character(Traffic.Area),
           gridL = as.character(Traffic.Area))
  for (i in 1:nrow(seqData)){
    if (tempData.x[i,3] %in% xnames){
      tempData.x[i,3] = tempData.x[i,2]
    }
    if (tempData.x[i,2] %in% xnames){
      tempData.x[i,3] = tempData.x[i,1]
    }
  }
  tempData.x <- tempData.x[!duplicated(tempData.x[,c(3,4)]),]
  tempSeq.sortV <- tempData.x[,3]
  tempSeq.gridL <- tempData.x[,4] %>%
    setNames(tempSeq.sortV)
  temSeq.z <- setNames(seq(1, nrow(tempData.x)), tempSeq.sortV)
  
  mydata = data[,c("Col_Traffic_Area", "Del_Traffic_Area",
                   "Col_Area", "Del_Area", 
                   "Col_Dist", "Del_Dist",
                   "Distance")] %>% 
    mutate(V1 = as.character(Col_Traffic_Area), 
           V2 = as.character(Del_Traffic_Area),
           V3 = as.character(Col_Area),
           V4 = as.character(Del_Area),
           V5 = as.character(Col_Dist),
           V6 = as.character(Del_Dist),
           value = Distance)
  #long
  for (i in 1:length(tempSeq.sortV)) {
    mydata$V1[tempSeq.sortV[i] == mydata$V3] <-
      mydata$V3[tempSeq.sortV[i] == mydata$V3]
    mydata$V1[tempSeq.sortV[i] == mydata$V5] <-
      mydata$V5[tempSeq.sortV[i] == mydata$V5]
    mydata$V2[tempSeq.sortV[i] == mydata$V4] <-
      mydata$V4[tempSeq.sortV[i] == mydata$V4]
    mydata$V2[tempSeq.sortV[i] == mydata$V6] <-
      mydata$V6[tempSeq.sortV[i] == mydata$V6]
  }
  mydata <- mydata %>% 
    mutate(V1 = factor(V1, levels = tempSeq.sortV, ordered = TRUE),
           V2 = factor(V2, levels = tempSeq.sortV, ordered = TRUE))
  
  
  tempFun.q <- function(x){
    x.q <- quantile(x, 0.95)
    x[x-x.q > 0] <- (x[x-x.q > 0]-x.q)/ (max(x) - x.q) * 
      sqrt(var(x[x-x.q <= 0]))*10 + x.q
    return(x)
  }
  
  tempData.heatmapcount = ws_genTable(mydata, "V1", "V2") 
  tempData.heatmapsum = ws_genTable(mydata, "V1", "V2", value = "value", fun = sum) %>%
    setNames(c("V1", "V2", "sum"))
  tempData.heatmapmean = ws_genTable(mydata, "V1", "V2", value = "value", fun = mean) %>%
    setNames(c("V1", "V2", "mean"))
  tempData.heatmap = left_join(tempData.heatmapcount, tempData.heatmapsum) %>%
    left_join(tempData.heatmapmean) %>%
    setNames(c("V1", "V2", "count", "sum", "mean"))  %>%
    mutate(value = ifelse(count == 0, NA, count)) %>%
    na.omit() %>%
    mutate(info = paste("Col =", V1, "in", tempSeq.gridL[V1], 
                        "\nDel =", V2, "in", tempSeq.gridL[V2],
                        "\ncount =", count, 
                        "\nsum =", round(sum),
                        "\nmean =", round(mean))) %>%
    mutate(zcolor = sum)
  return(list(heatmap = tempData.heatmap[c("V1","V2","zcolor","info")],
              sortv = tempSeq.sortV,
              gridname = tempSeq.gridL))
}

