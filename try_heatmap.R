# Heat Map!
# plotly? (legend is in another graph!)
# 

ws_heatmap <- function(data, names = c("V1", "V2", "zcolor", "string")){
  
  # ws heatmap generate a heatmap based on data with 4 columns
  # the first column is for x and then y and then zcolor with string explained
  # for interative
  
}
# 1 -----------------------------------------------------------------------

# We will sort our areas, 

mydata = data.Traffic[,c("Col_Dist", "Del_Dist", "Col_d", 
                "Del_d", "Revenue", "Cust Code" )] %>%
  setNames(c("V1", "V2", "T1", "T2", "value", "category"))

tempData.heatmap = ws_genTable(mydata, "V1", "V2") 
tempData.heatmapsum = ws_genTable(mydata, "V1", "V2", value = "value", fun = sum) %>%
  setNames(c("V1", "V2", "sum"))
tempData.heatmapmean = ws_genTable(mydata, "V1", "V2", value = "value", fun = mean) %>%
  setNames(c("V1", "V2", "mean"))
tempData.heatmap = left_join(tempData.heatmap, tempData.heatmapsum) %>%
  left_join(tempData.heatmapmean)


# Sort by area!
sortArea <- data.frame(area = ws_sort(tempData.heatmap[,c(1,2,3)])) %>%
  left_join(setNames(args.POSTAREA.TRAFFICAREA, c("a", "area", "trafficarea")), by = "area") %>%
  arrange(trafficarea)

tempData.heatmap <- setNames(tempData.heatmap, c("V1", "V2", "count", "sum", "mean"))  %>%
  mutate( V1 = as.character(V1),
          V2 = as.character(V2),
          value = ifelse(count == 0, NA, count)) %>%
  na.omit()
tempData.heatmap$V1 = factor(tempData.heatmap$V1, levels = sortArea$area, ordered = T)
tempData.heatmap$V2 = factor(tempData.heatmap$V2, levels = sortArea$area, ordered = T)

Fun.q <- function(x){
  x.q <- quantile(x, 0.95)
  x[x-x.q > 0] <- (x[x-x.q > 0]-x.q)/ (max(x) - x.q) * 
    sqrt(var(x[x-x.q <= 0]))*10 + x.q
  return(x)
}
sorta <- sortArea$area
sortt <- sortArea$trafficarea
sortn <- length(sorta)
sortgrid <- c(0)
for (i in seq(1, sortn-1)){
  if (sortt[i] != sortt[i+1]){
    sortgrid <- c(sortgrid, i+1)
  }
}
sortgrid <- c(sortgrid, sortn)
sortN <- setNames(seq(1,sortn), sorta)
tempData.heatmap <- tempData.heatmap %>%
  mutate(info = paste("Col =", sorta[V1], "in", sortt[V1], 
                      "\nDel =", sorta[V2], "in", sortt[V2],
                      "\ncount =", count, 
                      "\nsum =", round(sum),
                      "\nmean =", round(mean))) %>%
  mutate(zcolor = Fun.q(sum),
         V1 = sortN[V1],
         V2 = sortN[V2])

l1 <- list( tickmode = "array",
           tickvals = seq(1, sortn), 
           ticktext = sorta,
           tickfont = list(size = 8),
           title = "COLL")
l2 <- list( tickmode = "array",
            tickvals = seq(1, sortn), 
            ticktext = sorta,
            tickfont = list(size = 8),
            title = "DEL")
p <- plot_ly(height = sqrt(nrow(tempData.heatmap))*20, 
             width = sqrt(nrow(tempData.heatmap))*20,
             showscale=FALSE, showlegend = FALSE
            ) %>%
  add_trace(data = tempData.heatmap, x = ~V1, y = ~V2, 
        z = ~ zcolor, 
        type = "heatmap",
        text = ~ info,
        hoverinfo = "text",
        colors =  "YlOrRd") %>%
  layout(xaxis = l1,
         yaxis = l2)

for (i in sortgrid){
  p <- p %>% add_lines(data = NULL, x = c(i, i)-0.5, y = c(0,sortn)-0.5, color = I("black"), 
                       size = I(1), linetype = I("dotted"))
  p <- p %>% add_lines(data = NULL, x = c(0,sortn)-0.5, y = c(i, i)-0.5, color = I("black"), 
                       size = I(1), linetype = I("dotted"))
}

p

# 
# tempData.heatmap = tempData.heatmap %>%
#   mutate(count = ifelse(count == 0, NA, count))
# 
# 
# 
# tempData.heatmap <- tempData.heatmap %>%
#   mutate(z2 = ifelse(count < quantile(ifelse(count > 10, count, NA), 0.95, na.rm = T) & count > 10, count, NA)) %>%
#   mutate(z3 = ifelse(count > quantile(ifelse(count > 10, count, NA), 0.95, na.rm = T),
#                      count, NA))
# 
# # Levels relationship 
# library(networkD3)
# temp <-  c(as.character(unique(args.POSTAREA.TRAFFICAREA$trafficarea)),
#            as.character(args.POSTAREA.TRAFFICAREA$postarea))
# temp.edgename <- seq(0, length(temp)-1) %>%
#   setNames(temp)
# tempData.nodeD3 <- data.frame(label = temp) %>% 
#   mutate(id = c(as.character(unique(args.POSTAREA.TRAFFICAREA$trafficarea)), 
#                 as.character(args.POSTAREA.TRAFFICAREA$trafficarea)),
#          value = 1)
# tempData.edgeD3 <-  data.frame(from = as.numeric(temp.edgename[as.character(args.POSTAREA.TRAFFICAREA$trafficarea)]),
#                                to = as.numeric(temp.edgename[as.character(args.POSTAREA.TRAFFICAREA$postarea)]),
#                                weight = 1)
# forceNetwork(
#   Links = tempData.edgeD3, Nodes = tempData.nodeD3,  
#   Source = "from", Target = "to", 
#   NodeID = "label", Group = "id")
# 
# 
# 
# 
# plot_ly(data = tempData.heatmap, x = ~V1, y = ~V2, 
#         z = ~z2, 
#         type = "heatmap",
#         height = sqrt(nrow(tempData.heatmap))*5, 
#         width = sqrt(nrow(tempData.heatmap))*5+200,
#         colors = c("red", "blue"))  %>%
#   add_trace(type = "heatmap", data = tempData.heatmap,  x = ~V1, y = ~V2,
#             z = ~z3, colors = c("white", "green"))
# 
# 
# 
# 
# q <- p %>%
#   mutate(value = ifelse(value >= 10, value, NA)) %>%
#   mutate(value = log(value)) %>%
#   filter(!is.na(value))
# 
# q0 <- graph_from_data_frame(q) %>%
#   as_adj(attr = "value")
# eigen <-  eigen(q0)
# 
# eigenvec2_sort <- data.frame(eigen = eigen$vectors[, length(eigen$values) - 1]) %>%
#   mutate(row = row_number(),
#          names = q0@Dimnames[[1]]) %>%
#   arrange(eigen)
# 
# eigen_names <- eigenvec2_sort %>% pull(names)
# 
# sss <- quantile(diff(eigenvec2_sort %>% pull(eigen) %>% Mod()), 0.95)
# cummunity_df <- eigenvec2_sort %>%
#   mutate(community = c(0, diff(Mod(eigen)) > sss) %>% cumsum()) %>%
#   select(names, community)
# 
# lala <- q %>%
#   mutate(V1_chr = as.character(V1),
#          V2_chr = as.character(V2),
#          V1 = factor(V1, levels = eigen_names),
#          V2 = factor(V2, levels = eigen_names)) %>%
#   left_join(cummunity_df, by = c("V1_chr" = "names")) %>%
#   left_join(cummunity_df, by = c("V2_chr" = "names")) %>%
#   mutate(community = ifelse(community.x == community.y, community.x, NA),
#          community = ifelse(!is.na(value), community, NA))
# 
# 
# 
# qp <- lala %>%
#   ggplot(aes(V1, 
#              V2, 
#              alpha = value, 
#              fill = factor(community))) + #, 
#   geom_tile(color = "grey50") + #
#   scale_alpha(range = c(0.5, 1)) +
#   scale_fill_brewer(palette = "Set1", na.value = "grey50") +
#   theme_minimal() + 
#   theme(panel.grid.major = element_blank(),
#         axis.text.x = element_text(angle = 90, hjust = 1)) +
#   guides(fill = "none", alpha = "none") +
#   coord_fixed() +
#   labs(x = NULL, y = NULL, 
#        title = "1", 
#        subtitle = "1")
# qp
# 
# plot_ly(q, 
#         x = ~V1, 
#         y = ~V2,
#         z = ~value, 
#         type = "heatmap")
# 
# 
# ggplot(p) +
#   geom_boxplot(aes(y = value))
# # 2 -----------------------------------------------------------------------
# 
# d <- data.Traffic
# p <- combineTable(d, "Col_Area", "Del_Area")
