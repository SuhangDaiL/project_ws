
# 1 -----------------------------------------------------------------------

xnames <- c("WA", "NN", "YO", "A05", "A06", "A07", "A13")
ws_heatmap(ws_genHeatmapData(data.Traffic, xnames, args.DATASEQ), 
           ticktext = tempSeq.sortV,
           gridname = tempSeq.gridL)





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


# # Condition: Count > 100
# args.countlimit <-  1000
# mydata <- data.Traffic[,c("Col_Traffic_Area", "Del_Traffic_Area")]
# tempData.ta <- ws_genTable(mydata, "Col_Traffic_Area", "Del_Traffic_Area")
# tempFun.unique <- function(y, l){
#   y[,1] = as.character(y[,1])
#   y[,2] = as.character(y[,2])
#   x <- unique(y[,2])
#   for (i in 1:length(x)){
#     if (!l[i]){
#       y[y[,2] == x[i], 1] = x[i] 
#     }
#   }
#   unique(y[,1])
# }
# 
# 
# temp.y <- seqData[,c(3,4)]
# temp.l <- unique(as.character(seqData[,4])) %in%
#   unique(unlist(tempData.ta[tempData.ta$count > args.countlimit, c(1,2)]))
# temp.v <- tempFun.unique(temp.y, temp.l)
# 
# 
# 
# 
# # Heat Map!
# # plotly? (legend is in another graph!)
# # 
