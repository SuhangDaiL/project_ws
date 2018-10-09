
data <- data.Traffic[c("Col_d", "Del_d", "Col_Traffic_Area", "Del_Traffic_Area", "Distance")] %>%
  filter(Col_d < 20)

temp <- ws_workflow(data)
