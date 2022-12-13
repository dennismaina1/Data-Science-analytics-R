#display correlating tables (Preprocessing)
correlation <- function(object){
  
  data <- object
  data2 <- round(cor(data),3)
  diag(data2)<- NA
  data3 <- which(data2>.9, arr.ind = TRUE)
  data.frame(row_name = row.names(data2)[data3[,1]],column_name= colnames(data2)[data3[,2]])
}



