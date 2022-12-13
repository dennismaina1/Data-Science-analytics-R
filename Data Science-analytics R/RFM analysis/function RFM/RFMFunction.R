rfm_scores <-
function(ID,Recency,Monetary,Frequency){

rfm_df2<-data.frame(ID,Recency,Monetary,Frequency)  
  

quart<- quantile(rfm_df$Recency,.25)
mid<-quantile(rfm_df$Recency,.50)
tquart<-quantile(rfm_df$Recency,.75)


for (r in 1:nrow(rfm_df2)){
  rfm_df2$R_score <- 0
  rfm_df2$R_score[rfm_df2$Recency >= tquart] <- 1
  rfm_df2$R_score[rfm_df2$Recency >= mid & rfm_df2$Recency <tquart] <- 2
  rfm_df2$R_score[rfm_df2$Recency >= quart & rfm_df2$Recency <mid] <- 3
  rfm_df2$R_score[rfm_df2$Recency < quart ] <- 4
}

quart<- quantile(rfm_df$Monetary,.25)
mid<-quantile(rfm_df$Monetary,.50)
tquart<-quantile(rfm_df$Monetary,.75)


for (m in 1:nrow(rfm_df2)){
  rfm_df2$M_score<-0
  rfm_df2$M_score[rfm_df2$Monetary >= tquart] <- 4
  rfm_df2$M_score[rfm_df2$Monetary < tquart & rfm_df2$Monetary >= mid] <- 3
  rfm_df2$M_score[rfm_df2$Monetary >= quart & rfm_df2$Monetary < mid] <- 2
  rfm_df2$M_score[rfm_df2$Monetary <quart] <- 1
  
}

quart<- quantile(rfm_df2$Frequency,.25)
mid<-quantile(rfm_df2$Frequency,.50)
tquart<-quantile(rfm_df2$Frequency,.75)


for (f in 1:ncol( rfm_df2)){
  rfm_df2$F_score<- 0
  rfm_df2$F_score[rfm_df2$Frequency >=tquart ] <- 4
  rfm_df2$F_score[rfm_df2$Frequency <tquart & rfm_df$Frequency >= mid] <- 3
  rfm_df2$F_score[rfm_df2$Frequency <mid & rfm_df$Frequency >= quart] <- 2
  rfm_df2$F_score[rfm_df2$Frequency <quart] <- 1
}
rfm_df2$score <- rfm_df2$R_score+rfm_df2$F_score+rfm_df2$M_score
rfm_df2$loyalty <- cut(rfm_df2$score,breaks = c(0,3,6,9,12),labels = c('bronze','silver','gold','platinum'))

rfm_df2
}
