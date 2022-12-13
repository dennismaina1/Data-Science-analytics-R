rm(data)


  
library(class)

#create a date frame

rm(dataframe)
dataframe<-data.frame()

#append error rate values for knn
for(i in c(1:20)){
  
  knn =knn(trainxs,testxs,trainy,k=i)
  error_rate <- mean(knn!=Test$survived)
  dataframe = rbind(dataframe,c(i,error_rate))
 
  
 
}
names(dataframe)[1]<-'index'
names(dataframe)[2]<-"errorrate"


ggplot(data = dataframe, aes(x = index, y =errorrate )) +
 geom_line()+geom_point()+geom_text(aes(label=index),vjust = 1.3, hjust = .5,
                                    show.legend = FALSE)

print(paste("error_rate at",i,"is", error_rate))

