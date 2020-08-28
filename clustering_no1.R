airlines<-read.csv("F:/Excelr/Assignments/dataset/clustering/EastWestAirlines.csv")
View(airlines)
sum(is.na(airlines))
str(airlines)
#normalise the data
normal_data<-scale(airlines[,2:12])
View(normal_data)

distance<-dist(normal_data, method="euclidean")
fit<-hclust(distance,method="single")
str(fit)
plot(fit)
fitt<-hclust(distance,method="ward.D2")
str(fitt)
plot(fitt,hang = -1)

rect.hclust(fitt,k=8,border="violet")
group<-cutree(fitt,k=8)
group

members<-as.matrix(group)
View(members)
final_model<-data.frame(airlines[2:12],members)
View(final_model)
final_1<-final_model[,c(ncol(final_model),1:(ncol(final_model)-1))]
write.csv(final_1,file="clusterfile1",row.names = FALSE)
getwd()
aggregate(airlines[,-1],by=list(final_model$members),mean)





#kmeans
install.packages("kselection")
library(kselection)
install.packages("doParallel")
library(doParallel)
registerDoParallel(cores = 4)
k<-kselection(normal_data,parallel = TRUE,k_threshold = 0.95,max_centers = 13)
k

normal_data<-scale(airlines[,2:12])
fitt<-kmeans(normal_data,8)
str(fitt)
value<-fitt$cluster
final_model_k<-data.frame(airlines[,2:12],value)
final_modelk2<-final_model_k[,c(ncol(final_model_k),1:(ncol(final_model_k)-1))]
aggregate(airlines[,2:12],by=list(value),mean)


final2<-data.frame(crimedata,fit$cluster)
final3<-final2[,c(ncol(final2),1:(ncol(final2)-1))]
aggregate(crimedata[,2:4],by=list(fit$cluster),mean)


#elbow plot
twss<-NULL
for (i in 1:14)
  twss[i]=sum(kmeans(normal_data,centers=i)$tot.withinss)


plot(1:14,twss,type="b",xlab ="No. of clusters", ylab="within group of sum of squares")
