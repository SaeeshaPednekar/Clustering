crimedata<-read.csv("F:/Excelr/Assignments/dataset/clustering/crime_data.csv")
View(crimedata)
str(crimedata)
summary(crimedata)

normalize_data<-scale(crimedata[,2:4])#z score
data<-dist(normalize_data,method="euclidean")
fit<-hclust(data,method="complete")
plot(fit,hang=-1)
str(fit)


rect.hclust(fit,k=3,border="violet")
groups<-cutree(fit,k=3)
groups

membership<-as.matrix(groups)
View(membership)
final<-data.frame(crimedata,membership)
View(final)

final_1<-final[,c(ncol(final),1:(ncol(final)-1))]
write.csv(final_1,file="final1.csv",row.names = FALSE)
getwd()
aggregate(crimedata[,-1],by=list(final$membership),mean)



#kmeans
crimedata<-read.csv("F:/Excelr/Assignments/dataset/clustering/crime_data.csv")
View(crimedata)
str(crimedata)
summary(crimedata)

normalize_data<-scale(crimedata[,2:4])#z score
fit<-kmeans(normalize_data,3)
str(fit)
fit$cluster
final2<-data.frame(crimedata,fit$cluster)
final3<-final2[,c(ncol(final2),1:(ncol(final2)-1))]
aggregate(crimedata[,2:4],by=list(fit$cluster),mean)



install.packages("kselection")
library(kselection)
install.packages("doParallel")
library(doParallel)
registerDoParallel(cores = 4)
k<-kselection(normal_data,parallel = TRUE,k_threshold = 0.95,max_centers = 13)
k     # finds 8 clusters

#elbow plot
twss<-NULL
for (i in 1:14)
  twss[i]=sum(kmeans(normalize_data,centers=i)$tot.withinss)


plot(1:14,twss,type="b",xlab ="No. of clusters", ylab="within group of sum of squares")
