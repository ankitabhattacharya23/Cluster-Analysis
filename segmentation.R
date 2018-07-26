library(sas7bdat)

#Importing Data
file<-choose.files()                             
final<- read.sas7bdat(file)

str(final)              
names(final)    

data <- final
data$CUST_ID <- NULL

#Exploring data
View(data)
str(data)
names(data)

#Prepare final Data
#standardizing the data

inputdata_final = scale(data)

#View(inputdata_final)
#building clusters using k-means clustering 
str(data)

cluster_three <- kmeans(inputdata_final,3)
cluster_four <- kmeans(inputdata_final,4)
cluster_five <- kmeans(inputdata_final,5)
cluster_six <- kmeans(inputdata_final,6)

cluster_three$cluster

new<-cbind(data,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,km_clust_5=cluster_five$cluster ,km_clust_6=cluster_six$cluster   )
#View(telco_new)

#Graph based on k-means - Optional
require(cluster)

clusplot(data, #dataframe
         cluster_five$cluster, #clusterdata
         color = TRUE, #color
         #shade = TRUE, # Lines in clusters
         lines =6, # lines connecting centroids
         labels = 2 # Labels clusters and cases
)

###Profiling

#Converting into factors
new$km_clust_3=factor(new$km_clust_3)
new$km_clust_4=factor(new$km_clust_4)
new$km_clust_5=factor(new$km_clust_5)
new$km_clust_6=factor(new$km_clust_6)

names(data)

require(tables)
?tabular()
profile<-tabular(1+BALANCE+BALANCE_FREQUENCY+PURCHASES+ONEOFF_PURCHASES+INSTALLMENTS_PURCHASES+CASH_ADVANCE+
                   PURCHASES_FREQUENCY+ONEOFF_PURCHASES_FREQUENCY+PURCHASES_INSTALLMENTS_FREQUENCY+
                   PURCHASES_TRX+CASH_ADVANCE_TRX+
                   CASH_ADVANCE_FREQUENCY+CREDIT_LIMIT+PAYMENTS+MINIMUM_PAYMENTS+TENURE+
                   PRC_FULL_PAYMENT~
                   mean+(mean*new$km_clust_3)+(mean*new$km_clust_4)+(mean*new$km_clust_5)+(mean*new$km_clust_6),
                 data=new)

profile1<-as.matrix(profile)
profile1<-data.frame(profile1)
View(profile1)

profile<-tabular(1~length+(length*km_clust_3)+(length*km_clust_4)+(length*km_clust_5)+(length*km_clust_6),
                 data=new)

profile2<-as.matrix(profile)
profile2<-data.frame(profile2)
View(profile2)

write.csv(profile1,"profile1.csv",row.names = F)
write.csv(profile2,"profile2.csv",row.names = F)
#############################END OF k-Means Segmentation############################