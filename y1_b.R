#accademic publication 
#reproduce thesis projects with updates dataset
#find the distance  


#read the dataset

stat_and_geo<-read.csv("stat_and_geo.csv", sep = ";") 
head(stat_and_geo)
tail(stat_and_geo)


#compute the matrix of distance of a single business network using package sf

#replicate the algoritm of the thesis fow a network 

lng<-stat_and_geo$lng
lat<-stat_and_geo$lat

distance<-structure(list(lng=c(lng)[1:7], lat=c(lat)[1:7]))
#, class="data.frame"))
distance
class(distance)
distance<-as.data.frame(distance)
class(distance)

distance_sf<-st_as_sf(distance, coords = c("lng", "lat"), crs=4326)
distance_sf

dist1<-st_distance(distance_sf)
dist1 #matrix

dist1<-dist1/1000 #convert in km 
dist1

library(units)
dist1<-drop_units(dist1) #drop the unit of measure that is km
dist1
#mean(dist1) #value to correct for the correct number of entry of a matrix

num<-sum(dist1)
num
denom<-length(dist1)-sqrt(length(dist1))
length(dist1)
denom
media<-num/denom
media #that is the average distance in km for the 1st network

#extend the algoritm for the all dataset

net_id <- list()
net_mean <- list()

for (i in 1:9595) {
  
  network<-stat_and_geo[stat_and_geo$progr.==i,]
  network
  
  networks_distance<-structure(list(lng=c(network$lng), lat=c(network$lat)))
  networks_distance
  networks_distance<-as.data.frame(networks_distance)
  networks_distance
  distance_sf_net<-st_as_sf(networks_distance, coords=c("lng", "lat"), crs=4326)
  distance_sf_net
  dist_net<-drop_units(st_distance(distance_sf_net))
  print(dist_net)
  somma<-sum(dist_net)
  somma
 denom<-length(dist_net)-sqrt(length(dist_net))
  denom
  media_net<-somma/denom
  media_net<-media_net/1000
  media_net
  
  net_id<-append(net_id, i)
  net_mean<-append(net_mean, media_net)
  print(i)
  print(media_net)
  
}

#now convert the list in a dataframe and export in a file csv

net_id <-as.vector(net_id)
net_mean<- as.vector(net_mean)

matrice_i<-matrix(unlist(net_id), nrow=9595, ncol=1, byrow = T)
matrice_media<-matrix(unlist(net_mean), nrow = 9595, ncol=1, byrow = T)

mean_matrix<-cbind(matrice_i, matrice_media)
is.matrix(mean_matrix)

str(mean_matrix)
View(mean_matrix)
colnames(mean_matrix)<-c("id_network", "average_distance")


head(mean_matrix)

as.data.frame(mean_matrix)

write.table(mean_matrix, "y.csv",
            sep = ";", # punto e virgola
           row.names = FALSE, # se abbiamo la variabile ID
            dec = ",", # separatore di decimali
            na = "", # dati mancanti come celle vuote
            quote = TRUE
)
