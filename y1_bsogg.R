#compute the distance of each formal BNAs

sogg_and_geo<-read.csv("sogg_and_geo_stat.csv", sep = ";") 
head(sogg_and_geo)
tail(sogg_and_geo)

#compute the matrix of distance of a single business network using package sf
#replicate the algoritm of the thesis fow a network 

sogg_lng<-sogg_and_geo$lng
sogg_lat<-sogg_and_geo$lat

sogg_distance<-structure(list(sogg_lng=c(lng)[1:16], sogg_lat=c(lat)[1:16]))
sogg_distance
class(sogg_distance)
sogg_distance<-as.data.frame(sogg_distance)
class(sogg_distance)

#library(sf)
sogg_distance_sf<-st_as_sf(sogg_distance, coords = c("sogg_lng", "sogg_lat"), crs=4326)
sogg_distance_sf

sogg_dist1<-st_distance(sogg_distance_sf)
sogg_dist1 #matrix

sogg_dist1<-sogg_dist1/1000 #convert in km 
sogg_dist1

library(units)
sogg_dist1<-drop_units(sogg_dist1) #drop the unit of measure that is km
sogg_dist1

s_num<-sum(sogg_dist1)
s_denom<-length(sogg_dist1)-sqrt(length(sogg_dist1))
s_media<-s_num/s_denom
#media #that is the average distance in km for the 1st network

#extend the algoritm for the all dataset

sogg_net_id <- list()
sogg_net_mean <- list()

for (i in 1:1242) {
  
  network_sogg<-sogg_and_geo[sogg_and_geo$progr.==i,]
  network_sogg
  
  networks_distance_sogg<-structure(list(lng=c(network_sogg$lng), lat=c(network_sogg$lat)))
  networks_distance_sogg
  networks_distance_sogg<-as.data.frame(networks_distance_sogg)
  networks_distance_sogg
  dist_sf_net_sogg<-st_as_sf(networks_distance_sogg, coords=c("lng", "lat"), crs=4326)
  dist_sf_net_sogg
  dist_net_sogg<-drop_units(st_distance(dist_sf_net_sogg))
  print(dist_net_sogg)
  somma_sogg<-sum(dist_net_sogg)
  somma_sogg
  denom_sogg<-length(dist_net_sogg)-sqrt(length(dist_net_sogg))
  denom_sogg
  media_net_sogg<-somma_sogg/denom_sogg
  media_net_sogg<-media_net_sogg/1000
  media_net_sogg
  
  sogg_net_id<-append(sogg_net_id, i)
  sogg_net_mean<-append(sogg_net_mean, media_net_sogg)
  print(i)
  print(media_net_sogg)
  
}

#now convert the list in a dataframe and export in a file csv

sogg_net_id <-as.vector(sogg_net_id)
sogg_net_mean<- as.vector(sogg_net_mean)
length(sogg_net_id)
length(sogg_net_mean)

m_i_sogg<-matrix(unlist(sogg_net_id), nrow=1242, ncol=1, byrow = T)
m_media_sogg<-matrix(unlist(sogg_net_mean), nrow = 1242, ncol=1, byrow = T)

mean_m_sogg<-cbind(m_i_sogg, m_media_sogg)
is.matrix(mean_m_sogg)

str(mean_m_sogg)
View(mean_m_sogg)
colnames(mean_m_sogg)<-c("id_net_sogg", "average_distance")

head(mean_m_sogg)

as.data.frame(mean_m_sogg)

write.table(mean_m_sogg, "y_sogg.csv",
            sep = ";", # punto e virgola
            row.names = FALSE, # se abbiamo la variabile ID
            dec = ",", # separatore di decimali
            na = "", # dati mancanti come celle vuote
            quote = TRUE
)
