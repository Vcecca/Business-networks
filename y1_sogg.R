#compute the distance start from back_up_ref object computed for y variable
#y1.R script

back_up_ref

#try to do the join and compute the distance
library(purrr)
library(dplyr)

#now replicate the join 
sogg03.2023_join<-sogg03.2023[1:13315,]
tail(sogg03.2023_join)
length(sogg03.2023_join)
nrow(sogg03.2023_join)

#change the name of attribute
colnames(sogg03.2023_join)[5]<- "comune_rete"
colnames(sogg03.2023_join)[12]<- "comune"
#if there is missing value in comune sobstitue the value with the comune_rete value
#sogg_net_na$comune[is.na(sogg_net_na$comune)==TRUE]<-sogg_net_na$comune_rete

sogg03.2023_join$comune[is.na(sogg03.2023_join$comune)==TRUE]<-sogg03.2023_join$comune_rete

#sogg03.2023_join$comune[is.na(sogg03.2023_join$comune)==TRUE]
head(sogg03.2023_join)
sogg03.2023_join = sogg03.2023_join %>% left_join(back_up_ref, by="comune")
sogg03.2023_join

View(sogg03.2023_join)
print.default(which(is.na(sogg03.2023_join$lng)), max = 2000) #1075
View(sogg03.2023_join[is.na(sogg03.2023_join$lng),])
sogg_net_na<- sogg03.2023_join[is.na(sogg03.2023_join$lng),]
View(sogg_net_na)

#sogg_net_na$comune[is.na(sogg_net_na$comune)==TRUE]<-sogg_net_na$comune_rete

View(sogg_net_na)
#adding new rows with new values

back_up_ref$istat<-as.character(back_up_ref$istat)
str(back_up_ref)
str(sogg03.2023_join)
back_up_ref$lng<-as.character(back_up_ref$lng)
back_up_ref$lat<-as.character(back_up_ref$lat)

back_up_ref <- back_up_ref %>% 
  add_row(istat = "21076", comune = "SALORNO SULLA STRADA DEL VIN", 
          lng = "11.21297", lat="46.23911")

back_up_ref <- back_up_ref %>% 
  add_row(istat = "28082", comune = "SANT'ANGELO DI PIOVE DI SACC", 
          lng = "12.007736", lat="45.34616")

back_up_ref <- back_up_ref %>% 
  add_row(istat = "7073", comune = "VERRES", 
          lng = "7.689941", lat="45.66873")

back_up_ref <- back_up_ref %>% 
  add_row(istat = "93038", comune = "SAN GIORGIO DELLA RICHINVELD", 
          lng = "12.86774", lat="46.04541")

back_up_ref <- back_up_ref %>% 
  add_row(istat = "40005", comune = "CASTROCARO TERME E TERRA DEL", 
          lng = "11.95003", lat="44.17607")

back_up_ref <- back_up_ref %>% 
  add_row(istat = "21076", comune = "SALORNO SULLA STRADA DEL VIN", 
          lng = "11.21297", lat="46.23911")

back_up_ref <- back_up_ref %>% 
  add_row(istat = "21076", comune = "SALORNO SULLA STRADA DEL VIN", 
          lng = "11.21297", lat="46.23911")

back_up_ref <- back_up_ref %>% 
  add_row(istat = "21024", comune = "CORTACCIA SULLA STRADA DEL V", 
          lng = "11.22355148", lat="46.31331764")

back_up_ref <- back_up_ref %>% 
  add_row(istat = "21004", comune = "APPIANO SULLA STRADA DEL VIN", 
          lng = "11.25885656", lat="46.45527585")

back_up_ref <- back_up_ref %>% 
  add_row(istat = "23077", comune = "SANT'AMBROGIO DI VALPOLICELL", 
          lng = "10.83367953", lat="45.52462519")

back_up_ref <- back_up_ref %>% 
  add_row(istat = "22252", comune = "BORGO D'ANAUNIA", 
          lng = "11.137331", lat="46.438853")

back_up_ref <- back_up_ref %>% 
  add_row(istat = "7023", comune = "EMARESE", 
          lng = "7.70044113", lat="45.72439931")

back_up_ref <- back_up_ref %>% 
  add_row(istat = "32004", comune = "SAN DORLIGO DELLA VALLE DOLI", 
          lng = "13.85675254", lat="45.60794211")

back_up_ref <- back_up_ref %>% 
  add_row(istat = "67026", comune = "ISOLA DEL GRAN SASSO D'ITALI", 
          lng = "13.66201352", lat="42.50254059")

back_up_ref <- back_up_ref %>% 
  add_row(istat = "2170", comune = "ALTO SERMENZA", 
          lng = "8", lat="45.88333")
#export data for a back_up

class(back_up_ref)

write.table(back_up_ref, "back_up_geo_sogg.csv",
                        sep = ";", # punto e virgola
                        row.names = F, # if you have a ID variable
                        dec = ",", # separator of decimalals
                        na = "", # missing data as empty cells
                        quote = TRUE)

#there are missing values so attached in the final dataset,
#the information of missing data
#and export the dataset with all the data as back up 
#for further analysis 

#read the file and try to do the join for the second time

geo_sogg<-read.csv("back_up_geo_sogg.csv", sep=";", dec = ",", header = T)
View(geo_sogg)
#replicate the operation and adjust the data
sogg03.2023_join

#replicate the join for 2nd time
sogg03.2023_join2<-sogg03.2023[1:13315,]
tail(sogg03.2023_join2)
length(sogg03.2023_join2)
nrow(sogg03.2023_join2)

#change the name of attribute
colnames(sogg03.2023_join2)[5]<- "comune_rete"
colnames(sogg03.2023_join2)[12]<- "comune"
View(sogg03.2023_join2)

#sogg03.2023_join2$comune[is.na(sogg03.2023_join2$comune)==TRUE]<-sogg03.2023_join2$comune_rete

which(is.na(sogg03.2023_join2$comune))
#no empty line, do the join 


head(sogg03.2023_join)
sogg03.2023_join2 = sogg03.2023_join2 %>% left_join(geo_sogg, by="comune")
sogg03.2023_join2

which(is.na(sogg03.2023_join2$lng)) #no empty value export the dataframe as back_up

write.table(sogg03.2023_join2, "sogg_and_geo_stat.csv",
            sep = ";", # punto e virgola
            row.names = F, # if you have a ID variable
            dec = ",", # separator of decimalals
            na = "", # missing data as empty cells
            quote = TRUE, 
            #col.names = c("istat", "comune", "lng", "lat") #give colnames
)
