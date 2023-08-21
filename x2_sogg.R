#compute the value of entropy and the type of good 
#read the data 

H_data_sogg<-read_xls("statistiche.xls", sheet = "Sogg. Giu.")
View(H_data_sogg)

str(H_data_sogg)
length(H_data_sogg)
#rename colnames

colnames(H_data_sogg)[20]<- "ateco_code"
head(H_data_sogg)
colnames(H_data_sogg)
#divide the ateco code from the rest of description
#extract the ateco code 
library(dplyr)
library(stringr)
str(H_data_sogg$ateco_code)
H_data_sogg$ateco_code<-as.character(H_data_sogg$ateco_code)

#create an empty coloumn
H_data_sogg$ateco_extr<-NA

H_data_sogg$ateco_extr<-str_extract(H_data_sogg$ateco_code, "^.{2}")

colnames(H_data_sogg)
class(H_data_sogg$ateco_extr)
#use this columns to compte extropy

install.packages("splitstackshape")
library(splitstackshape)

#try to compute entropy only for the first network

H_data_sogg$ateco_extr<-as.factor(H_data_sogg$ateco_extr)

H_sogg_net1<-H_data_sogg$ateco_extr[1:16]


print(H_sogg_net1[2])
print(H_sogg_net1[16])

H_sogg_net1[2]==H_sogg_net1[4]

str(H_sogg_net1)

H_sogg_net1<-as.character(H_sogg_net1)
freq_x_sogg <- sort(table(unlist(strsplit(H_sogg_net1, "  "))),      # Create frequency table
               decreasing = TRUE)
freq_x_sogg

max(freq_x_sogg)
str(freq_x_sogg)

class(freq_x_sogg)
freq_x_sogg<-as.matrix(freq_x_sogg)
freq_x_sogg
#freq_x<-t(freq_x)
#freq_x
class(freq_x_sogg)
sum(freq_x_sogg)
##the data are in a table now i can compute the shannon entropy DescTools

library("DescTools")

Entropy(freq_x_sogg, base = 3) #shannon entropy 


##now extend all the algorithm for all the dataset

sogg_reti_id <- list()
H_rete_sogg <- list()

H_data_sogg<-H_data_sogg[-13316,]

for (i in 1:1242) {
  
  rete_sogg<-H_data_sogg[H_data_sogg$progr.==i,]
  rete_sogg
  
  rete_sogg$ateco_extr<-as.character(rete_sogg$ateco_extr)
  
  freq_x1<-sort(table(unlist(strsplit(rete_sogg$ateco_extr, "  "))))
  
  freq_x1
  freq_x1<-as.matrix(freq_x1)
  freq_x1
  if (sum(freq_x1)>0)
  {
    H<-Entropy(freq_x1, base = 3)
    H
    sogg_reti_id<-append(sogg_reti_id, i)
    H_rete_sogg<-append(H_rete_sogg, H)
    sogg_reti_id
    H_rete_sogg
    print(i) 
    print(H)
  }
}

#now convert the list in a dataframe and export in a file csv

sogg_reti_id <-as.vector(sogg_reti_id)
H_rete_sogg<- as.vector(H_rete_sogg)
length(H_rete_sogg)
length(sogg_reti_id)

m_i_sogg<-matrix(unlist(sogg_reti_id), nrow=1218, ncol=1, byrow = T)
m_H_sogg<-matrix(unlist(H_rete_sogg), nrow =1218, ncol=1, byrow = T)

H_sogg_m<-cbind(m_i_sogg, m_H_sogg)
is.matrix(H_sogg_m)

str(H_sogg_m)
View(H_sogg_m)
colnames(H_sogg_m)<-c("id_rete_sogg", "H_sogg")

head(H_sogg_m)

as.data.frame(H_sogg_m)

write.table(H_sogg_m, "x2_H_sogg.csv",
            sep = ";", # punto e virgola
            row.names = FALSE, # se abbiamo la variabile ID
            dec = ",", # separatore di decimali
            na = " ", # dati mancanti come celle vuote
            quote = TRUE
)

#normalize the entropy

#read the file

x2_normH_sogg<-read.csv("x2_H_sogg.csv", sep = ";", dec = ",")
View(x2_normH_sogg)

str(x1_sogg)
#read file 

str(x1_n_sogg)

log3_n_sogg<-log(x1_n_sogg$n, base = 3)
log3_n_sogg

#compute norm entropy
str(x2_normH_sogg)

H_norm_sogg<-x2_normH_sogg$H_sogg/log3_n_sogg
str(H_norm_sogg)
#export the file of normalised entropy in a new file then attach and do consistent then


normalised_entr<-data.frame(x1_n_sogg$sogg_id, H_norm_sogg)
head(normalised_entr)
#rename the coloumn 
library(dplyr)
#correct the data and see the funcion  
#rename(normalised_entr, id_sogg = x1_n_sogg.sogg_id)

View(normalised_entr)
head(normalised_entr)
colnames(normalised_entr$x1_n_sogg.sogg_id)

#export the file into a csv for back up

head(normalised_entr$x1_n_sogg.sogg_id)

write.table(normalised_entr$x1_n_sogg.sogg_id, "X2_H_norm_sogg.csv",
            sep = ";", # punto e virgola
            row.names = FALSE, # se abbiamo la variabile ID
            dec = ",", # separatore di decimali
            na = " ", # dati mancanti come celle vuote
            quote = TRUE)
