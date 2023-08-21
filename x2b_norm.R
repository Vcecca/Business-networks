#compute max normalised entropy - X2bis variable 
#read the file x2_H.csv
x2_H<-read.csv("x2_H.csv", header = T, sep = ";", dec = ",")
x2_H

#create a new object that contains log(n) with base =3

log_3_n<-log(x1$n, base = 3)
log_3_n

#install.packages("entropy")
#library(entropy)
#install.packages("igraph")
#library(igraph)
#install.packages("bio3d")
#library(bio3d)

#compute norm entropy

H_norm<-x2_H$H_data/log_3_n

str(H_norm)
max(H_norm)
H_norm

x2_H$H_norm<- H_norm

str(x2_H)
#export the object in a new csv file
write.table(x2_H, "X2_Sh_and_H_norm.csv",
            sep = ";", # punto e virgola
            row.names = FALSE, # se abbiamo la variabile ID
            dec = ",", # separatore di decimali
            na = " ", # dati mancanti come celle vuote
            quote = TRUE)
