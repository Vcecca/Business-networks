#define the sector of each network                   
#see the code used above and use net03.2023 object,that is ready for the analysis  

net03.2023$`settore attivita'`

#create new dataset with number of progr. and settore attivita'
#for defining the sector of each I use the same classication report reti d'impresa 

library("stringr")
#install.packages("tidyverse")
#install.packages("tidytext")
#install.packages("tm")
library(tidyverse)
library(tidytext)
library(tm)
#install.packages("knitr")
library(knitr)

net03.2023$`settore attivita'`<-as.vector(net03.2023$`settore attivita'`)

data.frame(table(unlist(strsplit(net03.2023$`settore attivita'`, "  "))))

classificazione<-list()
lista_i_class<-list()
for (i in 1:9595) {
  
  gruppo<-net03.2023[net03.2023$progr.==i,]
#  #gruppo[10] seleziona la colonna
  data.frame(table(unlist(strsplit(net03.2023$`settore attivita'`, "  "))))
  conteggio<-data.frame(table(unlist(strsplit(gruppo$`settore attivita'`, "  "))))
  conteggio
  #max(conteggio$Freq)
  sett<-print(conteggio$Var1[conteggio$Freq==max(conteggio$Freq)]) #stampa il settore che compare nella rete 
#  #con maggior frequenza
  sett<-as.vector(sett)
  
  if (length(sett)>1)
    
  { sett<-paste(sett, sep = " ", collapse = "-")  }
  
  classificazione<-append(classificazione, sett)
  lista_i_class<-append(lista_i_class, i) 
}
#classificazione

lista_i_class <-as.vector(lista_i_class)
classificazione<- as.vector(classificazione)

length(lista_i_class)
length(classificazione)
matrice_i_class<-matrix(unlist(lista_i_class), nrow=9595, ncol=1, byrow = T) #ok
matrice_class<-matrix(unlist(classificazione), nrow = 7140, ncol=1, byrow = T)

i_progr<-unique(net03.2023$progr.)
i_progr

i_progr<-as.matrix(i_progr)
length(i_progr)


matrice_class_rete<-cbind(i_progr, matrice_class)
is.matrix(matrice_class_rete)

str(matrice_class_rete)
View(matrice_class_rete)
colnames(matrice_class_rete)<-c("id_net", "sector")

head(matrice_class_rete)

as.data.frame(matrice_class_rete)

write.table(matrice_class_rete, "x3.csv",
            sep = ";", # punto e virgola
            row.names = FALSE, # se abbiamo la variabile ID
            dec = ",", # separatore di decimali
            na = " ", # dati mancanti come celle vuote
            quote = TRUE
)

#try to define the number of sector with numbers to compute the variable 
#read the file 
library(readr)
x3 <- read_delim("x3.csv", delim = ";", escape_double = FALSE, 
                 trim_ws = TRUE)
View(x3)

x3$sector
str(x3)
x3$sector<-as.character(x3$sector)
#add a new empty column in the dataset

x3$sec_var<-NA
head(x3)
#
#if (x3$sector[1230]=="INDUSTRIA/ARTIGIANATO") {x3$sec_var[1230]<-(4)}
str(x3)

#assing values to the variable
#AGRICOLTURA/PESCA
for (i in 1:9595)
{
  if (isTRUE(x3$sector[i]=="AGRICOLTURA/PESCA")) 
  {x3$sec_var[i]<-(1)}
  #print(x3$sec_var)
}
#	ALTRO SETTORE
for (i in 1:9595)
{
  if (isTRUE(x3$sector[i]=="ALTRO SETTORE")) 
  {x3$sec_var[i]<-(2)}
  #print(x3$sec_var)
}
#COMMERCIO
for (i in 1:9595)
{
  if (isTRUE(x3$sector[i]=="COMMERCIO")) 
  {x3$sec_var[i]<-(3)}
  #print(x3$sec_var)
}
#INDUSTRIA/ARTIGIANATO
for (i in 1:9595)
{
if (isTRUE(x3$sector[i]=="INDUSTRIA/ARTIGIANATO")) 
{x3$sec_var[i]<-(4)}
  #print(x3$sec_var)
}
#SERVIZI
for (i in 1:9595)
{
  if (isTRUE(x3$sector[i]=="SERVIZI")) 
  {x3$sec_var[i]<-(5)}
  #print(x3$sec_var)
}
#TURISMO
for (i in 1:9595)
{
  if (isTRUE(x3$sector[i]=="TURISMO")) 
  {x3$sec_var[i]<-(6)}
  #print(x3$sec_var)
}

#is.na(x3$sec_var)
#is.na(x3$sec_var[11]=="NA") 
#assign 7 to multisector variable 
for (i in 1:9595)
{
  if (is.na(x3$sec_var[i]=="NA")) 
  {x3$sec_var[i]<-(7)}

}

#add new coloumn to put dummy variables 

x3$d_agr<- NA
x3$d_altro<- NA
x3$d_comm<-NA
x3$d_ind<-NA
x3$d_serv<-NA
x3$d_tur<-NA
x3$d_multi<-NA
head(x3)

head(x3)
class(x3$d_agr)


for (i in 1:9595)
{
ifelse((isTRUE(x3$sec_var[i]==4)), x3$d_ind[i]<-1,  x3$d_ind[i]<-0)
}

for (i in 1:9595)
{
  ifelse((isTRUE(x3$sec_var[i]==1)), x3$d_agr[i]<-1,  x3$d_agr[i]<-0)
}
for (i in 1:9595)
{
  ifelse((isTRUE(x3$sec_var[i]==2)), x3$d_altro[i]<-1,  x3$d_altro[i]<-0)
}

for (i in 1:9595)
{
  ifelse((isTRUE(x3$sec_var[i]==3)), x3$d_comm[i]<-1,  x3$d_comm[i]<-0)
}
for (i in 1:9595)
{
  ifelse((isTRUE(x3$sec_var[i]==5)), x3$d_serv[i]<-1,  x3$d_serv[i]<-0)
}

for (i in 1:9595)
{
  ifelse((isTRUE(x3$sec_var[i]==6)), x3$d_tur[i]<-1,  x3$d_tur[i]<-0)
}

for (i in 1:9595)
{
  ifelse((isTRUE(x3$sec_var[i]==7)), x3$d_multi[i]<-1,  x3$d_multi[i]<-0)
}

#export the table in csv 

write.table(x3, "x3_dummy.csv",
            sep = ";", # punto e virgola
            row.names = FALSE, # se abbiamo la variabile ID
            dec = ",", # separatore di decimali
            na = " ", # dati mancanti come celle vuote
            quote = TRUE
)
