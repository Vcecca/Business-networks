#compute the x3 variable that is the sector of each network 
#missing data to the dataset 
#solve by ateco code 

sogg03.2023$`CODICE ATECO impresa`

#split the first two characters of ateco from the rest 

str(sogg03.2023)
library(tidyr)

#create an empty coloumn
sogg03.2023$ateco_gen<-NA
sogg03.2023$ateco_gen<-strtrim(sogg03.2023$`CODICE ATECO impresa`, 2)
str(sogg03.2023$ateco_gen)
sogg03.2023$ateco_gen<-as.numeric(sogg03.2023$ateco_gen)

sogg03.2023$settore<-NA

x3_sogg<-sogg03.2023
#agricoltura/pesca codice 1
for (i in 1:13316) 
{
  if 
(isTRUE(x3_sogg$ateco_gen[i]<3))
{x3_sogg$settore[i]<-1} #else {x3_sogg$settore[i]<-NA}
}

#altro settore 2
for (i in 1:13316) 
{
  if (isTRUE(x3_sogg$ateco_gen[i]==84 | 
             isTRUE(x3_sogg$ateco_gen[i]==94) | isTRUE(x3_sogg$ateco_gen[i]==99)))
      {x3_sogg$settore[i]<-2} 
#else  {x3_sogg$settore[i]<-NA}
}
#commercio 3
for (i in 1:13316) 
{
  if (isTRUE(x3_sogg$ateco_gen[i]==45 | 
             isTRUE(x3_sogg$ateco_gen[i]==46) | isTRUE(x3_sogg$ateco_gen[i]==47)))
  {x3_sogg$settore[i]<-3} 
 # else  {x3_sogg$settore[i]<-NA}
}
#industria/artigianato 4
for (i in 1:13316) 
{
  if (isTRUE(x3_sogg$ateco_gen[i]>=5 & x3_sogg$ateco_gen[i]<=33) | 
             isTRUE(x3_sogg$ateco_gen[i]>=41 & x3_sogg$ateco_gen[i]<=43) | isTRUE(x3_sogg$ateco_gen[i]==95))
  {x3_sogg$settore[i]<-4} 
#  else  {x3_sogg$settore[i]<-NA}
}

#servizi 5
for (i in 1:13316) 
{
  if (isTRUE(x3_sogg$ateco_gen[i]>=35 & x3_sogg$ateco_gen[i]<=39) | 
      isTRUE(x3_sogg$ateco_gen[i]>=49 & x3_sogg$ateco_gen[i]<=53) | 
      isTRUE(x3_sogg$ateco_gen[i]>=58 & x3_sogg$ateco_gen[i]<=82) |
      isTRUE(x3_sogg$ateco_gen[i]>=85 & x3_sogg$ateco_gen[i]<=93) |
      isTRUE(x3_sogg$ateco_gen[i]>=96 & x3_sogg$ateco_gen[i]<=98))
  {x3_sogg$settore[i]<-5} 
 # else  {x3_sogg$settore[i]<-NA}
}  
#turismo 6
for (i in 1:13316) 
{
  if (isTRUE(x3_sogg$ateco_gen[i]==55) | isTRUE(x3_sogg$ateco_gen[i]==56)) 
  {x3_sogg$settore[i]<-6} 
#  else  {x3_sogg$settore[i]<-NA}
}
View(x3_sogg)

#now compute the sector for each Formal networks
#transorm the sector in text

x3_sogg$settore<-as.character(x3_sogg$settore)

classific_sogg<-list()
i_class_sogg<-list()
for (i in 1:1242) {
  
  sett_sogg<-x3_sogg[x3_sogg$progr.==i,]
  data.frame(table(unlist(strsplit(x3_sogg$settore, "  "))))
  count_sogg<-data.frame(table(unlist(strsplit(sett_sogg$settore, "  "))))
  count_sogg
#  #max(conteggio$Freq)
  sett_net_sogg<-print(count_sogg$Var1[count_sogg$Freq==max(count_sogg$Freq)]) #stampa il settore che compare nella rete 
  #  #con maggior frequenza
  sett_net_sogg<-as.vector(sett_net_sogg)
  sett_net_sogg
  if (is.null(sett_net_sogg)) 
    {sett_net_sogg<-0}
  
  if (length(sett_net_sogg)>1)
    
  { sett_net_sogg<-paste(sett_net_sogg, sep = " ", collapse = "--")  }
  
  classific_sogg<-append(classific_sogg, sett_net_sogg)
  i_class_sogg<-append(i_class_sogg, i) 
}


i_class_sogg <-as.vector(i_class_sogg)
classific_sogg<- as.vector(classific_sogg)

length(i_class_sogg)
length(classific_sogg)
m_i_class<-matrix(unlist(i_class_sogg), nrow=1242, ncol=1, byrow = T) #ok
m_class_sogg<-matrix(unlist(classific_sogg), nrow = 1242, ncol=1, byrow = T)

View(m_class_sogg)

m_sett_sogg<-cbind(i_class_sogg, m_class_sogg)
is.matrix(m_sett_sogg)

head(m_sett_sogg)
str(m_sett_sogg)
View(m_class_sogg)
colnames(m_sett_sogg)<-c("id_net", "sector")

head(m_sett_sogg)


m_sett_sogg<-as.data.frame(m_sett_sogg)
m_sett_sogg$sett_code<-NA

head(m_sett_sogg)
colnames(m_sett_sogg)<- c("id_net", "sector", "sett_code")
nrow(m_sett_sogg)

m_sett_sogg$sett_code<-m_sett_sogg$sector
head(m_sett_sogg)

#add new coloumn and compute the dummy variable 

m_sett_sogg$d_agr<- NA
m_sett_sogg$d_altro<- NA
m_sett_sogg$d_comm<-NA
m_sett_sogg$d_ind<-NA
m_sett_sogg$d_serv<-NA
m_sett_sogg$d_tur<-NA
m_sett_sogg$d_multi<-NA
head(m_sett_sogg)

class(m_sett_sogg)
str(m_sett_sogg)
nrow(m_sett_sogg)

m_sett_sogg$sett_code<-as.numeric(m_sett_sogg$sett_code)


for (i in 1:1242)
{
  ifelse((isTRUE(m_sett_sogg$sett_code[i]==4)), m_sett_sogg$d_ind[i]<-1,
         m_sett_sogg$d_ind[i]<-0)
}

for (i in 1:1242)
{
  ifelse((isTRUE(m_sett_sogg$sett_code[i]==1)), m_sett_sogg$d_agr[i]<-1,  
         m_sett_sogg$d_agr[i]<-0)
}
for (i in 1:1242)
{
  ifelse((isTRUE(m_sett_sogg$sett_code[i]==2)), m_sett_sogg$d_altro[i]<-1,  
         m_sett_sogg$d_altro[i]<-0)
}

for (i in 1:1242)
{
  ifelse((isTRUE(m_sett_sogg$sett_code[i]==3)), m_sett_sogg$d_comm[i]<-1,
         m_sett_sogg$d_comm[i]<-0)
}
for (i in 1:1242)
{
  ifelse((isTRUE(m_sett_sogg$sett_code[i]==5)), m_sett_sogg$d_serv[i]<-1, 
         m_sett_sogg$d_serv[i]<-0)
}

for (i in 1:1242)
{
  ifelse((isTRUE(m_sett_sogg$sett_code[i]==6)), m_sett_sogg$d_tur[i]<-1,  
         m_sett_sogg$d_tur[i]<-0)
}

#insert value 
for (i in 1:1242)
{
if (is.na(m_sett_sogg$sett_code[i]))
  {m_sett_sogg$sett_code[i]<-7}
}

#View(m_class_sogg)
for (i in 1:1242)
{
  ifelse((isTRUE(m_sett_sogg$sett_code[i]==7)), m_sett_sogg$d_multi[i]<-1,  
         m_sett_sogg$d_multi[i]<-0)
}

head(m_sett_sogg)
class(m_sett_sogg)
str(m_sett_sogg)

m_sett_sogg$id_net<-as.numeric(m_sett_sogg$id_net)
m_sett_sogg$sector<-as.character(m_sett_sogg$sector)

#export the table in csv 

write.table(m_sett_sogg, "x3_dummy_sogg.csv",
            sep = ";", # punto e virgola
            row.names = FALSE, # se abbiamo la variabile ID
            dec = ",", # separatore di decimali
            na = " ", # dati mancanti come celle vuote
            quote = TRUE
)




