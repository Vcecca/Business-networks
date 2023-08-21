#define the region variables 

#try to identify the regiorn for 1st network
#i decide to assign to the network the region with max value
#check the correctness for each value 

net03.2023_1<-net03.2023[1:7,]
net03.2023_1

net03.2023_1$REG

str(net03.2023_1$REG)
net03.2023_1$REG<-as.vector(net03.2023_1$REG)

prova<-as.data.frame(table(net03.2020_1$REG))

str(prova)
prova$Var1<-as.vector(prova$Var1)


prova
str(prova)
max(prova$Freq)
reg<-print(prova$Var1[prova$Freq==max(prova$Freq)]) 

#try to repeat all the algorithm to all dataset
library(stringr)

lista_id_reg<-list()
lista_reg<-list()

for(i in 1:9595) {
  gruppo<-net03.2023[net03.2023$progr.==i,]
  gruppo$REG<- as.vector(gruppo$REG)
  gruppo
  reg_data<-as.data.frame(table(gruppo$REG))
  reg_data  
  reg_data$Var1<-as.vector(reg_data$Var1)

  regions<-print(reg_data$Var1[reg_data$Freq==max(reg_data$Freq)])
  
  if  (length(regions)>1){regions<-str_c(regions, collapse = "--")} 

  lista_id_reg<-append(lista_id_reg, i)
  lista_id_reg
  lista_reg<-append(lista_reg, regions)
  lista_reg

}
lista_id_reg
lista_reg
lista_id_reg<-as.vector(lista_id_reg)
lista_reg<-as.vector(lista_reg)
length(lista_reg)
length(lista_id_reg)

#matr_id<-matrix(unlist(lista_id_reg), nrow=9595, ncol=1, byrow = T) #no prendi i valori del dataset
matr_reg<-matrix(unlist(lista_reg), nrow=7140, ncol=1, byrow = T)

#View(matr_id)

id_region<-unique(net03.2023$progr.)

id_region<-as.matrix(id_region)
length(id_region)

matr_reg_code<-cbind(id_region, matr_reg)
tail(matr_reg_code)

colnames(matr_reg_code)<-c("id_net", "reg_code")

head(matr_reg_code)
tail(matr_reg_code)

matr_reg_code<-as.data.frame(matr_reg_code)

write.table(matr_reg_code, "x4.csv",
            sep = ";", # punto e virgola
            row.names = FALSE, # se abbiamo la variabile ID
            dec = ",", # separatore di decimali
            na = " ", # dati mancanti come celle vuote
            quote = TRUE
)

#read dataset

x4<-read.csv("x4.csv", sep = ";", header = T)
View(x4)

#for the classification and the construction dummy variables check 
#the table of master thesis

str(x4)

x4$reg_code<-as.numeric(x4$reg_code)

#insert in NA values 21 variables indicating the multi-region variable

x4$reg_code[is.na(x4$reg_code)]<-21

#insert and construct the dummy variable 

x4$d_piem<-NA
x4$d_aosta<-NA
x4$d_lomb<-NA
x4$d_trent<-NA
x4$d_veneto<-NA
x4$d_friuli<-NA
x4$d_liguria<-NA
x4$d_emilia<-NA
x4$d_toscana<-NA
x4$d_umbria<-NA
x4$d_marche<-NA
x4$d_lazio<-NA
x4$d_abruzzo<-NA
x4$d_molise<-NA
x4$d_camp<-NA
x4$d_puglia<-NA
x4$d_basil<-NA
x4$d_calab<-NA
x4$d_sic<-NA
x4$d_sard<-NA
x4$d_multi_reg<-NA
####################### 

head(x4)
str(x4)
#piemonte - 01 

for (i in 1:7140) 
  {
if (isTRUE(x4$reg_code[i]==1))
  {x4$d_piem[i]<-1} else {x4$d_piem[i]<-0}
}
#valle d'aosta - 02
for (i in 1:7140) 
{
  if (isTRUE(x4$reg_code[i]==2))
  {x4$d_aosta[i]<-1} else {x4$d_aosta[i]<-0}
}
#lombardia - 03
for (i in 1:7140) 
{
  if (isTRUE(x4$reg_code[i]==3))
  {x4$d_lomb[i]<-1} else {x4$d_lomb[i]<-0}
}
#trentino - 04
for (i in 1:7140) 
{
  if (isTRUE(x4$reg_code[i]==4))
  {x4$d_trent[i]<-1} else {x4$d_trent[i]<-0}
}
#veneto - 05
for (i in 1:7140) 
{
  if (isTRUE(x4$reg_code[i]==5))
  {x4$d_veneto[i]<-1} else {x4$d_veneto[i]<-0}
}
#friuli venezia giulia - 06
for (i in 1:7140) 
{
  if (isTRUE(x4$reg_code[i]==6))
  {x4$d_friuli[i]<-1} else {x4$d_friuli[i]<-0}
}
#liguria -07
for (i in 1:7140) 
{
  if (isTRUE(x4$reg_code[i]==7))
  {x4$d_liguria[i]<-1} else {x4$d_liguria[i]<-0}
}
#emilia romagna -08
for (i in 1:7140) 
{
  if (isTRUE(x4$reg_code[i]==8))
  {x4$d_emilia[i]<-1} else {x4$d_emilia[i]<-0}
}
#toscana - 09
for (i in 1:7140) 
{
  if (isTRUE(x4$reg_code[i]==9))
  {x4$d_toscana[i]<-1} else {x4$d_toscana[i]<-0}
}
#umbria - 10
for (i in 1:7140) 
{
  if (isTRUE(x4$reg_code[i]==10))
  {x4$d_umbria[i]<-1} else {x4$d_umbria[i]<-0}
}
#marche -11 
for (i in 1:7140) 
{
  if (isTRUE(x4$reg_code[i]==11))
  {x4$d_marche[i]<-1} else {x4$d_marche[i]<-0}
}
#lazio - 12
for (i in 1:7140) 
{
  if (isTRUE(x4$reg_code[i]==12))
  {x4$d_lazio[i]<-1} else {x4$d_lazio[i]<-0}
}
#abruzzo - 13
for (i in 1:7140) 
{
  if (isTRUE(x4$reg_code[i]==13))
  {x4$d_abruzzo[i]<-1} else {x4$d_abruzzo[i]<-0}
}
#molise - 14 
for (i in 1:7140) 
{
  if (isTRUE(x4$reg_code[i]==14))
  {x4$d_molise[i]<-1} else {x4$d_molise[i]<-0}
}
#campania -15
for (i in 1:7140) 
{
  if (isTRUE(x4$reg_code[i]==15))
  {x4$d_camp[i]<-1} else {x4$d_camp[i]<-0}
}
#puglia -16
for (i in 1:7140) 
{
  if (isTRUE(x4$reg_code[i]==16))
  {x4$d_puglia[i]<-1} else {x4$d_puglia[i]<-0}
}
#basilicata - 17
for (i in 1:7140) 
{
  if (isTRUE(x4$reg_code[i]==17))
  {x4$d_basil[i]<-1} else {x4$d_basil[i]<-0}
}
#calabria - 18
for (i in 1:7140) 
{
  if (isTRUE(x4$reg_code[i]==18))
  {x4$d_calab[i]<-1} else {x4$d_calab[i]<-0}
}
#sicilia -19 
for (i in 1:7140) 
{
  if (isTRUE(x4$reg_code[i]==19))
  {x4$d_sic[i]<-1} else {x4$d_sic[i]<-0}
}
#sardegna - 20
for (i in 1:7140) 
{
  if (isTRUE(x4$reg_code[i]==20))
  {x4$d_sard[i]<-1} else {x4$d_sard[i]<-0}
}
#multi-region 21
for (i in 1:7140) 
{
  if (isTRUE(x4$reg_code[i]==21))
  {x4$d_multi_reg[i]<-1} else {x4$d_multi_reg[i]<-0}
}

##### following the (Guarini, 2022) and my thesis and according the NUT1 level 
#classification by eurostat
#adding empty coloumn
x4$nw<-NA
x4$ne<-NA
x4$cn<-NA
x4$south<-NA
x4$ins<-NA
#north-west 01
for (i in 1:7140)
{
 if 
 (sum(x4$d_piem[i], x4$d_liguria[i], x4$d_lomb[i], x4$d_aosta[i])==1)
  {x4$nw[i]<-1} else {x4$nw[i]<-0}
}
#noth-east 
for (i in 1:7140)
{
  if 
  (sum(x4$d_emilia[i], x4$d_friuli[i], x4$d_trent[i], x4$d_veneto[i])==1)
  {x4$ne[i]<-1} else {x4$ne[i]<-0}
}
#centre
for (i in 1:7140)
{
  if 
  (sum(x4$d_toscana[i], x4$d_umbria[i], x4$d_lazio[i], x4$d_marche[i])==1)
  {x4$cn[i]<-1} else {x4$cn[i]<-0}
}
#south
for (i in 1:7140)
{
  if 
  (sum(x4$d_abruzzo[i], x4$d_basil[i], x4$d_calab[i], x4$d_camp[i],
       x4$d_molise[i], x4$d_puglia[i])==1)
  {x4$south[i]<-1} else {x4$south[i]<-0}
}
#islands 
for (i in 1:7140)
{
  if 
  (sum(x4$d_sic[i], x4$d_sard[i])==1)
  {x4$ins[i]<-1} else {x4$ins[i]<-0}
}

#if multi-reg is gthe control variable for multireg variable too
#export the x4 dataset in csv file for back up 

write.table(x4, "x4_dummy.csv",
            sep = ";", # punto e virgola
            row.names = FALSE, # se abbiamo la variabile ID
            dec = ",", # separatore di decimali
            na = " ", # dati mancanti come celle vuote
            quote = TRUE
)



