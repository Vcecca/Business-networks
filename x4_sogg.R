#compute the region of formal BNAs 
#x4

#define the region variables 

#try to identify the regiorn for 1st network
#i decide to assign to the network the region with max value
#check the correctness for each value 

sogg03.2023_1<-sogg03.2023[1:16,]
sogg03.2023_1

sogg03.2023_1$`regione impresa`

str(sogg03.2023_1$`regione impresa`)
sogg03.2023_1$`regione impresa`<-as.vector(sogg03.2023_1$`regione impresa`)

prova_sogg<-as.data.frame(table(sogg03.2023_1$`regione impresa`))

str(prova_sogg)
prova_sogg$Var1<-as.vector(prova_sogg$Var1)

prova_sogg
#prova
str(prova_sogg)
max(prova_sogg$Freq)
reg<-print(prova_sogg$Var1[prova_sogg$Freq==max(prova_sogg$Freq)]) 

#try to repeat all the algorithm to all dataset
library(stringr)

lista_id_reg_sogg<-list()
lista_reg_sogg<-list()

for(i in 1:1242) {
  gruppo_sogg<-sogg03.2023[sogg03.2023$progr.==i,]
  gruppo_sogg$`regione impresa`<- as.vector(gruppo_sogg$`regione impresa`)
  gruppo_sogg
  reg_data_sogg<-as.data.frame(table(gruppo_sogg$`regione impresa`))
  reg_data_sogg  
  reg_data_sogg$Var1<-as.vector(reg_data_sogg$Var1)
  
  regions_sogg<-print(reg_data_sogg$Var1[reg_data_sogg$Freq==max(reg_data_sogg$Freq)])
#  
  if  (length(regions_sogg)>1)
    {regions_sogg<-str_c(regions_sogg, collapse = "--")} 
  
  lista_id_reg_sogg<-append(lista_id_reg_sogg, i)
  lista_id_reg_sogg
  lista_reg_sogg<-append(lista_reg_sogg, regions_sogg)
  lista_reg_sogg
  
}
lista_id_reg_sogg
lista_reg_sogg
lista_id_reg_sogg<-as.vector(lista_id_reg_sogg)
lista_reg_sogg<-as.vector(lista_reg_sogg)
length(lista_reg_sogg)
length(lista_id_reg_sogg)

matr_id_sogg<-matrix(unlist(lista_id_reg_sogg), nrow=1242, ncol=1, byrow = T) #no prendi i valori del dataset
matr_reg_sogg<-matrix(unlist(lista_reg_sogg), nrow=1218, ncol=1, byrow = T)


id_region_sogg<-unique(sogg03.2023$progr.)
View(id_region_sogg)
id_region_sogg<-as.matrix(id_region_sogg)
id_region_sogg<-id_region_sogg[1:1242]
head(id_region_sogg)
#use m_i_sogg varialble already used for computation and do all the variables 

matr_reg_code_sogg<-cbind(m_i_sogg, matr_reg_sogg)
tail(matr_reg_code_sogg)
View(matr_reg_code_sogg)
colnames(matr_reg_code_sogg)<-c("id_net_sogg", "reg_code")

head(matr_reg_code_sogg)
tail(matr_reg_code_sogg)

matr_reg_code_sogg<-as.data.frame(matr_reg_code_sogg)

write.table(matr_reg_code_sogg, "x4_sogg.csv",
            sep = ";", # punto e virgola
            row.names = FALSE, # se abbiamo la variabile ID
            dec = ",", # separatore di decimali
            na = " ", # dati mancanti come celle vuote
            quote = TRUE
)

#read dataset

x4_sogg<-read.csv("x4_sogg.csv", sep = ";", header = T)
View(x4_sogg)

#for the classification and the construction dummy variables check 
#the table of master thesis

str(x4_sogg)

x4_sogg$reg_code<-as.numeric(x4_sogg$reg_code)

##insert in NA values 21 variables indicating the multi-region variable

x4_sogg$reg_code[is.na(x4_sogg$reg_code)]<-21

#insert and construct the dummy variable 

x4_sogg$d_piem<-NA
x4_sogg$d_aosta<-NA
x4_sogg$d_lomb<-NA
x4_sogg$d_trent<-NA
x4_sogg$d_veneto<-NA
x4_sogg$d_friuli<-NA
x4_sogg$d_liguria<-NA
x4_sogg$d_emilia<-NA
x4_sogg$d_toscana<-NA
x4_sogg$d_umbria<-NA
x4_sogg$d_marche<-NA
x4_sogg$d_lazio<-NA
x4_sogg$d_abruzzo<-NA
x4_sogg$d_molise<-NA
x4_sogg$d_camp<-NA
x4_sogg$d_puglia<-NA
x4_sogg$d_basil<-NA
x4_sogg$d_calab<-NA
x4_sogg$d_sic<-NA
x4_sogg$d_sard<-NA
x4_sogg$d_multi_reg<-NA
####################### 

head(x4_sogg)
str(x4_sogg)
#piemonte - 01 

for (i in 1:1242) 
{
  if (isTRUE(x4_sogg$reg_code[i]==1))
  {x4_sogg$d_piem[i]<-1} else {x4_sogg$d_piem[i]<-0}
}
#valle d'aosta - 02
for (i in 1:1242) 
{
  if (isTRUE(x4_sogg$reg_code[i]==2))
  {x4_sogg$d_aosta[i]<-1} else {x4_sogg$d_aosta[i]<-0}
}
#lombardia - 03
for (i in 1:1242) 
{
  if (isTRUE(x4_sogg$reg_code[i]==3))
  {x4_sogg$d_lomb[i]<-1} else {x4_sogg$d_lomb[i]<-0}
}
#trentino - 04
for (i in 1:1242) 
{
  if (isTRUE(x4_sogg$reg_code[i]==4))
  {x4_sogg$d_trent[i]<-1} else {x4_sogg$d_trent[i]<-0}
}
#veneto - 05
for (i in 1:1242) 
{
  if (isTRUE(x4_sogg$reg_code[i]==5))
  {x4_sogg$d_veneto[i]<-1} else {x4_sogg$d_veneto[i]<-0}
}
#friuli venezia giulia - 06
for (i in 1:1242) 
{
  if (isTRUE(x4_sogg$reg_code[i]==6))
  {x4_sogg$d_friuli[i]<-1} else {x4_sogg$d_friuli[i]<-0}
}
#liguria -07
for (i in 1:1242) 
{
  if (isTRUE(x4_sogg$reg_code[i]==7))
  {x4_sogg$d_liguria[i]<-1} else {x4_sogg$d_liguria[i]<-0}
}
#emilia romagna -08
for (i in 1:1242) 
{
  if (isTRUE(x4_sogg$reg_code[i]==8))
  {x4_sogg$d_emilia[i]<-1} else {x4_sogg$d_emilia[i]<-0}
}
#toscana - 09
for (i in 1:1242) 
{
  if (isTRUE(x4_sogg$reg_code[i]==9))
  {x4_sogg$d_toscana[i]<-1} else {x4_sogg$d_toscana[i]<-0}
}
#umbria - 10
for (i in 1:1242) 
{
  if (isTRUE(x4_sogg$reg_code[i]==10))
  {x4_sogg$d_umbria[i]<-1} else {x4_sogg$d_umbria[i]<-0}
}
#marche -11 
for (i in 1:1242) 
{
  if (isTRUE(x4_sogg$reg_code[i]==11))
  {x4_sogg$d_marche[i]<-1} else {x4_sogg$d_marche[i]<-0}
}
#lazio - 12
for (i in 1:1242) 
{
  if (isTRUE(x4_sogg$reg_code[i]==12))
  {x4_sogg$d_lazio[i]<-1} else {x4_sogg$d_lazio[i]<-0}
}
#abruzzo - 13
for (i in 1:1242) 
{
  if (isTRUE(x4_sogg$reg_code[i]==13))
  {x4_sogg$d_abruzzo[i]<-1} else {x4_sogg$d_abruzzo[i]<-0}
}
#molise - 14 
for (i in 1:1242) 
{
  if (isTRUE(x4_sogg$reg_code[i]==14))
  {x4_sogg$d_molise[i]<-1} else {x4_sogg$d_molise[i]<-0}
}
#campania -15
for (i in 1:1242) 
{
  if (isTRUE(x4_sogg$reg_code[i]==15))
  {x4_sogg$d_camp[i]<-1} else {x4_sogg$d_camp[i]<-0}
}
#puglia -16
for (i in 1:1242) 
{
  if (isTRUE(x4_sogg$reg_code[i]==16))
  {x4_sogg$d_puglia[i]<-1} else {x4_sogg$d_puglia[i]<-0}
}
#basilicata - 17
for (i in 1:1242) 
{
  if (isTRUE(x4_sogg$reg_code[i]==17))
  {x4_sogg$d_basil[i]<-1} else {x4_sogg$d_basil[i]<-0}
}
#calabria - 18
for (i in 1:1242) 
{
  if (isTRUE(x4_sogg$reg_code[i]==18))
  {x4_sogg$d_calab[i]<-1} else {x4_sogg$d_calab[i]<-0}
}
#sicilia -19 
for (i in 1:1242) 
{
  if (isTRUE(x4_sogg$reg_code[i]==19))
  {x4_sogg$d_sic[i]<-1} else {x4_sogg$d_sic[i]<-0}
}
#sardegna - 20
for (i in 1:1242) 
{
  if (isTRUE(x4_sogg$reg_code[i]==20))
  {x4_sogg$d_sard[i]<-1} else {x4_sogg$d_sard[i]<-0}
}
#multi-region 21
for (i in 1:1242) 
{
  if (isTRUE(x4_sogg$reg_code[i]==21))
  {x4_sogg$d_multi_reg[i]<-1} else {x4_sogg$d_multi_reg[i]<-0}
}

##### following the (Guarini, 2022) and my thesis and according the NUT1 level 
#classification by eurostat
#adding empty coloumn
x4_sogg$nw<-NA
x4_sogg$ne<-NA
x4_sogg$cn<-NA
x4_sogg$south<-NA
x4_sogg$ins<-NA
##north-west 01
for (i in 1:1242)
{
  if 
  (sum(x4_sogg$d_piem[i], x4_sogg$d_liguria[i], x4_sogg$d_lomb[i], x4_sogg$d_aosta[i])==1)
  {x4_sogg$nw[i]<-1} else {x4_sogg$nw[i]<-0}
}
#noth-east 
for (i in 1:1242)
{
  if 
  (sum(x4_sogg$d_emilia[i], x4_sogg$d_friuli[i], x4_sogg$d_trent[i], x4_sogg$d_veneto[i])==1)
  {x4_sogg$ne[i]<-1} else {x4_sogg$ne[i]<-0}
}
#centre
for (i in 1:1242)
{
  if 
  (sum(x4_sogg$d_toscana[i], x4_sogg$d_umbria[i], x4_sogg$d_lazio[i], x4_sogg$d_marche[i])==1)
  {x4_sogg$cn[i]<-1} else {x4_sogg$cn[i]<-0}
}
#south
for (i in 1:1242)
{
  if 
  (sum(x4_sogg$d_abruzzo[i], x4_sogg$d_basil[i], x4_sogg$d_calab[i], x4_sogg$d_camp[i],
       x4_sogg$d_molise[i], x4_sogg$d_puglia[i])==1)
  {x4_sogg$south[i]<-1} else {x4_sogg$south[i]<-0}
}
#islands 
for (i in 1:1242)
{
  if 
  (sum(x4_sogg$d_sic[i], x4_sogg$d_sard[i])==1)
  {x4_sogg$ins[i]<-1} else {x4_sogg$ins[i]<-0}
}

#if multi-reg is gthe control variable for multireg variable too
#export the x4 dataset in csv file for back up 

write.table(x4_sogg, "x4_dummy_sogg.csv",
            sep = ";", # punto e virgola
            row.names = FALSE, # se abbiamo la variabile ID
            dec = ",", # separatore di decimali
            na = " ", # dati mancanti come celle vuote
            quote = TRUE
)



