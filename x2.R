# compute Shannon entropy for each network                  


#read the dataset (file statistiche.xls)

H_data<-read_xls("statistiche.xls", sheet = "Elenco")
View(H_data)

#rename colnames

colnames(H_data)[18]<- "ateco_code"

#divide the ateco code from the rest of description

H_data$ateco_code<-strsplit(H_data$ateco_code, split = " ")

install.packages("splitstackshape")
library(splitstackshape)
att2<-cSplit(H_data, "ateco_code", "wide",sep =",", type.convert = F)
att2
View(att2)

#delete unuseful row in the dataset

att2$ateco_code_14<-NULL
att2$ateco_code_13<-NULL
att2$ateco_code_12<-NULL
att2$ateco_code_11<-NULL
att2$ateco_code_10<-NULL
att2$ateco_code_09<-NULL
att2$ateco_code_08<-NULL
att2$ateco_code_07<-NULL
att2$ateco_code_06<-NULL
att2$ateco_code_05<-NULL
att2$ateco_code_04<-NULL
att2$ateco_code_03<-NULL

head(att2)
str(att2)

#remove c( from att.01

class(att2$ateco_code_01)
print(att2$ateco_code_01)

class(att2$ateco_code_02)
print(att2$ateco_code_02)

att2$att_gen<-substr(att2$ateco_code_01, 4,4 )
att2$att_spec<-substr(att2$ateco_code_02, 2,3)
head(att2)

#delete unuseful coloumns
att2$ateco_code_01<-NULL
att2$ateco_code_02<-NULL
head(att2)

#unit att_gen and att_spec in one coloumn

att2$ateco_code<-paste(att2$att_gen, att2$att_spec)
head(att2)

#delete unuseful coloums
att2$att_gen<-NULL
att2$att_spec<-NULL
#att2$attivita.<-NULL
head(att2)
#export table in csv 

class(att2)

write.table(att2, "x2_ateco.csv",
            sep = ";", # punto e virgola
            row.names = FALSE, # se abbiamo la variabile ID
            dec = ",", # separatore di decimali
            na = "", # dati mancanti come celle vuote
            quote = TRUE
)


#################################
#x2b

#read dataset #att2 object or x2_ateco.csv file

#H_data<-read_xls("statistiche.xls", sheet = "Elenco")
head(att2)

str(att2)

#try to compute entropy only for the first network

att2$ateco_code<-as.factor(att2$ateco_code)

H_data_net1<-att2$ateco_code[1:7]

##isTRUE(H_data_rete1[H_data_rete1[2]==H_data_rete1[4]])

print(H_data_net1[2])
print(H_data_net1[4])

H_data_net1[2]==H_data_net1[4]

str(H_data_net1)

H_data_net1<-as.character(H_data_net1)
freq_x <- sort(table(unlist(strsplit(H_data_net1, "  "))),      # Create frequency table
               decreasing = TRUE)
freq_x

max(freq_x)
str(freq_x)

class(freq_x)
freq_x<-as.matrix(freq_x)
freq_x
freq_x<-t(freq_x)
freq_x
class(freq_x)
sum(freq_x)
##the data are in a table now i can compute the shannon entropy DescTools

library("DescTools")

Entropy(freq_x, base = 3) #shannon entropy 

#ateco code's frequency distribution 
ateco<-att2$ateco_code
ateco

ateco<-as.character(ateco)
freq_x1 <- sort(table(unlist(strsplit(ateco, "  "))),      # Create frequency table
                decreasing = TRUE)
freq_x1
freq_x1<-as.matrix(freq_x1)
freq_x1
View(freq_x1)
sum(freq_x1)
#now extend all the algorithm for all the dataset

reti_id <- list()
H_data_rete <- list()

#H_data

for (i in 1:9595) {
  
  rete<-att2[att2$progr.==i,]
  rete
  
  att2$ateco_code<-as.character(att2$ateco_code)
  
  freq_x1<-sort(table(unlist(strsplit(rete$ateco_code, "  "))))
  
  freq_x1
  freq_x1<-as.matrix(freq_x1)
  freq_x1
  if (sum(freq_x1)>0)
  {
    H<-Entropy(freq_x1, base = 3)
    H
    reti_id<-append(reti_id, i)
    H_data_rete<-append(H_data_rete, H)
    reti_id
    H_data_rete
    print(i) 
    print(H)
  }
}

#now convert the list in a dataframe and export in a file csv

reti_id <-as.vector(reti_id)
H_data_rete<- as.vector(H_data_rete)
#length(H_data_rete)

matrice_i<-matrix(unlist(reti_id), nrow=7140, ncol=1, byrow = T)
matrice_H<-matrix(unlist(H_data_rete), nrow =7140, ncol=1, byrow = T)

H_data_matrix<-cbind(matrice_i, matrice_H)
is.matrix(H_data_matrix)

str(H_data_matrix)
View(H_data_matrix)
colnames(H_data_matrix)<-c("identificativo_rete", "H_data")

head(H_data_matrix)

as.data.frame(H_data_matrix)

write.table(H_data_matrix, "x2_H.csv",
            sep = ";", # punto e virgola
            row.names = FALSE, # se abbiamo la variabile ID
            dec = ",", # separatore di decimali
            na = " ", # dati mancanti come celle vuote
            quote = TRUE
)


