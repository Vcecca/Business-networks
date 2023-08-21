#compute entropy with log10 for the entropy

#try to compute entropy only for the first network
shannon10<-read.csv("x2_ateco.csv", sep = ";")
View(shannon10)

shannon10$ateco_code<-as.factor(shannon10$ateco_code)

H_10_net1<-shannon10$ateco_code[1:7]

##isTRUE(H_data_rete1[H_data_rete1[2]==H_data_rete1[4]])

print(H_10_net1[2])
print(H_10_net1[4])

H_10_net1[2]==H_10_net1[4]

str(H_10_net1)

H_10_net1<-as.character(H_10_net1)
freq10_x <- sort(table(unlist(strsplit(H_10_net1, "  "))),      # Create frequency table
               decreasing = TRUE)
freq10_x

max(freq10_x)
str(freq10_x)

class(freq10_x)
freq10_x<-as.matrix(freq10_x)
freq10_x
freq10_x<-t(freq10_x)
freq10_x
class(freq10_x)
sum(freq10_x)
##the data are in a table now i can compute the shannon entropy DescTools

library("DescTools")

Entropy(freq10_x, base = 10) #shannon entropy 

#ateco code's frequency distribution 
ateco10<-shannon10$ateco_code
ateco10

ateco10<-as.character(ateco10)
freq10_x1 <- sort(table(unlist(strsplit(ateco10, "  "))),      # Create frequency table
                decreasing = TRUE)
freq10_x1
freq10_x1<-as.matrix(freq10_x1)
freq10_x1
View(freq10_x1)
sum(freq10_x1)
#now extend all the algorithm for all the dataset

reti10_id <- list()
H_10_rete <- list()

#H_data

for (i in 1:9595) {
  
  rete10<-shannon10[shannon10$progr.==i,]
  rete10
  
  shannon10$ateco_code<-as.character(shannon10$ateco_code)
  
  freq10_x1<-sort(table(unlist(strsplit(rete10$ateco_code, "  "))))
  
  freq10_x1
  freq10_x1<-as.matrix(freq10_x1)
  freq10_x1
  if (sum(freq10_x1)>0)
  {
    H10<-Entropy(freq10_x1, base = 10)
    H10
    reti10_id<-append(reti10_id, i)
    H_10_rete<-append(H_10_rete, H10)
    reti10_id
    H_10_rete
    print(i) 
    print(H10)
  }
}

#now convert the list in a dataframe and export in a file csv

reti10_id <-as.vector(reti10_id)
H_10_rete<- as.vector(H_10_rete)
length(H_10_rete)

matrice_10i<-matrix(unlist(reti10_id), nrow=7140, ncol=1, byrow = T)
matrice_10H<-matrix(unlist(H_10_rete), nrow =7140, ncol=1, byrow = T)

H_10_matrix<-cbind(matrice_10i, matrice_10H)
is.matrix(H_data_matrix)

str(H_10_matrix)
View(H_10_matrix)
colnames(H_10_matrix)<-c("identificativo_rete", "H_data")

head(H_10_matrix)

as.data.frame(H_10_matrix)

write.table(H_10_matrix, "x2_10H.csv",
            sep = ";", # punto e virgola
            row.names = FALSE, # se abbiamo la variabile ID
            dec = ",", # separatore di decimali
            na = " ", # dati mancanti come celle vuote
            quote = TRUE
)
