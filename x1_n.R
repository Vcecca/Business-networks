#accademic publication 
#reproduce thesis projects with updates dataset
#source: https://contrattidirete.registroimprese.it/reti/ 
#last available update 03.2023

#set directoory - run  only first time
#setwd("C:/Users/hp/Desktop/progetto_paper")

#read the dataset
net03.2023<- read_xls("statistiche.xls", sheet = "Elenco")

#clean data and delete form dataset unseful variables as: numero del repertorio,
# il numero dell'atto,  il n area, il codice fiscale e NG
net03.2023
head(net03.2023)
net03.2023$`numero repertorio`<-NULL
net03.2023$`numero atto` <- NULL
net03.2023$n.rea <- NULL
net03.2023$NG<- NULL
net03.2023$`impresa di riferimento`<-NULL
net03.2023$`codice ATECO 2007`<-NULL

str(net03.2023)

# set numeric format in network program 
is.numeric(net03.2023$progr.)
net03.2023$progr. <- as.numeric(as.character(net03.2023$progr.))
mode(net03.2023$progr.)

mode(net03.2023$`data atto`)
net03.2023$`data atto` <- as.Date(as.character(net03.2023$`data atto`), "%d/%m/%Y")
mode(net03.2023$`data atto`) #the date is expressed in the Italian format

#Compute number of a company for each business network

id_n_impr<-table(net03.2023$progr.)
print(id_n_impr) #6362 element

#length(n_imprese)
length(id_n_impr) #7140
#number of company per network
x1_regr<- data.frame(id_n_impr)
x1_regr

# create a dataset and export in a csv !!!Note that i use the italian format
write.table(id_n_impr, "x1.csv",
            sep = ";", # punto e virgola
            row.names = F, # if you have a ID variable
            dec = ",", # separator of decimalals
            na = "", # missing data as empty cells
            quote = TRUE, 
            col.names = c("id", "n") #give colnames
)

x1<- read.csv("x1.csv",header = T, sep = ";")
View(x1)
str(x1)

