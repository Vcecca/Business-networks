#compute the data for formal business networks Reti-soggetto

sogg03.2023<- read_xls("statistiche.xls", sheet = "Sogg. Giu.")

#format the data

sogg03.2023$`N.Rea Contratto`<-NULL
sogg03.2023$NG<-NULL
sogg03.2023$`N.REA impresa`<-NULL
sogg03.2023$`numero repertorio contratto`<-NULL
sogg03.2023$`NG  impresa`<-NULL

# set numeric format in network program 
is.numeric(sogg03.2023$progr.)
sogg03.2023$progr. <- as.numeric(as.character(sogg03.2023$progr.))
mode(sogg03.2023$progr.)

mode(sogg03.2023$data)
sogg03.2023$data <- as.Date(as.character(sogg03.2023$data), "%d/%m/%Y")
mode(sogg03.2023$data) #the date is expressed in the Italian format

#Compute number of a company for each business network

sogg_n_impr<-table(sogg03.2023$progr.)
print(sogg_n_impr) #1242 elements

#length(n_imprese)
length(sogg_n_impr) #1242
#number of company per soggwork
x1_sogg<- data.frame(sogg_n_impr)
x1_sogg

# create a dataset and export in a csv !!!Note that i use the italian format
write.table(sogg_n_impr, "x1_sogg.csv",
            sep = ";", # punto e virgola
            row.names = F, # if you have a ID variable
            dec = ",", # separator of decimalals
            na = "", # missing data as empty cells
            quote = TRUE, 
            col.names = c("sogg_id", "n") #give colnames
)

x1_n_sogg<- read.csv("x1_sogg.csv",header = T, sep = ";")
View(x1_n_sogg)
str(x1_n_sogg) #1242 rows
tail(x1_n_sogg)
