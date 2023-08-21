#accademic publication 
#reproduce thesis projects with updates dataset
#find the coordinates 

c_ref<-read_xlsx("italy_geo.xlsx", sheet = "italy_geo") 
tail(c_ref)
c_ref <- c_ref[1:7978,] #delete the coloumn of total #7996
c_ref$istat<-as.character(c_ref$istat)

#modify also the accent and have same charachter name 
c_ref$comune<-toupper(c_ref$comune) 
View(c_ref)
str(c_ref)
#modifiche fatte al c_ref quindi adattate al dataset delle reti

c_ref$comune<-gsub("È", "E'", c_ref$comune) #Alt+0200
c_ref$comune<-gsub("É", "E'", c_ref$comune) #Alt+0201
c_ref$comune<-gsub("Ì", "I'", c_ref$comune) #Alt+0204
c_ref$comune<-gsub("À", "A'", c_ref$comune)#Alt + 0192
c_ref$comune<-gsub("Ò", "O'", c_ref$comune)#Alt + 0210
c_ref$comune<-gsub("Ù", "U'", c_ref$comune)#Alt + 0217
c_ref$comune<-gsub("Ú", "U'", c_ref$comune)#Alt + 0218
c_ref$comune<-gsub("Â", "A", c_ref$comune)##Alt + 0194
c_ref$comune<-gsub("SAINT-", "SAINT ", c_ref$comune)##Alt + 0194
#Modify names of cities 
c_ref$comune<-gsub("REGGIO NELL'EMILIA", "REGGIO EMILIA", c_ref$comune)
c_ref$comune<-gsub("SCARPERIA", "SCARPERIA E SAN PIERO", c_ref$comune)
c_ref$comune<-gsub("REGGIO CALABRIA", "REGGIO DI CALABRIA", c_ref$comune)
#c_ref$comune<-gsub("REGGIO DI CALABRIA", "REGGIO CALABRIA", c_ref$comune)
c_ref$comune<-gsub("ROSSANO", "CORIGLIANO-ROSSANO", c_ref$comune)
c_ref$comune<-gsub("FIUMICELLO", "FIUMICELLO VILLA VICENTINA", c_ref$comune)
c_ref$comune<-gsub("LEINI", "LEINI'", c_ref$comune) #"LEINI'"
c_ref$comune<-gsub("PRIMIERO SAN MARTINO DI CASTROZZA",
                   "PRIMIERO SAN MARTINO DI CASTRO", c_ref$comune)
c_ref$comune<-gsub("JOVENÇAN", "JOVENCAN", c_ref$comune)
c_ref$comune<-gsub("CASSANO ALL'IONIO", "CASSANO ALLO IONIO", c_ref$comune)
c_ref$comune<-gsub("CAVALLINO-TREPORTI", "CAVALLINO TREPORTI", c_ref$comune)
c_ref$comune<-gsub("RODENGO SAIANO", "RODENGO-SAIANO", c_ref$comune)
c_ref$comune<-gsub("SAN DORLIGO DELLA VALLE-DOLINA",
                   "SAN DORLIGO DELLA VALLE DOLINA", c_ref$comune)
c_ref$comune<-gsub("GATTICO", "GATTICO-VERUNO", c_ref$comune) 
c_ref$comune<-gsub("CASTROCARO TERME E TERRA DEL SOLE", 
                   "CASTROCARO TERME E TERRA DEL S", c_ref$comune) 
#CASTROCARO TERME E TERRA DEL SOLE
c_ref$comune<-gsub("BELVI'", "BELVI", c_ref$comune) 
c_ref$comune<-gsub("SGONICO", "SGONICO-ZGONIK", c_ref$comune) 
c_ref$comune<-gsub("DUINO-AURISINA", "DUINO AURISINA", c_ref$comune) 
c_ref$comune<-gsub("CORTACCIA SULLA STRADA DEL VINO", 
                   "CORTACCIA SULLA STRADA DEL VIN", c_ref$comune)
#CORTACCIA SULLA STRADA DEL VINO
c_ref$comune<-gsub("GALTELLI'", "GALTELLI", c_ref$comune)
c_ref$comune<-gsub("CALATAFIMI-SEGESTA", "CALATAFIMI SEGESTA", c_ref$comune)
#SAINT-RHE'MY-EN-BOSSES
c_ref$comune<-gsub("SAINT RHE'MY-EN-BOSSES", "SAINT RHEMY EN BOSSES",
                   c_ref$comune)
c_ref$comune<-gsub("COSIO D'ARROSCIA", "COSIO DI ARROSCIA", c_ref$comune)
c_ref$comune<-gsub("PONT-SAINT MARTIN", "PONT SAINT MARTIN", c_ref$comune)
#add a coloum with same name written differently
#	RASUN-ANTERSELVA
c_ref$comune<-gsub("RASUN-ANTERSELVA", "RASUN ANTERSELVA", c_ref$comune)
c_ref$comune<-gsub("LATERINA", "LATERINA PERGINE VALDARNO", c_ref$comune)
#GRESSONEY-SAINT-JEAN
c_ref$comune<-gsub("GRESSONEY-SAINT JEAN", "GRESSONEY SAINT JEAN", c_ref$comune)
#SAINT-VINCENT
c_ref$comune<-gsub("SAINT-VINCENT", "SAINT VINCENT", c_ref$comune)
c_ref$comune<-gsub("CAMPIGLIONE FENILE", "CAMPIGLIONE-FENILE", c_ref$comune)
c_ref$comune<-gsub("ONANI'", "ONANI", c_ref$comune)
c_ref$comune<-gsub("HÔNE", "HONE", c_ref$comune)
#MACCAGNO CON PINO E VEDDASCA
c_ref$comune<-gsub("MACCAGNO CON PINO E VEDDASCA", "MACCAGNO", c_ref$comune)
#SENALE-SAN FELICE
c_ref$comune<-gsub("SENALE-SAN FELICE", "SENALE SAN FELICE", c_ref$comune)
c_ref$comune<-gsub("CERESOLE ALBA", "CERESOLE D'ALBA", c_ref$comune)
c_ref$comune<-gsub("VILLA SANTA LUCIA DEGLI ABRUZZI", 
                   "VILLA SANTA LUCIA DEGLI ABRUZZ", c_ref$comune)
c_ref$comune<-gsub("GRESSONEY-LA-TRINITE'", 
                   "GRESSONEY LA TRINITE'", c_ref$comune)

str(c_ref)
library(tidyverse)

c_ref <- c_ref %>% 
  add_row(istat = "78108", comune = "CORIGLIANO-ROSSANO", 
          lng = "16.63286791", lat = "39.57523072")
c_ref <- c_ref %>% 
  add_row(istat = "80063", comune = "REGGIO CALABRIA", 
          lng = "15.64345359", lat = "38.10922766")
c_ref <- c_ref %>% 
  add_row(istat = "24059", comune = "COLCERESA", 
          #FUSIONE NEL 2019 DI  Mason Vicentino e Molvena
          lng = "11.61222518", lat = "45.73374116")
c_ref<- c_ref %>% 
  add_row(istat = "98007", comune = "CASTELGERUNDO", 
          #FUSIONE NEL 2016 DI Camairago e Cavacurta (LODI)
          lng = "9.72690145", lat = "45.20543687")
c_ref<- c_ref %>% 
  add_row(istat = "24011", comune = "BARBARANO MOSSANO", 
          #FUSIONE NEL 2018 DI  Barbarano Vicentino e Mossano
          lng = "11.53957638", lat = "45.41001941")
c_ref<- c_ref %>% 
  add_row(istat = "20040", comune = "BORGO MANTOVANO", #pieve di coriano
          ## Pieve di Coriano, Revere e Villa Poma
          lng = "11.10811985", lat = "45.03293697")
#
c_ref<- c_ref %>% 
  add_row(istat = "48053", comune = "SCARPERIA", 
          lng = "11.3560", lat = "43.9960")
c_ref<- c_ref %>% 
  add_row(istat = "48053", comune = "SCARPERIA E SAN PIERO", 
          lng = "11.3560", lat = "43.9960")
#
c_ref<- c_ref %>% 
  add_row(istat = "81003", comune = "CALATAFIMI SEGESTA", 
          lng = "12.86271362", lat = "37.91416321")
c_ref<- c_ref %>% 
  add_row(istat = "38024", comune = "TRESIGNANA", 
          lng = "11.89681521", lat = "44.81778085")
c_ref <- c_ref %>% 
  add_row(istat = "24088", comune = "ROSSANO VENETO", 
          lng = "11.79989962", lat = "45.70506862")
#48052
c_ref <- c_ref %>% 
  add_row(istat = "48052", comune = "FIGLINE VALDARNO", 
          lng = "11.4449", lat = "43.6291")
c_ref <- c_ref %>% 
  add_row(istat = "48052", comune = "INCISA VALDARNO", 
          lng = "11.4449", lat = "43.6291")
c_ref <- c_ref %>% 
  add_row(istat = "27044", comune = "CAVALLINO-TREPORTI", 
          lng = "12.45371806", lat = "45.45347560")
c_ref <- c_ref %>% 
  add_row(istat = "23052", comune = "NEGRAR DI VALPOLICELLA", 
          lng = "10.93782510", lat = "45.53065538")
c_ref <- c_ref %>% 
  add_row(istat = "25074", comune = "BORGO VALBELLUNA", 
          lng = "12.079611", lat = "46.062")
c_ref <- c_ref %>% 
  add_row(istat = "41059", comune = "SASSOCORVARO AUDITORE", 
          lng = "12.49531808", lat = "43.78094127")
c_ref <- c_ref %>% 
  add_row(istat = "48003", comune = "BARBERINO TAVARNELLE", 
          lng = "11.17116650", lat = "43.54173193")
c_ref <- c_ref %>% 
  add_row(istat = "34050", comune = "POLESINE PARMENSE", 
          lng = "10.1307", lat = "45.0194")
c_ref <- c_ref %>% 
  add_row(istat = "33031", comune = "ALTA VAL TIDONE", 
          lng = "9.38362340", lat = "44.87549564")
c_ref <- c_ref %>% 
  add_row(istat = "19071", comune = "PIADENA DRIZZONA", 
          lng = "10.36788370", lat = "45.13115093")
c_ref <- c_ref %>% 
  add_row(istat = "26054", comune = "PIEVE DEL GRAPPA", 
          lng = "11.85247417", lat = "45.82652202")
c_ref <- c_ref %>% 
  add_row(istat = "96084", comune = "VALDILANA", 
          lng = "8.13639953", lat = "45.64643296")
c_ref <- c_ref %>% 
  add_row(istat = "20057", comune = "SAN GIORGIO BIGARELLO", 
          lng = "10.84589851", lat = "45.16526842")
c_ref <- c_ref %>% 
  add_row(istat = "49015", comune = "RIO", 
          lng = "10.42518064", lat = "42.81458647")
c_ref <- c_ref %>% 
  add_row(istat = "24054", comune = "LUSIANA CONCO", 
          lng = "11.57676483", lat = "45.78627131")
c_ref <- c_ref %>% 
  add_row(istat = "7207", comune = "FENIS", 
          lng = "7.494371", lat = "45.736013")
c_ref <- c_ref %>% 
  add_row(istat = "34037", comune = "SORBOLO MEZZANI", 
          lng = "10.44834770", lat = "44.84647613")
c_ref <- c_ref %>% 
  add_row(istat = "96051", comune = "QUAREGNA CERRETO", 
          lng = "8.16475846", lat = "45.57635631")
c_ref <- c_ref %>% 
  add_row(istat = "12028", comune = "CADREZZATE CON OSMATE", 
          lng = "8.64290340", lat = "45.79438636")
c_ref <- c_ref %>% 
  add_row(istat = "30125", comune = "TREPPO LIGOSULLO", 
          lng = "13.04237979", lat = "46.53343245")
c_ref <- c_ref %>% 
  add_row(istat = "28107", comune = "BORGO VENETO", 
          lng = "11.538563", lat = "45.229109")
c_ref <- c_ref %>% 
  add_row(istat = "22152", comune = "NOVELLA", 
          lng = "11.05782178", lat = "46.39103316")
c_ref <- c_ref %>% 
  add_row(istat = "13253", comune = "CENTRO VALLE INTELVI", 
          lng = "9.0271", lat = "45.9848")
c_ref <- c_ref %>% 
  add_row(istat = "24023", comune = "VALBRENTA", 
          lng = "11.70229205", lat="45.82764540")
c_ref <- c_ref %>% 
  add_row(istat = "6006", comune = "ALLUVIONI PIOVERA", 
          lng = "8.77257309", lat="44.99538211")
c_ref <- c_ref %>% 
  add_row(istat = "7032", comune = "GRESSONEY LA TRINITE'", 
          lng = "7.82308558", lat="44.99538211")

#export the c_ref dataset as back-up - not run 
str(c_ref)

write.table(c_ref, "c_ref_back_up.csv",
            sep = ";", # punto e virgola
            row.names = F, # if you have a ID variable
            dec = ",", # separator of decimalals
            na = "", # missing data as empty cells
            quote = TRUE, 
            col.names = c("istat", "comune", "lng", "lat") #give colnames
)

back_up_ref<-read.csv("c_ref_back_up.csv", sep = ";")
head(back_up_ref)
str(back_up_ref)
############
#join the dataset !!! to the end not run now
#net03.2023_join<-net03.2023

#net03.2023_join= net03.2023_join %>% left_join(c_ref,by="comune")
#net03.2023_join

#which(is.na(net03.2023_join$lng))
#print.default(which(is.na(net03.2023_join$lng)), max = 2000) #1075

#View(net03.2023[is.na(net03.2023$lng),])
#net_na<- net03.2023_join[is.na(net03.2023_join$lng),]
#net_na$comune
#View(net_na)
#impute to comune missing variable the province 
 
library(purrr)

#net_na$comune[net_na$PV=="BA"]<-"BARI" 
#net_na$comune[net_na$PV=="AV"]<-"AVELLINO"
#net_na$comune[net_na$PV=="AN"]<-"ANCONA"
#net_na$comune[net_na$PV=="FI"]<-"FIRENZE"
#net_na$comune[net_na$PV=="PE"]<-"PERUGIA"
#net_na$comune[net_na$PV=="LC"]<-"LECCO"
#net_na$comune[net_na$PV=="MI"]<-"MILANO"
#net_na$comune[net_na$PV=="VR"]<-"VERONA"
#net_na$comune[net_na$PV=="CB"]<-"CAMPOBASSO"
#net_na$comune[net_na$PV=="TR"]<-"TERNI"
#net_na$comune[net_na$PV=="MO"]<-"MODENA"
#net_na$comune[net_na$PV=="SP"]<-"LA SPEZIA"
#net_na$comune[net_na$PV=="VE"]<-"VENEZIA"
#net_na$comune[net_na$PV=="CN"]<-"CUNEO"
#net_na$comune[net_na$PV=="MN"]<-"MANTOVA"
#net_na$comune[net_na$PV=="UD"]<-"UDINE"
#net_na$comune[net_na$PV=="LO"]<-"LODI"
#net_na$comune[net_na$PV=="BS"]<-"BRESCIA"
#net_na$comune[net_na$PV=="NA"]<-"NAPOLI"
#net_na$comune[net_na$PV=="PD"]<-"PADOVA"
#net_na$comune[net_na$PV=="SA"]<-"SALERNO"
#net_na$comune[net_na$PV=="TO"]<-"TORINO"
#net_na$comune[net_na$PV=="CZ"]<-"CATANZARO"
#net_na$comune[net_na$PV=="RN"]<-"RIMINI"

#net_na$comune
#is.na(net03.2023_join$comune)
#net03.2023_join$comune[net03.2023_join$comune=="NA"]
#net03.2023_join$comune[net03.2023_join$comune=="NA"]

#net03.2023_join$comune[is.na(net03.2023_join$comune)==TRUE]<-
#  net03.2023_join$comune[net03.2023_join$PV=="BA"]<-"BARI"

#first to join the two dataset add missing values in net03.2023 then join

net03.2023$comune[is.na(net03.2023$comune)==TRUE]<-
  net03.2023$comune[net03.2023$PV=="BA"]<-"BARI"

net03.2023$comune[is.na(net03.2023$comune)==TRUE]<-
  net03.2023$comune[net03.2023$PV=="AV"]<-"AVELLINO"

net03.2023$comune[is.na(net03.2023$comune)==TRUE]<-
  net03.2023$comune[net03.2023$PV=="AN"]<-"ANCONA"

net03.2023$comune[is.na(net03.2023$comune)==TRUE]<-
  net03.2023$comune[net03.2023$PV=="FI"]<-"FIRENZE"

net03.2023$comune[is.na(net03.2023$comune)==TRUE]<-
  net03.2023$comune[net03.2023$PV=="PE"]<-"PERUGIA"

net03.2023$comune[is.na(net03.2023$comune)==TRUE]<-
  net03.2023$comune[net03.2023$PV=="LC"]<-"LECCO"

net03.2023$comune[is.na(net03.2023$comune)==TRUE]<-
  net03.2023$comune[net03.2023$PV=="MI"]<-"MILANO"

net03.2023$comune[is.na(net03.2023$comune)==TRUE]<-
  net03.2023$comune[net03.2023$PV=="VR"]<-"VERONA"

net03.2023$comune[is.na(net03.2023$comune)==TRUE]<-
  net03.2023$comune[net03.2023$PV=="CB"]<-"CAMPOBASSO"

net03.2023$comune[is.na(net03.2023$comune)==TRUE]<-
  net03.2023$comune[net03.2023$PV=="TR"]<-"TERNI"

net03.2023$comune[is.na(net03.2023$comune)==TRUE]<-
  net03.2023$comune[net03.2023$PV=="MO"]<-"MODENA"

net03.2023$comune[is.na(net03.2023$comune)==TRUE]<-
  net03.2023$comune[net03.2023$PV=="SP"]<-"LA SPEZIA"

net03.2023$comune[is.na(net03.2023$comune)==TRUE]<-
  net03.2023$comune[net03.2023$PV=="VE"]<-"VENEZIA"

net03.2023$comune[is.na(net03.2023$comune)==TRUE]<-
  net03.2023$comune[net03.2023$PV=="CN"]<-"CUNEO"

net03.2023$comune[is.na(net03.2023$comune)==TRUE]<-
  net03.2023$comune[net03.2023$PV=="MN"]<-"MANTOVA"

net03.2023$comune[is.na(net03.2023$comune)==TRUE]<-
  net03.2023$comune[net03.2023$PV=="UD"]<-"UDINE"

net03.2023$comune[is.na(net03.2023$comune)==TRUE]<-
  net03.2023$comune[net03.2023$PV=="LO"]<-"LODI"

net03.2023$comune[is.na(net03.2023$comune)==TRUE]<-
  net03.2023$comune[net03.2023$PV=="BS"]<-"BRESCIA"

net03.2023$comune[is.na(net03.2023$comune)==TRUE]<-
  net03.2023$comune[net03.2023$PV=="NA"]<-"NAPOLI"

net03.2023$comune[is.na(net03.2023$comune)==TRUE]<-
  net03.2023$comune[net03.2023$PV=="PD"]<-"PADOVA"

net03.2023$comune[is.na(net03.2023$comune)==TRUE]<-
  net03.2023$comune[net03.2023$PV=="SA"]<-"SALERNO"

net03.2023$comune[is.na(net03.2023$comune)==TRUE]<-
  net03.2023$comune[net03.2023$PV=="TO"]<-"TORINO"

net03.2023$comune[is.na(net03.2023$comune)==TRUE]<-
  net03.2023$comune[net03.2023$PV=="CZ"]<-"CATANZARO"

net03.2023$comune[is.na(net03.2023$comune)==TRUE]<-
  net03.2023$comune[net03.2023$PV=="RN"]<-"RIMINI"

#now replicate the join 
net03.2023_join<-net03.2023

net03.2023_join= net03.2023_join %>% left_join(c_ref,by="comune")
net03.2023_join

#which(is.na(net03.2023_join$lng))
print.default(which(is.na(net03.2023_join$lng)), max = 2000) #1075
#View(net03.2023[is.na(net03.2023$lng),])
#net_na<- net03.2023_join[is.na(net03.2023_join$lng),]
#net_na$comune
#View(net_na)

#not missing values and export the dataset with all the data as back up 
#and for further analysis 

#remove duplicates #df2 <- df[!duplicated(df), ]

net03.2023_join2<- net03.2023_join[!duplicated(net03.2023_join),]

library(dplyr)
net03.2023_join2 <- net03.2023_join2 %>% distinct()

print(net03.2023_join2$`denominazione impresa`)
net03.2023_join2$`denominazione impresa`[!duplicated(net03.2023_join2)]

isTRUE(duplicated(net03.2023_join2$`denominazione impresa`))

write.table(net03.2023_join2, "stat_and_geo.csv",
            sep = ";", # punto e virgola
            row.names = F, # if you have a ID variable
            dec = ",", # separator of decimalals
            na = "", # missing data as empty cells
            quote = TRUE, 
            #col.names = c("istat", "comune", "lng", "lat") #give colnames
)

stat_and_geo<-read.csv("stat_and_geo.csv", sep = ";") #10 row plus i do not why 

