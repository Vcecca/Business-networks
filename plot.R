#do the trend plot 
#read the file

library(readxl)
reti<-read_xlsx("reti03.2023.xlsx", sheet = "net")
View(reti)

#trend line
sogg<-read_xlsx("reti03.2023.xlsx", sheet = "sogg")
contr<-read_xlsx("reti03.2023.xlsx", sheet = "contr")

str(reti)
#tranform the character to date format

#dati22$`data atto` <- as.Date(as.character(dati22$`data atto`), "%d/%m/%Y")

reti$date<- as.Date(as.character(reti$date), "%d/%m/%Y")

#add a 1 vector to have more quantity

reti$q<-1

library(plotly)
library(TSstudio)
#install.packages("rtweet")
library(rtweet)

date<-reti[,"date"]
str(date)

library(ggplot2)

p<-hist(x = reti$date, breaks = "months", freq = T, xlab = "Date of BNAs foundation",
        ylab ="Number of BNAs", col="lightblue" , main ="BNAs foundation", border ="lightblue")
#axis.Date(1, x = reti$date)
library(graphics)


library(tidyverse)
library(maps)
#install.packages("mapdata")
library(mapdata)


mapdata<-map('italy', fill = T, col = 1:20)
mapdata
             
#install.packages("sf")
library(sf)
# install.packages("dplyr")
library(dplyr)
#install.packages("giscoR")
library(giscoR)
 
reg_graphs<-read_xlsx("regions.xlsx", sheet = "reg")
head(reg_graphs)
year_ref <- 2021

nuts2_IT <- gisco_get_nuts(
  year = year_ref,
  resolution = 20, 
  nuts_level = 2,
  country = "Italy")  %>%
  select(NUTS_ID, NAME_LATN)

nuts2_IT
plot(st_geometry(nuts2_IT)) 

nuts2_IT_4326 <- st_transform(nuts2_IT, 4326)
#View(nuts2_IT_4326)
plot(st_geometry(nuts2_IT_4326))

#disp_income <- giscoR::tgs00026 %>%
#  filter(time == year_ref) %>%
#  select(-time)

#nuts2_IT_32632_data <- nuts2_IT_4326 %>%
#  left_join(disp_income, by = c("NUTS_ID" = "geo"))

#plot(nuts2_IT_32632_data[, "values"],
#     breaks = "jenks",
#     main = "Choropleth map")

#plot(nuts2_IT_32632_data[, "values"],
#     breaks = "jenks",
#     nbreaks = 10,
#     pal = hcl.colors(10),
#     main = "Choropleth map")

nuts2_IT_32632_data <- nuts2_IT_4326 %>%
  full_join(reg_graphs, by = c("NAME_LATN" ="NAME_LATIN"))
#str(nuts2_IT_32632_data)
#nrow(nuts2_IT_32632_data)
View(nuts2_IT_32632_data)
#before plotting add missing data manually

nuts2_IT_32632_data$geometry[22]<-nuts2_IT_32632_data$geometry[2]
nuts2_IT_32632_data$geometry[23]<-nuts2_IT_32632_data$geometry[3]

plot(nuts2_IT_32632_data[,(3:6)], breaks = "hclust", 
     pal = hcl.colors(10, 
                      #palette = "RdBu")
                      palette = "Blues", rev = T), key.pos = 4,
     key.width = lcm(0.9), key.length = 1, ylim = c(35, 45))
#750X300

plot(nuts2_IT_32632_data[,(3:6)], breaks = "hclust", 
     pal = hcl.colors(10, palette = "RdBu", rev = T), key.pos = 4,
     key.width = lcm(0.9), key.length = 1)

length(it)
#inser the legend 
#install.packages("autoimage")
#library(autoimage)
#leg_net<-legend.scale(zlim = c(0, 1200), 
            # col = colorspace::sequential_hcl(n = 10, palette = "Blues"))

legend(x = "bottom",
       title = "Scale",
       legend = "disr",
       cex = 0.7,
       bty = "o",
       inset = 0.4,
       y.intersp = 1,
       fill = colorspace::sequential_hcl(n = 10, palette = "Blues", rev = T))

image(1, nuts2_IT_32632_data$BNAs, t(seq_along(nuts2_IT_32632_data$BNAs)), 
      col=colorspace::sequential_hcl(n = 10, palette = "Blues"), axes=FALSE)


#BNAs total
plot(nuts2_IT_32632_data[, "BNAs"],
     breaks = "equal",
     nbreaks = 5,
     pal = hcl.colors(5, palette = "Blues", rev = T),
     main = "BNAs") 

plot(nuts2_IT_32632_data[, "BNAs"],
     breaks = "equal",
     nbreaks = 5,
     pal = hcl.colors(5, palette = "RdBu", rev = T),
     main = "BNAs") 

#informal bnas
plot(nuts2_IT_32632_data[, "Informal BNAs"],
     breaks = "equal",
     nbreaks = 5,
     pal = hcl.colors(5, palette = "RdBu", rev = T),
     main = "Informal BNAs") 

plot(nuts2_IT_32632_data[, "Informal BNAs"],
     breaks = "equal",
     nbreaks = 5,
     pal = hcl.colors(5, palette = "Blues", rev = T),
     main = "Informal BNAs") 

#formal bnas

plot(nuts2_IT_32632_data[, "Formal BNAs"],
     breaks = "equal",
     nbreaks = 5,
     pal = hcl.colors(5, palette = "RdBu", rev = T),
     main = "Formal BNAs") 

plot(nuts2_IT_32632_data[, "Formal BNAs"],
     breaks = "equal",
     nbreaks = 5,
     pal = hcl.colors(5, palette = "Blues", rev = T),
     main = "Formal BNAs") 

