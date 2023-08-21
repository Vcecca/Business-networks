#compute the regression 

#load the data 
library(readxl)
regr<-read_xlsx("C:/Users/hp/Desktop/progetto_paper/regr03.2023.xlsx", 
                sheet = "regr03.2023")
View(regr)
#compute econometrics model with Shannon entropy

model<-lm(avg_dist~n+H+
            d_agr+d_altro+d_comm+d_ind+d_serv+d_tur
          +nw+ne+cn+south+ins, data = regr)
summary(model)

#y1<-read.csv("y.csv", sep=";")

m1<-lm(avg_dist~n+H
       #+d_agr+d_altro+d_comm+d_ind+d_serv+d_tur
       #  +nw+ne+cn+south+ins
       , data = regr)
summary(m1)

m2<-lm(avg_dist~n+H
       +d_agr+d_altro+d_comm+d_ind+d_serv+d_tur
       #  +nw+ne+cn+south+ins
       , data = regr)
summary(m2)

m3<-lm(avg_dist~n+H
       #+d_agr+d_altro+d_comm+d_ind+d_serv+d_tur
       +nw+ne+cn+south+ins
       , data = regr)
summary(m3)

m4<-lm(avg_dist~n+H
       +d_agr+d_altro+d_comm+d_ind+d_serv+d_tur
       +nw+ne+cn+south+ins
       , data = regr)
summary(m4)

#with normalised entropy


m5<-lm(avg_dist~n+H_norm
       #+d_agr+d_altro+d_comm+d_ind+d_serv+d_tur
       #  +nw+ne+cn+south+ins
       , data = regr)
summary(m5)

m6<-lm(avg_dist~n+H_norm
       +d_agr+d_altro+d_comm+d_ind+d_serv+d_tur
       #  +nw+ne+cn+south+ins
       , data = regr)
summary(m6)

m7<-lm(avg_dist~n+H_norm
       #+d_agr+d_altro+d_comm+d_ind+d_serv+d_tur
       +nw+ne+cn+south+ins
       , data = regr)
summary(m7)

m8<-lm(avg_dist~n+H_norm
       +d_agr+d_altro+d_comm+d_ind+d_serv+d_tur
       +nw+ne+cn+south+ins
       , data = regr)
summary(m8)