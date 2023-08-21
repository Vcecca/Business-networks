#compute regression with new file 
#both formal and informal BNAs
#install.packages("regclass")
library(regclass)

library(readxl)
net_regr<-read_xlsx("reti03.2023.xlsx", sheet = "net")
head(net_regr)
View(net_regr)
#H
m1_net<-lm(avg_dist~H+n+d_agr+d_altro+d_comm+d_ind+d_serv+d_tur
           +nw+ne+cn+south+ins+post_2016+infor, data = net_regr)
summary(m1_net)
AIC(m1_net)
BIC(m1_net)
VIF(m1_net)


m2_net<-lm(avg_dist~H+n+
             d_agr+d_altro+d_comm+d_ind+d_serv+d_tur
           +nw+ne+cn+south+ins 
           #+post_2016
           +infor
           , data = net_regr)
summary(m2_net)
AIC(m2_net)
BIC(m2_net)
VIF(m2_net)

m3_net<-lm(avg_dist~H+n+
             d_agr+d_altro+d_comm+d_ind+d_serv+d_tur
           #+post_2016
            +nw+ne+cn+south+ins
           +infor
           
           , data = net_regr)
summary(m3_net)
AIC(m3_net)
BIC(m3_net)
VIF(m3_net)

m4_net<-lm(avg_dist~H+n
           +d_agr+d_altro+d_comm+d_ind+d_serv+d_tur
           #+post_2016
           # +nw+ne+cn+south+ins
           #+infor
           , data = net_regr)
summary(m4_net)
AIC(m4_net)
BIC(m4_net)
VIF(m4_net)

m5_net<-lm(avg_dist~H
           +n
           #+d_agr+d_altro+d_comm+d_ind+d_serv+d_tur
           #+post_2016
           # +nw+ne+cn+south+ins
           #+infor
           , data = net_regr)
summary(m5_net)
AIC(m5_net)
BIC(m5_net)
VIF(m5_net)

m6_net<-lm(avg_dist~H
           #+n
           #+d_agr+d_altro+d_comm+d_ind+d_serv+d_tur
           #+post_2016
           # +nw+ne+cn+south+ins
           #+infor
           , data = net_regr)
summary(m6_net)
AIC(m6_net)
BIC(m6_net)


#normalised entropy

m7_net<-lm(avg_dist~H_norm+n+d_agr+d_altro+d_comm+d_ind+d_serv+d_tur
           +nw+ne+cn+south+ins+infor+post_2016, data = net_regr)
summary(m7_net)
AIC(m7_net)
BIC(m7_net)
VIF(m7_net)

m8_net<-lm(avg_dist~H_norm+n+
             d_agr+d_altro+d_comm+d_ind+d_serv+d_tur
           +nw+ne+cn+south+ins
           +infor 
           #+post_2016
           , data = net_regr)
summary(m8_net)
AIC(m8_net)
BIC(m8_net)
VIF(m8_net)

m9_net<-lm(avg_dist~H_norm+n+
             d_agr+d_altro+d_comm+d_ind+d_serv+d_tur
           #+post_2016
            +nw+ne+cn+south+ins
           #+infor
           , data = net_regr)
summary(m9_net)
AIC(m9_net)
BIC(m9_net)
VIF(m9_net)

m10_net<-lm(avg_dist~H_norm+n
           #+post_2016
           +d_agr+d_altro+d_comm+d_ind+d_serv+d_tur
           # +nw+ne+cn+south+ins
           #+infor
           , data = net_regr)
summary(m10_net)
AIC(m10_net)
BIC(m10_net)
VIF(m10_net)

m11_net<-lm(avg_dist~H_norm
            +n
            #+post_2016
            #+d_agr+d_altro+d_comm+d_ind+d_serv+d_tur
            # +nw+ne+cn+south+ins
            #+infor
            , data = net_regr)
summary(m11_net)
AIC(m11_net)
BIC(m11_net)
VIF(m11_net)

m12_net<-lm(avg_dist~H_norm
            #+n
            #+post_2016
            #+d_agr+d_altro+d_comm+d_ind+d_serv+d_tur
            # +nw+ne+cn+south+ins
            #+infor
            , data = net_regr)
summary(m12_net)
AIC(m12_net)
BIC(m12_net)
##########experiment just to see
#cor(x=net_regr$n, y =net_regr$avg_dist )

#cor(x=net_regr$H, y =net_regr$avg_dist )

#cor(x=net_regr$H_norm, y =net_regr$avg_dist )

#cor(x=net_regr$infor, y =net_regr$avg_dist )

#cov(x=net_regr$n, y=net_regr$avg_dist)

#cov(x=net_regr$H, y=net_regr$avg_dist)

#cov(x=net_regr$infor, y=net_regr$avg_dist)
########
