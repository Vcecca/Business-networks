#print results of regression model in latex output 

install.packages("texreg")
library(texreg)

#texreg(list(due_log_r_2, due_r_5))
texreg(list(m1_net, m2_net))

install.packages("stargazer")
library(stargazer)
library(broom)
stargazer(m1_net, m2_net, type = "text")
#install.packages("jtools")
library(jtools)

stargazer(m6_net, m5_net, m4_net, m3_net, m2_net, m1_net, type = "latex", font.size = "tiny")
summ(m1_net)
export_summs(summ(m1_net))
coef(m6_net)
coef(m5_net)

coef(m4_net)
coef(m3_net)
coef(m2_net)
#stargazer(r_2,  r_3, r_13, r_12, r_4, r_5, type = "latex")

AIC(m6_net, m5_net, m4_net, m3_net, m2_net, m1_net)
BIC(m6_net, m5_net, m4_net, m3_net, m2_net, m1_net)

stargazer(m12_net, m11_net, m10_net, m9_net, m8_net, m7_net, type = "latex", font.size = "tiny")
AIC(m12_net, m11_net, m10_net, m9_net, m8_net, m7_net)
BIC(m12_net, m11_net, m10_net, m9_net, m8_net, m7_net)



#summ(due_r_2)
#summ(due_r_3)
library(huxtable)
#export_summs(summ(due_r_2)
#             ,summ(due_r_3))


#summ(as.list(r_2, r_3, r_13, r_12, r_4, r_5))

#exorting table in html format

#install.packages("sjPlot")
library(sjPlot)
#install.packages("sjmisc")
library(sjmisc)
#install.packages("sjlabelled")
library(sjlabelled)


install.packages("officer")
library(officer)
install.packages("flextable")
library(flextable)
#write excel file with models
export_summs(m1_net, m2_net, scale = TRUE, to.file = "xlsx", file.name = "test.xlsx")

#summary statistics
summary.data.frame(net_regr)
summary.table(net_regr)