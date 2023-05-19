library(dplyr)
library(ggplot2)
#создайте модель множественной линейной регрессии ночных потоков паров 
#воды за период 2013 года 
#по данным измерений методом турбулентной пульсации

##чтение
data = read.csv("eddypro.csv", header = FALSE, skip=3, na=c("","NA","-9999","-9999.0"))
headers = read.csv("eddypro.csv", skip = 1, header=FALSE, nrows=1)
#false - night
#true - day

##Задаем заголовки признакам и удаляем признак roll
colnames(data) = headers
data = subset(data, select = -roll)
##Фильтруем данные по заданию (2013 год, ночь)
data = filter(data, daytime == FALSE  & date >= "2013-01-01" & date <= "2013-12-31")

##char в factor
data = data %>% mutate_if(is.character, as.factor)

##numeric переменные
data_num = data[,sapply(data, is.numeric)]
str(data_num)

##- na
data_num = na.exclude(data_num)

##корреляция
cor_d = data.frame(cor(data_num))
##коэффициент детерминации и названия признаков, где он значимый
coef_det = cor_d$h2o_flux^2
names(coef_det) = names(cor_d)

coef_det

##Выбираем значимые
coef_det = coef_det[coef_det>0.2]
names(coef_det) %>% na.exclude()



names(data_num)[names(data_num) == "w/h2o_cov"] = "w_h2o_cov"

##Регрессия
################################################################################
mr1 = lm(data = data_num, h2o_flux ~ LE +  rand_err_LE
         + rand_err_h2o_flux  + air_temperature + es + RH + VPD 
         + un_LE + un_h2o_flux + w_h2o_cov )
##коэффициенты
coef(mr1)
##остатки
resid(mr1)
##доверительный интервал
confint(mr1)
##p-value
summary(mr1)
##дисперсионный анализ
anova(mr1)


##plots
plot(mr1,2)
plot(mr1$fitted.values, data_num$h2o_flux)
abline(a=0, b=1, col = "red")

plot(data_num$h2o_flux,mr1$residuals)
t_mr1 = lm(mr1$residuals ~ data_num$h2o_flux)
abline(a = t_mr1[1], b = t_mr1[2], col="red")
################################################################################
mr2 = lm(data = data_num, h2o_flux ~ ( LE +  LE +  rand_err_LE
                                       + rand_err_h2o_flux  + air_temperature + es + RH + VPD 
                                       + un_LE + un_h2o_flux + w_h2o_cov )^2 )

coef(mr2)
resid(mr2)
confint(mr2)
summary(mr2)
anova(mr2)

plot(mr2, 2)
plot(mr2$fitted.values, data_num$h2o_flux)
abline(a=0, b=1, col="green")

plot(data_num$h2o_flux, mr2$residuals)
t_mr2 = lm(mr2$residuals ~ data_num$h2o_flux)
abline(a=t_mr2[1], b = t_mr2[2], col="orange")
################################################################################
mr3 = lm(data = data_num, h2o_flux ~ (LE +  LE +  rand_err_LE
                                      + rand_err_h2o_flux  + air_temperature + es + RH + VPD 
                                      + un_LE + un_h2o_flux + w_h2o_cov )^2 - un_LE:w_h2o_cov - un_LE:un_h2o_flux)
coef(mr3)
resid(mr3)
confint(mr3)
summary(mr3)
anova(mr3)

plot(mr3, 2)
plot(mr3$fitted.values, data_num$h2o_flux)
abline(a=0, b=1, col="green")

plot(data_num$h2o_flux, mr3$residuals)
t_mr3 = lm(mr3$residuals ~ data_num$h2o_flux)
abline(a=t_mr3[1], b = t_mr3[2], col="orange")
################################################################################

mr4 = lm(data = data_num, h2o_flux ~ (LE +  LE +  rand_err_LE
                                      + rand_err_h2o_flux  + air_temperature + es + RH + VPD 
                                      + un_LE + un_h2o_flux + w_h2o_cov )^2 - un_LE:w_h2o_cov - un_LE:un_h2o_flux - un_h2o_flux:w_h2o_cov)
coef(mr4)
resid(mr4)
confint(mr4)
summary(mr4)
anova(mr4)


plot(mr4, 2)
plot(mr4$fitted.values, data_num$h2o_flux)
abline(a=0, b=1, col="green")

plot(data_num$h2o_flux, mr4$residuals)
t_mr4 = lm(mr3$residuals ~ data_num$h2o_flux)
abline(a=t_mr4[1], b = t_mr4[2], col="orange")
