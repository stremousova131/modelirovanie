#для региона 39 рассчитайте урожайность пшеницы в 2016 году, взяв для рассчета средние суммы активных температур за предыдущие 13 лет, 
#с 11 ближайших метеостанций но убирая из рассчета активных температур дни с температурой выше 27 градусов
library(tidyverse)
library(rnoaa)

station_data = ghcnd_stations()
write.csv(station_data, file = "station_data.csv")

station_data = read.csv("station_data.csv")
kd = data.frame(id="KALININGRAD", latitude = 54.7065, longitude = 20.511)
kd_around = meteo_nearby_stations(lat_lon_df = kd, station_data = station_data,
                                      limit = 11, var = c("TAVG"),
                                      year_min = 2003, year_max = 2016)
kd_around
kd_around = kd_around[[1]]

kd_id = kd_around[["KALININGRAD"]][["id"]]

all_kd_data=meteo_tidy_ghcnd(stationid = kd_id)
summary(all_kd_data)

all_kd_data$tavg = all_kd_data$tavg / 10

write.csv(all_kd_data,"all_kd_data.csv")
kaliningrad_data=read.csv("all_kd_data.csv")

kaliningrad_data = kaliningrad_data %>% mutate(year=year(date), month=month(date), day=day(date))

kaliningrad_data = kaliningrad_data[kaliningrad_data$date >= "2003-01-01" & kaliningrad_data$date <= "2015-12-31",]

kaliningrad_data[is.na(kaliningrad_data$tavg),"tavg"] = 0
kaliningrad_data[kaliningrad_data$tavg<5, "tavg"] = 0
kaliningrad_data[kaliningrad_data$tavg>27, "tavg"] = 0
summary(kaliningrad_data)

group_data =kaliningrad_data %>% group_by(id,year,month)
sum_group_data = group_data %>% summarise(tsum=sum(tavg))
group_month=sum_group_data%>%group_by(month)


sum_month=group_month%>%summarise(St=mean(tsum))

y = 1.0
afi = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
di = c(0.00,0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
Kf = 300 
Qj = 1600
Lj = 2.2 
Ej = 25 

sum_month = sum_month %>% mutate(Fi = afi+bfi*y*St)
sum_month = sum_month %>% mutate( Yi = ((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))
Y = (sum(sum_month$Yi)) 
Y #15.03213