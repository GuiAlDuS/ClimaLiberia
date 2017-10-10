# clear memory
rm(list=ls())

library(tidyverse)
library(lubridate)
library(stringr)
library(scales)
setwd("~/OneDrive/Guillermo/ClimaLiberia")

datosIMN <- read_tsv("Liberia_074020_PREC.txt", col_types = cols(.default = col_character()))

datosIMN$FECHA <- dmy(datosIMN$FECHA)
datosIMN <- rename(datosIMN, fecha = FECHA)

datosIMN$precip <- as.numeric(datosIMN$DATO)
datosIMN <- datosIMN %>% mutate (precip = replace(precip, precip<0, 0))

datosIMN$juliano <- yday(datosIMN$fecha)
datosIMN$aNo <- year(datosIMN$fecha)

datosIMN <- datosIMN %>% group_by(aNo) %>% mutate(acumulado = cumsum(precip))

ggplot(datosIMN, aes(x = juliano, y = acumulado, group = aNo)) + geom_line()

errores <- datosIMN %>% filter(acumulado < 0)
aNos <- datosIMN %>% group_by(aNo) %>% summarise(mediciones = sum(juliano))

ggplot(filter(datosIMN, aNo == 2013), aes(juliano, acumulado)) + geom_line()

##datos rusos
datosRusiaRAW <- read_delim("RP5_Liberia_clean.csv", delim = ";")
datosRusia <- select(datosRusiaRAW, Time, tR)

datosRusia$fechahora <- dmy_hm(datosRusia$Time)
datosRusia$fecha <- as.Date(datosRusia$fechahora)
datosRusia <- arrange(datosRusia, aNo, juliano)

#cambiar NA por 0
datosRusia <- datosRusia %>% replace_na(list(tR = 0))

datosRusia_dia <- datosRusia %>% group_by(fecha) %>% summarise((precip = sum(tR)))

colnames(datosRusia_dia) <- c("fecha", "lluvia")
datosRusia_dia$juliano <- yday(datosRusia_dia$fecha)
datosRusia_dia$aNo <- year(datosRusia_dia$fecha)

datosRusia_dia <- datosRusia_dia %>% group_by(aNo) %>% mutate(acumulado = cumsum(lluvia))
ggplot(datosRusia_dia, aes(x = juliano, y = acumulado, group = aNo)) + geom_line()

ggplot(filter(datosRusia_dia, aNo == 2013), aes(juliano, acumulado)) + geom_line()

## archivo ruso 2 para seleccionar 2014
datosRusia2 <- read_csv("LiberiaAeropuertoCSV.csv")
datosRusia2$fechahora <- dmy_hm(datosRusia2$Hora)
datosRusia2$fecha <- as.Date(datosRusia2$fechahora)

datosRusia2 <- arrange(datosRusia2, aNo, juliano)

#cambiar NA por 0
datosRusia2 <- datosRusia2 %>% replace_na(list(tR = 0))

datosRusia2_dia <- datosRusia2 %>% group_by(fecha) %>% summarise((precip = sum(tR)))
colnames(datosRusia2_dia) <- c("fecha", "lluvia")

datosRusia2_dia$juliano <- yday(datosRusia2_dia$fecha)
datosRusia2_dia$aNo <- year(datosRusia2_dia$fecha)

datosRusia2_dia <- datosRusia2_dia %>% group_by(aNo) %>% mutate(acumulado = cumsum(lluvia))

filter2014 <- datosRusia2_dia %>% filter(aNo == 2014)

#juntar todos las tablas
datosIMN <- rename(datosIMN, lluvia = precip)

cols_com <- intersect(colnames(datosIMN), colnames(datosRusia_dia))
datosIMN_limpio <- subset(datosIMN, select = cols_com)
datos2014_limpio <- subset(filter2014, select = cols_com)
datos2015_2016 <- subset(datosRusia_dia, select = cols_com)
datosLiberia <- rbind(datosIMN_limpio, datos2014_limpio, datos2015_2016)

ggplot(datosLiberia, aes(x = juliano, y = acumulado, group = aNo)) + geom_line()

#promedio
aNos <- datosLiberia %>% group_by(aNo) %>% summarise(total = max(acumulado))
mean(aNos$total)

datosLiberiaPromedio <- datosLiberia %>% group_by(juliano) %>% summarise(promedio = mean(lluvia))
datosLiberiaPromedio <- datosLiberiaPromedio %>% mutate(acumulado = cumsum(promedio))

#columna de fecha
origin2015 <- ymd("2014-12-31", tz = "America/Guatemala")
datosLiberia$mes <-origin2015 + ddays(datosLiberia$juliano)

datosLiberiaPromedio$mes <- origin2015 + ddays(datosLiberiaPromedio$juliano)



ggplot(datosLiberia, aes(x = mes, y = acumulado)) + 
  geom_line(aes(group = factor(aNo)), size = 0.1) + 
  geom_line(data = filter(datosLiberia, aNo > 2012), aes(group = factor(aNo), colour = factor(aNo)), size = 1) +
  geom_line(data = datosLiberiaPromedio, aes(x = mes, y = acumulado, colour = "red"), size = 2) +
  labs( x = "Month", y = "Cumulative precipitation (mm)", title = "Cumulative precipitation at the Liberia International Airport", subtitle = "From 1980 to 2016 (36 years of daily precipitation)") + 
  scale_colour_discrete(name = "",
                        labels = c("2013", "2014", "2015", "2016", "Average")) +
  scale_x_datetime(labels = date_format("%b"), date_breaks = "1 month")

