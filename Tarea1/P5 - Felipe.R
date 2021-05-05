library(dplyr)
library(data.table)
library(ggplot2)

setwd('C:/Users/Felipe/Documents/GitHub/2021-1/marketinglabs/Tarea1')

data <- fread('supermarket_sales.csv')

data <- data %>%
  mutate(Satisfecho = ifelse(Rating >= 7,1,0))

data$hora <- as.POSIXct(data$Time, format="%H:%M")
data$Date2 <- as.Date(data$Date, format='%m/%d/%Y')
data$day <- weekdays(data$Date2)
#La cantidad de satisfechos es independiente de la hora ni del medio de pago
ggplot(data, aes(x=Total, fill = factor(Satisfecho)))+
  geom_density(alpha=0.4)+
  facet_grid(Branch ~ day)

#La cantidad de satisfechos es independiente de la hora ni del medio de pago
ggplot(data, aes(x=`Total`, color = factor(Satisfecho),
                 fill = factor(Satisfecho)))+
  geom_histogram(aes(y=..density..),
                 alpha=0.5, position = "identity",
                 bins=22)+
  geom_density(alpha=.2,
               fill="#FF6666")+
  facet_grid(Branch ~ Quantity)

ggplot(data, aes(x=Total, fill = factor(Quantity)))+
  geom_histogram(position= 'identity', alpha=.5)+
  facet_grid(Branch ~ .)

ggplot(data, aes(x=`Unit price`, fill = factor(Satisfecho)))+
  geom_density(position= 'identity', alpha=.5)+
  facet_grid(Branch ~ `Quantity`)

ggplot(data, aes(x=`Unit price`, fill = factor(Satisfecho)))+
  geom_density(position= 'identity', alpha=.5)+
  facet_grid(Branch ~ .)



ggplot(data, aes(x=hora, fill = factor(Satisfecho)))+
  geom_density(alpha=0.3)+
  facet_grid(Branch ~ Gender)

ggplot(data, aes(x=hora, fill = factor(Satisfecho)))+
  geom_density(alpha=0.3)+
  facet_grid(Branch ~ Quantity)

ggplot(data, aes(x=hora, fill = factor(Satisfecho)))+
  geom_density(alpha=0.3)+
  facet_grid(Branch ~ Payment)

#Agregación: por total de compra
data$c_total <- data$Total%/%250
data3 <- data %>%
  group_by(Branch, `Product line`, c_total) %>%
  summarise(Satisfechos = sum(Satisfecho), Clientes = n()) %>%
  mutate(Satisfechos_perc = round(Satisfechos*100 / Clientes, digits = 2),
         Insatisfechos = Clientes - Satisfechos,
         Insatisfechos_perc = 100-Satisfechos_perc,
         c_total = c_total*250)

ggplot(data3, aes(x=c_total, y = Satisfechos_perc))+
  geom_line()+
  facet_grid(Branch ~ `Product line`)+
  scale_x_log10()


#Agregación: % satisfaccion por hora
data$rating2 <- round(data$Rating, digits = 0)
data$hora2 <- hour(data$hora)
data2 <- data %>%
  group_by(Branch, `Product line`, hora2) %>%
  summarise(Satisfechos = sum(Satisfecho), Clientes = n()) %>%
  mutate(Satisfechos_perc = round(Satisfechos*100 / Clientes, digits = 2),
         Insatisfechos = Clientes - Satisfechos,
         Insatisfechos_perc = 100-Satisfechos_perc)

ggplot(data2, aes(x=hora2, y=Satisfechos_perc))
  

ggplot(data2, aes(x=hora2, y=Satisfechos_perc))+
  geom_line()+
  facet_grid(Branch ~ `Product line`)

ggplot(data, aes(x=hora, fill = factor(Satisfecho)))+
  geom_density(position = 'fill')+
  facet_grid(. ~ `Branch`)

#Agregación: % satisfaccion por hora

ggplot(data, aes(x=Total, fill = factor(Satisfecho)))+
  geom_density(alpha = 0.4)+
  facet_grid(Branch ~ `Product line`)

ggplot(data, aes(x=Total, fill = factor(rating2)))+
  geom_density(alpha = 0.4)+
  facet_grid(Branch ~ .)




ggplot(data, aes(x=Total, fill = factor(Satisfecho)))+
  geom_density(alpha = 0.4)+
  facet_grid(Branch ~ `Product line`)+
  scale_x_log10()

ggplot(data, aes(x=Total))+
  geom_density(alpha = 0.4)




ggplot(data, aes(x=Total, fill = factor(Satisfecho)))+
  geom_density(alpha = 0.4)+
  facet_grid(Branch ~ .)+
  scale_x_log10()

ggplot(data, aes(x=`Total`, fill = factor(Satisfecho)))+
  geom_density(alpha = 0.4)+
  facet_grid(Branch ~ .)+
  scale_x_log10()




ggplot(data, aes(x=`Total`, fill = factor(Satisfecho)))+
  geom_density(alpha = 0.4, position="fill")+
  facet_grid(Branch ~ .)+
  scale_x_log10()

ggplot(data, aes(x=`Total`, fill = factor(Satisfecho)))+
  geom_density(position="fill")+
  facet_grid(Branch ~ .)+
  scale_x_log10()

ggplot(data, aes(x=Total, fill = factor(Satisfecho)))+
  geom_density(alpha = 0.4)+
  facet_grid(Branch ~ `Product line`)+
  scale_x_log10()

ggplot(data, aes(x=Total, fill = factor(Satisfecho)))+
  geom_density(alpha = 0.4)+
  facet_grid(Branch ~ `Quantity`)+
  scale_x_log10()

ggplot(data, aes(x=`Total`, fill = factor(Satisfecho)))+
  geom_density(alpha = 0.4)+
  facet_grid(Branch ~ `Product line`)
  #scale_x_log10()

ggplot(data, aes(x=Total, y=Quantity, color = factor(Satisfecho)))+
  geom_point()+
  facet_grid(Branch ~ Satisfecho)
