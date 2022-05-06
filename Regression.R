#10 PS am 01.12.21
#Autor: Fabian Seisl
# Regressionsanalyse
#--------------------------------------------

rm(list=objects())
setwd("C:/Users/fabia/iCloudDrive/WS21/statistische Datenanalyse")


#-----------------------------------------
#Beispiel: CO2 und globale Erwärmung
#--------------------------------------------------------

Temp <- c(16.6, 18.6, 24, 38.4, NA, NA)
CO2  <- c(341, 349, 356, 364, 372,382)

# ausd diesen Vektoren
# eigenen Datenframe daraus bauen

daten.klima <- data.frame(cbind(Temp,CO2))
head(daten.klima)

#scatterplot
plot(Temp ~ CO2, data = daten.klima[c(1:4), ])

#Regressionsmodell schätzen

?lm

linmodel <- lm(Temp ~ CO2, data = daten.klima)
coef(linmodel)

#->
#(Intercept)         CO2 
#-304.437370    0.932872 



plot(Temp ~ CO2, data = daten.klima[c(1:4), ])
abline(linmodel, lwd =2)

#Daten Schätzen

predict(linmodel, newdata = daten.klima)
#->
#       1        2        3        4        5        6 
#13.67197 21.13495 27.66505 35.12803 42.59100 51.91972 

# grafik zu schätzern
plot(Temp ~ CO2, data = daten.klima,
      xlim = c(min(daten.klima$CO2), max(daten.klima$CO2)),
      ylim = c(min(daten.klima$Temp, na.rm = TRUE) - 10,
                + max(daten.klima$Temp, na.rm = TRUE) + 20),
     pch = 19)
abline(linmodel, cex = 1.5)
points(daten.klima$CO2, predict(linmodel, newdata = daten.klima),
        col = "darkblue", pch = 18, cex = 1.5)

#----------------------------------------------------------------------
#Beispiel

load("Daten/klima.rda")
m_co2 <- lm(Temp ~ CO2, data = klima)
coef(m_co2)
#->
#(Intercept)         CO2 
#-338.245030    1.030171 

plot(Temp ~ CO2, data = klima)
abline(m_co2)




