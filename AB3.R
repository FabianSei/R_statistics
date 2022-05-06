# Aufgabenblatt 3 
# Lineare Regression
# PS Statistische Datenanalyse
# Fabian 
#---------------------------------------------------------------------

rm(list = objects())

setwd("C:/Users/fabia/iCloudDrive/WS21/statistische Datenanalyse/Daten")
load("WHR2019_prep2.rda")

#a)
#Multiples lineares Regressionsmodell für Glückswert:
# mit logGDP, LifeExp, Freedom2 und Corruption2

# Y = alpha + beta1 * logGDP + beta2 * LifeExp + beta3 * Freedom2 
#     + beta4 * Corruption2 + epsilon

# beta1  logGDP     (+)
# beta2  LifeExp    (+)
# beta3  Freedom2   (+)
# beta4  Corruption (-)


#b)


str(WHR2019)
summary(WHR2019)

# Befehl um zu checken was die Referenzkategorie ist
#------------------------------------------------------

levels(WHR2019$Income)
# Output: [1] "niedrigem" "niedrigem-mittleren" "hohen-mittleren"   "hohem"

# Befehl um die Refernzkategorie umzuändern
#----------------------------------------------
WHR2019$Income <- relevel(WHR2019$Income, ref="hohem")
levels(WHR2019$Income)
# Output: "hohem"  "niedrigem"  "niedrigem-mittleren" "hohen-mittleren"
# Nun ist die Refernzkategorie "hohem"

lm1 <- lm(Happy ~ Income + LifeExp ,data = WHR2019)
summary(lm1)

# Alternativer Befehl um die Refernzkategorie umzuändern
#-----------------------------------------------------------

levels(WHR2019$Income)
WHR2019$Income <- factor(WHR2019$Income, levels=c("niedrigem", "niedrigem-mittleren",
                                                  "hohem", "hohen-mittleren"))

levels(WHR2019$Income)
# Nun ist "niedrigem-mittleren" die Referenzkategorie, da sie im Befehl
# in Zeile 50 als erste in levels=c( ) genannt wurde.

#------------------------------------------------------------------------------
# *************
#WHR2019$Corruption2 <- as.numeric(WHR2019$Corruption2)
WHR2019$Corruption2
str(WHR2019$Corruption2)
summary(WHR2019$Corruption2)

#WHR2019$Freedom2 <- as.numeric(WHR2019$Freedom2)
WHR2019$Freedom2


#WHR2019$Income <- as.numeric(WHR2019$Income)
WHR2019$Income 

WHR2019_ohne_na <- na.omit(WHR2019)
rm(WHR2019)

WHR2019$Income <- WHR2019$Income[WHR2019$logGDP!="NA"]
#*************










# ***
#WHR2019_ohne_na$Corruption2 <- factor(WHR2019_ohne_na$Corruption2, 
#                                      levels = c("wenig", "mittel", "hoch"))
# ***
#WHR2019_ohne_na$Freedom2 <- factor(WHR2019_ohne_na$Freedom2, 
#                                   levels = c("nicht frei","partiell frei", 
#                                              "frei"))
table(WHR2019_ohne_na$Corruption2)
table(WHR2019_ohne_na$Freedom2)

res_lm <- lm(Happy ~ logGDP + LifeExp + Freedom2 + Corruption2, 
             data = WHR2019_ohne_na)
res_lm
summary(res_lm)












#---------------------------------------------------------------------------
#Alternative
#  data <- na.omit(WHR2019[c("Happy", "logGDP", "LifeExp", "Freedom2", "Corruption2")])
#y <- data[,1]
#x <- data[,-1]
#x <- as.matrix(cbind(Intercept = rep(1, nrow(x)), x))
#beta_hat <- solve(t(x) %*% x) %*% t(x) %*% y
#lm_WHR
#summary(lm_WHR)
#----------------------------------------------------------------------------
#b)


# Y = alpha + beta1 * logGDP + beta2 * LifeExp + beta3 * Freedom2 
#     + beta4 * Corruption2 + epsilon

# Y = -1.92842 + 0.35498 * logGDP + 0.05968 * LifeExp + 0.26672 * Freedom2 
#     + -0.17564  * Corruption2 + epsilon


#Schätzung 1: Happy
# für logGDP = 8
#     LifeExp = 70
#     Corruption2 = 1 (wenig)
#     Freedom2 =  3 (frei)

#***
# y = -2.00196 + 0.34893 * 8 + 0.06028 * 70 + 1 * 0.71435 
Happy_Land1 <-  -2.00196 + 0.34893 * 8 + 0.06028 * 70 + 1 * 0.71435 
Happy_Land1
# = 5.72343




#-----------------------------------------------------------------------------
#Schätzung 2: Happy
# für logGDP = 8
#     LifeExp = 70
#     Corruption2 = wenig
#     Freedom2 =  nicht frei

# y = -2.00196 + 0.34893 * 8 + 0.06028 * 70 

Happy_Land2 <-  -2.00196 + 0.34893 * 8 + 0.06028 * 70
Happy_Land2
# = 5.00908

#-----------------------------------------------------------------------------
#Schätzung Alternative:


#Schätzung 1

CountryPred1 <- data.frame(logGDP=8, LifeExp=70, Corruption2=1, 
                           Freedom2=3)

CountryPred1

predict(res_lm, CountryPred1)
#Output:
# => 5.723379 

#Output neu(as.numeric)
# => 5.713229 

#Schätzung 2
CountryPred2 <- data.frame(logGDP=8, LifeExp=70, Corruption2=1, 
                           Freedom2=1)

CountryPred2

predict(res_lm, CountryPred2)

# Output:
# => 5.009034 

#Output neu (as.numeric)
# => 5.17978

#----------------------------------------------------------------------------

predict(lm1, newdata = data.frame(logGDP = 8, LifeExp = 70,
                                  Freedom2 = "frei", Corruption2 = "wenig"))











#------------------------------------------------------------------------------
#c)
#Geben Sie fur das in Punkt b) geschatzte Regressionsmodell 
#den entsprechenden Output der Varianzanalyse an und interpretieren Sie 
#diesen

#Output:
#------------------------------------------------------------------------------
#Coefficients:
#                        Estimate  Std.Error  t value Pr(>|t|)   
#  (Intercept)           -2.00196    0.70741  -2.830  0.00547 **
#  logGDP                 0.34893    0.10510   3.320  0.00120 **
#  LifeExp                0.06028    0.01796   3.357  0.00106 **
#  Freedom2partiell frei  0.53658    0.27396   1.959  0.05250 . 
#  Freedom2frei           0.71435    0.28399   2.515  0.01322 * 
#  Corruption2mittel     -0.19816    0.18924  -1.047  0.29715   
#  Corruption2hoch       -0.38851    0.18713  -2.076  0.04004 * 
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.6681 on 119 degrees of freedom
#Multiple R-squared:  0.6674,	Adjusted R-squared:  0.6506 
#F-statistic:  39.8 on 6 and 119 DF,  p-value: < 2.2e-16
#------------------------------------------------------------------------------
# Imterpretation:

# p-Wert ist mit 2.2e^-16 signifikant daher kann die Null Hypothese verworfen
# werden, da das Modell einen Erklärungsbeitrag leistet.

# Multiple R-squared von 0.6674 besagt, dass die Variabeln im Modell 66.74% der
# abhängigen Variable (y) = Happy erklären kann. (Varianzaufklärung)

# Adjusted R-squared ist der angepasste Wert für die Anzahl an unabhängigen 
# Variablen (x). 
# Adjusted R^2 = 0.6506 => 65.06% von Happy erklärbar mit unseren x-Variablen.

# Koeffizienten
# Intercept = alpha oder beta0

# beta1 Koeffizient von logGDP ist mit 0.0012 signifikant und besitzt den Wert 
#  0.34893. Damit stellt er einen positiven signifikanten einfluss auf den 
# Glückswert Happy dar.
# => bei einer Erhöhung des logGDP um 1 erhöht sich der Glückswert Happy 
# schätzungsweise um 0.34893 Einheiten.

# beta2 Koeffizient von LifeExp = 0,06028 bei Sig. von 0.00106
# positiver signifikanter einfluss 
# => bei einer Erhöhung von LifeExp um 1 steigt der Glückswert Happy schätzungsweise
# um 0.06028

# beta3 Koeffizient von Freedom2 ist für nicht freie Länder 0 da, da diese als 
# Referenzkathegorie im Modell bereits beinhaltet sind
# Für partiell freie Länder kann der beta3 Koeffizient nicht verwendet werden 
# da das Signifikanzniveau mit 0.0525 zu hoch ist.
# Für freie Länder ist der Beta3 Koeffizient mit 0.01322 ausreichend signifikant
# und beträgt 0.71435
# => ein Freies Land hat schätzungsweise einen um 0.71435 Einheiten höheren 
# Glückswert Happy als ein nicht freies.

# beta4 Koeffizien von Corruption2:
# für wenig Korruption = 0 da Referenzkategorie
# für mittel Korr. = nicht signifikant (0.29715 ) daher nicht verwendbar.
# für hoch = -0.38851 bei Signifikanzniveau von 0.04004
# => für Länder mit hoher Korruption sinkt der Glückswert Happy schätzungsweise
# um 0.38851 Einheiten.

#------------------------------------------------------------------------------
# Varianzanalyse

var(res_lm)
anova(res_lm)

#> anova(res_lm)
#Analysis of Variance Table

#Response: Happy
#Df Sum Sq Mean Sq  F value    Pr(>F)    
#logGDP        1 91.864  91.864 207.2114 < 2.2e-16 ***
#  LifeExp       1  8.517   8.517  19.2106  2.51e-05 ***
#  Freedom2      1  3.818   3.818   8.6120  0.003997 ** 
#  Corruption2   1  1.861   1.861   4.1984  0.042624 *  
#  Residuals   121 53.644   0.443                       
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Reihenfolge der Variabeln hat einfluss auf die ANOVA.
#--------------------------------------------------------------------------
#d)
#Berechnen Sie nun erneut das Regressionsmodell aus Punkt b). 
#Verwenden Sie dazu anstelle der Variable logGDP die Variable Income. Geben
#Sie die berechnete (geschatzte) Regressionsgleichung an und interpretieren 
#Sie Ihre Ergebnisse

table(WHR2019_ohne_na$Income)

# y = alpha + beta1 * Income + beta2 * LifeExp + beta3 * Freedom2 
#     + beta4 * Corruption2 + epsilon

res2_lm <- lm(Happy ~ Income + LifeExp + Freedom2 + Corruption2, data = WHR2019_ohne_na)

summary(res2_lm)

# y = 0.47435 + 0.37305 * Income + 0.05845 * LifeExp + 0.29194 * Freedom2 

# Income, LifeExp und Freedom2 haben einen positive signifikanten einfluss auf 
# den Glückswert Happy
# Corruption 2 hat keinen signifikanten Einfluss.
# Das Modell erklärt 65.59% der abhängigen Variable.



#----------------------------------------------------------------------------------
#e)

#Berechnen Sie nun erneut das Regressionsmodell aus Punkt b) mit zwei
#zus¨atzlichen, von Ihnen gewahlten erklarenden Variablen. 
#Interpretieren Sie diese Ergebnisse.

res3_lm <- lm(Happy ~ logGDP +  LifeExp + Social + Freedom2
              + Corruption2 + Positive, 
              data = WHR2019_ohne_na)
res3_lm

summary(res3_lm)

#Results:
#------------
#Coefficients:
#               Estimate  Std. Error t value Pr(>|t|)    
#  (Intercept) -2.84916    0.65286  -4.364 2.73e-05 ***
#  logGDP       0.08222    0.10333   0.796 0.427818    
#  LifeExp      0.06669    0.01575   4.235 4.52e-05 ***
#  Social       2.83334    0.70837   4.000 0.000110 ***
#  Freedom2    -0.08815    0.12018  -0.733 0.464716    
#  Corruption2 -0.23250    0.07505  -3.098 0.002432 ** 
#  Positive     2.44346    0.61599   3.967 0.000125 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.5769 on 119 degrees of freedom
#Multiple R-squared:  0.752,	Adjusted R-squared:  0.7395 
#F-statistic: 60.13 on 6 and 119 DF,  p-value: < 2.2e-16
#------------

# Model: 

# y = alpha + beta1 * lodGDP + beta2 * LifeExp + beta3 * Social 
#     + beta4 * Freedom2 + beta5 * Corruption2 + beta6 * Positive
#      + epsilon

# y = -2.84916  
#     + beta1 * logGDP [ausschluss weil nicht signifikant]
#     + 0.06669 * LifeExp 
#     + 2.83334 * Social 
#     + -0.08815  * Freedom2 [ausschluss weil nicht signifikant]
#     + -0.23250  * Corruption2 
#     + 2.44346  * Positive
#      + epsilon

#Interpretation:

# Model insgesamt signifikant
# Erklärung der Signifikanz von Happy (y) = 73.95%

#Ausschluss von beta1 mit logGDP und beta4 mit Freedom2 aufgrund unzureichender 
# Signifikanz.

# Positiver signifikanter Einfluss von LifeExp, Social und Positive.
# Negativer Signifikanter Einfluss von Corruption2


#---------------------------------------------------------------------------
#f)
# Vergleichen Sie nun alle drei berechneten Regressionmodelle mit geeigneten 
# Modellselektionskriterien. Fur welches dieser drei Modelle würden 
# Sie sich entscheiden?

summary(res_lm)
summary(res2_lm)
summary(res3_lm)

#Das Model 3 bietet das höchste Erklärungsmaß der Varianz der abhänigen 
# Variabel y (Glückswert) bei gleicher Signifikanz.


AIC(res_lm, res2_lm, res3_lm)

BIC(res_lm, res2_lm, res3_lm)

# ???



#---------------------------------------------------------------------------
#g) 
# Fuhren Sie nun eine erste Residuendiagnostik fur das von Ihnen in 
# Punkt f) favorisierte Regressionsmodell durch.

plot(fitted(res3_lm), residuals(res3_lm),
     ylab = "Residuen", xlab = "gefittete Werte")

par(mfrow = c(1,2))
plot(WHR2019_ohne_na$LifeExp, residuals(res3_lm),
        xlab = "Life Expactation", ylab = "Residuen")

plot(WHR2019_ohne_na$Social, residuals(res3_lm),
     xlab = "Social", ylab = "Residuen")

plot(WHR2019_ohne_na$Positive, residuals(res3_lm),
     xlab = "Positive", ylab = "Residuen")


hist(residuals(res3_lm), freq = FALSE, xlab = "Residuen", main = "")



cor.test(WHR2019_ohne_na$LifeExp,
         WHR2019_ohne_na$Social)
# => 0.6975692


plot(res3_lm)

hist(resid(res3_lm))

plot(fitted(res3_lm), resid(res3_lm))




#---------------------------------------------------------------------------
# ***



#install.packages("lmtest")
library(lmtest)

bptest(res3_lm)
#	studentized Breusch-Pagan test

#data:  res3_lm
#BP = 14.966, df = 6, p-value = 0.02052


# -> p-wert
# Ho (dass Homoskelastizität vorliegt) zugunsten H1 verworfen
# Homoskelastizität verworfen -> Heteroskelastizität liegt vor.


#--------------------------------------------------------------------------
# ***
# robuste standartfehler schätzen
# grafisch

plot(fitted.values(res3_lm), residuals(res3_lm))


# residuen standartisieren
plot(fitted.values(res3_lm), rstandard(res3_lm))


plot(res3_lm, 1)


#install.packages("car")
library(car)


# Varianz Inflations Faktoren
vif(res3_lm)

#->
#     logGDP      LifeExp      Social     Freedom2    Corruption2  Positive 
#     5.282370    4.399128    2.821910    1.972884    1.190093    1.710219 

# Je höher die Werte desto eher liegt Multikoloniarität vor.
# 

# Toleranzwerte
1/vif(res3_lm)

#-> 
#     logGDP     LifeExp      Social      Freedom2    Corruption2    Positive 
#     0.1893090   0.2273178   0.3543699   0.5068720   0.8402707     0.5847204
# Werte unter 0.1 sind Problematisch

#--------------------------------------------------
