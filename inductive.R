#Date: 17.11.21
#Author: Fabian
# 7 induktive Statistik 
#---------------------------------------------------------------------------

rm(list = objects())

#Aufgabe 1

#setwd ...
load(Daten/mietspiegel.rda)


#a) Ist die durchschnittliche Monatsmiete pro Quadratmeter unterschiedlich in
#Abhängigkeit ob eine Zentralheizung vorhanden ist oder nicht?

t.test(mietspiegel$mieteqm[
  mietspiegel$zh== "mit"],
  mietspiegel$mieteqm[
    mietspiegel$zh== "ohne"],
  paired=FALSE, alternative = "two.sided",
  var.equal = TRUE, conf.level = 0.95)

# Interpretation: Der P-Wert ist kleiner als
# das gewählte Signifikanzniveau alpha=0.05,
# deshalb kann die Nullhypothese verworfen
# werden. Konfidenzintervall kann bei einem
# t-test mit zweiseitiger Alternative auch
# herangezogen werden, um festzustellen, ob
# die H0 zu verwerfen ist. Enthält das
# Konfidenzintervall der Nullhypothese mu=0
# nicht, dann ist die Hullhypothese abzulehnen.

# D.h. die durchschnittliche Monatsmiete pro
# Quadratmeter unterscheidet sich

#------------------------------------------------------------------------------
#b)Unterstützt unsere Stichprobe die Alternativhypothese, dass im Schnitt
#Wohnungen mit einer gehobenen Ausstattung des Bades eine höhere
#Monatsmiete pro Quadratmeter erzielen?
  

t.test(mietspiegel$mieteqm[mietspiegel$bad=="gehoben"],
       mietspiegel$mieteqm[mietspiegel$bad=="normal"],
       paired = FALSE, 
       alternative = "greater",
       var.equal = TRUE,
       conf.level = 0.95
       )
#-> output:
#t = 2.7828, df = 3080, p-value = 0.002711
#alternative hypothesis: true difference in means is greater than 0
#95 percent confidence interval:
#  0.2061607       Inf
#sample estimates:
#  mean of x mean of y 
#7.563187  7.058803 


#--------------------------------------------------------------------------
#c)Kann mit einer gehobenen Ausstattung der Küche eine höhere Monatsmiete
#pro Quadratmeter erzielt werden?
  
#H0: m_ud <=0
#H1: m_ud > 0


t.test(mietspiegel$mieteqm[mietspiegel$kueche=="gehoben"],
       mietspiegel$mieteqm[mietspiegel$kueche=="normal"],
       paired = FALSE, 
       alternative = "greater",
       var.equal = TRUE,
       conf.level = 0.95
)

#->
#t = 6.9033, df = 3080, p-value = 3.073e-12
#alternative hypothesis: true difference in means is greater than 0
#95 percent confidence interval:
# 1.131692      Inf
#sample estimates:
#  mean of x mean of y 
#8.512737  7.026906 

#Interpretation p-wert < alpha
# D. h. Nullhypothese wird verworfen

#----------------------------------------------------------------------------

# d) Erzielt eine Wohnung in bester Lage im Schnitt eine höhere Monatsmiete
#pro Quadratmeter als eine Wohnung in guter Lage?

#H0: mu_d =< 0
#H1: mu_d > 0


t.test(mietspiegel$mieteqm[mietspiegel$lage=="beste Lage"],
       mietspiegel$mieteqm[mietspiegel$lage=="gute Lage"],
       paired = FALSE, 
       alternative = "greater",
       var.equal = TRUE,
       conf.level = 0.95
)


#-> Output
#t = 2.9474, df = 1286, p-value = 0.001631
#alternative hypothesis: true difference in means is greater than 0
#95 percent confidence interval:
#  0.3962307       Inf
#sample estimates:
#  mean of x mean of y 
#8.147970  7.250568 



#Interpretation: p-wert < als Signifikanzniveau
# --> H0 wird abgelehnt
#D.h. Wohnung on bester Lage erzieheln eine signifikant höhere Miete/m2 als 
# jene in guter Lage

#----------------------------------------------------------------------------
#Aufgabe 2)
#Haben die Teilnehmer*innen am US-Masters 2021 (augusta2021.rda) die Runden
#2 und 4 unterschiedlich gut absolviert (Variablen R2 und R4)?
#  Gehen Sie davon aus, dass die Voraussetzungen für den passenden statistischen
#Test erfüllt sind.
#Quelle: https://www.augusta.com/masters/leaderboard


load("Daten/augusta2021.rda")

head(augusta2021)

# Verbundene Stichprobe
# H0:  (R4 - R2) = mu_d = 0
# H1:  mu_d != 0
# (zweiseitige Alternative)

t.test(augusta2021$R2, augusta2021$R4,
       mu=0,
       paired = TRUE,
       alternative = "two.sided",
       conf.level = 0.95)


#->
#t = -3.8044, df = 53, p-value = 0.0003698
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -2.9978608 -0.9280651
#sample estimates:
#  mean of the differences 
#-1.962963 

#Interpretation: Der p-wert ist kleiner als das gewählte Signifikanzniveau
# von 0.05, dementspreched ist die Nullhypothese zu verwerfen.

#D.h. die Teilnehmer haben die Runden 2 und 4 signifikant
# unterschiedlich absolviert.

















