################################################################################
################################################################################
### Begleitender Code zu Block 4
### 11.06.2013
################################################################################
################################################################################

rm(list=ls())
setwd("Y:/Lehre/Kurs_Statistik_R_Studienbeitraege/Theorie/Block_4_Daten")

load("TR.2.RData")
# Daten heißen TR.2

load("HairEyeData.RData")
# basiert auf einen bestehenden R Datensatz (HairEyeColor), umgeformt, so dass es eine Datentabelle darstellt

################################################################################
### 3 Dim kategorial                                                                                                                                                                                                                    ############
################################################################################

### Tabellen

dim3  = table(HairEyeData)
dim3  = table(HairEyeData$Eye, HairEyeData$Sex, HairEyeData$Hair)               # Zeile, Spalte, 3 Dimension (Tabellen getrennt nach)
dim3a = xtabs(~ HairEyeData$Eye + HairEyeData$Sex + HairEyeData$Hair)           # als Formel mit xtabs()

# Da solche Tabellen die komplette Information beinhalten, können sie auch
# zur Datenspeicherung verwendet werden (z.B. Urdaten für HairEyeData)

HairEyeColor
dim3b = table(HairEyeData$Hair, HairEyeData$Eye, HairEyeData$Sex)

# Achtung bei prop.table
prop.table(dim3b)
addmargins(prop.table(dim3b))

addmargins(prop.table(dim3b,1))
addmargins(prop.table(dim3b,2))
addmargins(prop.table(dim3b,3))

# bezieht sich immer auf die Randsumme der genutzte Dimension

### Grafik
mosaicplot(dim3b)

par(mfrow = c(3,3))
for (i in 1:3){
  for (j in 1:3){
    if (i != j){
      mosaicplot(table(HairEyeData[,c(i,j)]), main = paste(names(HairEyeData)[i],"vs",names(HairEyeData)[j]))
    } else {
      barplot(table(HairEyeData[,c(i)]), ylim = c(0,350))
    }
  }
}

################################################################################
### 3 Dim metrisch                                                                                                                                                                                                                   ############
################################################################################

par(mfrow = c(1,1))
plot(TR.2$Aussehen, TR.2$Evaluierung, pch = c("x","o")[TR.2$Minderheit], ylab = "Evaluierung", xlab = "Aussehen")
legend("topleft", pch = c("x","o"), legend = c("Nein", "Ja") , title = "Minderheit")

plot(TR.2$Aussehen, TR.2$Evaluierung, col = c("blue","red")[TR.2$Minderheit], ylab = "Evaluierung", xlab = "Aussehen", pch = 16, cex = .7)
legend("topleft", pch = 15, pt.cex = 2, col = c("blue","red"), legend = c("Nein", "Ja") , title = "Minderheit")

par(mfrow = c(3,3))
vars = c("Aussehen","Evaluierung","Minderheit")
for (i in 1:3){
  for (j in 1:3){
    if (i != j){
      plot(TR.2[,vars[i]] ~ TR.2[,vars[j]], xlab = names(TR.2[,vars])[j], ylab = names(TR.2[,vars])[i] )
    } else {
      if (i != 3){
        hist(TR.2[,vars[i]], xlab =  names(TR.2[,vars])[i] ,main = "")
      } else {
        barplot(table(TR.2[,vars[i]]), xlab =  names(TR.2[,vars])[i] )
      }
    }
  }
}

par(mfrow = c(6,6))
vars = c(2,5,6,10,11,16)
for (i in 1:6){
  for (j in 1:6){
   if (i != j){
      plot(TR.2[,vars[i]] ~ TR.2[,vars[j]], xlab = names(TR.2[,vars])[j], ylab = names(TR.2[,vars])[i] )
    } else {
      hist(TR.2[,vars[i]], xlab =  names(TR.2[,vars])[i] ,main = "")
    }
  }
}

library("ellipse")
par(mfrow = c(1,1))
plotcorr(cor(TR.2[,vars]))

################################################################################
### Regression                                                                                                                                                                                                                  ############
################################################################################

lm1.0 = lm(Evaluierung ~ 1, data = TR.2)                                        # "Nulllmodell - nur Intercept"
lm1   = lm(Evaluierung ~ Alter + Geschlecht + TeilnehmerKurs + Minderheit + Muttersprachler + Kurstyp, data = TR.2)
summary(lm1)

anova(lm1)                                                                      # Varianzzerlegung Modell
anova(lm1.0, lm1)                                                               # Vergleich der Modelle

lm1.2   = lm(Evaluierung ~ Alter*Geschlecht + TeilnehmerKurs + Minderheit + Muttersprachler + Kurstyp, data = TR.2)
summary(lm1.2)

anova(lm1,lm1.2)

#wenn Zeit
plot(lm1)


### glm - Binomialmodell, Logit link, läßt sich die Herkungt aus Kurscharakteristika herleiten
lm2.0 = glm(Minderheit ~  1, family = "binomial", data = TR.2)
lm2   = glm(Minderheit ~  Evaluierung + Aussehen + Alter + TeilnehmerKurs, family = "binomial", data = TR.2)
summary(lm2)

anova(lm2.0, lm2)                                                               # Hier wird nur die Devianz, aber kein p-Wert angegeben

# W´keit bei Intercept und alle anderen Koeffizienten = 0
exp(coefficients(lm2)[1]/(1+coefficients(lm2)[1]))

################################################################################
### Qualität                                                                                                                                                                                                                 ############
################################################################################

# Design
par(mfrow = c(2,2))
plot(mtcars$hp, mtcars$mpg)
plot(mtcars$hp, mtcars$mpg, xlab = "Pferdestärken", ylab = "Verbrauch in Meilen pro Gallone", main = "Verbrauch und Leistung von KFZs", sub = "Datensatz: mtcars, Quelle: library(base)")
plot(mtcars$hp, 235/mtcars$mpg, xlab = "Pferdestärken", ylab = "Verbrauch in Liter pro Km", main = "Verbrauch und Leistung von KFZs", sub = "Datensatz: mtcars, Quelle: library(base)")
plot(mtcars$hp, 235/mtcars$mpg, xlab = "Pferdestärken", ylab = "Verbrauch in Liter pro Km", main = "Verbrauch und Leistung von KFZs", sub = "Datensatz: mtcars, Quelle: library(base)",
     ylim = c(0, max(235/mtcars$mpg)), xlim = c(0,max(mtcars$hp)), pch = 16, cex = 0.8, col = "#00000060")


# Farben
par(mfrow = c(2,2))
plot(mtcars$hp, 235/mtcars$mpg, xlab = "Pferdestärken", ylab = "Verbrauch in Liter pro Km", main = "Verbrauch und Leistung von KFZs", sub = "Datensatz: mtcars, Quelle: library(base)",
     ylim = c(0, max(235/mtcars$mpg)), xlim = c(0,max(mtcars$hp)), pch = 16, cex = 0.8, col = "#00000060")

cols = c(colorRampPalette(c("gray","black"))(12))
colv = rep(NA,length(235/mtcars$mpg))
for (i in 1:length(unique(round(235/mtcars$mpg,0)))){
  t1 = sort(unique(round(235/mtcars$mpg,0)))[i]
  colv[(round(235/mtcars$mpg,0)) == t1] = cols[i]
}

plot(mtcars$hp, 235/mtcars$mpg, xlab = "Pferdestärken", ylab = "Verbrauch in Liter pro Km", main = "Verbrauch und Leistung von KFZs", sub = "Datensatz: mtcars, Quelle: library(base)",
     ylim = c(0, max(235/mtcars$mpg)), xlim = c(0,max(mtcars$hp)), pch = 16, cex = 0.8, col = colv)


cols = c(colorRampPalette(c("red","blue"))(12))
colv = rep(NA,length(235/mtcars$mpg))
for (i in 1:length(unique(round(235/mtcars$mpg,0)))){
  t1 = sort(unique(round(235/mtcars$mpg,0)))[i]
  colv[(round(235/mtcars$mpg,0)) == t1] = cols[i]
}

plot(mtcars$hp, 235/mtcars$mpg, xlab = "Pferdestärken", ylab = "Verbrauch in Liter pro Km", main = "Verbrauch und Leistung von KFZs", sub = "Datensatz: mtcars, Quelle: library(base)",
     ylim = c(0, max(235/mtcars$mpg)), xlim = c(0,max(mtcars$hp)), pch = 16, cex = 0.8, col = colv)

cols = c(colorRampPalette(c("green","red"))(12))
colv = rep(NA,length(235/mtcars$mpg))
for (i in 1:length(unique(round(235/mtcars$mpg,0)))){
  t1 = sort(unique(round(235/mtcars$mpg,0)))[i]
  colv[(round(235/mtcars$mpg,0)) == t1] = cols[i]
}

plot(mtcars$hp, 235/mtcars$mpg, xlab = "Pferdestärken", ylab = "Verbrauch in Liter pro Km", main = "Verbrauch und Leistung von KFZs", sub = "Datensatz: mtcars, Quelle: library(base)",
     ylim = c(0, max(235/mtcars$mpg)), xlim = c(0,max(mtcars$hp)), pch = 16, cex = 0.8, col = colv)

################################################################################
### Farben
################################################################################

## Farben1

par(mfrow = c(1,2))
plot(0,0, xlim = c(0,1), ylim = c(0,1), col = NA, xaxt = "n", yaxt = "n", xlab = "", ylab = "", sub = "Originalfarben")
polygon(c(0,0.5,0.5,0), c(0,0,0.5,0.5), col = "#ff0000")
polygon(c(0,0.5,0.5,0), c(0,0,0.5,0.5)+.5, col = "#00ff00")
polygon(c(0,0.5,0.5,0)+.5, c(0,0,0.5,0.5), col = "#0000ff")
polygon(c(0,0.5,0.5,0)+.5, c(0,0,0.5,0.5)+.5, col = "#000000")

plot(0,0, xlim = c(0,1), ylim = c(0,1), col = NA, xaxt = "n", yaxt = "n", xlab = "", ylab = "", sub = "0.21 R + 0.71 G + 0.07 B")
polygon(c(0,0.5,0.5,0), c(0,0,0.5,0.5), col = rgb(0.21 * 1,0.21 * 1,0.21 * 1) )
polygon(c(0,0.5,0.5,0), c(0,0,0.5,0.5)+.5, col = rgb(0.71 * 1,0.71 * 1,0.71 * 1))
polygon(c(0,0.5,0.5,0)+.5, c(0,0,0.5,0.5), col = rgb(0.07 * 1,0.07 * 1,0.07 * 1))
polygon(c(0,0.5,0.5,0)+.5, c(0,0,0.5,0.5)+.5, col = "#000000")

# Farben2
par(mfrow = c(1,2))
plot(0,0, xlim = c(0,1), ylim = c(0,1), col = NA, xaxt = "n", yaxt = "n", xlab = "", ylab = "", sub = "RGB-Farbraum")
polygon(c(0,0.5,0.5,0), c(0,0,0.5,0.5), col = "#ff0000")
polygon(c(0,0.5,0.5,0), c(0,0,0.5,0.5)+.5, col = "#00ff00")
polygon(c(0,0.5,0.5,0)+.5, c(0,0,0.5,0.5), col = "#0000ff")
polygon(c(0,0.5,0.5,0)+.5, c(0,0,0.5,0.5)+.5, col = "#000000")

plot(0,0, xlim = c(0,1), ylim = c(0,1), col = NA, xaxt = "n", yaxt = "n", xlab = "", ylab = "", sub = "HCL-Farbraum: Achtung, nicht direkt vergleichbar")
polygon(c(0,0.5,0.5,0), c(0,0,0.5,0.5), col = hcl(0,100,85))
polygon(c(0,0.5,0.5,0), c(0,0,0.5,0.5)+.5, col = hcl(120,100,85) )
polygon(c(0,0.5,0.5,0)+.5, c(0,0,0.5,0.5), col = hcl(240,100,85))
polygon(c(0,0.5,0.5,0)+.5, c(0,0,0.5,0.5)+.5, col = hcl(0,0,85))

# Farben3
par(mfrow = c(1,3))
plot(0,0, xlim = c(0,1), ylim = c(0,1), col = NA, xaxt = "n", yaxt = "n", xlab = "", ylab = "", sub = "HCL-Farbkontrast")
polygon(c(0,0.5,0.5,0), c(0,0,1,1), col = hcl(0,35,85))
polygon(c(0,0.5,0.5,0)+.5, c(0,0,1,1), col = hcl(180,35,85))

plot(0,0, xlim = c(0,1), ylim = c(0,1), col = NA, xaxt = "n", yaxt = "n", xlab = "", ylab = "", sub = "HCL-Farbkontrast")
polygon(c(0,0.33,0.33,0), c(0,0,1,1), col = hcl(0,35,85))
polygon(c(0,0.33,0.33,0)+.33, c(0,0,1,1), col = hcl(120,35,85))
polygon(c(0,0.33,0.33,0)+.66, c(0,0,1,1), col = hcl(240,35,85))

plot(0,0, xlim = c(0,1), ylim = c(0,1), col = NA, xaxt = "n", yaxt = "n", xlab = "", ylab = "", sub = "HCL-Farbkontrast")
polygon(c(0,0.25,0.25,0), c(0,0,1,1), col = hcl(0,35,85))
polygon(c(0,0.25,0.25,0)+.25, c(0,0,1,1), col = hcl(90,35,85))
polygon(c(0,0.25,0.25,0)+.50, c(0,0,1,1), col = hcl(180,35,85))
polygon(c(0,0.25,0.25,0)+.75, c(0,0,1,1), col = hcl(270,35,85))

# Farben4
par(mfrow = c(1,3))
plot(0,0, xlim = c(0,1), ylim = c(0,1), col = NA, xaxt = "n", yaxt = "n", xlab = "", ylab = "", sub = "HCL-Farbverlauf: Graustufen, Helligkeit")
polygon(c(0,0.25,0.25,0), c(0,0,1,1), col = hcl(0,0,80))
polygon(c(0,0.25,0.25,0)+.25, c(0,0,1,1), col = hcl(0,0,60))
polygon(c(0,0.25,0.25,0)+.50, c(0,0,1,1), col = hcl(0,0,40))
polygon(c(0,0.25,0.25,0)+.75, c(0,0,1,1), col = hcl(0,0,20))

plot(0,0, xlim = c(0,1), ylim = c(0,1), col = NA, xaxt = "n", yaxt = "n", xlab = "", ylab = "", sub = "HCL-Farbverlauf: Rot, Helligkeit")
polygon(c(0,0.25,0.25,0), c(0,0,1,1), col = hcl(0,60,80))
polygon(c(0,0.25,0.25,0)+.25, c(0,0,1,1), col = hcl(0,60,60))
polygon(c(0,0.25,0.25,0)+.50, c(0,0,1,1), col = hcl(0,60,40))
polygon(c(0,0.25,0.25,0)+.75, c(0,0,1,1), col = hcl(0,60,20))

plot(0,0, xlim = c(0,1), ylim = c(0,1), col = NA, xaxt = "n", yaxt = "n", xlab = "", ylab = "", sub = "HCL-Farbverlauf, Blau, Farbintensität")
polygon(c(0,0.25,0.25,0), c(0,0,1,1), col = hcl(240,00,85))
polygon(c(0,0.25,0.25,0)+.25, c(0,0,1,1), col = hcl(240,33,85))
polygon(c(0,0.25,0.25,0)+.50, c(0,0,1,1), col = hcl(240,66,85))
polygon(c(0,0.25,0.25,0)+.75, c(0,0,1,1), col = hcl(240,100,85))

# Farbblindheit
library(dichromat)                                                              # für Farbblindheit

par(mfrow = c(2,1))
plot(table(HairEyeData), col = c("saddlebrown","steelblue2","tan3","green3"), main = "Originalfarben")
plot(table(HairEyeData), col = dichromat(c("saddlebrown","steelblue2","tan3","green3")), main = "Wirkung bei Farbsehstörung" )


################################################################################
### Grafiken
################################################################################

par(mfrow = c(2,2))
for (i in 1:2){
  for (j in 1:2){
    if (i != j){
      mosaicplot(table(HairEyeData[,c(i,j)]), main = paste(names(HairEyeData)[i],"vs",names(HairEyeData)[j]))
    } else {
      barplot(table(HairEyeData[,c(i)]), ylim = c(0,350))
    }
  }
}

###
?axis

par(mfrow = c(1,1), mar = c(5,4,4,5)+.1)

boxplot(mtcars$mpg, mtcars$qsec, ylab = "Verbrauch in Meilen pro Gallone", main = "Darstellung des Verbrauchs und der Beschleunigung", xaxt = "n")

axis(1, c(1,2), labels = c("Verbrauch", "Beschleunigung"), las = 1)             # las zum drehen
mtext("Beschleunigung in Sek/Viertelmeile", 4, line = 3)
axis(1, c(1.5), labels = c(""), lwd = 2)
abline(v = 1.5, lty = "dotted", col = "gray")
axis(4, at = seq(10,30, by = 5))


par(mar = c(5, 4, 4, 2) + 0.1.)

###
?legend

par(mfrow = c(1,1), mar = c(5,4,4,5)+.1)

boxplot(mtcars$mpg, mtcars$qsec, ylab = "Verbrauch in Meilen pro Gallone",
         main = "Darstellung des Verbrauchs und der Beschleunigung", xaxt = "n",
         col = c("blue","orange"))

axis(1, c(1,2), labels = c("Verbrauch", "Beschleunigung"), las = 1)             # las zum drehen
mtext("Beschleunigung in Sek/Viertelmeile", 4, line = 3)
axis(1, c(1.5), labels = c(""), lwd = 2)
abline(v = 1.5, lty = "dotted", col = "gray")
axis(4, at = seq(10,30, by = 5))

legend("topright", pch = 15, pt.cex = 2.8, col = c("blue","orange"), legend = c("Verbrauch", "Beschleunigung"))


## Bsp von oben
par(mfrow = c(1,1))

plot(TR.2$Aussehen, TR.2$Evaluierung, col = c("blue","red")[TR.2$Minderheit], ylab = "Evaluierung", xlab = "Aussehen", pch = 16, cex = .7)
abline(coefficients(lm(Evaluierung ~ Aussehen, data = TR.2[TR.2$Minderheit == "nein",])), col = "blue")
abline(coefficients(lm(Evaluierung ~ Aussehen, data = TR.2[TR.2$Minderheit == "ja",])), col = "red")
legend("topleft", pch = c(15,15,NA,NA), lty = c(NA,NA,1,1), pt.cex = 2, col = rep(c("blue","red"),2), legend = rep(c("Nein", "Ja"),2) , title = "Minderheit")
