library(agricolae)
library(data.table)

Varietas<-paste("V",seq(3),sep="")
Nitrogen<-paste("N",seq(5),sep="")

trts<-expand.grid(Varietas,Nitrogen)
trts$Perlakuan<-paste(trts$Var1,trts$Var2,sep="")

trts$Perlakuan

bagan<-design.crd(trts$Perlakuan,r=3,serie=0)$book


data<-read.delim("clipboard")
longdata<-reshape2::melt(id.vars=c("Varietas","Ulan"),
                         value.vars=(paste(N,seq(5),sep="")),
                         value.name = "Tinggi",
                          data)
colnames(longdata)[3]<-"Nitrogen"
colnames(longdata)[3]
longdata$Varietas<-as.factor(longdata$Varietas)

contrasts(longdata$Varietas)<-cbind(c(2,-1,-1),c(0,1,-1))
contrasts(longdata$Nitrogen)<-cbind(c(4,-1,-1,-1,-1),c(1,1,0,-1,-1),
                                    c(1,-1,0,0,0),c(0,0,0,1,-1))

longdata$Tinggi<-as.numeric(longdata$Tinggi)
summary(aov(Tinggi~Varietas*Nitrogen,longdata))


summary(aov(Tinggi~Varietas*Nitrogen,longdata),
        split=list(Varietas=list("1 vs Lainnya"=1, "2 vs 3"=2),
                   Nitrogen=list("1 vs lainnya"=1, "1, 2 vs 4,5"=2,
                                 "1 vs 2"=3, "4 vs 5"=4)))
par(mfrow=c(2,2))
plot(aov(Tinggi~Varietas*Nitrogen,longdata))

library(phia)
mod2<-lm(Tinggi~Varietas*Nitrogen,data=longdata)
im2=interactionMeans(mod2)
knitr::kable(head(im2),n=5) # tabel

im2
plot(im2) # plot

library(multcomp)
library(emmeans)
marginal = emmeans(aovFakt,~ Varietas:Nitrogen)

cld(marginal,alpha=0.05,Letters=letters, adjust="sidak")



aovFakt<-aov(Tinggi~Varietas*Nitrogen,longdata)
library(lmtest)
ks.test(aovFakt$residuals,pnorm, mean(aovFakt$residuals), sd(aovFakt$residuals))
shapiro.test(x=aovFakt$residuals)
ad.test(aovFakt)

#normal - kec uji lilliefors

library(nortest)
lillie.test(aovFakt$residuals)
ad.test(aovFakt$residuals)

lillie.test(aovFakt$residuals)

bartlett.test(Tinggi ~ Nitrogen, data = longdata)
bartlett.test(Tinggi ~ Varietas, data = longdata)
bartlett.test(Tinggi ~ Nitrogen:Varietas, data = longdata)

#di nitrogen ragamnya tidak homogen

library(car)
leveneTest(Tinggi~Nitrogen, data=longdata, center = median) #default (Brown Forsythe)
leveneTest(Tinggi~Varietas, data=longdata, center = median) # Levene (mean)

bptest(aovFakt)

leveneTest(Produksi~Perlakuan, data=DataRAKL, center = mean,trim=0.1) # Levene (trimmed)

library(snpar)
runs.test(aovFakt$residuals,exact=T,alternative ="two.sided") #exact=p-value tepat

#taraf 5% saling bebas - pengacakan itu telah dilakukan dengan benar