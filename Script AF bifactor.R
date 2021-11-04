## II JORNADA QUANTITATIVA
## Slides curso "Modelos bifator"
## Prof Dr Nelson Hauck

### loading packages
library(psych)
library(GPArotation)
library(lavaan)
library(dplyr)

### reading data
data("Holzinger.9")
View(Holzinger.9)

### SCHMID-LEIMAN TRANSFORMATION
fit<- schmid(Holzinger.9, nfactors = 3, fm = "minres", rotate="oblimin")
# Add "cor="poly" if data are categorical
print(fit,sort= FALSE)
omega(Holzinger.9)

### EFA BIFACTOR JENNRICH-BENTLER
fit<-fa(Holzinger.9, nfactors = 4, fm = "minres", rotate="bifactor")
# Add "cor="poly" if data are categorical
print(fit,sort= FALSE)

###EFA BIFACTOR TARGET ROTATION
###Specifying a theoretical target matrix 
Targ <- make.keys(9,list(fg=1:9,f1=1:3,f2=4:6,f3=7:9)) 
Targ <- scrub(Targ,isvalue=1)  #fix the 0s, allow the NAs to be estimated
Targ <- list(Targ)
Targ

fit_holz<-fa(Holzinger.9,4,rotate="TargetQ",Target=Targ)
print(fit_holz,sort= FALSE)

