## II JORNADA QUANTITATIVA
## Slides curso "Modelos fatoriais de intercepto randômico"
## Prof Dr Nelson Hauck

### loading packages
library(psych)
library(GPArotation)
library(lavaan)
library(dplyr)

####DATA
#Generate simulated data using the following factor model (one acquiescence factor, three trait factors)
#Such data simulate a situation in which the researcher has a scale containing six items
#that measure a given trait, with three positive and three negative items 

model=
'AQ =~ .3*V1+.3*V2+.3*V3+.3*V4+.3*V5+.3*V6
trait =~ .5*V1+.5*V2+.5*V3+(-.5)*V4+(-.5)*V5+(-.5)*V6
AQ~~.0*trait
'
data1<-simulateData(model = model, model.type = "sem", standardized=TRUE,sample.nobs = 1000L)
data1_cut<-apply(data1,2,function (x) cut(x,breaks=c(5), labels=c("1","2","3","4","5")))
data1_cut<-as.data.frame(apply(data1_cut,2,as.numeric))

##FIT EXPLORATORY MODELS TO UNCOVER THE FACTOR STRUCTURE
fa.parallel(data1_cut,cor="poly")
VSS(data1_cut,cor="poly")

fit_1f<-fa(data1_cut, nfactors = 1, fm = "wls", cor="poly",rotate="oblimin")
print(fit_1f,sort= FALSE)
fit_2f<-fa(data1_cut, nfactors = 2, fm = "wls", cor="poly",rotate="oblimin")
print(fit_2f,sort= FALSE)

##FIT CFA MODELS
CFA_1f=
  'trait =~ V1+V2+V3+V4+V5+V6
'
fit_CFA_1f <- lavaan::cfa(CFA_1f, data = data1_cut,
                          ordered = names(data1_cut))
summary(fit_CFA_1f, standardized=TRUE,fit.measures=TRUE)


CFA_2f=
'trait1 =~ V1+V2+V3
trait2 =~ V4+V5+V6
'
fit_CFA_2f <- lavaan::cfa(CFA_2f, data = data1_cut,
                                ordered = names(data1_cut))
summary(fit_CFA_2f, standardized=TRUE,fit.measures=TRUE)


##FIT RANDOM INTERCEPT FACTOR MODELS
model_detection=
  'AQ =~ .1*V1+.1*V2+.1*V3+.1*V4+.1*V5+.1*V6
trait1 =~ V1+V2+V3
trait2 =~ V4+V5+V6
AQ~~AQ
AQ~~.0*trait1
AQ~~.0*trait2
'
fit_RI_detection <- lavaan::cfa(model_detection, data = data1_cut,
                                ordered = names(data1_cut))
summary(fit_RI_detection, standardized=TRUE,fit.measures=TRUE)


model_RI=
'AQ =~ .1*V1+.1*V2+.1*V3+.1*V4+.1*V5+.1*V6
trait =~ V1+V2+V3+V4+V5+V6
AQ~~AQ
AQ~~.0*trait
'
fit_RI <- lavaan::cfa(model_RI, data = data1_cut,
                      ordered = names(data1_cut))
summary(fit_RI, standardized=TRUE,fit.measures=TRUE)



