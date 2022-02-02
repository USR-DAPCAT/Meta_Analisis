########################################
#[02.02.2022]
########################################
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
packages <-c("metafor","esc","readxl","esc","dummies")

ipak<-function(pkg){
  new.pkg<-pkg[!(pkg%in%installed.packages()[,"Package"])]
  if (length(new.pkg))
    install.packages(new.pkg,dependencies=TRUE)
  sapply(pkg,require, character.only=TRUE)}

ipak(packages)

library("meta") 
library("dplyr")
library("ggplot2")
library("metafor")
library("esc")
library("dummies")

#
########################################
#
#
citation(package = "dplyr", lib.loc = NULL, auto = NULL)
citation(package = "meta", lib.loc = NULL, auto = NULL)
citation(package = "ggplot2", lib.loc = NULL, auto = NULL)
citation(package = "metafor", lib.loc = NULL, auto = NULL)
citation(package = "esc", lib.loc = NULL, auto = NULL)
#
#
#########################################
set.seed(888)
#-------------------------------------------------------------------------------#
event   <-rep(0:1,60)
Freq    <-abs(round(rnorm(120,50,20)))
treat   <-rep(rep(0:1,30),each = 2)
study   <-rep(1:30,each = 4)
char    <-rep(rbinom(30,1,0.5),each = 4)
meta    <-c(rep(1,16),rep(2,20),rep(3,24),rep(4,28),rep(5,32))
#-------------------------------------------------------------------------------#
data<-data.frame(meta,study,char,treat,event,Freq)
#str(data)

data



data.expand<-data[rep(rownames(data),data$Freq),1:(ncol(data)-1)]
#str(data.expand)



data.dummy<-dummy.data.frame(data.expand,names = c("meta","study"))





model.logit<-glm(event~.+treat:char
                        +treat:meta2
                        +treat:meta3
                        +treat:meta4
                        +treat:meta5-char-study1-meta1-meta2-meta3-meta4-meta5,data = data.dummy,family = binomial)

summary(model.logit)

summary(model.logit)$coefficients["char:treat",]

exp(summary(model.logit)$coefficients["char:treat",1:2])

confint(model.logit)["char:treat",]

exp(confint(model.logit)["char:treat",])



data1<-data.expand[data.expand$meta == 1,-1]

data2<-data.expand[data.expand$meta == 2,-1]
data2$study<-data2$study-4


data3<-data.expand[data.expand$meta == 3,-1]
data3$study<-data3$study-9

data4<-data.expand[data.expand$meta == 4,-1]
data4$study<-data4$study-15

data5<-data.expand[data.expand$meta == 5,-1]
data5$study<-data5$study-22


data1.dummy<-dummy.data.frame(data1,names = c("study"))[,-1]
data2.dummy<-dummy.data.frame(data2,names = c("study"))[,-1]
data3.dummy<-dummy.data.frame(data3,names = c("study"))[,-1]
data4.dummy<-dummy.data.frame(data4,names = c("study"))[,-1]
data5.dummy<-dummy.data.frame(data5,names = c("study"))[,-1]


model1<-glm(event~.+treat:char-char,data = data1.dummy,family = binomial)
model2<-glm(event~.+treat:char-char,data = data2.dummy,family = binomial)
model3<-glm(event~.+treat:char-char,data = data3.dummy,family = binomial)
model4<-glm(event~.+treat:char-char,data = data4.dummy,family = binomial)
model5<-glm(event~.+treat:char-char,data = data5.dummy,family = binomial)

summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)




ROR<-exp(c(model1$coefficients["char:treat"],model2$coefficients
            ["char:treat"],model3$coefficients["char:treat"],model4$coefficients
            ["char:treat"],model5$coefficients["char:treat"]))

SE<-exp(c(summary(model1)$coefficients["char:treat",2],summary
           (model2)$coefficients["char:treat",2],summary(model3)$coefficients
           ["char:treat",2],summary(model4)$coefficients["char:treat",2],summary
           (model5)$coefficients["char:treat",2]))

reslt<-data.frame(cbind(ROR,SE,meta = 1:5))



meta<-metagen(reslt$ROR,reslt$SE,studlab = reslt$meta,sm = 'ROR')
forest(meta)



#1# .	Death ~ Macrovascular disease (unadjusted)
#2# .	Death ~ Macrovascular disease + Age + Sex
#3# .	Death ~ Macrovascular disease + Age + Sex + Hypertension + Type of Diabetes + Microvascular disease 
#4# .	Death ~ Macrovascular disease + Age + Sex + Hypertension + Type of Diabetes + Microvascular disease + Ethnic group + BMI 


#falta canviar SE-France
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#
#
ROR_covid1<-c(2.64881,2.49,2.06)
SE_covid1<-c((0.6495893),(0.141),exp(0.09718))
META_covid1 = c("UK","France","Spain")
res<-data.frame(cbind(ROR_covid1,SE_covid1))
#
ROR_covid1<-c(0.974110483,0.91228271,0.722705983)
SE_covid1<-c((0.280447825),(0.141),(0.09718))
META_covid1 = c("UK","France","Spain")
res<-data.frame(cbind(ROR_covid1,SE_covid1))


meta2<-metagen(TE=res$ROR_covid1,
               seTE=res$SE_covid1,
               studlab = META_covid1,
               sm = 'OR',
               title = "Associations of macrovascular complications with death in patients with diabetes hospitalized for Coronavirus disease(COVID-19)")

summary(meta2)
forest(meta2)


qnorm(1-.05/2)


#
#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#
#
ROR_covid2<-c(2.062049,1.70,1.60)
SE_covid2<-c(exp(0.5323558),exp(0.5),exp(0.472713))
META_covid2 = c("UK","France","Spain")
res2<-data.frame(cbind(ROR_covid2,SE_covid2))
#
meta2<-metagen(res2$ROR_covid2,res2$SE_covid2,studlab = META_covid2,sm = 'ROR')
forest(meta2)
#
#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#
#
ROR_covid3<-c(2.184311,1.45,1.56)
SE_covid3<-c(exp(0.5779912),exp(0.5),exp(0.444231))
META_covid3 = c("UK","France","Spain")
res3<-data.frame(cbind(ROR_covid3,SE_covid3))
#
meta3<-metagen(res3$ROR_covid3,res3$SE_covid3,studlab = META_covid3,sm = 'ROR')
forest(meta3)
#
#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#



