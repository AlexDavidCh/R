###### MODELO DE SCORING

#Data
library(readxl)
data <- read_excel("C:/Users/MariaRosa/Desktop/ALEX DEIIV/R/Modelo de scoring/German scoring/GermanCredit.xls")
str(data)

#Existen valores faltantes ?
library(naniar)
n_miss(data)
miss_var_summary(data)
miss_var_table(data)
vis_miss(data)

#Descripción de tipo de variables
library(dplyr)
data<-select(data,-c(`OBS#`))

data1<-data %>% mutate(RESPONSE=factor(RESPONSE, levels = c(0,1), labels = c("No","Si")),
                      FOREIGN=factor(FOREIGN,levels = c(0,1), labels = c("No","Si")),
                      TELEPHONE=factor(TELEPHONE,levels = c(0,1), labels = c("No","Si")),
                      NUM_DEPENDENTS=as.numeric(NUM_DEPENDENTS),
                      JOB=factor(JOB, levels = c(0,1,2,3), labels = c("Desempleado","No calificado","Calificado","Gerente-Autónomo")),
                      NUM_CREDITS=as.numeric(NUM_CREDITS),
                      OWN_RES=factor(OWN_RES,levels = c(0,1), labels = c("No","Si") ),
                      RENT=factor(RENT,levels = c(0,1), labels = c("No","Si") ),
                      OTHER_INSTALL=factor(OTHER_INSTALL,levels = c(0,1), labels = c("No","Si")),
                      AGE=as.numeric(AGE),
                      PROP_UNKN_NONE=factor(PROP_UNKN_NONE, levels=c(0,1),labels = c("No","Si")),
                      REAL_ESTATE=factor(REAL_ESTATE, levels = c(0,1), labels = c("No","Si")),
                      PRESENT_RESIDENT=factor(PRESENT_RESIDENT, levels= c(1,2,3,4),labels = c("0 a 1","1 a 2","2 a 3","mayor a 3")),
                      GUARANTOR=factor(GUARANTOR, levels = c(0,1), labels = c("No","Si")),
                      `CO-APPLICANT`=factor(`CO-APPLICANT`, levels = c(0,1), labels = c("No","Si")),
                      MALE_MAR_or_WID=factor(MALE_MAR_or_WID, levels = c(0,1), labels = c("No","Si")),
                      MALE_SINGLE=factor(MALE_SINGLE,levels = c(0,1), labels = c("No","Si")),
                      MALE_DIV=factor(MALE_DIV,levels = c(0,1), labels = c("No","Si")),
                      INSTALL_RATE=as.numeric(INSTALL_RATE),
                      EMPLOYMENT=factor(EMPLOYMENT, levels = c(0,1,2,3,4)),
                      SAV_ACCT=factor(SAV_ACCT),
                      AMOUNT=as.numeric(AMOUNT),
                      TYPE=as.factor(ifelse(RETRAINING==1,"RETRAINING",ifelse(EDUCATION==1,"EDUCATION",ifelse(`RADIO/TV`==1,"RADIO/TV",ifelse(FURNITURE==1,"FORNITURE",ifelse(USED_CAR==1,"USED.CAR","NEW.CAR")))))),
                      HISTORY=factor(HISTORY),
                      DURATION=as.numeric(DURATION),
                      CHK_ACCT=factor(CHK_ACCT,levels = c(0,1,2,3), labels = c( "0 DM","0 A 200DM","mayor a200"," No tiene")))

data1<-select(data1, -c("MALE_DIV", "MALE_SINGLE","MALE_MAR_or_WID","EDUCATION","RADIO/TV" ,"USED_CAR","NEW_CAR","FURNITURE","RETRAINING"))
Civil<-ifelse(data$MALE_DIV==1,"Male_div",ifelse(data$MALE_SINGLE,"M-single",ifelse(data$MALE_MAR_or_WID==1,"M.mar.wid","other")))
Civil<-as.factor(Civil)
data1<-data.frame(data1,Civil)

#resumen de la data
summary(data1)


# Tratamiento de datos atípicos.

## Division de la data en tipo de variables
library(PCAmixdata)
corte<-splitmix(data1)
dquanti<-as.data.frame(corte$X.quanti)
dcuali<-as.data.frame(corte$X.quali)

## Knn:Datos cuantitativos 

library(FNN)

Knn_nv <- function(Data, nv)
{
  X<-get.knn(data = as.data.frame(scale(Data)),k=nv)
  score<-rowMeans(X$nn.dist)
  index<-which.max(score)
  plot(score, col="blue")
  return(index)
}

Knn_nv(dquanti,5)

## LOF

library(dbscan)
lof<-lof(as.data.frame(scale(dquanti)),5)
which.max(lof)
summary(lof)

### limite para declarar atípicos, con el score del lof
lim<-quantile(lof,c(0.75,0.94))[2]
indexout<-which(lof>lim)

## MIXTO
library(HDoutliers)
out<- HDoutliers(data1, alpha = 0.5)
out
plotHDoutliers(data1, out)
plotHDoutliers

# Eliminación de la observación atípica

data1<-data1[-indexout,]
dcuali<-dcuali[-indexout,]
dquanti<-dquanti[-indexout,]
# Preprocesamiento (Binning)

## Corelaciones


### Correlaciones Cuantitativas
library(dplyr)
library(corrplot)
matriz<-cor(dquanti)
corrplot(matriz)


data %>%
  filter(RESPONSE == "Si") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

X11()
data %>%
  select(AGE, AMOUNT,DURATION, INSTALL_RATE,NUM_CREDITS,NUM_DEPENDENTS) %>%
  gather(metric, value) %>%
  ggplot(aes(value, fill = metric)) +
  geom_density(show.legend = FALSE) +
  facet_wrap(~ metric, scales = "free")

#Correlaciones Cualitativas

#Ho: las variables son independientes
#Ha: las variables son dependientes

chisq.test(dcuali$RESPONSE,dcuali$SAV_ACCT, correct=FALSE)
chisq.test(dcuali$RESPONSE,dcuali$EMPLOYMENT, correct=FALSE)
chisq.test(dcuali$RESPONSE,dcuali$CO.APPLICANT, correct=FALSE)
chisq.test(dcuali$RESPONSE,dcuali$GUARANTOR, correct=FALSE)
chisq.test(dcuali$RESPONSE,dcuali$PRESENT_RESIDENT, correct=FALSE)
chisq.test(dcuali$RESPONSE,dcuali$REAL_ESTATE, correct=FALSE)
chisq.test(dcuali$RESPONSE,dcuali$PROP_UNKN_NONE, correct=FALSE)
chisq.test(dcuali$RESPONSE,dcuali$OTHER_INSTALL, correct=FALSE)
chisq.test(dcuali$RESPONSE,dcuali$RENT, correct=FALSE)
chisq.test(dcuali$RESPONSE,dcuali$TYPE, correct=FALSE)
chisq.test(dcuali$RESPONSE,dcuali$Civil, correct=FALSE)


# Análisis descriptivo

library(plotly)
library(ggplot2)

BP<-plot_ly(type = 'box')
BP<-BP %>% add_boxplot(y=data$AMOUNT,name="AMMOUNT",boxpoints=TRUE)
BP

Hst<-plot_ly( x=data$AMOUNT, type='histogram' )
Hst %>% layout(barmode = "overlay",
         title = "Histogram of Amount",
         xaxis = list(title = "Amount",
                      zeroline = TRUE),
         yaxis = list(title = "Count",
                      zeroline = TRUE))

# Descriptivo cualitativo

p1 <- ggplot(data,aes(x=AGE,fill=RESPONSE))+
  geom_histogram()+
  labs(y="Apariciones",title="AGE")
p1

p2 <- ggplot(data,aes(x=AMOUNT,fill=RESPONSE))+
  geom_histogram()+
  labs(y="Apariciones",title="AGE")
p2


p3 <- ggplot(data1,aes(x=TYPE,fill=RESPONSE))+
  geom_bar()+
  labs(y="Apariciones",title="Fin de crédito")
p3

p4 <- ggplot(data1, aes(x=JOB, fill=RESPONSE))+
  geom_bar()+
  labs(y="Apariciones",title="Condición empleado")
p4

p5<-ggplot(data1, aes(x=CHK_ACCT, fill=RESPONSE))+
  geom_bar()+
  labs(y="Apariciones",title = "Cuenta en el banco")
p5

# Primeras ideas para seleccionar variables

## calcular el WOE e IV


library(sd)

library(Information)
str(data1$RESPONSE)

Y<-as.numeric(data1$RESPONSE)
str(Y)
Y<-ifelse(Y==1,"0","1")
Y<-as.numeric(Y)
aux<-select(data1,-"RESPONSE")
aux<-data.frame(aux,Y)
IV<-create_infotables(data=aux, y="Y", bins=10, parallel=FALSE)
IV$Summary
print(head(IV$Summary,10), row.names=FALSE)

library(gridExtra)
grid.table(IV$Summary, rows=NULL)

Variable         IV
CHK_ACCT 0.67536894
DURATION 0.32209412
HISTORY 0.29158453
SAV_ACCT 0.20902530
TYPE 0.17477467
AGE 0.10744065
EMPLOYMENT 0.09598798
AMOUNT 0.08343887
OTHER_INSTALL 0.07990917
OWN_RES 0.07909583

#División de la data, train y test.
library(rsample)
set.seed(123)
corte<- initial_split(data1, prop = 0.7, strata = "RESPONSE")
train <- training(corte)
test  <- testing(corte)

table(train$RESPONSE) %>% prop.table()
table(test$RESPONSE) %>% prop.table()

## Solo para ver si cambia algo
cort<- initial_split(aux, prop = 0.7, strata = "Y")
aux.train <- training(cort)
IV <- create_infotables(data=aux.train,y="Y")
print(head(IV$Summary, 10), row.names = FALSE)

#### SMBInning woe e Iv , 2da forma.
library(data.table)
library(compareGroups)
options(scipen=10)
library(smbinning)


re=smbinning(df=data1, y="RESPONSE",x="CHK_ACCT")
re
?smbinning
str(smbsimdf1)
re$ivtable
re[2]
re$ctree
plot(re$ctree)

# Seleccion de variables por árboles de clasificación
## Arbol 1

library(data.table)
library(rpart)
arb <- rpart(RESPONSE ~.,data=data1)
library(rpart.plot)
X11()
rpart.plot(arb)
summary(arb)

Variable importance
CHK_ACCT           AMOUNT          HISTORY         DURATION             TYPE 
26               12               11               10               10 
SAV_ACCT PRESENT_RESIDENT              AGE          OWN_RES   PROP_UNKN_NONE 
9                4                3                3                2 
JOB             RENT       EMPLOYMENT     INSTALL_RATE      NUM_CREDITS 
2                2                2                1                1 
Civil        GUARANTOR 
1                1

###ARBOL 2

library(party)
arb2<-ctree(RESPONSE ~., data=data1)
X11()
plot(arb2)
arb2

# Modelo logit
relevel(train$RESPONSE,ref = "Si")
M1<-glm(formula = RESPONSE ~. ,data=train, family = binomial(link = "logit"))
summary(M1)

y_fit<-as.factor(ifelse(M1$fitted.values>0.5,"Si","No"))
confusionMatrix(train$RESPONSE,y_fit, positive = "Si")

M_IV<-glm(formula = RESPONSE ~ CHK_ACCT + AMOUNT + HISTORY + DURATION + TYPE
          + SAV_ACCT +AGE+ EMPLOYMENT + AMOUNT + OTHER_INSTALL + OWN_RES , data=data1, family = binomial(link = "logit"))
summary(M_IV)
M_arb<-glm(formula = RESPONSE ~ CHK_ACCT + AMOUNT + HISTORY + DURATION + TYPE
           + SAV_ACCT + PRESENT_RESIDENT + AGE + OWN_RES + PROP_UNKN_NONE+ JOB+ RENT
           + EMPLOYMENT + INSTALL_RATE + NUM_CREDITS + Civil + GUARANTOR ,data=data1, family = binomial(link="logit") )
summary(M_arb)

model<-glm(formula = RESPONSE ~ CHK_ACCT  + DURATION + TYPE
           + SAV_ACCT  + AGE + OWN_RES + PROP_UNKN_NONE+ RENT
           + INSTALL_RATE + Civil + GUARANTOR -1,data=data1, family = binomial(link="logit") )

summary(model)

#  stepwise

modelo<-glm(RESPONSE ~., data=train, family = binomial())

library(MASS)

step(modelo) #stepwise
stepAIC(modelo,direction="forward")

M_step<-glm(formula = RESPONSE ~ CHK_ACCT + DURATION + HISTORY + AMOUNT + 
      SAV_ACCT + EMPLOYMENT + INSTALL_RATE + GUARANTOR + PRESENT_RESIDENT + 
      OTHER_INSTALL + RENT + FOREIGN + TYPE + Civil, family = binomial(), 
    data = train)

summary(M_step)

#ELIMINACIÓN de VARIABLE y actualización del modelo

drop1(M_arb, test = 'LRT')
m2 <- update(M_arb, .~.-PRESENT_RESIDENT -JOB -EMPLOYMENT)
summary(m2)

# Prediccion

predicion<-predict(M_arb, newdata = train, type = "response")
predicted_class<-ifelse(predicion>0.5,"Si","No") 
predicted_class<-as.factor(predicted_class)

# Matriz de confusion

library(caret)
sum(ifelse(train$RESPONSE=="No",1,0))
A<-confusionMatrix(train$RESPONSE,predicted_class)

#Curva ROC

library(pROC)
plot.roc(train$RESPONSE,predicion,main="Confidence interval of a threshold", percent=TRUE,
         ci=TRUE, of="thresholds",
         thresholds="best", 
         print.thres="best")

#'### BONDAD DE AJUSTE
library(broom)
# Opcion 1
broom::glance(M_arb) %>%
  summarize(pR2 = 1 - deviance/null.deviance)

# Opcion 2
library(sigr)
R2 <- wrapChiSqTest(M_arb)
R2$pseudoR2


# TEORÍA DE LA DESICIÓN
P<-as.matrix(A$table)
VP<-P[1,1]
FP<-P[1,2]
FN<-P[2,1]
VN<-P[2,2]


#porcentaje
vpp<-VP/sum(VP+FP)
fpp<-FP/sum(VP+FP)
fnp<-FN/sum(FN+VN)
vnp<-VN/sum(FN+VN)

#matriz P
P<-matrix(c(vpp,fnp,fpp,vnp),nrow=2,ncol=2)

#interes
#links
https://rpubs.com/luisgasco/casopracticoMarketing