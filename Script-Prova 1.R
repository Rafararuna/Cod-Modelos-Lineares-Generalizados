############################### PROVA 1 ####################################

##### DESCRIÇÃO #####

## Varável resposta:

### Y = 1 : se a doença foi diagnosticada
### Y = 0 ; se a doença não foi diagnosticada

## Variáveis explicativas:

### X1 : corresponde à idade em anos completos

### X2 e X3 : correspondem a 3 categorias de classe socioeconômica
###           X2 = 0 e X3 = 0 : alta
###           X2 = 1 e X3 = 0 : média
###           X2 = 0 e X3 = 1: baixa

### X4 : corresponde ao setor da cidade
###      X4 = 0 : setor 1
###      X4 = 1 : setor 2

## A probabilidade de ocorrència da doença (Y) deve ser ajustado a 
## um Modelo Bernoulli

## Considere uma subamostra aleatória de 50 observações para constituir a base 
## de treinamento do modelo



##### BANCO DE DADOS #####

library(tidyverse)

base.total <- read.table(file.choose(), header = F)
colnames(base.total) <- c("ID", "x1", "x2", "x3", "x4", "Y")

base.total$V.CE <- ifelse(base.total$x2 == 0 & base.total$x3 == 0, "alta",
                        ifelse(base.total$x2 == 1 & base.total$x3 == 0, "média",
                               ifelse(base.total$x2 == 0 & base.total$x3 == 1, 
                                      "baixa", NA)))
base.total$V.CE <- as.factor(base.total$V.CE)

base.total$V.SETOR <- ifelse(base.total$x4 == 0, "setor 1",
                            ifelse(base.total$x4 == 1, "setor 2", NA))
base.total$V.SETOR <- as.factor(base.total$V.SETOR)

base.total$Y.factor <- factor(as.factor(base.total$Y),levels=c("0","1"),
                              labels=c("Não tem doença","Tem doença"))

set.seed(180026798) # colocar o número da matrícula
amostra <- sample(1:98,size = 50, replace = F)
base.treino <- base.total[amostra,]
base.valida <- base.total[-amostra,]
xtable(head(base.total))

##### ANALISE DESCRITIVA #####

# Analise descritiva para as variaveis qualitativas

v.doenca <- base.treino$Y # diagnostico da doença (numerico)
v.doenca.fac <- base.treino$Y.factor # diagnostico da doença (fator)
v.ce <- base.treino$V.CE  # classe economica
v.setor <- base.treino$V.SETOR # setor
n <- length(v.doenca)

## por classe economica
d_doen_ce <- data.frame(v.doenca,v.ce)
rdoen1 <- ddply(d_doen_ce,.(v.ce),summarise,media=mean(v.doenca),
                dp=sqrt(var(v.doenca)),vari=var(v.doenca),
                cv=100*((sqrt(var(v.doenca))/mean(v.doenca))),
                n=length(v.doenca))
xtable(rdoen1)

## por setor
d_doen_set <- data.frame(v.doenca,v.setor)
rdoen2 <- ddply(d_doen_set,.(v.setor),summarise,media=mean(v.doenca),
                dp=sqrt(var(v.doenca)),vari=var(v.doenca),
                cv=100*((sqrt(var(v.doenca))/mean(v.doenca))),
                n=length(v.doenca))
xtable(rdoen2)

## por ce x setor
d_doen <- data.frame(v.doenca,v.ce,v.setor)
rdoen3 <- ddply(d_doen,.(v.ce,v.setor),summarise,media=mean(v.doenca),
                dp=sqrt(var(v.doenca)),vari=var(v.doenca),
                cv=100*((sqrt(var(v.doenca))/mean(v.doenca))),
                n=length(v.doenca))
xtable(rdoen3)


## tabelas de contingência
t1 <- table(v.doenca.fac,v.ce)
t2 <- table(v.doenca.fac,v.setor)
ta1 <- 100*(t(t1)/(apply(t1,2,sum)))
ta2 <- 100*(t(t2)/(apply(t2,2,sum)))
xtable(rbind(ta1,ta2))


### Gráficos das proporções

#### Por CE
par(mfrow=c(2,1))
plot(c(ta1[1,],ta1[2,], ta1[3,]),axes=FALSE,ylab="Proporções",
     xlab="Diagnóstico por CE",cex=1.2,cex.axis=1.2,cex.lab=1.2,pch=19)
axis(1,c(1,2,3,4,5,6),labels=c("alta-não tem","alta-tem","baixa-não tem",
                               "baixa-tem", "média-não tem","média-tem"))
axis(2,at=seq(round(min(ta1),2),round(max(ta1),2),2))


#### Por Setor
plot(c(ta2[1,],ta2[2,]),axes=FALSE,ylab="Proporções",
     xlab="Diagnóstico por Setor",cex=1.2,cex.axis=1.2,cex.lab=1.2,pch=19)
axis(1,c(1,2,3,4),labels=c("setor 1-não tem","setor 1-tem","setor 2-não tem",
                           "setor 2-tem"))
axis(2,at=seq(round(min(ta2),2),round(max(ta2),2),2))


# Gráfico de perfis
medias <- rdoen3$media
mediasa <- medias
dp <- rdoen3$dp
vn <- rdoen3$n

par(mfrow=c(1,1))
ez <- qnorm(0.975)
plot(medias[1:2],axes=FALSE,ylim=c(-0.1,1),cex.lab=1.5,xlab="Setor",
     ylab="Proporção de diagnóstico")
axis(2,cex.axis=1.2)
axis(1,1:2,c("Setor 1","Setor 2"),cex.axis=1.2)
plotCI(medias[1:2],liw=ez*sqrt(medias[1:2]*(1-medias[1:2]))/sqrt(vn[1:2]),
       uiw=ez*sqrt(medias[1:2]*(1-medias[1:2]))/sqrt(vn[1:2]),pch=19,add=TRUE,
       cex.lab=1.5,slty=1,lwd=2,col=4,cex=1.2)
lines(medias[1:2],lwd=2,col=4)
plotCI(medias[3:4],liw=ez*sqrt(medias[3:4]*(1-medias[3:4]))/sqrt(vn[3:4]),
       uiw=ez*sqrt(medias[3:4]*(1-medias[3:4]))/sqrt(vn[3:4]),pch=23,add=TRUE,
       cex.lab=1.5,slty=1,lwd=2,col=2,pt.bg=2,cex=1.2)
lines(medias[3:4],col=2,lwd=2)
plotCI(medias[5:6],liw=ez*sqrt(medias[5:6]*(1-medias[5:6]))/sqrt(vn[5:6]),
       uiw=ez*sqrt(medias[5:6]*(1-medias[5:6]))/sqrt(vn[5:6]),pch=25,add=TRUE,
       cex.lab=1.5,slty=1,lwd=2,col=3,pt.bg=3,cex=1.2)
lines(medias[5:6],col=3,lwd=2)
legend(1.1,0.8,col=c(4,2,3),lwd=c(2,2,2),pch=c(19,23,25),pt.bg=c(2,2,3),
       legend=c("alta","baixa", "média"),bty="n",cex=1.5)


# Análise descritiva para a variavel quantitativa:

## VARIAVEL x1:

# boxplots
par(mfrow=c(2,2))
boxplot(base.treino$x1~v.doenca,cex=1.2,cex.lab=1.2,cex.axis=1.2,
        xlab="Diagnóstico", ylab="Idade",names=c("não tem doença","tem doença"))
boxplot(base.treino$x1~v.doenca*v.ce,cex=1.2,cex.lab=1.2,cex.axis=1.2,
        xlab="Diagnóstico", ylab="Idade",names=c("não tem-alta","tem-alta",
                                                 "não tem-baixa","tem-baixa", 
                                                 "não tem-média","tem-média"))
boxplot(base.treino$x1~v.doenca*v.setor,cex=1.2,cex.lab=1.2,cex.axis=1.2,
        xlab="Diagnóstico", ylab="Idade",names=c("não tem-setor 1",
                                                 "tem-setor 1",
                                                 "não tem-setor 2",
                                                 "tem-setor 2"))
boxplot(base.treino$x1~v.doenca*v.ce*v.setor,cex=1.2,cex.lab=1.2,cex.axis=1.2,
        xlab="Diagnóstico", ylab="Idade",names=c("NT-A-S1","T-A-S1","NT-B-S1",
                                                 "T-B-S1","NT-M-S1", "T-M-S1",
                                                 "NT-A-S2","T-A-S2","NT-B-S2",
                                                 "T-B-S2","NT-M-S2", "T-M-S2"))


# Gráficos de dispersão
aux <- data.frame(cbind(v.doenca,as.factor(base.treino$x1),v.ce))
obsa1 <- c(v.doenca[v.ce=="alta" & v.setor=="setor 1"])
obsa2 <- c(v.doenca[v.ce=="alta" & v.setor=="setor 2"])
obsb1 <- c(v.doenca[v.ce=="baixa" & v.setor=="setor 1"])
obsb2 <- c(v.doenca[v.ce=="baixa" & v.setor=="setor 2"])
obsm1 <- c(v.doenca[v.ce=="média" & v.setor=="setor 1"])
obsm2 <- c(v.doenca[v.ce=="média" & v.setor=="setor 2"])

par(mfrow=c(3,2))
plot(base.treino$x1[v.ce=="alta" & v.setor=="setor 1"],obsa1,pch=19,cex=1.2,
     cex.axis=1.2,cex.lab=1.2,xlab="Idade",ylab="Diagnóstico",
     main="alta-setor 1")
plot(base.treino$x1[v.ce=="alta" & v.setor=="setor 2"],obsa2,pch=19,cex=1.2,
     cex.axis=1.2,cex.lab=1.2,xlab="Idade",ylab="Diagnóstico",
     main="alta-setor 2")
plot(base.treino$x1[v.ce=="baixa" & v.setor=="setor 1"],obsb1,pch=19,cex=1.2,
     cex.axis=1.2,cex.lab=1.2,xlab="Idade",ylab="Diagnóstico",
     main="baixa-setor 1")
plot(base.treino$x1[v.ce=="baixa" & v.setor=="setor 2"],obsb2,pch=19,cex=1.2,
     cex.axis=1.2,cex.lab=1.2,xlab="Idade",ylab="Diagnóstico",
     main="baixa-setor 2")
plot(base.treino$x1[v.ce=="média" & v.setor=="setor 1"],obsm1,pch=19,cex=1.2,
     cex.axis=1.2,cex.lab=1.2,xlab="Idade",ylab="Diagnóstico",
     main="média-setor 1")
plot(base.treino$x1[v.ce=="média" & v.setor=="setor 2"],obsm2,pch=19,cex=1.2,
     cex.axis=1.2,cex.lab=1.2,xlab="Idade",ylab="Diagnóstico",
     main="média-setor 2")



##### AJUSTE APENAS COM VARIAVEIS EXPLICATIVAS CATEGORICAS #####

# Primeiramente:

## Pacotes
library(xtable)
library(plotrix)
library(plyr)
op <- options()

## Diagnósstico e envelope para o modelo Bernoulli
source("https://www.ime.unicamp.br/~cnaber/diag_Bern.r")
source("https://www.ime.unicamp.br/~cnaber/envel_Bern.r")

## Funções de Análise do Desvio e do teste CB=M
source("https://www.ime.unicamp.br/~cnaber/AnaDesvTestCBM.r")


# Ajuste dos modelos completos (comparação dos modelos)

## Logito
fit.model <- result <- glm(v.doenca~v.ce+v.setor+v.ce*v.setor,
                           family=binomial(link="logit"))
g <- summary(fit.model)
xtable(g$coefficients)

diagBern(fit.model)
envelBern(fit.model,"logit")

rebeta <- (summary(result))$coef
rebetaLL <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                  rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLL)

AICLL <- AIC(fit.model)
BICLL <- BIC(fit.model)

mrebeta <- rbind(rebetaLL)
pred <- predict(fit.model,type=c("response"),se.fit = TRUE)
mupred <- pred$fit

## Probito
fit.model <- result <- glm(v.doenca~v.ce+v.setor+v.ce*v.setor,
                           family=binomial(link="probit"))
g <- summary(fit.model)
xtable(g$coefficients)

diagBern(fit.model)
envelBern(fit.model,"probit")

rebeta <- (summary(result))$coef
rebetaLP <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                  rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLP)

AICLP <- AIC(fit.model)
BICLP <- BIC(fit.model)

mrebeta <- rbind(rebetaLP)
predLP <- predict(fit.model,type=c("response"),se.fit = TRUE)
mupredLP <- predLP$fit

## Cauchito
fit.model <- result <- glm(v.doenca~v.ce+v.setor+v.ce*v.setor,
                           family=binomial(link="cauchit"))
g <- summary(fit.model)
xtable(g$coefficients)

diagBern(fit.model)
envelBern(fit.model,"cauchit")

rebeta <- (summary(result))$coef
rebetaLC <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                  rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLC)

AICLC <- AIC(fit.model)
BICLC <- BIC(fit.model)

mrebeta <- rbind(rebetaLC)
predLC <- predict(fit.model,type=c("response"),se.fit = TRUE)
mupredLC <- predLC$fit

## Cloglog
fit.model <- result <- glm(v.doenca~v.ce+v.setor+v.ce*v.setor,
                           family=binomial(link="cloglog"))
g <- summary(fit.model)
xtable(g$coefficients)


diagBern(fit.model)
envelBern(fit.model,"cloglog")

rebeta <- (summary(result))$coef
rebetaLCLL <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                    rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLCLL)

AICLCLL <-AIC(fit.model)
BICLCLL <- BIC(fit.model)

mrebeta <- rbind(rebetaLCLL)
predLCLL <-predict(fit.model,type=c("response"),se.fit = TRUE)
mupredLCLL <- predLCLL$fit


## AIC e BIC
AICBIC <- rbind(cbind(AICLL,AICLP,AICLC,AICLL),cbind(BICLL,BICLP,BICLC,BICLL))
colnames(AICBIC) <- c("logito","probito","cauchito","cloglog")
rownames(AICBIC) <- c("AIC","BIC")
xtable(t(AICBIC))

AICBIC <- rbind(cbind(AICLL,AICLP,AICLL),cbind(BICLL,BICLP,BICLL))
colnames(AICBIC) <- c("logito","probito","cloglog")
rownames(AICBIC) <- c("AIC","BIC")
xtable(t(AICBIC))

## Desvio absolutos médios
damLL <- mean(abs(v.doenca-mupred))
damLP <- mean(abs(v.doenca-mupredLP))
damLC <- mean(abs(v.doenca-mupredLC))
damLCLL <- mean(abs(v.doenca-mupredLCLL))

AICBICDAM <- rbind(cbind(AICLL,AICLP,AICLC,AICLCLL),
                   cbind(BICLL,BICLP,BICLC,BICLCLL),
                   cbind(damLL,damLP,damLC,damLCLL))
colnames(AICBICDAM) <- c("logito","probito","cauchito","cloglog")
rownames(AICBICDAM) <- c("AIC","BIC","DAM")
xtable(t(AICBICDAM))

## Desvio absolutos médios sem o modelo Cauchito
damLL <- mean(abs(v.doenca-mupred))
damLP <- mean(abs(v.doenca-mupredLP))
damLCLL <- mean(abs(v.doenca-mupredLCLL))

AICBICDAM <- rbind(cbind(AICLL,AICLP,AICLCLL),cbind(BICLL,BICLP,BICLCLL),
                   cbind(damLL,damLP,damLCLL))
colnames(AICBICDAM) <- c("logito","probito","cloglog")
rownames(AICBICDAM) <- c("AIC","BIC","DAM")
xtable(t(AICBICDAM))

# ANALISE: > observando as saídas do SUMMARY, nota-se que, em ambos os modelos, 
#            nenhuma variável e nem a interação foram significativas.
#          > alé disso, observando os valores de AIC, BIC e DAM, nota-se que, 
#            desconsiderando o modelo com a função de ligação cauchito,os demais
#            modelos possuem os mesmo valores, ou seja, eles possuem o mesmo 
#            comportamento. Dessa forma, vai ser feito agora uma análise dos 
#            modelos sem interação a fim de notar se alguma variável ou se a 
#            interação se tornam significativas e, também,analisar se os valores
#            de AIC, BIC e DAM se diferem entre os modelos.


# Ajuste dos modelo sem interação

## Logito
fit.model <- result <- glm(v.doenca~v.ce+v.setor,family=binomial(link="logit"))
g <- summary(fit.model)
xtable(g$coefficients)

diagBern(fit.model)
envelBern(fit.model,"logit")

rebeta <- (summary(result))$coef
rebetaLL <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                  rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLL)

AICLL <- AIC(fit.model)
BICLL <- BIC(fit.model)

mrebeta <- rbind(rebetaLL)
pred <- predict(fit.model,type=c("response"),se.fit = TRUE)
mupred <- pred$fit

## Probito
fit.model <- result <- glm(v.doenca~v.ce+v.setor,family=binomial(link="probit"))
g <- summary(fit.model)
xtable(g$coefficients)

diagBern(fit.model)
envelBern(fit.model,"probit")

rebeta <- (summary(result))$coef
rebetaLP <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                  rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLP)

AICLP <- AIC(fit.model)
BICLP <- BIC(fit.model)

mrebeta <- rbind(rebetaLP)
predLP <- predict(fit.model,type=c("response"),se.fit = TRUE)
mupredLP <- predLP$fit

## Cauchito
fit.model <- result <- glm(v.doenca~v.ce+v.setor,
                           family=binomial(link="cauchit"))
g <- summary(fit.model)
xtable(g$coefficients)

diagBern(fit.model)
envelBern(fit.model,"cauchit")

rebeta <- (summary(result))$coef
rebetaLC <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                  rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLC)

AICLC <- AIC(fit.model)
BICLC <- BIC(fit.model)

mrebeta <- rbind(rebetaLC)
predLC <- predict(fit.model,type=c("response"),se.fit = TRUE)
mupredLC <- predLC$fit

## Cloglog
fit.model <- result <- glm(v.doenca~v.ce+v.setor,
                           family=binomial(link="cloglog"))
g <- summary(fit.model)
xtable(g$coefficients)

diagBern(fit.model)
envelBern(fit.model,"cloglog")

rebeta <- (summary(result))$coef
rebetaLCLL <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                    rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLCLL)

AICLCLL <-AIC(fit.model)
BICLCLL <- BIC(fit.model)

mrebeta <- rbind(rebetaLCLL)
predLCLL <-predict(fit.model,type=c("response"),se.fit = TRUE)
mupredLCLL <- predLCLL$fit


## AIC e BIC
AICBIC <- rbind(cbind(AICLL,AICLP,AICLC,AICLL),cbind(BICLL,BICLP,BICLC,BICLL))
colnames(AICBIC) <- c("logito","probito","cauchito","cloglog")
rownames(AICBIC) <- c("AIC","BIC")
xtable(t(AICBIC))

## Desvio absolutos médios
damLL <- mean(abs(v.doenca-mupred))
damLP <- mean(abs(v.doenca-mupredLP))
damLC <- mean(abs(v.doenca-mupredLC))
damLCLL <- mean(abs(v.doenca-mupredLCLL))

AICBICDAM <- rbind(cbind(AICLL,AICLP,AICLC,AICLCLL),
                   cbind(BICLL,BICLP,BICLC,BICLCLL),
                   cbind(damLL,damLP,damLC,damLCLL))
colnames(AICBICDAM) <- c("logito","probito","cauchito","cloglog")
rownames(AICBICDAM) <- c("AIC","BIC","DAM")
xtable(t(AICBICDAM))
xtable(AICBICDAM)

# OBS: > analisando as saídas dos SUMMARY's, nota-se que nos modelos, com 
#        exceção do cauchito, o intercepto e a variável setor passaram a ser 
#        significativas.
#      > além disso, os modelos sem interação apresentaram valores de AIC, BIC e
#        DAM menores em comparação com os modelos com interação, assim, 
#        percebe-se que a interação não é necessária
#      > dos modelos sem interação, o que obteve menores valores de AIC, BIC e 
#        DAM foi o modelo PROBITO, portanto ele é o mais adequado.
#      > Agora, realiza-se uma analise em cima do modelo probito sem interação, 
#        com o intuito de verificar se é melhor ter ambas variaveis categoricas 
#        no modelo ou apenas uma delas


##### ANALISE SOBRE O MODELO PROBITO #####

# Probito
fit.model <- result <- glm(v.doenca~v.ce+v.setor,family=binomial(link="probit"))
summary(fit.model) #a componente de setor passa a ser significativa

diagBern(fit.model)
envelBern(fit.model,"probit")

rebeta <- (summary(result))$coef
rebetaLP <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                  rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLP)

AICLP <- AIC(fit.model)
BICLP <- BIC(fit.model)

mrebeta <- rbind(rebetaLP)
predLP <- predict(fit.model,type=c("response"),se.fit = TRUE)
mupredLP <- predLP$fit


# somente com setor
fit.model <- result2 <- glm(v.doenca~v.setor,family=binomial(link="probit"))
summary(fit.model)

diagBern(fit.model)
envelBern(fit.model,"probit")

rebeta <- (summary(result2))$coef
rebetaLL <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                  rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLL)

AICLPS <- AIC(fit.model)
BICLPS <- BIC(fit.model)

mrebeta <- rbind(rebetaLL)
pred <- predict(fit.model,type=c("response"),se.fit = TRUE)
mupred <- pred$fit

## OBS > percebe-se que os modelo apenas com a variavel setor, devido ao fato de
##       ser mais parcimonioso e obter menos valores de AIC e BIC, é melhor, se 
##       ajusta melhor aos dados.


# modelo completo
fit.model<-result<- glm(v.doenca~v.ce+v.setor+v.ce*v.setor,
                        family=binomial(link="probit"))

#modelo somente com o intercepto
fit.model0<-result0<- glm(v.doenca~1,family=binomial(link="probit"))

# Seleção automatizada

step(fit.model0,scope=list(lower=result0,upper=result),direction=c("forward"))
step(fit.model,direction=c("backward"))
step(fit.model0,scope=list(upper=result),direction=c("both"))

## OBS > ambos algoritmos de seleção automatizada covergiram para o mesmo modelo, 
##       que é com apenas a variavel setor.
##     > assim, pode-se dizer que, conforme o que foi visto no grafico de perfis, 
##       a diferença das proporções entre os setores é significativa, mas a 
##       diferença entres as CE's não são.



##### AJUSTE INSERINDO A VARIAVEL IDADE #####

# Modelo com a idade com um coeficiente para cada grupo.
mida <- mean(base.treino$x1) # idade media
auxida <- base.treino$x1-mida # variavel corrigida da idade media
auxida2 <- auxida^2

# Modelo PROBITO

fit.model <-result <- glm(v.doenca~v.ce+v.setor+v.ce*v.setor+auxida*v.ce*v.setor,
                          family=binomial(link="probit"))
g <- summary(fit.model)
xtable(g$coefficients)

diagBern(fit.model)
envelBern(fit.model,"probit")

rebeta <- (summary(result))$coef
rebetaLL <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                  rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLL)

AIC(fit.model)
BIC(fit.model)

mrebeta <- rbind(rebetaLL)

predLP <-predict(fit.model,type=c("response"),se.fit = TRUE)
mupredLP <- predLP$fit
damLP <- mean(abs(v.doenca-mupredLP))


## Único coeficiente para a idade ; sem fazer iteração entre idade e os demais 
## componentes
fit.model0 <- result <- glm(v.doenca~v.ce+v.setor+v.ce*v.setor+auxida,
                            family=binomial(link="probit"))
g <- summary(fit.model0)
xtable(g$coefficients)

diagBern(fit.model0)
envelBern(fit.model0,"probit")

rebeta <- (summary(result))$coef
rebetaLL <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                  rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLL)

AIC(fit.model0)
BIC(fit.model0)

mrebeta <- rbind(rebetaLL)

predLP2 <-predict(fit.model0,type=c("response"),se.fit = TRUE)
mupredLP2 <- predLP2$fit
damLP2 <- mean(abs(v.doenca-mupredLP2))


## Análise do desvio
anadesv(fit.model0,fit.model) # não ha evidencias para rejeitar h0, entao optamos
                              # por considerar esses coeficientes de iteração 
                              # iguais a zero, ou seja, optamos pelo modelo mais
                              # parcimonioso


## Único coeficiente para a idade sem interação v.ce x v.setor
fit.model <- result <- glm(v.doenca~v.ce+v.setor+auxida,
                           family=binomial(link="probit"))
g <- summary(fit.model)
xtable(g$coefficients)

diagBern(fit.model)
envelBern(fit.model,"probit")

rebeta <- (summary(result))$coef
rebetaLL <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                  rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLL)

AICLL <- AIC(fit.model)
BICLL <- BIC(fit.model)

mrebeta <- rbind(rebetaLL)
predLP3 <-predict(fit.model,type=c("response"),se.fit = TRUE)
mupredLP3 <- predLP3$fit
damLP3 <- mean(abs(v.doenca-mupredLP3))


## Único coeficiente para a idade sem interação v.cexv.setor e sem o fator v.ce
fit.model <- result <- glm(v.doenca~v.setor+auxida,
                           family=binomial(link="probit"))
g <- summary(fit.model)
xtable(g$coefficients)

diagBern(fit.model)
envelBern(fit.model,"probit")

rebeta <- (summary(result))$coef
rebetaLL <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                  rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLL)

AICLL <- AIC(fit.model)
BICLL <- BIC(fit.model)

mrebeta <- rbind(rebetaLL)

predLP4 <-predict(fit.model,type=c("response"),se.fit = TRUE)
mupredLP4 <- predLP4$fit
damLP4 <- mean(abs(v.doenca-mupredLP4))

## OBS > Realizando as analises com a variavel idade, percebe-se que ela também 
##       não apresentou signifcancia, portanto, não deve entrar no modelo, 
##         ou seja, mantem-se o modelo probito apenas com a variavel setor.

## OBS > analisando a saída do SUMMARY, percebe-se que, apesar da variável idade 
##       não ser significativa, o modelo com ela possui menor valor de AIC, e os 
##       graficos ficaram aparentam estar melhor ajustados ; assim, opta-se por 
##       deixá-lo no modelo


## modelo completo
fit.model<-result<- glm(v.doenca~v.ce+v.setor+v.ce*v.setor+auxida*v.ce*v.setor,
                        family=binomial(link="probit"))

## modelo somente com o intercepto
fit.model0<-result0<- glm(v.doenca~1,family=binomial(link="probit"))

## Seleção automatizada

step(fit.model0,scope=list(lower=result0,upper=result),direction=c("forward"))
step(fit.model,direction=c("backward"))
step(fit.model0,scope=list(upper=result),direction=c("both"))

## OBS > nota-se que os modelos selecionador por forward e stepwise convergiram 
##       pro msm modelo, que é a variavel setor e a variavel auxida


## Modelo Logito

fit.model <-result <- glm(v.doenca~v.ce+v.setor+v.ce*v.setor+auxida*v.ce*v.setor,
                          family=binomial(link="logit"))
g <- summary(fit.model)
xtable(g$coefficients)

diagBern(fit.model)
envelBern(fit.model,"logit")

rebeta <- (summary(result))$coef
rebetaLL <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                  rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLL)

AIC(fit.model)
BIC(fit.model)

mrebeta <- rbind(rebetaLL)

predLL <-predict(fit.model,type=c("response"),se.fit = TRUE)
mupredLL <- predLL$fit
damLL <- mean(abs(v.doenca-mupredLL))


# Único coeficiente para a idade ; sem fazer iteração entre idade e os demais 
# componentes
fit.model0 <- result <- glm(v.doenca~v.ce+v.setor+v.ce*v.setor+auxida,
                            family=binomial(link="logit"))
g <- summary(fit.model0)
xtable(g$coefficients)

diagBern(fit.model0)
envelBern(fit.model0,"logit")

rebeta <- (summary(result))$coef
rebetaLL <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                  rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLL)

AIC(fit.model0)
BIC(fit.model0)

mrebeta <- rbind(rebetaLL)

predLL2 <-predict(fit.model0,type=c("response"),se.fit = TRUE)
mupredLL2 <- predLL2$fit
damLL2 <- mean(abs(v.doenca-mupredLL2))

# Análise do desvio
anadesv(fit.model0,fit.model) # não ha evidencias para rejeitar h0, entao optamos 
                              # por considerar esses coeficientes de iteração 
                              # iguais a zero, ou seja, optamos pelo modelo mais 
                              # parcimonioso


# Único coeficiente para a idade sem interação v.ce x v.setor
fit.model <- result <- glm(v.doenca~v.ce+v.setor+auxida,
                           family=binomial(link="logit"))
g <- summary(fit.model)
xtable(g$coefficients)

diagBern(fit.model)
envelBern(fit.model,"logit")

rebeta <- (summary(result))$coef
rebetaLL <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                  rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLL)

AICLL <- AIC(fit.model)
BICLL <- BIC(fit.model)

mrebeta <- rbind(rebetaLL)
predLL3 <-predict(fit.model,type=c("response"),se.fit = TRUE)
mupredLL3 <- predLL3$fit
damLL3 <- mean(abs(v.doenca-mupredLL3))


# Único coeficiente para a idade sem interação v.cexv.setor e sem o fator v.ce
fit.model <- result <- glm(v.doenca~v.setor+auxida,
                           family=binomial(link="logit"))
summary(fit.model)

diagBern(fit.model)
envelBern(fit.model,"logit")

rebeta <- (summary(result))$coef
rebetaLL <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                  rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLL)

AICLL <- AIC(fit.model)
BICLL <- BIC(fit.model)

mrebeta <- rbind(rebetaLL)

## OBS > Realizando as analises com a variavel idade, percebe-se que ela também 
##         não apresentou signifcancia, portanto, não deve entrar no modelo, 
##         ou seja, mantem-se o modelo probito apenas com a variavel setor.

## OBS > analisando a saída do SUMMARY, percebe-se que, apesar da variável idade 
##       não ser significativa, o modelo com ela possui menor valor de AIC, e os 
##       graficos ficaram aparentam estar melhor ajustados ; assim, opta-se por 
##       deixá-lo no modelo


# modelo completo
fit.model<-result<- glm(v.doenca~v.ce+v.setor+v.ce*v.setor+auxida*v.ce*v.setor,
                        family=binomial(link="logit"))

#modelo somente com o intercepto
fit.model0<-result0<- glm(v.doenca~1,family=binomial(link="logit"))

# Seleção automatizada

step(fit.model0,scope=list(lower=result0,upper=result),direction=c("forward"))
step(fit.model,direction=c("backward"))
step(fit.model0,scope=list(upper=result),direction=c("both"))

## OBS > nota-se que os modelos selecionador por forward e stepwise convergiram 
##       pro msm modelo, que é apenas com a  variavel setor


## Modelo Cauchito

fit.model <-result <- glm(v.doenca~v.ce+v.setor+v.ce*v.setor+auxida*v.ce*v.setor,
                          family=binomial(link="cauchit"))
g <- summary(fit.model)
xtable(g$coefficients)

diagBern(fit.model)
envelBern(fit.model,"cauchit")

rebeta <- (summary(result))$coef
rebetaLL <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                  rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLL)

AIC(fit.model)
BIC(fit.model)

mrebeta <- rbind(rebetaLL)

predLC <-predict(fit.model,type=c("response"),se.fit = TRUE)
mupredLC <- predLC$fit
damLC <- mean(abs(v.doenca-mupredLC))


# Único coeficiente para a idade ; sem fazer iteração entre idade e os demais 
# componentes
fit.model0 <- result <- glm(v.doenca~v.ce+v.setor+v.ce*v.setor+auxida,
                            family=binomial(link="cauchit"))
g <- summary(fit.model0)
xtable(g$coefficients)

diagBern(fit.model0)
envelBern(fit.model0,"cauchit")

rebeta <- (summary(result))$coef
rebetaLL <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                  rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLL)

AIC(fit.model0)
BIC(fit.model0)

mrebeta <- rbind(rebetaLL)


# Análise do desvio
anadesv(fit.model0,fit.model) # não ha evidencias para rejeitar h0, entao optamos 
                              # por considerar esses coeficientes de iteração 
                              # iguais a zero, ou seja, optamos pelo modelo mais 
                              # parcimonioso


# Único coeficiente para a idade sem interação v.ce x v.setor
fit.model <- result <- glm(v.doenca~v.ce+v.setor+auxida,
                           family=binomial(link="cauchit"))
summary(fit.model)

diagBern(fit.model)
envelBern(fit.model,"cauchit")

rebeta <- (summary(result))$coef
rebetaLL <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                  rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLL)

AICLL <- AIC(fit.model)
BICLL <- BIC(fit.model)

mrebeta <- rbind(rebetaLL)
pred <- predict(fit.model,type=c("response"),se.fit = TRUE)
mupred <- pred$fit


# Único coeficiente para a idade sem interação v.cexv.setor e sem o fator v.ce
fit.model <- result <- glm(v.doenca~v.setor+auxida,
                           family=binomial(link="cauchit"))
summary(fit.model)

diagBern(fit.model)
envelBern(fit.model,"cauchit")

rebeta <- (summary(result))$coef
rebetaLL <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                  rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLL)

AICLL <- AIC(fit.model)
BICLL <- BIC(fit.model)

mrebeta <- rbind(rebetaLL)

## OBS > Realizando as analises com a variavel idade, percebe-se que ela também 
##       não apresentou signifcancia, portanto, não deve entrar no modelo, 
##       ou seja, mantem-se o modelo probito apenas com a variavel setor, isso 
##       se considerarmos uma significancia de 10%.


# modelo completo
fit.model<-result<- glm(v.doenca~v.ce+v.setor+v.ce*v.setor+auxida*v.ce*v.setor,
                        family=binomial(link="cauchit"))

#modelo somente com o intercepto
fit.model0<-result0<- glm(v.doenca~1,family=binomial(link="cauchit"))

# Seleção automatizada

step(fit.model0,scope=list(lower=result0,upper=result),direction=c("forward"))
step(fit.model,direction=c("backward"))
step(fit.model0,scope=list(upper=result),direction=c("both"))

## OBS > nota-se que os modelos selecionador por forward e stepwise convergiram 
##       pro msm modelo, que é apenas com a  variavel setor


## Modelo Cloglog

fit.model <-result <- glm(v.doenca~v.ce+v.setor+v.ce*v.setor+auxida*v.ce*v.setor,
                          family=binomial(link="cloglog"))
g <- summary(fit.model)
xtable(g$coefficients)

diagBern(fit.model)
envelBern(fit.model,"cloglog")

rebeta <- (summary(result))$coef
rebetaLL <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                  rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLL)

AIC(fit.model)
BIC(fit.model)

mrebeta <- rbind(rebetaLL)

predLCLL <-predict(fit.model,type=c("response"),se.fit = TRUE)
mupredLCLL <- predLCLL$fit
damLCLL <- mean(abs(v.doenca-mupredLCLL))


# Único coeficiente para a idade ; sem fazer iteração entre idade e os demais 
# componentes
fit.model0 <- result <- glm(v.doenca~v.ce+v.setor+v.ce*v.setor+auxida,
                            family=binomial(link="cloglog"))
g <- summary(fit.model0)
xtable(g$coefficients)

diagBern(fit.model0)
envelBern(fit.model0,"cloglog")

rebeta <- (summary(result))$coef
rebetaLL <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                  rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLL)

AIC(fit.model0)
BIC(fit.model0)

mrebeta <- rbind(rebetaLL)

predLCLL2 <-predict(fit.model0,type=c("response"),se.fit = TRUE)
mupredLCLL2 <- predLCLL2$fit
damLCLL2 <- mean(abs(v.doenca-mupredLCLL2))


# Análise do desvio
anadesv(fit.model0,fit.model) # não ha evidencias para rejeitar h0, entao optamos
                              # por considerar esses coeficientes de iteração 
                              # iguais a zero, ou seja, optamos pelo modelo mais 
                              # parcimonioso


# Único coeficiente para a idade sem interação v.ce x v.setor
fit.model <- result <- glm(v.doenca~v.ce+v.setor+auxida,
                           family=binomial(link="cloglog"))
g <- summary(fit.model)
xtable(g$coefficients)

diagBern(fit.model)
envelBern(fit.model,"cloglog")

rebeta <- (summary(result))$coef
rebetaLL <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                  rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLL)

AICLL <- AIC(fit.model)
BICLL <- BIC(fit.model)

mrebeta <- rbind(rebetaLL)
predLCLL3 <-predict(fit.model,type=c("response"),se.fit = TRUE)
mupredLCLL3 <- predLCLL3$fit
damLCLL3 <- mean(abs(v.doenca-mupredLCLL3))

# Único coeficiente para a idade sem interação v.cexv.setor e sem o fator v.ce
fit.model <- result <- glm(v.doenca~v.setor+auxida,
                           family=binomial(link="cloglog"))
summary(fit.model)

diagBern(fit.model)
envelBern(fit.model,"cloglog")

rebeta <- (summary(result))$coef
rebetaLL <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                  rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLL)

AICLL <- AIC(fit.model)
BICLL <- BIC(fit.model)

mrebeta <- rbind(rebetaLL)

## OBS > Realizando as analises com a variavel idade, percebe-se que ela também 
##       não apresentou signifcancia, portanto, não deve entrar no modelo, 
##       ou seja, mantem-se o modelo probito apenas com a variavel setor.


# modelo completo
fit.model<-result<- glm(v.doenca~v.ce+v.setor+v.ce*v.setor+auxida*v.ce*v.setor,
                        family=binomial(link="cloglog"))

#modelo somente com o intercepto
fit.model0<-result0<- glm(v.doenca~1,family=binomial(link="cloglog"))

# Seleção automatizada

step(fit.model0,scope=list(lower=result0,upper=result),direction=c("forward"))
step(fit.model,direction=c("backward"))
step(fit.model0,scope=list(upper=result),direction=c("both"))

## OBS > nota-se que os modelos selecionador por forward e stepwise convergiram 
##       pro msm modelo, que é apenas com a  variavel setor



############################## modelo final ##############################

fit.model <- result <- glm(v.doenca~v.setor+auxida,
                           family=binomial(link="probit"))
summary(fit.model)

xtable(confint(fit.model))


# Médias preditas
pred <- predict(fit.model,type=c("response"),se.fit = TRUE)
mupred <- pred$fit
semupred <- pred$se.fit
liIC = apply(cbind(0,mupred-ez*semupred),1,max)
lsIC = apply(cbind(1,mupred+ez*semupred),1,min)

par(mfrow=c(1,2))
plot(base.treino$x4,v.doenca,cex=1.2,cex.axis=1.2,cex.lab=1.2,xlab="Setor",
     ylab="Diagnóstico",pch=16,xlim=c(-1.3,1.31))
plotCI(base.treino$x4,mupred,li=liIC,ui=lsIC,cex=1.2,cex.axis=1.2,cex.lab=1.2,
       col=2,add=TRUE,pch=16)
legend(-1.49,0.9,legend=c("observado","predita"),pch=c(16,16),col=c(1,2),
       bty="n",cex=1.2)
plot(base.treino$x1,v.doenca,cex=1.2,cex.axis=1.2,cex.lab=1.2,xlab="Idade",
     ylab="Diagnóstico",pch=16)
plotCI(base.treino$x1,mupred,li=liIC,ui=lsIC,cex=1.2,cex.axis=1.2,cex.lab=1.2,
       col=2,add=TRUE,pch=16)
legend(-3,0.95,legend=c("observado","predita"),pch=c(16,16),col=c(1,2),bty="n",
       cex=1.2)


##setor 1
s1 <- base.treino %>% filter(x4 == 0)
auxida1 <- s1$x1 - mean(s1$x1)

fit.model <- result <- glm(s1$Y~auxida1,family=binomial(link="probit"))
summary(fit.model)

xtable(confint(fit.model))

### Médias preditas
pred <- predict(fit.model,type=c("response"),se.fit = TRUE)
mupred <- pred$fit
semupred <- pred$se.fit
liIC = apply(cbind(0,mupred-ez*semupred),1,max)
lsIC = apply(cbind(1,mupred+ez*semupred),1,min)

par(mfrow=c(2,1))
plot(s1$x4,s1$Y,cex=1.2,cex.axis=1.2,cex.lab=1.2,xlab="Setor 1",
     ylab="Diagnóstico",pch=16,xlim=c(-1.3,1.31))
plotCI(s1$x4,mupred,li=liIC,ui=lsIC,cex=1.2,cex.axis=1.2,cex.lab=1.2,col=2,
       add=TRUE,pch=16)
legend(-1.2,0.7,legend=c("observado","predita"),pch=c(16,16),col=c(1,2),bty="n",
       cex=1.2)
plot(s1$x1,s1$Y,cex=1.2,cex.axis=1.2,cex.lab=1.2,xlab="Idade",ylab="Diagnóstico",
     pch=16)
plotCI(s1$x1,mupred,li=liIC,ui=lsIC,cex=1.2,cex.axis=1.2,cex.lab=1.2,col=2,
       add=TRUE,pch=16)
legend(-1.5,0.95,legend=c("observado","predita"),pch=c(16,16),col=c(1,2),bty="n",
       cex=1.2)

##setor 2
s2 <- base.treino %>% filter(x4 == 1)
auxida2 <- s2$x1 - mean(s2$x1)

fit.model <- result <- glm(s2$Y~auxida2,family=binomial(link="probit"))
summary(fit.model)

xtable(confint(fit.model))

### Médias preditas
pred <- predict(fit.model,type=c("response"),se.fit = TRUE)
mupred <- pred$fit
semupred <- pred$se.fit
liIC = apply(cbind(0,mupred-ez*semupred),1,max)
lsIC = apply(cbind(1,mupred+ez*semupred),1,min)

par(mfrow=c(2,1))
plot(s2$x4,s2$Y,cex=1.2,cex.axis=1.2,cex.lab=1.2,xlab="Setor 2",
     ylab="Diagnóstico",pch=16,xlim=c(-1.3,1.31))
plotCI(s2$x4,mupred,li=liIC,ui=lsIC,cex=1.2,cex.axis=1.2,cex.lab=1.2,col=2,
       add=TRUE,pch=16)
legend(-1.2,0.9,legend=c("observado","predita"),pch=c(16,16),col=c(1,2),bty="n",
       cex=1.2)
plot(s2$x1,s2$Y,cex=1.2,cex.axis=1.2,cex.lab=1.2,xlab="Idade",ylab="Diagnóstico",
     pch=16)
plotCI(s2$x1,mupred,li=liIC,ui=lsIC,cex=1.2,cex.axis=1.2,cex.lab=1.2,col=2,
       add=TRUE,pch=16)
legend(60,0.25,legend=c("observado","predita"),pch=c(16,16),col=c(1,2),bty="n",
       cex=1.2)


###OUTRO CODIGO PRA FAZER O GRAFICO SEPARADO:

fit.model <- result <- glm(v.doenca~v.setor+auxida,
                           family=binomial(link="probit"))

##### Médias preditas
pred <- predict(fit.model,type=c("response"),se.fit = TRUE)
mupred <- pred$fit
semupred <- pred$se.fit
liIC = apply(cbind(0,mupred-ez*semupred),1,max)
lsIC = apply(cbind(1,mupred+ez*semupred),1,min)


pred<-predict(fit.model,type=c("response"),se.fit = TRUE)
base.treino$mupred <- pred$fit
base.treino$semupred <- pred$se.fit
base.treino$liIC = apply(cbind(0,mupred-ez*semupred),1,max)
base.treino$lsIC = apply(cbind(1,mupred+ez*semupred),1,min)

par(mfrow=c(2,1))
plot(base.treino$x1[v.setor=="setor 1"],base.treino$Y[v.setor=="setor 1"],
     cex=1.2,cex.axis=1.2,cex.lab=1.2,
     xlab="Idade",ylab="Diagnóstico",
     main="Probabilidades estimadas e valores observados (setor 1)",pch=16,
     xlim=c(0,80))
plotCI(base.treino$x1[v.setor=="setor 1"],mupred[v.setor=="setor 1"],
       li=base.treino$liIC[v.setor=="setor 1"],
       ui=base.treino$lsIC[v.setor=="setor 1"],cex=1.2,cex.axis=1.2,cex.lab=1.2,
       col=2,add=TRUE,pch=16)
legend(-1.49,0.9,legend=c("observado","predita"),pch=c(16,16),col=c(1,2),bty="n",
       cex=1.2)

plot(base.treino$x1[v.setor=="setor 2"],base.treino$Y[v.setor=="setor 2"],
     cex=1.2,cex.axis=1.2,cex.lab=1.2,
     xlab="Idade",ylab="Diagnóstico",
     main="Probabilidades estimadas e valores observados (setor 2)",pch=16,
     xlim=c(0,80))
plotCI(base.treino$x1[v.setor=="setor 2"],mupred[v.setor=="setor 2"],
       li=base.treino$liIC[v.setor=="setor 2"],
       ui=base.treino$lsIC[v.setor=="setor 2"],cex=1.2,cex.axis=1.2,cex.lab=1.2,
       col=2,add=TRUE,pch=16)
legend(57,0.9,legend=c("observado","predita"),pch=c(16,16),col=c(1,2),bty="n",
       cex=1.2)



# Valor predito para a variável resposta
n <- length(v.doenca)
ypred <- rbinom(n,1,mupred)

par(mfrow=c(1,2))
plot(base.treino$x4,v.doenca,cex=1.2,cex.axis=1.2,cex.lab=1.2,xlab="Setor",
     ylab="Diagnóstico",pch=16,
     main="Diagnósticos observados e preditos pelo modelo")
lines(base.treino$x4,ypred,type="p",cex=1.2,pch=19,col=2)
legend(0,0.6,legend=c("observado","predito"),pch=c(16,19),col=c(1,2),bty="n",
       cex=1.2)
plot(base.treino$x1,v.doenca,cex=1.2,cex.axis=1.2,cex.lab=1.2,xlab="Idade"
     ,ylab="Diagnóstico",pch=16,
     main="Diagnósticos observados e preditos pelo modelo")
lines(base.treino$x1,ypred,type="p",cex=1.2,pch=19,col=2)
legend(-1,0.6,legend=c("observado","predito"),pch=c(16,19),col=c(1,2),bty="n",
       cex=1.2)


##OUTRO METODO DE CALCULAR:
fit.model <- result <- glm(v.doenca~v.setor+auxida,
                           family=binomial(link="probit"))
summary(fit.model)
rebeta <- (summary(fit.model))$coef
rebetaLL <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                  rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
pred<-predict(fit.model,type=c("response"),se.fit = TRUE)
mupred <- pred$fit
semupred <- pred$se.fit

n <- length(base.treino$Y)
set.seed(180026798)
base.treino$ypred <- rbinom(n,1,mupred)

par(mfrow=c(2,1))
plot(base.treino$x1[v.setor=="setor 1"],base.treino$Y[v.setor=="setor 1"],
     cex=1.2,cex.axis=1.2,cex.lab=1.2,xlab="Idade",
     ylab="Diagnóstico",pch=16,
     main="Ocorrências da doença observadas e preditas pelo modelo (setor 1)")
lines(base.treino$x1[v.setor=="setor 1"],base.treino$ypred[v.setor=="setor 1"],
      type="p",cex=1.2,pch=19,col=2)
legend(4,0.9,legend=c("observado","predito"),pch=c(16,19),col=c(1,2),bty="n",
       cex=1.2)
plot(base.treino$x1[v.setor=="setor 2"],base.treino$Y[v.setor=="setor 2"],
     cex=1.2,cex.axis=1.2,cex.lab=1.2,xlab="Idade",
     ylab="Diagnóstico",pch=16,
     main="Ocorrências da doença observadas e preditas pelo modelo (setor 2)")
lines(base.treino$x1[v.setor=="setor 2"],base.treino$ypred[v.setor=="setor 2"],
      type="p",cex=1.2,pch=19,col=2)
legend(4,0.9,legend=c("observado","predito"),pch=c(16,19),col=c(1,2),bty="n",
       cex=1.2)


## Grafico de comparação entre as probab estimadas e a variavel resposta
fit.model <- result <- glm(v.doenca~v.setor+auxida,
                           family=binomial(link="probit"))
summary(fit.model)

round(predict(fit.model, base.treino, type = 'response'),3)

par(mfrow = c(2,1))
plot(predict(fit.model, base.treino, type = 'response'), pch=16,
     ylab = "Probabilidades Estimadas", xlab = "Observações")
plot(base.treino$Y, pch=16, ylab = "Variável Resposta", xlab = "Observações")


############## BASE VALIDAÇÃO ####################

v.doenca_valida <- base.valida$Y # diagnostico da doença (numerico)
v.doenca.fac_valida <- base.valida$Y.factor # diagnostico da doença (fator)
v.ce__valida <- base.valida$V.CE  # classe economica
v.setor_valida <- base.valida$V.SETOR # setor
n <- length(v.doenca_valida)

auxida_valida <- base.valida$x1 - mean(base.valida$x1)

fit.model <- result <- glm(v.doenca_valida~v.setor_valida+auxida_valida,
                           family=binomial(link="probit"))
summary(fit.model) # setor deixa de ser significativa e variavel passa a ser 
                   # significativa

diagBern(fit.model)
envelBern(fit.model,"probit")

rebeta <- (summary(fit.model))$coef
rebetaLL <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],
                  rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLL)


fit.model <- result <- glm(v.doenca_valida~v.setor_valida+auxida_valida,family=binomial(link="probit"))
summary(fit.model) # v.setor deixou de ser significante
rebeta <- (summary(fit.model))$coef
rebetaLL <- cbind(rebeta[,1],rebeta[,2],rebeta[,1]-ez*rebeta[,2],rebeta[,1]+ez*rebeta[,2],rebeta[,3],rebeta[,4])
xtable(rebetaLL)
pred<-predict(fit.model,type=c("response"),se.fit = TRUE)
mupred <- pred$fit
semupred <- pred$se.fit

n <- length(base.valida$Y)
set.seed(180026798)
base.valida$ypred <- rbinom(n,1,mupred)
par(mfrow=c(2,1))
plot(base.valida$x1[v.setor_valida=="setor 1"],base.valida$Y[v.setor_valida=="setor 1"],cex=1.2,cex.axis=1.2,cex.lab=1.2,xlab="Idade",
     ylab="Diagnóstico",pch=16,main="Ocorrências da doença observadas e preditas pelo modelo (setor 1)")
lines(base.valida$x1[v.setor_valida=="setor 1"],base.valida$ypred[v.setor_valida=="setor 1"],type="p",cex=1.2,pch=19,col=2)
legend(0.5,0.9,legend=c("observado","predito"),pch=c(16,19),col=c(1,2),bty="n",cex=1.2)
plot(base.valida$x1[v.setor_valida=="setor 2"],base.valida$Y[v.setor_valida=="setor 2"],cex=1.2,cex.axis=1.2,cex.lab=1.2,xlab="Idade",
     ylab="Diagnóstico",pch=16,main="Ocorrências da doença observadas e preditas pelo modelo (setor 2)")
lines(base.valida$x1[v.setor_valida=="setor 2"],base.valida$ypred[v.setor_valida=="setor 2"],type="p",cex=1.2,pch=19,col=2)
legend(4,0.9,legend=c("observado","predito"),pch=c(16,19),col=c(1,2),bty="n",cex=1.2)


l <- table(base.valida$Y[v.setor_valida=="setor 1"] == base.valida$ypred[v.setor_valida=="setor 1"])
u <- table(base.valida$Y[v.setor_valida=="setor 2"] == base.valida$ypred[v.setor_valida=="setor 2"])
xtable(l)
xtable(u)