############### AVALIAÇÃO 2 - MLG ###############

# Pacotes:
library(xtable)
library(plotrix)
library(plyr)
library(lattice)
library(HistogramTools)
library(MASS)
library(e1071)

op <- options()

## Diagnóstico e envelope para o modelo Poisson
source("https://www.ime.unicamp.br/~cnaber/diag_pois.r")
source("https://www.ime.unicamp.br/~cnaber/envel_pois.r")

## Diagnóstico e envelope para o modelo binomial negativo
source("https://www.ime.unicamp.br/~cnaber/diag_nbin.r")
source("https://www.ime.unicamp.br/~cnaber/envel_nbin.r")

## Teste CB=M Binomial Negativa
source("https://www.ime.unicamp.br/~cnaber/TestCBMBinNeg.r")

# Abrindo e tratando o banco de dados:

base.total <- read.table(file.choose(), header = F)

colnames(base.total) <- c("Y", "X1", "X2", "X3", "X4")

base.total$X1_fac <- ifelse(base.total$X1 == 0, "apenas educação",
                          ifelse(base.total$X1 == 1, "educação com exercícios", NA))
base.total$X1_fac <- as.factor(base.total$X1_fac)

base.total$X2_fac <- ifelse(base.total$X2 == 0, "feminino",
                        ifelse(base.total$X2 == 1, "masculino", NA))
base.total$X2_fac <- as.factor(base.total$X2_fac)

base.total$ID <- seq(1:100)

base.total <- base.total[c("ID","Y","X1","X1_fac","X2","X2_fac","X3","X4")]
xtable(head(base.total))

# Selecionando a amostra
set.seed(180026798) #colocar o número da matrícula
amostra <- sample(1:100,size = 70, replace = F)
base.treino <- base.total[amostra,]
base.valida <- base.total[-amostra,]

# Definindo variaveis:
Y <- base.treino$Y
v_int <- base.treino$X1
v_int_fac <- base.treino$X1_fac
v_gen <- base.treino$X2
v_gen_fac <- base.treino$X2_fac
v_eq <- base.treino$X3
v_forca <- base.treino$X4
n <- length(Y)



# Análise Descritiva

## Variaveis qualitativas:

datafalt <- data.frame(v_int_fac,v_gen_fac,Y)

### medidas resumo por intervenção
cint_fac <- ddply(datafalt,.(v_int_fac),summarise,media=mean(Y),dp=sqrt(var(Y)),vari=var(Y),ca=skewness(Y),minimo=min(Y),maximo=max(Y),cv=100*((sqrt(var(Y))/mean(Y))),n=length(Y))
xtable(cint_fac)

### medidas resumo por gênero
cgen_fac <- ddply(datafalt,.(v_gen_fac),summarise,media=mean(Y),dp=sqrt(var(Y)),vari=var(Y),ca=skewness(Y),minimo=min(Y),maximo=max(Y),cv=100*((sqrt(var(Y))/mean(Y))),n=length(Y))
xtable(cgen_fac)

### medidas resumo (intervenção vs gênero)
c_int_gen <- ddply(datafalt,.(v_int_fac,v_gen_fac),summarise,media=mean(Y),dp=sqrt(var(Y)),vari=var(Y),cv=100*((sqrt(var(Y))/mean(Y))),min=min(Y),max=max(Y),n=length(Y))
xtable(c_int_gen)


### Gráficos de perfis

#### por gênero x intervenção

par(mfrow=c(1,1))
ez <- qnorm(0.975)

plotCI(c_int_gen$media[c_int_gen$v_gen_fac=="masculino"],uiw=ez*c_int_gen$dp[c_int_gen$v_gen_fac=="masculino"]/sqrt(c_int_gen$n[c_int_gen$v_gen_fac=="masculino"]),liw=ez*c_int_gen$dp[c_int_gen$v_gen_fac=="masculino"]/sqrt(c_int_gen$n[c_int_gen$v_gen_fac=="masculino"]),axes=FALSE,cex.lab=1.2,cex.axis=1.2,cex=1.2,xlab="Intervenção",ylab="Número de quedas",pch=19,col=1)
lines(c_int_gen$media[c_int_gen$v_gen_fac=="masculino"],lwd=2,col=1)
axis(2,cex.axis=1.2)
axis(1,1:2,c("apenas educação","ed. c/ exercícios"),cex.axis=1.2)
plotCI(c_int_gen$media[c_int_gen$v_gen_fac=="feminino"],uiw=ez*c_int_gen$dp[c_int_gen$v_gen_fac=="feminino"]/sqrt(c_int_gen$n[c_int_gen$v_gen_fac=="feminino"]),liw=ez*c_int_gen$dp[c_int_gen$v_gen_fac=="feminino"]/sqrt(c_int_gen$n[c_int_gen$v_gen_fac=="feminino"]),axes=FALSE,cex.lab=1.2,cex.axis=1.2,cex=1.2,xlab="Intervenção",ylab="Número de quedas",pch=17,col=2,add=TRUE)
lines(c_int_gen$media[c_int_gen$v_gen_fac=="feminino"],lwd=2,col=2)
legend(1,2.5,c("masculino","feminino"),col=c(1,2),lwd=c(2,2),pch=c(19,17),bty="n",cex=1.2)


### Gráficos das proporções

#### Tabelas de contingência
t1 <- table(Y,v_int_fac)
t2 <- table(Y,v_gen_fac)
ta1 <- 100*(t(t1)/(apply(t1,2,sum)))
ta2 <- 100*(t(t2)/(apply(t2,2,sum)))
xtable(rbind(ta1,ta2))
xtable(ta1)
xtable(ta2)

#### Por intervenção
par(mfrow=c(1,1))
plot(c(ta1[1,],ta1[2,]),axes=FALSE,ylab="Proporções",
     xlab="Número de quedas por Intervenção",cex=1.2,cex.axis=1.2,cex.lab=1.2,pch=19)
axis(1,c(1,2,3,4,5,6,7,8,9,10,11,
         12,13,14,15,16,17,18,19,20,21,22,23,24),labels=c("0","1","2","3","4","5","6",
                       "7","8","9","10","11","0","1","2","3","4","5","6",
                       "7","8","9","10","11"))
axis(2,at=seq(round(min(ta1),2),round(max(ta1),2),2))


#### Por gênero
plot(c(ta2[1,],ta2[2,]),axes=FALSE,ylab="Proporções",
     xlab="Número de quedas por Gênero",cex=1.2,cex.axis=1.2,cex.lab=1.2,pch=19)
axis(1,c(1,2,3,4,5,6,7,8,9,10,11,
         12,13,14,15,16,17,18,19,20,21,22,23,24),labels=c("0","1","2","3","4","5","6",
                                                          "7","8","9","10","11","0","1","2","3","4","5","6",
                                                          "7","8","9","10","11"))
axis(2,at=seq(round(min(ta2),2),round(max(ta2),2),2))


## Variáveis Quantitativas:

### Gráfico de dispersão

par(mfrow=c(1,2))
plot(v_eq,Y,cex=1.3,cex.axis=1.3,cex.lab=1.3,xlab="Índice de equilíbrio",ylab="Número de quedas",pch=19)
plot(v_forca,Y,cex=1.3,cex.axis=1.3,cex.lab=1.3,xlab="Índice de força",ylab="Número de quedas",pch=19)

#### dispersões por grupo
par(mfrow=c(2,2))
vgrupo <- interaction(v_gen_fac,v_int_fac)

plot(v_eq[vgrupo=="masculino.apenas educação"],Y[vgrupo=="masculino.apenas educação"],cex=1.3,cex.axis=1.3,cex.lab=1.3,xlab="Índice de equilíbrio",ylab="Número de quedas",pch=19)
title("homens - apenas educação",cex=1.2)
plot(v_eq[vgrupo=="feminino.apenas educação"],Y[vgrupo=="feminino.apenas educação"],cex=1.3,cex.axis=1.3,cex.lab=1.3,xlab="Índice de equilíbrio",ylab="Número de quedas",pch=19)
title("mulheres - apenas educação",cex=1.2)

plot(v_eq[vgrupo=="masculino.educação com exercícios"],Y[vgrupo=="masculino.educação com exercícios"],cex=1.3,cex.axis=1.3,cex.lab=1.3,xlab="Índice de equilíbrio",ylab="Número de quedas",pch=19)
title("homens - educação com exercícios",cex=1.2)
plot(v_eq[vgrupo=="feminino.educação com exercícios"],Y[vgrupo=="feminino.educação com exercícios"],cex=1.3,cex.axis=1.3,cex.lab=1.3,xlab="Índice de equilíbrio",ylab="Número de quedas",pch=19)
title("mulheres - educação com exercícios",cex=1.2)


plot(v_forca[vgrupo=="masculino.apenas educação"],Y[vgrupo=="masculino.apenas educação"],cex=1.3,cex.axis=1.3,cex.lab=1.3,xlab="Índice de força",ylab="Número de quedas",pch=19)
title("homens - apenas educação",cex=1.2)
plot(v_forca[vgrupo=="feminino.apenas educação"],Y[vgrupo=="feminino.apenas educação"],cex=1.3,cex.axis=1.3,cex.lab=1.3,xlab="Índice de força",ylab="Número de quedas",pch=19)
title("mulheres - apenas educação",cex=1.2)

plot(v_forca[vgrupo=="masculino.educação com exercícios"],Y[vgrupo=="masculino.educação com exercícios"],cex=1.3,cex.axis=1.3,cex.lab=1.3,xlab="Índice de força",ylab="Número de quedas",pch=19)
title("homens - educação com exercícios",cex=1.2)
plot(v_forca[vgrupo=="feminino.educação com exercícios"],Y[vgrupo=="feminino.educação com exercícios"],cex=1.3,cex.axis=1.3,cex.lab=1.3,xlab="Índice de força",ylab="Número de quedas",pch=19)
title("mulheres - educação com exercícios",cex=1.2)


# Box plot
par(mfrow=c(1,1))
summary(v_eq[vgrupo=="feminino.apenas educação"])
summary(v_eq[vgrupo=="masculino.apenas educação"])
summary(v_eq[vgrupo=="feminino.educação com exercícios"])
summary(v_eq[vgrupo=="masculino.educação com exercícios"])

var(v_eq[vgrupo=="feminino.apenas educação"])
var(v_eq[vgrupo=="masculino.apenas educação"])
var(v_eq[vgrupo=="feminino.educação com exercícios"])
var(v_eq[vgrupo=="masculino.educação com exercícios"])

boxplot(v_eq~vgrupo,names=c("mulheres-apenas educação","homens-apenas educação","mulheres-educação com exercícios","homens-educação com exercícios"),cex=1.2,cex.lab=1.2,cex.main=1.2,
        ylab = "Índice de equilíbrio", xlab = "Combinações")
title("Índice de equilíbrio por combinação")


summary(v_forca[vgrupo=="feminino.apenas educação"])
summary(v_forca[vgrupo=="masculino.apenas educação"])
summary(v_forca[vgrupo=="feminino.educação com exercícios"])
summary(v_forca[vgrupo=="masculino.educação com exercícios"])

var(v_forca[vgrupo=="feminino.apenas educação"])
var(v_forca[vgrupo=="masculino.apenas educação"])
var(v_forca[vgrupo=="feminino.educação com exercícios"])
var(v_forca[vgrupo=="masculino.educação com exercícios"])

boxplot(v_forca~vgrupo,names=c("mulheres-apenas educação","homens-apenas educação","mulheres-educação com exercícios","homens-educação com exercícios"),cex=1.2,cex.lab=1.2,cex.main=1.2,
        ylab = "Índice de força", xlab = "Combinações")
title("Índice de força por combinação")



# Ajuste de modelos apenas com as variaveis categoricas:

## Ajuste do modelo de Poisson com função log e com interação entre as variaveis categoricas
fit.model_1 <- glm(Y~v_gen_fac+v_int_fac+v_gen_fac*v_int_fac,family=poisson("log"))
summary(fit.model_1)
xtable(summary(fit.model_1))

desvioM1 <- deviance(fit.model_1)
p <- ncol(model.matrix(fit.model_1))
pvdesvM1 <- 1-pchisq(desvioM1,df=n-p) # percebe-se um p-valor muito baixo, ou seja, se de fato ocorre nessa convergencia para a Qui-quadrado, o ajuste do modelo 
                                      # esta sendo rejeitado, ou seja, tem-se um indicativo de que os dados não tão se adequando ao modelo

diagPoisson(fit.model_1) 
par(mfrow=c(1,1))
envelPoisson(fit.model_1,"log") 

AICM1 <- AIC(fit.model_1)
BICM1 <- BIC(fit.model_1)

ez <- qnorm(0.975)
rebeta1 <- (summary(fit.model_1))$coef
rebetaM1 <- cbind(rebeta1[,1],rebeta1[,2],rebeta1[,1]-ez*rebeta1[,2],rebeta1[,1]+ez*rebeta1[,2],rebeta1[,3],rebeta1[,4])
xtable(rebetaM1)
mrebeta1 <- rbind(rebetaM1)
covbetaM1 <- vcov(fit.model_1)


## Ajuste do modelo de Poisson com função log e sem interação entre as variaveis categoricas
fit.model_2 <- glm(Y~v_gen_fac+v_int_fac+v_gen_fac,family=poisson("log"))
summary(fit.model_2)
xtable(summary(fit.model_2))

desvioM2 <- deviance(fit.model_2)
p <- ncol(model.matrix(fit.model_2))
pvdesvM2 <- 1-pchisq(desvioM2,df=n-p) # percebe-se um p-valor muito baixo, ou seja, se de fato ocorre nessa convergencia para a Qui-quadrado, o ajuste do modelo 
                                      # esta sendo rejeitado, ou seja, tem-se um indicativo de que os dados não tão se adequando ao modelo

diagPoisson(fit.model_2) 
par(mfrow=c(1,1))
envelPoisson(fit.model_2,"log") 

AICM2 <- AIC(fit.model_2)
BICM2 <- BIC(fit.model_2)

ez <- qnorm(0.975)
rebeta2 <- (summary(fit.model_2))$coef
rebetaM2 <- cbind(rebeta2[,1],rebeta2[,2],rebeta2[,1]-ez*rebeta2[,2],rebeta2[,1]+ez*rebeta2[,2],rebeta2[,3],rebeta2[,4])
xtable(rebetaM2)
mrebeta2 <- rbind(rebetaM2)
covbetaM2 <- vcov(fit.model_2)


## Ajuste dos modelo Possion com a função log e com metodos automaticos

fit.model <- result <- glm(Y~v_gen_fac+v_int_fac+v_gen_fac*v_int_fac,family=poisson("log"))
fit.model0 <- result0 <- glm(Y~1,family=poisson("log"))

# Seleção automatizada

step(fit.model0,scope=list(lower=result0,upper=result),direction=c("forward"))
step(fit.model,direction=c("backward"))
step(fit.model0,scope=list(upper=result),direction=c("both"))



## Ajuste do modelo Binomial Negativo com função log e com interação entre as variaveis categoricas
fit.model_3 <- glm.nb(Y~v_gen_fac+v_int_fac+v_gen_fac*v_int_fac,link="log")
summary(fit.model_3)
xtable(summary(fit.model_3))

desvioM3 <- deviance(fit.model_3)
p <- ncol(model.matrix(fit.model_3))
pvdesvM3 <- 1-pchisq(desvioM3,df=n-p) # percebe-se um p-valor alto, ou seja, se de fato ocorre nessa convergencia para a Qui-quadrado, o ajuste do modelo 
                                      # não esta sendo rejeitado, ou seja, tem-se um indicativo de que os dados tão se adequando ao modelo

diagPoisson(fit.model_3) 
par(mfrow=c(1,1))
envelPoisson(fit.model_3,"log") 

AICM3 <- AIC(fit.model_3)
BICM3 <- BIC(fit.model_3)

ez <- qnorm(0.975)
rebeta3 <- (summary(fit.model_3))$coef
rebetaM3 <- cbind(rebeta3[,1],rebeta3[,2],rebeta3[,1]-ez*rebeta3[,2],rebeta3[,1]+ez*rebeta3[,2],rebeta3[,3],rebeta3[,4])
xtable(rebetaM3)
mrebeta3 <- rbind(rebetaM3)
covbetaM3 <- vcov(fit.model_3)

## Ajuste do modelo Binomial Negativo com função log e sem interação entre as variaveis categoricas
fit.model_4 <- glm.nb(Y~v_gen_fac+v_int_fac,link="log")
summary(fit.model_4)
xtable(summary(fit.model_4))

desvioM4 <- deviance(fit.model_4)
p <- ncol(model.matrix(fit.model_4))
pvdesvM4 <- 1-pchisq(desvioM4,df=n-p) # percebe-se um p-valor alto, ou seja, se de fato ocorre nessa convergencia para a Qui-quadrado, o ajuste do modelo 
                                      # não esta sendo rejeitado, ou seja, tem-se um indicativo de que os dados tão se adequando ao modelo

diagPoisson(fit.model_4) 
par(mfrow=c(1,1))
envelPoisson(fit.model_4,"log") 

AICM4 <- AIC(fit.model_4)
BICM4 <- BIC(fit.model_4)

ez <- qnorm(0.975)
rebeta4 <- (summary(fit.model_4))$coef
rebetaM4 <- cbind(rebeta4[,1],rebeta4[,2],rebeta4[,1]-ez*rebeta4[,2],rebeta4[,1]+ez*rebeta4[,2],rebeta4[,3],rebeta4[,4])
xtable(rebetaM4)
mrebeta4 <- rbind(rebetaM4)
covbetaM4 <- vcov(fit.model_4)

## Ajuste dos modelo Binomial Negativo com a função log e com metodos automaticos

fit.model <- result <- glm.nb(Y~v_gen_fac+v_int_fac+v_gen_fac*v_int_fac,link="log")
fit.model0 <- result0 <- glm.nb(Y~1,link="log")

# Seleção automatizada

step(fit.model0,scope=list(lower=result0,upper=result),direction=c("forward"))
step(fit.model,direction=c("backward"))
step(fit.model0,scope=list(upper=result),direction=c("both"))



# Ajuste de modelo inserindo as variaveis quantitativas:

## centralizando nas médias
v_eq_c <-  v_eq - mean(v_eq)
v_forca_c  <-  v_forca - mean(v_forca)

## Ajuste do modelo de Poisson com função log e com interação entre as variaveis quantitativas e entre as variaveis categoricas 
fit.model_5 <- glm(Y~v_gen_fac+v_int_fac+v_gen_fac*v_int_fac+v_eq_c+v_forca_c+v_eq_c*v_forca_c,
                   family=poisson("log"))
summary(fit.model_5)
xtable(summary(fit.model_5))

desvioM5 <- deviance(fit.model_5)
p <- ncol(model.matrix(fit.model_5))
pvdesvM5 <- 1-pchisq(desvioM5,df=n-p) # percebe-se um p-valor baixo, ou seja, se de fato ocorre nessa convergencia para a Qui-quadrado, o ajuste do modelo 
                                      # esta sendo rejeitado, ou seja, tem-se um indicativo de que os dados não tão se adequando ao modelo

diagPoisson(fit.model_5) 
par(mfrow=c(1,1))
envelPoisson(fit.model_5,"log") 

AICM5 <- AIC(fit.model_5)
BICM5 <- BIC(fit.model_5)

ez <- qnorm(0.975)
rebeta5 <- (summary(fit.model_5))$coef
rebetaM5 <- cbind(rebeta5[,1],rebeta5[,2],rebeta5[,1]-ez*rebeta5[,2],rebeta5[,1]+ez*rebeta5[,2],rebeta5[,3],rebeta5[,4])
xtable(rebetaM5)
mrebeta5 <- rbind(rebetaM5)
covbetaM5 <- vcov(fit.model_5)


## Ajuste do modelo de Poisson com função log e sem interação entre as variaveis quantitativas e entre as variaveis categoricas 
fit.model_6 <- glm(Y~v_gen_fac+v_int_fac+v_eq_c+v_forca_c,
                   family=poisson("log"))
summary(fit.model_6)
xtable(summary(fit.model_6))

desvioM6 <- deviance(fit.model_6)
p <- ncol(model.matrix(fit.model_6))
pvdesvM6 <- 1-pchisq(desvioM6,df=n-p) # percebe-se um p-valor baixo (porem muito proximo do nivel de significancia), ou seja, se de fato ocorre nessa convergencia para a Qui-quadrado, o ajuste do modelo 
                                      # esta sendo rejeitado, ou seja, tem-se um indicativo de que os dados não tão se adequando ao modelo

diagPoisson(fit.model_6) 
par(mfrow=c(1,1))
envelPoisson(fit.model_6,"log") 

AICM6 <- AIC(fit.model_6)
BICM6 <- BIC(fit.model_6)

ez <- qnorm(0.975)
rebeta6 <- (summary(fit.model_6))$coef
rebetaM6 <- cbind(rebeta6[,1],rebeta6[,2],rebeta6[,1]-ez*rebeta6[,2],rebeta6[,1]+ez*rebeta6[,2],rebeta6[,3],rebeta6[,4])
xtable(rebetaM6)
mrebeta6 <- rbind(rebetaM6)
covbetaM6 <- vcov(fit.model_6)

## Ajuste do modelo Possion com a função log e com metodos automaticos

fit.model <- result <- glm(Y~v_gen_fac+v_int_fac+v_gen_fac*v_int_fac+v_eq_c+v_forca_c+v_eq_c*v_forca_c,
                           family=poisson("log"))
fit.model0 <- result0 <- glm(Y~1,family=poisson("log"))

# Seleção automatizada

step(fit.model0,scope=list(lower=result0,upper=result),direction=c("forward"))
step(fit.model,direction=c("backward"))
step(fit.model0,scope=list(upper=result),direction=c("both"))



## Ajuste do modelo Binomial Negativo com função log e com interação entre as variaveis quantitativas e entre as variaveis categoricas 
fit.model_7 <- glm.nb(Y~v_gen_fac+v_int_fac+v_gen_fac*v_int_fac+v_eq_c+v_forca_c+v_eq_c*v_forca_c,
                      link="log")
summary(fit.model_7)
xtable(summary(fit.model_7))

desvioM7 <- deviance(fit.model_7)
p <- ncol(model.matrix(fit.model_7))
pvdesvM7 <- 1-pchisq(desvioM7,df=n-p) # percebe-se um p-valor alto, ou seja, se de fato ocorre nessa convergencia para a Qui-quadrado, o ajuste do modelo 
                                      # não esta sendo rejeitado, ou seja, tem-se um indicativo de que os dados tão se adequando ao modelo

diagPoisson(fit.model_7) 
par(mfrow=c(1,1))
envelPoisson(fit.model_7,"log") 

AICM7 <- AIC(fit.model_7)
BICM7 <- BIC(fit.model_7)

ez <- qnorm(0.975)
rebeta7 <- (summary(fit.model_7))$coef
rebetaM7 <- cbind(rebeta7[,1],rebeta7[,2],rebeta7[,1]-ez*rebeta7[,2],rebeta7[,1]+ez*rebeta7[,2],rebeta7[,3],rebeta7[,4])
xtable(rebetaM7)
mrebeta7 <- rbind(rebetaM7)
covbetaM7 <- vcov(fit.model_7)


## Ajuste do modelo Binomial Negativo com função log e sem interação entre as variaveis quantitativas e entre as variaveis categoricas 
fit.model_8 <- glm.nb(Y~v_gen_fac+v_int_fac+v_eq_c+v_forca_c,
                      link="log")
summary(fit.model_8)
xtable(summary(fit.model_8))

desvioM8 <- deviance(fit.model_8)
p <- ncol(model.matrix(fit.model_8))
pvdesvM8 <- 1-pchisq(desvioM8,df=n-p) # percebe-se um p-valor alto, ou seja, se de fato ocorre nessa convergencia para a Qui-quadrado, o ajuste do modelo 
                                      # não esta sendo rejeitado, ou seja, tem-se um indicativo de que os dados tão se adequando ao modelo

diagPoisson(fit.model_8) 
par(mfrow=c(1,1))
envelPoisson(fit.model_8,"log") 

AICM8 <- AIC(fit.model_8)
BICM8 <- BIC(fit.model_8)

ez <- qnorm(0.975)
rebeta8 <- (summary(fit.model_8))$coef
rebetaM8 <- cbind(rebeta8[,1],rebeta8[,2],rebeta8[,1]-ez*rebeta8[,2],rebeta8[,1]+ez*rebeta8[,2],rebeta8[,3],rebeta8[,4])
xtable(rebetaM8)
mrebeta8 <- rbind(rebetaM8)
covbetaM8 <- vcov(fit.model_8)


## Ajuste do modelo Bonimial Negativo com a função log e com metodos automaticos

fit.model <- result <- glm.nb(Y~v_gen_fac+v_int_fac+v_gen_fac*v_int_fac+v_eq_c+v_forca_c+v_eq_c*v_forca_c,
                              link="log")
fit.model0 <- result0 <- glm.nb(Y~1,link="log")

# Seleção automatizada

step(fit.model0,scope=list(lower=result0,upper=result),direction=c("forward"))
step(fit.model,direction=c("backward"))
step(fit.model0,scope=list(upper=result),direction=c("both"))



# Modelo Reduzido

## Modelo Bonomial Negativo Reduzido

### Recaptulando o ultimo modelo binomial negativo e vendo que a variavel genero não é significativa

fit.model_8 <- glm.nb(Y~v_gen_fac+v_int_fac+v_eq_c+v_forca_c,
                      link="log")
summary(fit.model_8)
xtable(summary(fit.model_8))

desvioM8 <- deviance(fit.model_8)
p <- ncol(model.matrix(fit.model_8))
pvdesvM8 <- 1-pchisq(desvioM8,df=n-p) # percebe-se um p-valor alto, ou seja, se de fato ocorre nessa convergencia para a Qui-quadrado, o ajuste do modelo 
                                      # não esta sendo rejeitado, ou seja, tem-se um indicativo de que os dados tão se adequando ao modelo

diagPoisson(fit.model_8) 
par(mfrow=c(1,1))
envelPoisson(fit.model_8,"log") 

AICM8 <- AIC(fit.model_8)
BICM8 <- BIC(fit.model_8)

ez <- qnorm(0.975)
rebeta8 <- (summary(fit.model_8))$coef
rebetaM8 <- cbind(rebeta8[,1],rebeta8[,2],rebeta8[,1]-ez*rebeta8[,2],rebeta8[,1]+ez*rebeta8[,2],rebeta8[,3],rebeta8[,4])
xtable(rebetaM8)
mrebeta8 <- rbind(rebetaM8)
covbetaM8 <- vcov(fit.model_8)


### Tirando a variavel genero

fit.model_10 <- glm.nb(Y~v_int_fac+v_eq_c+v_forca_c,
                      link="log")
summary(fit.model_10)
xtable(summary(fit.model_10))

desvioM10 <- deviance(fit.model_10)
p <- ncol(model.matrix(fit.model_10))
pvdesvM10 <- 1-pchisq(desvioM10,df=n-p) # percebe-se um p-valor alto, ou seja, se de fato ocorre nessa convergencia para a Qui-quadrado, o ajuste do modelo 
                                        # não esta sendo rejeitado, ou seja, tem-se um indicativo de que os dados tão se adequando ao modelo

diagPoisson(fit.model_10) 
par(mfrow=c(1,1))
envelPoisson(fit.model_10,"log") 

AICM10 <- AIC(fit.model_10)
BICM10 <- BIC(fit.model_10)

ez <- qnorm(0.975)
rebeta10 <- (summary(fit.model_10))$coef
rebetaM10 <- cbind(rebeta10[,1],rebeta10[,2],rebeta10[,1]-ez*rebeta10[,2],rebeta10[,1]+ez*rebeta10[,2],rebeta10[,3],rebeta10[,4])
xtable(rebetaM10)
mrebeta10 <- rbind(rebetaM10)
covbetaM10 <- vcov(fit.model_10)


## Teste para retirar os fatores

mC <- rbind(cbind(0,0,1,0),cbind(0,0,0,1))
mM <- rbind(0,0)

testeFCBMBinNeg(fit.model_10,mC,mM)
### OBS > rejeitamos a hipotese nula de que os parametros de equilibrio e força são iguais a zero, ou seja, eles sao significativos
###     > portanto, ajusta-se um modelo com esses dois fatores


## Ajuste do modelo Bonimial Negativo com a função log e com metodos automaticos

fit.model <- result <- glm.nb(Y~v_gen_fac+v_int_fac+v_gen_fac*v_int_fac+v_eq_c+v_forca_c+v_eq_c*v_forca_c,
                              link="log")
fit.model0 <- result0 <- glm.nb(Y~1,link="log")

# Seleção automatizada

step(fit.model0,scope=list(lower=result0,upper=result),direction=c("forward"))
step(fit.model,direction=c("backward"))
step(fit.model0,scope=list(upper=result),direction=c("both"))



# Modelo Final

fit.model_10 <- glm.nb(Y~v_int_fac+v_eq_c+v_forca_c,
                       link="log")

# IC's dos coeficientes e interpretação

ez <- qnorm(0.975)
rebeta10 <- (summary(fit.model_10))$coef
rebetaM10 <- cbind(rebeta10[,1],rebeta10[,2],rebeta10[,1]-ez*rebeta10[,2],rebeta10[,1]+ez*rebeta10[,2],rebeta10[,3],rebeta10[,4])
xtable(rebetaM10)
mrebeta10 <- rbind(rebetaM10)
covbetaM10 <- vcov(fit.model_10)

confint(fit.model_10)



# Comparando os valores preditos

## Analaise preditiva sobre a base treino

fit.model_10 <- glm.nb(Y~v_int_fac+v_eq_c+v_forca_c,
                       link="log")

pred<-predict(fit.model_10,type=c("response"),se.fit = TRUE)
mupred <- pred$fit
semupred <- pred$se.fit
liIC= mupred-ez*semupred
lsIC= mupred+ez*semupred


plot(Y,cex=1.2,cex.axis=1.2,cex.lab=1.2,pch=19,xlab="Índice",ylab="Número de quedas",
     ylim = c(0,12))
plotCI(mupred,li=liIC,ui=lsIC,pch=17,col=2,add=TRUE)
legend(40,13,c("observado","predito"),col=c(1,2),pch=c(19,17),bty="n",cex=1.3)
xtable(table(liIC <= Y & Y <= lsIC))
#poucos valores observados ficaram dentro dos IC



# Aplicando na base de validação

## Definindo variaveis:
Y_valida <- base.valida$Y
v_int_valida <- base.valida$X1
v_int_fac_valida <- base.valida$X1_fac
v_gen_valida <- base.valida$X2
v_gen_fac_valida <- base.valida$X2_fac
v_eq_valida <- base.valida$X3
v_forca_valida <- base.valida$X4
n_valida <- length(Y_valida)

## centralizando nas médias
v_eq_c_valida <-  v_eq_valida - mean(v_eq_valida)
v_forca_c_valida  <-  v_forca_valida - mean(v_forca_valida)

## Analisando

fit.model_11 <- glm.nb(Y_valida~v_int_fac_valida+v_eq_c_valida+v_forca_c_valida,
                       link="log")
summary(fit.model_11)
xtable(summary(fit.model_11))

desvioM11 <- deviance(fit.model_11)
p <- ncol(model.matrix(fit.model_11))
pvdesvM11 <- 1-pchisq(desvioM11,df=n-p) # percebe-se um p-valor alto, ou seja, se de fato ocorre nessa convergencia para a Qui-quadrado, o ajuste do modelo 
                                        # não esta sendo rejeitado, ou seja, tem-se um indicativo de que os dados tão se adequando ao modelo

diagPoisson(fit.model_11) 
par(mfrow=c(1,1))
envelPoisson(fit.model_11,"log") 

AICM11 <- AIC(fit.model_11)
BICM11 <- BIC(fit.model_11)

ez <- qnorm(0.975)
rebeta11 <- (summary(fit.model_11))$coef
rebetaM11 <- cbind(rebeta11[,1],rebeta11[,2],rebeta11[,1]-ez*rebeta11[,2],rebeta11[,1]+ez*rebeta11[,2],rebeta11[,3],rebeta11[,4])
xtable(rebetaM11)
mrebeta11 <- rbind(rebetaM11)
covbetaM11 <- vcov(fit.model_11)


## Analisando a capacidade preditiva

fit.model_11 <- glm.nb(Y_valida~v_int_fac_valida+v_eq_c_valida+v_forca_c_valida,
                       link="log")

pred<-predict(fit.model_11,type=c("response"),se.fit = TRUE)
mupred <- pred$fit
semupred <- pred$se.fit
liIC= mupred-ez*semupred
lsIC= mupred+ez*semupred


plot(Y_valida,cex=1.2,cex.axis=1.2,cex.lab=1.2,pch=19,xlab="Índice",ylab="Número de quedas", 
     ylim = c(0,12))
plotCI(mupred,li=liIC,ui=lsIC,pch=17,col=2,add=TRUE)
legend(3,11,c("observado","predito"),col=c(1,2),pch=c(19,17),bty="n",cex=1.3)
table(liIC <= Y_valida & Y_valida <= lsIC)
