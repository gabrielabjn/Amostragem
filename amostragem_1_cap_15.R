install.packages('sampling')
library(sampling)
set.seed(666)

# n = 50 -----------------------------------------------------------------------

set.seed(666)
MU284 <- read.csv("/home/ice/Downloads/MU284.csv", sep = "")

hist(MU284$RMT85, col="gray")

summary(MU284$RMT85)
sd(MU284$RMT85)

IAASs<-srswor(80,284)
AASs<-getdata(MU284,IAASs)

media<-mean(AASs$RMT85)
media

sd<-sd(AASs$RMT85)
sd

hist(AASs$RMT85, col="gray")

EPAASs<-sqrt((1-80/284)*var(AASs$RMT85)/80)
EPAASs # erro padrao estiamdo

EPAASs/media # CV estimado

media - 1.96*EPAASs_sem3
media + 1.96*EPAASs_sem3

# n = 80 -----------------------------------------------------------------------
rm(list = ls())

set.seed(666)
MU284 <- read.csv("/home/ice/Downloads/MU284.csv", sep = "")
hist(MU284$RMT85, col="gray")

summary(MU284$RMT85)
sd(MU284$RMT85)

IAASs<-srswor(80,284)
AASs<-getdata(MU284,IAASs)

media<-mean(AASs$RMT85)
media

sd<-sd(AASs$RMT85)
sd

hist(AASs$RMT85, col="gray")

EPAASs<-sqrt((1-80/284)*var(AASs$RMT85)/80)
EPAASs # erro padrao estiamdo

EPAASs/mean(AASs$RMT85) # CV estimado

media - 1.96*EPAASs
media + 1.96*EPAASs

# Remover os 3 Maiores Municipios ----------------------------------------------

set.seed(666)
MU284 <- read.csv("/home/ice/Downloads/MU284.csv", sep = "")

# Ordenar o data frame com base na coluna "P85" em ordem decrescente
MU284_ordenado <- MU284[order(-MU284$P85), ]
nrow(MU284_ordenado)

# Remover os três maiores valores
MU284_ordenado <- MU284_ordenado[-(1:3), ]
nrow(MU284_ordenado)

# Repetir para n = 80 ----------------------------------------------------------
      
hist(MU284_ordenado$RMT85, col="gray")

summary(MU284_ordenado$RMT85)
sd(MU284_ordenado$RMT85)

IAASs_sem3<-srswor(80,284)
AASs_sem3<-getdata(MU284_ordenado,IAASs_sem3)

length(AASs_sem3$RMT85)

media_sem3<-mean(AASs_sem3$RMT85[1:78])
media_sem3

sd_sem3<-sd(AASs_sem3$RMT85[1:78])
sd_sem3

hist(AASs_sem3$RMT85[1:78], col="gray")

EPAASs_sem3<-sqrt((1-80/284)*var(AASs_sem3$RMT85[1:78])/80)
EPAASs_sem3 # erro padrao estiamdo

EPAASs_sem3/media_sem3 # CV estimado

media_sem3 - 1.96*EPAASs_sem3
media_sem3 + 1.96*EPAASs_sem3
