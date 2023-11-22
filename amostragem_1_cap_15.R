install.packages('sampling')
library(sampling)

MU284 <- read.csv("/home/ice/Downloads/MU284.csv", sep = "")
hist(MU284$RMT85, col="gray")

summary(MU284$RMT85)
sd(MU284$RMT85)

IAASs<-srswor(50,284)
AASs<-getdata(MU284,IAASs)

mean(AASs$RMT85)
sd(AASs$RMT85)
hist(AASs$RMT85, col="gray")

EPAASs<-sqrt((1-50/284))*((var(AASs$RMT85))/50)
EPAASs

# erro padrao grande





