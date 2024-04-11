############### Amostragem II ###############
# Data: 21/03/2024

#### Parte 00 ####

# Mudar diretório de trabalho
setwd("/home/ice/Downloads")

# Carrega o pacote necessário para importar dados
library(foreign)

# Leitura dos dados da população de N=338 fazendas
p00ex1.data = read.csv(file="Fazendas.csv", sep=";")

# Obtém tabela 00.1
colunas=c("AREA", "QUANT", "RECEITA", "DESPESA")
Tabela00.1 = colSums(p00ex1.data[,colunas])

# Define valores necessarios para calculos com amostra AAS
N = nrow(p00ex1.data)
n = 50
TamPop = rep(N, times=n)

# Seleciona amostra AAS de n=50 fazendas da populacao
aas50.id = sample(1:N, n, replace=FALSE)
aas50.data = p00ex1.data[aas50.id,]

# Carrega o pacote necessário para analisar dados amostrais complexos
library(survey)

# Define objeto com estrutura do plano amostral
aas50.plan = svydesign(data=aas50.data, ids = ~1, fpc = ~TamPop)

# Calcula estimativas simples dos totais populacionais
Tabela00.2.est = svytotal(~AREA+QUANT+RECEITA+DESPESA,aas50.plan)

# Obtem valor da razão populacional Producao por hectare
Tabela00.3 = sum(p00ex1.data$QUANT)/sum(p00ex1.data$AREA)

# Calcula estimativa simples da razao de dois totais populacionais
Tabela00.4.est = svyratio(~QUANT, ~AREA, aas50.plan)

# Calcula proporção de fazendas com área menor ou igual a 40 na população
Tabela00.5 = mean(p00ex1.data$AREA<=40)

# Estima proporcao de fazendas com area menor ou igual a 40 usando amostra
Tabela00.6.est = svymean(~AREA<=40, aas50.plan)

#### Parte 01 ####

# Abrir arquivo “Alunos.txt”
Alunos = read.table("Alunos.txt", header=T)

# Instala e carrega o pacote sampling
install.packages("sampling")
library(sampling)

# Calcula proporção de alunos por estrato
prop.table(table(Alunos$rede))

# Calcula o tamanho da amostra de alunos por estrato
ceiling(500*(prop.table(table(Alunos$rede))))

IAESs=sampling::strata(Alunos, stratanames=c("rede"), c(163,284,55),
                       method=c("srswor"))
AESs=getdata(Alunos,IAESs)
fpc=rep(c(1948,3400,652),c(163,284,55))
Plano=svydesign(~1, strata=~rede, data = AESs, probs=~IAESs$Prob, fpc=~fpc)
svymean(~port,Plano)

# 1. Selecionar uma amostra aleatoria simples sem reposicao do mesmo tamanho da amostra 
# estratificada.
N <- nrow(Alunos)
n <- 502
TamPop = rep(N, times=n)

AASs.id <- sample(1:N, n, replace=FALSE)
AASs.data <- Alunos[AASs.id,]

# 2. Estime a media da proficiencia em lingua portuguesa e tambem o erro padrao da media.
media <- mean(AASs.data$port)
erro <- sd(AASs.data$port) / sqrt(n)

# 3. Compare os resultados da amostragem estratificada com os da amostragem aleatoria simples.

# 4. Qual metodo resultou em um erro padrao melhor?

mean(c(4.084, 3.98, 4.21, 4.155))
mean(c(4.3415, 4.13, 4.41, 4.16))
