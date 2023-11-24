# Carrega o pacote necessário para importar dados
library(foreign)

# Leitura dos dados da população de N=338 fazendas
p16ex1.data = read.csv("/home/ice/Downloads/fazendas.csv", sep=";")

# Obtém tabela 16.1
colunas=c("AREA", "QUANT", "RECEITA", "DESPESA")
Tabela16.1 = colSums(p16ex1.data[,colunas])

# Define valores necessarios para calculos com amostra AAS
N = nrow(p16ex1.data)
n = 50
TamPop = rep(N, times=n)

# Seleciona amostra AAS de n=50 fazendas da populacao
aas50.id = sample(1:N, n, replace=FALSE)
aas50.data = p16ex1.data[aas50.id,]

# Carrega o pacote necessário para analisar dados amostrais complexos
library(survey)
# Define objeto com estrutura do plano amostral
aas50.plan = svydesign(data=aas50.data, ids = ~1, fpc =
                         ~TamPop)

# Calcula estimativas simples dos totais populacionais
Tabela16.2.est =
  svytotal(~AREA+QUANT+RECEITA+DESPESA,aas50.plan)

# Obtem valor da razão populacional Producao por hectare
Tabela16.3 = sum(p16ex1.data$QUANT)/sum(p16ex1.data$AREA)

# Calcula estimativa simples da razao de dois totais populacionais
Tabela16.4.est = svyratio(~QUANT, ~AREA, aas50.plan)

# Calcula proporção de fazendas com área menor ou igual a 40 na populacao
Tabela16.5 = mean(p16ex1.data$AREA<=40)

# Estima proporcao de fazendas com area menor ou igual a 40 usando amostra
Tabela16.6.est = svymean(~AREA<=40, aas50.plan)

