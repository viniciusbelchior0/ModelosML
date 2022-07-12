# Manual de Analise de Dados
# Cap 11. Analise de Correspondencia Simples e Multipla

# Exercicios
#1. Atendimento_Preco ####
library(haven)
Atendimento_Preco <- read_dta("C:/Users/NOTEBOOK CASA/Desktop/Matérias/Estatística/Modelos e Testes/Manual de Análise de Dados/BancosdeDados/Cap. 11/Atendimento x Preço.dta")
View(Atendimento_Preco)

#Tabela de contingencia e teste qui-quadrado
tc <- table(Atendimento_Preco$atendimento, Atendimento_Preco$preço)
tc

chisq.test(tc)

# Analise de correspondencia simples
library(ca)
sca_exe1 <- ca(tc)
summary(sca_exe1) # A parte superior mostra as inercias, que explicam
#quanta variacao cada dimensao explica (eigenvalues), tentar representar
#a relacao entre as variaveis no menor numero de dimensoes possivel

plot(sca_exe1)
#terms are close if they have similar frequency counts. this means the rows
# have similar profiles - rather than similar relationships to a latent
# variable

# the distances on the map are a representation of the chisquared values
# of each row/column to the average profile

plot3d.ca(sca_exe1, labels = c(1,1)) #plot 3d


