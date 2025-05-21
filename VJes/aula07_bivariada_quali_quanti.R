#############################################
#### Codigos da aula - Analise Bivariada ####
####      Qualitativa x Quantitativa      ###
#############################################

require(tidyverse)


tab= read.table("life_expectancy.csv", head=TRUE, sep=";")


### Exemplo 1

tapply(tab$salario, tab$instrucao, summary)

boxplot(salario ~ instrucao, data = tab,
        col=c("cadetblue1", "lightskyblue1", "paleturquoise3"),
        main = "Salários por Grau de Instrução",
        xlab = "Grau de Instrução",
        ylab = "Salário")



### Exemplo 2

tapply(tab$salario, tab$regiao, summary)

boxplot(salario ~ regiao, data = tab,
        col=c("seashell1", "seashell2", "seashell3"),
        main = "Salários por Região de Procedência",
        xlab = "Região de Procedência",
        ylab = "Salário")



#########################################
###### Ajuda com o trabalho #############
#########################################
# Leitura dos dados
dados <- read.table("life_expectancy.csv", head=TRUE, sep=";", dec=",")

siglas <- c(
  "Africa" = "AF",
  "Asia" = "AS",
  "Central America and Caribbean" = "CA",
  "European Union" = "EU",
  "Middle East" = "ME",
  "North America" = "NA",
  "Oceania" = "OC",
  "Rest of Europe" = "RE",
  "South America" = "SA"
)

dados$Region <- siglas[dados$Region]


results <- tapply(dados$Life_expectancy, dados$Region, summary)


results_frame <- do.call(rbind, results)
results_frame <- data.frame(Regiao = rownames(results_frame), results_frame, row.names = NULL)

print(results_frame)

# boxplot
boxplot(dados$Life_expectancy ~ dados$Region,
        main = "Expectativa de vida por região (siglas)",
        xlab = "Região",
        ylab = "Expectativa de vida")

boxplot(dados$Infant_deaths ~ dados$Region,
        main = "Mortes infantis por região",
        xlab = "Região",
        ylab = "Mortes Infantis")
        