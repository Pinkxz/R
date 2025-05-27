# Lista - Atividades
## Função para facilitar minha vida

install.packages("moments")
sumario = function(vetor) {
  if (!is.numeric(vetor)) {
    stop("O vetor deve ser numérico.")
  }
  
  resumo <- list(
    comprimento = length(vetor),
    soma = sum(vetor),
    media = mean(vetor),
    mediana = median(vetor),
    desvio_padrao = sd(vetor),
    variancia = var(vetor),
    minimo = min(vetor),
    maximo = max(vetor),
    quartis = quantile(vetor),
    assimetria = moments::skewness(vetor),
    curtose = moments::kurtosis(vetor),
    valores_unicos = unique(vetor),
    tabela_frequencia = table(vetor),
    sumario_R = summary(vetor)
  )
  
  return(resumo)
}

moda <- function(v) {
  freq <- table(v)
  moda_valor <- as.numeric(names(freq[freq == max(freq)]))
  return(moda_valor)
}


# Exercício 1

# a)

(0 * 25 + 1 * 20 + 2 * 3 + 3 * 1 + 4 * 1) / 50
# Média de erros = 0,66

# b)
vector_Erro = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 4)

(50 + 1) /2 
# Mediana = 45,5

# c) 
sd(vector_Erro)
# desvio padrão = 0.847

# d)
erros <- c(0, 1, 2, 3, 4)
frequencias <- c(25, 20, 3, 1, 1)

barplot(frequencias, names.arg=erros,
        col="skyblue", main="Distribuição de erros por página",
        xlab="Número de erros", ylab="Frequência")

# e)
# Média de erros por 500 páginas = média de erro * páginas
mean(vector_Erro) * 500
# Resposta = 330


# Exercício 2
num_Casas = c (2, 2, 3, 10, 13, 14, 15, 15, 16, 16,
               18, 18, 20, 21, 22, 22, 23, 24, 25, 25,
               26, 27, 29, 29, 30, 32, 36, 42, 44, 45,
               45, 46, 48, 52, 58, 59, 61, 61, 61, 65,
               66, 66, 68, 75, 78, 80, 89, 90, 92, 97)
# a)
mean(num_Casas)
# 40,42
sd(num_Casas)
# 26,07

# b)
hist(num_Casas, breaks=5, col="skyblue", main="Histograma - Nº de casas por quarteirão",
     xlab="Nº de casas", ylab="Frequência", right=FALSE)
# C)

boxplot(num_Casas, col="orange", main="Boxplot - Nº de casas por quarteirão")

# d)
# Analisando, percebemos que:
  
#  A mediana provavelmente será inferior à média, indicando assimetria para baixo.
# O boxplot mostrará cauda superior alongada, reforçando a assimetria positiva.
# No mais uma distribuição bem concetrada por volta de uns 25-80, com nenhum valor sendo outlier.


# Exercicío 3

# Número de filhos (representando "mais que 5" como 6)
num_filhos <- c(0, 1, 2, 3, 4, 5, 6)
freq_familias <- c(17, 20, 28, 19, 7, 4, 5)

familias <- rep(num_filhos, freq_familias)
# a)
median(familias)
# 2

# b)
moda(familias)
# 2

# c)
# A parte de mais que 5 filhos impede a gente de ter um média confiavel, já que não sabemos o total de todas
# familias.
mean(familias)
# Mas usando esse vetor a média é 2.11

# Exercicio 4

# Intervalos e frequências
classes <- c("0-6", "6-12", "12-18", "18-24", "24-30")
freq <- c(2800, 1400, 600, 150, 50)
ponto_medio <- c(3, 9, 15, 21, 27)  # (limite_inf + limite_sup) / 2

media <- sum(freq * ponto_medio) / sum(freq)
media
# A média de anos juntos é 6.9
mediana <- 0 + ((2500 - 0) / 2800) * 6
mediana
# A mediana é 5.35

media <- sum(freq * ponto_medio) / sum(freq)
variancia <- sum(freq * (ponto_medio - media)^2) / sum(freq)
desvio_padrao <- sqrt(variancia)

variancia
desvio_padrao

# var 27
#desvio_padrao 5,25

# c)
barplot(freq,
        names.arg = classes,
        col = "skyblue",
        main = "Número de divórcios por anos de casamento",
        xlab = "Anos de casamento",
        ylab = "Nº de divórcios")

# d)
# Sim, é possível (essa confesso que peguei do gpt)
# Q1 (k = 0.25)
k1 <- 0.25 * total  # 1250
classe_q1 <- which(acumulada >= k1)[1]
L1 <- lim_inf[classe_q1]
F1 <- ifelse(classe_q1 == 1, 0, acumulada[classe_q1 - 1])
f1 <- freq[classe_q1]
q1 <- L1 + ((k1 - F1) / f1) * h

# Q3 (k = 0.75)
k3 <- 0.75 * total  # 3750
classe_q3 <- which(acumulada >= k3)[1]
L3 <- lim_inf[classe_q3]
F3 <- ifelse(classe_q3 == 1, 0, acumulada[classe_q3 - 1])
f3 <- freq[classe_q3]
q3 <- L3 + ((k3 - F3) / f3) * h

q1
q3


# Exericico 5
# a) Todas as observações se multiplicam por 2, por ex: se uma média seria 5, ela vira 10

# b) Média: Aumenta em 10: Se era 5, vira 15. Mediana: Também aumenta em 10, e a posição não muda, só valor.
#    Desvio padrão: Não muda.

# c) Isso centraliza os dados em torno de 0. Ex: se a média era 5, e se era x=(2,5,8), então𝑥′=(−3, 0, 3)
# Média: Vira 0, pois os dados agora estão centrados.

# Mediana: Pode mudar, mas tende a ficar próxima de 0 se os dados forem simétricos.

# Desvio padrão: Permanece o mesmo, pois as distâncias dos valores em relação à média continuam iguais.

# d) Média: vira 0
# Mediana: Fica próxima de 0.
# DP: fica próximo de 1


# Exercicío 6

# a) Menos. Devido a média estar sendo puxado basicamente por outliers, ficando acima do terceiro quartil.
# Porém a maioria dos valores ainda ficam abaixo dessa cauda pesada.
# b) Devido a variância mínima, a chance de todos ganharem por volta de 7000 são altas.
# Fica então um situação de segurança x risco.

# Exercício 7

#