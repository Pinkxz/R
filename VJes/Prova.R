############ Tipos
# Quantitativa x Quantitativa = plot correlação
# Quantitativa x Qualitativa = bloxpot
# Qualitativa x Qualitativa = 
########## Preenchendo dados
banco = read.table("problema3.csv", head=TRUE, sep=";", dec=".")


names(banco)

Tipo = banco$Tipo_Empresa
Area = banco$Area_Estagio
Satis = banco$Satisfacao_Estagio
CH = banco$CH_Estagio
MG = banco$Media_Geral
Disc = banco$Disciplinas_Semestre
################################

########### Sumários
sumario(CH)
sumario(MG)
sumario(Disc)
#######################

# vetores e tabela
tabela <- table(Satis, Area) ; tabela
unique(banco$Satisfacao_Estagio)
tab_Satis = table(Satis) ; tab_Satis
barplot(tab_Satis,
        main = "Nível de Satisfação",
        xlab = "Categorias",
        ylab = "Frequência",
        col = "skyblue",
        border = "black")

# 2
tab_CH = table(CH) ; tab_CH
barplot(tab_CH,         
        main = "Carga Horária dos estagiários",
        xlab = "Carga Horária",
        ylab = "Quantidade de alunos",
        col = "skyblue",
        border = "black")

boxplot(tab_CH, col = "skyblue",
        main = "Carga Horária dos estagiários",
        xlab = "Carga Horária",
        ylab = "Quantidade de alunos")
sumario(tab_CH)
moda(tab_CH)

tab_Area = table(Area) ; tab_Area
tab_Satis = table(Satis) ; tab_Satis
# 3

barplot(t(tabela), main = "Satisfação por área",
        beside = TRUE,
        col = c("lightgreen", "lightblue", "lightyellow", "pink"),
        xlab = "Satisfação",
        ylab = "Frequência",
        names.arg = rownames(tabela),  
        legend.text = colnames(tabela))



# 4
boxplot(CH ~ Tipo,
        main = "Carga Horária x Tipo de empresa",
        xlab = "Tipo de Empresa",
        ylab = "Carha horária",
        col = c("lightgreen", "lightblue", "lightyellow"))



# 5

plot(MG, CH, main="Diagrama de Dispersão - Carga Horária x Média Geral",
     xlab = "Média Geral",
     ylab = "Carga Horária",
     col = "blue")
cor(MG,CH)

lm(MG~CH)
abline(lm(CH~MG), col="red")


plot(Disc, CH, main="Diagrama de Dispersão - Carga Horária x Disciplinas",
     xlab = "Quantidade de disciplinas",
     ylab = "Carga Horária")
cor(Disc,CH)

lm(Disc~CH)
abline(lm(CH~Disc), col="red")


boxplot(CH ~ Disc,
        main = "Carga Horária x Disciplinas",
        xlab = "Quantidade de disciplinas",
        ylab = "Carha horária",
        col = c("lightgreen", "lightblue", "lightyellow"))








































# Histograma com curva de densidade
plot_histograma_densidade <- function(vetor, titulo = "Histograma com Densidade") {
  hist(vetor, probability = TRUE, col = rgb(0.2, 0.4, 0.6, 0.4), 
       main = titulo, xlab = "Valores", ylab = "Densidade", border = "white")
  lines(density(vetor), col = "blue", lwd = 2)
}

# Boxplot simples
plot_boxplot <- function(vetor, titulo = "Boxplot") {
  boxplot(vetor, col = "lightgreen", main = titulo, ylab = "Valores")
}

# Gráfico de densidade
plot_densidade <- function(vetor, titulo = "Gráfico de Densidade") {
  plot(density(vetor), main = titulo, xlab = "Valores", ylab = "Densidade", 
       col = "darkred", lwd = 2)
  polygon(density(vetor), col = rgb(1, 0, 0, 0.3), border = NA)
}

plot_histograma_densidade(a)
plot_boxplot(a)
plot_densidade(a)

dados_agrupados <- function(intervalos, freq) {
  # Calcular os pontos médios dos intervalos
  ponto_medio <- (intervalos[,1] + intervalos[,2]) / 2
  
  # Total de elementos
  N <- sum(freq)
  
  # Frequência relativa
  freq_relativa <- freq / N
  
  # Média aproximada
  media <- sum(ponto_medio * freq) / N
  
  # Variância e desvio padrão
  variancia <- sum(freq * (ponto_medio - media)^2) / N
  desvio_padrao <- sqrt(variancia)
  
  # Frequência acumulada
  freq_acumulada <- cumsum(freq)
  
  # Função auxiliar para mediana e quartis
  encontrar_classe <- function(p) {
    for (i in 1:length(freq_acumulada)) {
      if (freq_acumulada[i] >= p) {
        return(i)
      }
    }
  }
  
  # Tamanho de classe (assumindo classes iguais)
  h <- intervalos[1,2] - intervalos[1,1]
  
  # Mediana
  n2 <- N / 2
  i_med <- encontrar_classe(n2)
  L_med <- intervalos[i_med,1]
  F_ant_med <- ifelse(i_med > 1, freq_acumulada[i_med - 1], 0)
  f_med <- freq[i_med]
  mediana <- L_med + ((n2 - F_ant_med) / f_med) * h
  
  # Quartis
  calc_quartil <- function(k) {
    nk <- N * k
    i <- encontrar_classe(nk)
    L <- intervalos[i,1]
    F_ant <- ifelse(i > 1, freq_acumulada[i - 1], 0)
    f <- freq[i]
    return(L + ((nk - F_ant) / f) * h)
  }
  
  Q1 <- calc_quartil(0.25)
  Q2 <- mediana
  Q3 <- calc_quartil(0.75)
  
  # Resultados
  return(list(
    media = media,
    mediana = mediana,
    Q1 = Q1,
    Q2 = Q2,
    Q3 = Q3,
    variancia = variancia,
    desvio_padrao = desvio_padrao,
    freq_relativa = freq_relativa,
    ponto_medio = ponto_medio
  ))
}

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
    cv = sd(vetor)/mean(vetor) * 100,
    sumario_R = summary(vetor)
  )
  
  return(resumo)
}

moda <- function(v) {
  freq <- table(v)
  moda_valor <- as.numeric(names(freq[freq == max(freq)]))
  return(moda_valor)
}

# Exemplo de uso de intervalos
# Dados da questão 4
intervalos <- matrix(c(
  0, 6,
  6, 12,
  12, 18,
  18, 24,
  24, 30
), ncol = 2, byrow = TRUE)

freq <- c(2800, 1400, 600, 150, 50)

resultado <- dados_agrupados(intervalos, freq)

# Exemplo de acesso aos resultados
resultado$media
resultado$mediana
resultado$Q3
resultado$desvio_padrao

sumario(banco)

