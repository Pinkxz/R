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
    sumario_R = summary(vetor)
  )
  
  return(resumo)
}

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

