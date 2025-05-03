# Carregando dados
pobreza <- read.table("pobreza_brasil.csv", head = TRUE, sep = ";", dec = ".")

# Função para criar os gráficos
plot_bar_porcent <- function(var, xlab) {
  par(mar = c(5, 4, 4, 6))  # Aumenta a margem da direita
  
  # Normalizando dados em porcentagem
  tab <- table(pobreza$Pobreza, pobreza[[var]])
  tab_porcent <- prop.table(tab, margin = 2) * 100
  
  # Plotando os gráficos
  bp <- barplot(tab_porcent, beside = TRUE,
                col = c("lightblue", "lightgreen"),
                ylab = "Porcentagem", xlab = xlab,
                ylim = c(0, 100))
  
  # Titulos e Quadrado de legenda (Foi um saco deixar ele fixo)
  text(x = bp, y = tab_porcent, labels = round(tab_porcent, 1), pos = 3, cex = 0.8)
  legend(x = max(bp), y = 100,
         legend = rownames(tab),
         fill = c("lightblue", "lightgreen"),
         bty = "y", xpd = TRUE)
}

# Gerando todos os gráficos com uma função (Evitando uma repetição de código)
plot_bar_porcent("Cor", "Cor")
plot_bar_porcent("Escolaridade", "Escolaridade")
plot_bar_porcent("Reg_metrop", "Pobreza por Região")
plot_bar_porcent("Cart_assinada", "Pobreza x Carteira Assinada")
plot_bar_porcent("Trabalho", "Pobreza x Trabalho")
