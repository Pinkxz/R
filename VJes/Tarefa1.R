########################################
##### Atividade - Dados de Pobreza #####
########################################
########## Por: Victor Mateus ##########

# Carregando dados
pobreza <- read.table("pobreza_brasil.csv", head = TRUE, sep = ";", dec = ".")


tab <- table(pobreza$Pobreza)

# Normalizando porcetagem
pct <- round(prop.table(tab) * 100, 1)


labels <- paste(pct, "%")

pie(tab, labels = labels,
    main = "Pobreza no Brasil em 2020",
    col = c("lightblue", "lightgreen"))

legend("topright", 
       legend = names(tab),
       fill = c("lightblue", "lightgreen"),
       title = "Situação de Pobreza") 



# Função para criar gráfico, tabela e porcentagem
plot_bar_porcent <- function(var, xlab) {
  par(mar = c(5, 4, 4, 6))  # Aumentando a margem da direita
  
  # Tabelas e normalização
  tab_abs <- table(pobreza$Pobreza, pobreza[[var]])
  tab_pct <- round(prop.table(tab_abs, margin = 2) * 100, 1)
  
  # Exibindo tabelas no console
  cat("\n==============================\n")
  cat("Análise de Pobreza por", xlab, "\n")
  cat("==============================\n")
  print("Tabela de Frequência Absoluta:")
  print(tab_abs)
  print("Tabela de Porcentagem (% por coluna):")
  print(tab_pct)
  
  # Gráficos
  cores <- c("lightblue", "lightgreen")
  bp <- barplot(tab_pct, beside = TRUE,
                col = cores,
                ylab = "Porcentagem", xlab = xlab,
                ylim = c(0, 100),
                main = paste("Pobreza por", xlab))
  
  # Titulos e Quadrado de legenda (Foi um saco deixar ele fixo)
  text(x = bp, y = tab_pct, labels = tab_pct, pos = 3, cex = 0.8)
  legend(x = max(bp), y = 100,
         legend = rownames(tab_abs),
         fill = cores,
         bty = "y", xpd = TRUE)
}

# Gerando todos os gráficos com uma função (Evitando uma repetição de código)
plot_bar_porcent("Cor", "Cor")
plot_bar_porcent("Escolaridade", "Escolaridade")
plot_bar_porcent("Reg_metrop", "Região")
plot_bar_porcent("Cart_assinada", "Carteira Assinada")
plot_bar_porcent("Trabalho", "Trabalho")
