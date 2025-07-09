# ======== VARIÁVEL ALEATÓRIA DISCRETA ========

# Cria variável aleatória discreta a partir de valores e probabilidades
criar_variavel_discreta <- function(valores, probabilidades) {
  if (length(valores) != length(probabilidades)) stop("Tamanhos incompatíveis.")
  if (abs(sum(probabilidades) - 1) > 1e-6) stop("As probabilidades devem somar 1.")
  return(data.frame(X = valores, P = probabilidades))
}

# Função de distribuição acumulada (f.d.a)
fda <- function(variavel) {
  variavel$acumulada <- cumsum(variavel$P)
  return(variavel)
}

# Esperança de variável discreta
esperanca <- function(variavel) {
  return(sum(variavel$X * variavel$P))
}

# Variância de variável discreta
variancia <- function(variavel) {
  ex <- esperanca(variavel)
  return(sum((variavel$X - ex)^2 * variavel$P))
}

# Impressão da f.d.p. (tabela)
fdp <- function(variavel) {
  print("Função de distribuição de probabilidade (f.d.p):")
  print(variavel[, c("X", "P")])
}

# ======== VARIÁVEL ALEATÓRIA CONTÍNUA UNIFORME ========

# f.d.p. da uniforme contínua
f_uniforme <- function(x, a, b) {
  ifelse(x >= a & x <= b, 1 / (b - a), 0)
}

# Probabilidade de X ∈ [x1, x2] para uniforme contínua
prob_uniforme <- function(a, b, x1, x2) {
  if (x1 < a) x1 <- a
  if (x2 > b) x2 <- b
  return(integrate(function(x) f_uniforme(x, a, b), x1, x2)$value)
}

# Esperança da uniforme contínua
esperanca_uniforme <- function(a, b) {
  return((a + b) / 2)
}

# Variância da uniforme contínua
variancia_uniforme <- function(a, b) {
  return(((b - a)^2) / 12)
}

# ======== FUNÇÕES GENÉRICAS (PARA QUALQUER f.d.p. CONTÍNUA) ========

# Esperança de f(x) contínua via integração
esperanca_fdp <- function(f, a, b) {
  integrand <- function(x) x * f(x)
  return(integrate(integrand, a, b)$value)
}

# Variância de f(x) contínua via integração
variancia_fdp <- function(f, a, b) {
  ex <- esperanca_fdp(f, a, b)
  integrand <- function(x) (x - ex)^2 * f(x)
  return(integrate(integrand, a, b)$value)
}

# ======== EXEMPLOS DE USO ========

# Variável aleatória discreta: X = {0, 1, 2}, P = {0.2, 0.5, 0.3}
v <- criar_variavel_discreta(c(0, 1, 2), c(0.2, 0.5, 0.3))
print(v)
print(fda(v))
print(esperanca(v))      # Esperado: 1.1
print(variancia(v))      # Esperado: 0.49
fdp(v)

# Variável uniforme contínua no intervalo [1, 5]
a <- 1
b <- 5

# Probabilidade de X entre 2 e 4
print(prob_uniforme(a, b, 2, 4))          # Esperado: 0.5

# Esperança e variância (analítica)
print(esperanca_uniforme(a, b))          # Esperado: 3
print(variancia_uniforme(a, b))          # Esperado: 1.333...

# Esperança e variância usando integração genérica
f <- function(x) f_uniforme(x, a, b)
print(esperanca_fdp(f, a, b))            # Esperado: 3
print(variancia_fdp(f, a, b))            # Esperado: 1.333...
