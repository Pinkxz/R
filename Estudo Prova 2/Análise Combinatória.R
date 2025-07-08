# Análise combinatória
# Funções básicas
# Função PFC: recebe um vetor com o número de possibilidades por etapa
pfc <- function(...){
  etapas <- c(...)
  resultado <- prod(etapas)
  return(resultado)
}

# Exemplo de uso:
print(pfc(3, 4, 2))

##############################################

# Permutação de n elementos: P(n) = n!
permutacao <- function(n) {
  if (n < 0) stop("n deve ser >= 0")
  return(factorial(n))
}

# Exemplo de uso:
print(permutacao(5))

##############################################

# Arranjo simples: A(n, k) = n! / (n - p)!
arranjo <- function(n, k) {
  if (n < 0 || k < 0 || k > n) stop("Valores inválidos para n e p")
  return(factorial(n) / factorial(n - k))
}

# Exemplo de uso:
print(arranjo(5, 3))

##############################################

# Combinação simples: C(n, k) = n! / [k! × (n - k)!]
combinacao <- function(n, k) {
  if (n < 0 || k < 0 || k > n) stop("Valores inválidos para n e p")
  return(choose(n, k))  # ou: factorial(n) / (factorial(p) * factorial(n - p))
}

# Exemplo de uso:
print(combinacao(5, 3))

############################################