f_uniforme <- function(x, a, b) {
  ifelse(x >= a & x <= b, 1 / (b - a), 0)
}

esperanca_uniforme <- function(a, b) {
  (a + b) / 2
}

variancia_uniforme <- function(a, b) {
  ((b - a)^2) / 12
}

f_exponencial <- function(x, lambda) {
  ifelse(x >= 0, lambda * exp(-lambda * x), 0)
}

esperanca_exponencial <- function(lambda) {
  1 / lambda
}

variancia_exponencial <- function(lambda) {
  1 / (lambda^2)
}

f_normal <- function(x, mu, sigma) {
  dnorm(x, mean = mu, sd = sigma)
}

esperanca_normal <- function(mu, sigma) {
  mu
}

variancia_normal <- function(mu, sigma) {
  sigma^2
}

# Normal padrão N(0,1)
f_normal_padrao <- function(x) {
  dnorm(x, mean = 0, sd = 1)
}

esperanca_normal_padrao <- function() {
  0
}

variancia_normal_padrao <- function() {
  1
}

# Exemplos
print(f_normal_padrao(0))            # ~0.3989
print(esperanca_normal_padrao())     # 0
print(variancia_normal_padrao())     # 1

# Uniforme contínua [2, 6]
print(f_uniforme(3, 2, 6))             # 0.25
print(esperanca_uniforme(2, 6))        # 4
print(variancia_uniforme(2, 6))        # 1.333...

# Exponencial com lambda = 2
print(f_exponencial(1, 2))             # ~0.2707
print(esperanca_exponencial(2))        # 0.5
print(variancia_exponencial(2))        # 0.25

# Normal com média 0 e desvio 1 (N(0,1))
print(f_normal(0, 0, 1))               # ~0.3989
print(esperanca_normal(0, 1))          # 0
print(variancia_normal(0, 1))          # 1

# Normal Padrão
print(f_normal_padrao(0))            # ~0.3989
print(esperanca_normal_padrao())     # 0
print(variancia_normal_padrao())     # 1

