# ========================================================
# DISTRIBUIÇÕES DISCRETAS DE VARIÁVEIS ALEATÓRIAS
# ========================================================

# ---------------------- 1. Bernoulli ---------------------
# X ~ Bernoulli(p)
# E[X] = p
# Var(X) = p(1 - p)
bernoulli_pmf <- function(x, p) {
  ifelse(x == 0, 1 - p, ifelse(x == 1, p, 0))
}
esperanca_bernoulli <- function(p) p
variancia_bernoulli <- function(p) p * (1 - p)


# ---------------------- 2. Binomial ----------------------
# X ~ Binomial(n, p)
# E[X] = n * p
# Var(X) = n * p * (1 - p)
binomial_pmf <- function(x, n, p) {
  dbinom(x, n, p)
}
esperanca_binomial <- function(n, p) n * p
variancia_binomial <- function(n, p) n * p * (1 - p)


# ---------------------- 3. Geométrica --------------------
# X ~ Geométrica(p) (número de tentativas até o 1º sucesso)
# E[X] = 1 / p
# Var(X) = (1 - p) / p²
geometrica_pmf <- function(x, p) {
  dgeom(x - 1, p)  # R começa do zero, ajustamos para começar em 1
}
esperanca_geometrica <- function(p) 1 / p
variancia_geometrica <- function(p) (1 - p) / p^2


# ------------------ 4. Binomial Negativa ----------------
# X ~ Binomial Negativa(r, p) (tentativas até r sucessos)
# E[X] = r / p
# Var(X) = r(1 - p) / p²
binomial_negativa_pmf <- function(x, r, p) {
  dnbinom(x - r, size = r, prob = p)
}
esperanca_binomial_negativa <- function(r, p) r / p
variancia_binomial_negativa <- function(r, p) r * (1 - p) / p^2


# -------------------- 5. Hipergeométrica ----------------
# X ~ Hipergeométrica(m, n, k)
# E[X] = k * (m / (m + n))
# Var(X) = k * (m/n) * ((n / (m + n)) * ((m + n - k)/(m + n - 1)))
hipergeometrica_pmf <- function(x, m, n, k) {
  dhyper(x, m, n, k)
}
esperanca_hipergeometrica <- function(m, n, k) {
  k * (m / (m + n))
}
variancia_hipergeometrica <- function(m, n, k) {
  N <- m + n
  k * (m / N) * (n / N) * ((N - k) / (N - 1))
}


# ---------------------- 6. Poisson -----------------------
# X ~ Poisson(λ)
# E[X] = λ
# Var(X) = λ
poisson_pmf <- function(x, lambda) {
  dpois(x, lambda)
}
esperanca_poisson <- function(lambda) lambda
variancia_poisson <- function(lambda) lambda


# ---------------------- EXEMPLOS -------------------------

# Bernoulli com p = 0.7
print(bernoulli_pmf(1, 0.7))                 # P(X = 1)
print(esperanca_bernoulli(0.7))              # E[X]
print(variancia_bernoulli(0.7))              # Var(X)

# Binomial: n = 10, p = 0.5, P(X = 4)
print(binomial_pmf(4, 10, 0.5))
print(esperanca_binomial(10, 0.5))
print(variancia_binomial(10, 0.5))

# Geométrica: p = 0.3, P(X = 3)
print(geometrica_pmf(3, 0.3))
print(esperanca_geometrica(0.3))
print(variancia_geometrica(0.3))

# Binomial Negativa: r = 3, p = 0.5, P(X = 5)
print(binomial_negativa_pmf(5, 3, 0.5))
print(esperanca_binomial_negativa(3, 0.5))
print(variancia_binomial_negativa(3, 0.5))

# Hipergeométrica: m = 7, n = 13, k = 5, P(X = 2)
print(hipergeometrica_pmf(2, 7, 13, 5))
print(esperanca_hipergeometrica(7, 13, 5))
print(variancia_hipergeometrica(7, 13, 5))

# Poisson: λ = 2, P(X = 4)
print(poisson_pmf(4, 2))
print(esperanca_poisson(2))
print(variancia_poisson(2))
