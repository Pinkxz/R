# Função para probabilidade condicional P(A|B)
prob_condicional <- function(P_A_e_B, P_B) {
  if (P_B == 0) stop("P(B) não pode ser zero.")
  return(P_A_e_B / P_B)
}

# Exemplo:
# P(A ∩ B) = 0.12, P(B) = 0.3
print(prob_condicional(0.12, 0.3))  # Resultado esperado: 0.4

# Função para Teorema de Bayes: P(A|B)
bayes <- function(P_B_dado_A, P_A, P_B) {
  if (P_B == 0) stop("P(B) não pode ser zero.")
  return((P_B_dado_A * P_A) / P_B)
}

# Exemplo:
# P(B|A) = 0.9, P(A) = 0.2, P(B) = 0.5
print(bayes(0.9, 0.2, 0.5))  # Resultado esperado: 0.36
