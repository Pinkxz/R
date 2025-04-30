# Exercicio 1
tempo = c(18, 14, 14, 15, 14, 34, 16, 17, 21, 26)

min = min(tempo)
max = max(tempo)
mean = mean(tempo)
min
max
mean
tempo[6] = 15
mean



# Exercicio 2
x<-c(1,3,5,7,9)
y<-c(2,3,5,7,11,13)

x+1
# Prévia (a) -> Soma de todos os elementos +1.
# Resposta -> Correto.

y*2
# Prévia (b) -> Multiplicação de todos os elementos por 2.
# Resposta -> Correto.

length(x) 
length(y)
# Prévia (c) -> Retorna o tamanho do objeto passado como parâmetro.
# Resposta -> Correto.

x+y
# Prévia (d) -> Erro. Não tem como somar dois objetos de tamanhos diferentes.
# Resposta -> Meio correto. Objetos de tamanhos múltiplos do outro podem, apesar de que não compreendi muito bem ainda como.

y[3]
# Prévia (e) -> Retorna o valor da terceira posição do objeto.
# Resposta -> Correto.

y[-3]
# Prévia (f) -> Remove o terceiro valor do objeto.
# Resposta -> Correto.



# Exercício 3
A = c(1, 2, 3, 4, 5)
B = c(4, 5, 6, 7)

# Une os valores e retorna
union(A, B)

# Captura os valores que se repetem e retorna
intersect(A, B)

# Captura os elementos de A que não estão em B e retorna.
setdiff(A, B)

# Captura os elementos de B que não estão em A e retorna.
setdiff(B, A)




# Exercício 4
locais = rep(c("A", "B"), each = 5)
dados = c(12, 41, 12, 24, 42, 45, 51, 52, 51, 53)

tabela = data.frame(locais, dados)

print(tabela)



# Exercício 5
num = 1:60
sample(num, 6)

# Tranformando em função
mega_sena = function(){
  sorteio = sample(1:60, 6)
  return(sort(sorteio))
}

mega_sena()




# Exercício 6 
# $ para acessar uma coluna/linha especifica de uma tabela x. Ex: alunos$nome
# Exemplo de dados
alunos = data.frame(
  nome = c("João", "Maria", "Pedro", "Ana"),
  altura = c(1.75, 1.60, 1.80, 1.65), 
  peso = c(70, 55, 80, 60)
)


# A
# Função para calcular o IMC de todos os alunos
calcular_imc_todos = function(alunos) {
  alunos$imc <- alunos$peso / (alunos$altura^2)
  return(alunos)
}

# Chamando a função
alunos_imc = calcular_imc_todos(alunos)
print(alunos_imc)



# B
# Função para calcular o IMC de um aluno específico
calcular_imc_aluno = function(nome_aluno, alunos) {
  aluno = alunos[alunos$nome == nome_aluno, ]
  if (nrow(aluno) == 0) {
    return("Aluno não encontrado.")
  }
  imc = aluno$peso / (aluno$altura^2)
  return(imc)
}


imc_joao = calcular_imc_aluno("João", alunos)
print(imc_joao)



# Exercicio 7
temp = c(9, 0, 10, 13, 15, 17, 18, 17, 22, 11, 15)

# Função que retorna todos os resultados de forma identificada
transformacoes = function(temp) {
  resultados = list(
    raiz_quadrada = sqrt(temp),            
    log_natural = log(temp),  
    log_x_mais_1 = log(temp + 1),  
    ao_quadrado = temp^2  
  )
  
  return(resultados)
}


resultados= transformacoes(temp)

# Exibindo os resultados
print(resultados)

