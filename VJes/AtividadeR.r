# Aluno: Victor Mateus

# EX1

n=60               # Amostra
media=45.21; media # Media
alpha=0.10         # Taxa de erro
dp_pop=6.32        # Desvio padrão populacional

z_alpha=qnorm(p=alpha/2, mean=0, sd=1); z_alpha
dp_amostral=dp_pop/sqrt(n); dp_amostral
erro_amostral = abs(z_alpha)*dp_amostral; erro_amostral


l_inf = media - abs(z_alpha)*dp_amostral
l_sup = media + abs(z_alpha)*dp_amostral

IC = c(l_inf, l_sup); IC

# EX2

##########################

alpha=0.05
z_alpha=qnorm(p=alpha/2, mean=0, sd=1); z_alpha

dp_pop=sqrt(350)

erro_max=5

n =(z_alpha*dp_pop/erro_max)^2; n

IC = c(l_inf, l_sup); IC

floor(n)+1

# EX3
  
alpha=0.03
z_alpha=qnorm(p=alpha/2, mean=0, sd=1); z_alpha


n=500
p_amostral=120/500 ; p_amostral
p_pop=0.24
q_pop=0.76

dp_amostral=sqrt(p_pop*q_pop/n); dp_amostral

erro_amostral = abs(z_alpha)*dp_amostral; erro_amostral

l_inf = p_amostral - abs(z_alpha)*dp_amostral
l_sup = p_amostral + abs(z_alpha)*dp_amostral


IC = c(l_inf, l_sup); IC

#alternativa

IC = 100*round(c(l_inf, l_sup),3); IC


# EX4

n=400               # Amostra
media=1000; media # Media
alpha=0.01         # Taxa de erro
dp_pop=200        # Desvio padrão populacional

z_alpha=qnormm(p=alpha/2, mean=0, sd=1); z_alpha
dp_amostral=dp_pop/sqrt(n); dp_amostral
erro_amostral = abs(z_alpha)*dp_amostral; erro_amostral


l_inf = media - abs(z_alpha)*dp_amostral
l_sup = media + abs(z_alpha)*dp_amostral

IC = c(l_inf, l_sup); IC


# EX5 
# EX6




Aluno: Victor Mateus

# EX1
IC = 43.86 46.55

# EX2
N = 54

# EX3


# EX4
# EX5 
# EX6