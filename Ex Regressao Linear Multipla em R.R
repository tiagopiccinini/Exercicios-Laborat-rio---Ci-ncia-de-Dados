install.packages("ggcorrplot")
library(ggcorrplot)

###########################################################################################################################
# Fazer a matriz de correlações. Dizer quais são as 3 variáveis com maior correlação com o nível de ansiedade.
###########################################################################################################################

matriz_cor <- cor(ansiedade, use = "complete.obs", method = "pearson")

ggcorrplot(matriz_cor, lab = TRUE) # As 3 variáveis são: nível de estresse, sessões de terapia por mês e horas de sono


###########################################################################################################################
# Fazer o diagrama de dispersão para nível de ansiedade X cada uma das variáveis que você mencionou no item anterior
###########################################################################################################################

# Para o nível de estresse
ggplot(ansiedade) +
  aes(x = nivel_stress, y = nivel_ansiedade) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  theme_minimal()

# Para sessões de terapia por mês
ggplot(ansiedade) +
  aes(x = sessoes_terapia_mes, y = nivel_ansiedade) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  theme_minimal()

# Para horas de sono
ggplot(ansiedade) +
  aes(x = horas_de_sono, y = nivel_ansiedade) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  theme_minimal()


###########################################################################################################################
# Separar dados em treino (80%) e teste
###########################################################################################################################

set.seed(123)  # pra garantir reprodutibilidade

# Número total de observações
n <- nrow(ansiedade)

# Índices para os dados de treino (80%)
indices_treino <- sample(1:n, size = 0.8 * n)

# Cria os conjuntos
dados_treino <- ansiedade[indices_treino, ]
dados_teste <- ansiedade[-indices_treino, ]


###########################################################################################################################
# Ajustar um modelo de regressão com "nível de ansiedade" como y
###########################################################################################################################

modelo <- lm(nivel_ansiedade ~ nivel_stress + sessoes_terapia_mes + horas_de_sono, data = dados_treino)

summary(modelo)

previsoes <- predict(modelo, newdata = dados_teste)


###########################################################################################################################
# Calcular a acurácia do modelo (MAPE)
###########################################################################################################################

mape <- mean(abs((dados_teste$nivel_ansiedade - previsoes) / dados_teste$nivel_ansiedade)) * 100
print(paste("MAPE:", round(mape, 2), "%"))





