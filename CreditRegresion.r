# Instalar y cargar las librerías necesarias
install.packages("ISLR")
install.packages("GGally")
install.packages("ggplot2")
install.packages("car")
install.packages("corrplot")

library(ISLR)
library(GGally)
library(ggplot2)
library(car)
library(corrplot)
# Cargar los datos
data("Credit", package = "ISLR")

# Explorar los datos
head(Credit)
summary(Credit)
str(Credit)

# Visualizar relaciones entre variables
ggpairs(Credit[, c("Income", "Limit", "Rating", "Cards", "Age", "Education", "Balance")])


# Verificar valores faltantes
sum(is.na(Credit))

# Convertir variables categóricas a factores
Credit$Gender <- as.factor(Credit$Gender)
Credit$Student <- as.factor(Credit$Student)
Credit$Married <- as.factor(Credit$Married)
Credit$Ethnicity <- as.factor(Credit$Ethnicity)

# Modelo de regresión múltiple
modelo <- lm(Balance ~ Income + Limit + Rating + Cards + Age + Education + Gender + Student + Married + Ethnicity, data = Credit)

# Resumen del modelo
summary(modelo)

# Diagnóstico del modelo
  
# Verificar multicolinealidad
vif(modelo)


# Matriz de correlación
cor_matrix <- cor(Credit[, c("Income", "Limit", "Rating", "Cards", "Age", "Education", "Balance")])
print(cor_matrix)

# Visualización de la matriz de correlación

corrplot(cor_matrix, method = "circle", addCoef.col = "black")


# Predicciones del modelo
Credit$Predicciones <- predict(modelo, newdata = Credit)
print(Credit[, c("Balance", "Predicciones")])

# Gráfico de valores reales vs predichos
ggplot(Credit, aes(x = Balance, y = Predicciones)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(x = "Valores Reales", y = "Valores Predichos", title = "Valores Reales vs Predichos")

# Gráfico de residuos
Credit$residuals <- residuals(modelo)
ggplot(Credit, aes(x = Predicciones, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  labs(x = "Valores Predichos", y = "Residuos", title = "Valores Predichos vs Residuos")

# Rehacer el modelo sin la variable 'Rating'
modelo_sin_rating <- lm(Balance ~ Income + Limit + Rating, data = Credit)
print(summary(modelo_sin_rating))

# Calcular los VIFs nuevamente
vif(modelo_sin_rating)

# Matriz de correlación
cor_matrix_sin_rating <- cor(Credit[, c("Income", "Limit", "Balance")])
print(cor_matrix_sin_rating)

# Visualización de la matriz de correlación
corrplot(cor_matrix_sin_rating, method = "circle", addCoef.col = "black")


# Predicciones del nuevo modelo
Credit$Predicciones_sin_rating <- predict(modelo_sin_rating, newdata = Credit)
print(Credit[, c("Balance", "Predicciones_sin_rating")])

# Gráfico de valores reales vs predichos
ggplot(Credit, aes(x = Balance, y = Predicciones_sin_rating)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(x = "Valores Reales", y = "Valores Predichos", title = "Valores Reales vs Predichos (sin DATA innecesaria)")


# Gráfico de VALORES REALES vs VALORES PREDICHOS vs Valores PREDICHOS sin (DATA innecesaria)
p <- ggplot(Credit, aes(x = Balance)) +
  geom_density(aes(fill = "Real"), alpha = 0.5) +
  geom_density(aes(x = Predicciones, fill = "Predicho"), alpha = 0.5) +
  geom_density(aes(x = Predicciones_sin_rating, fill = "Predicho sin data innecesaria"), alpha = 0.5) +
  labs(x = "Balance", y = "Densidad", title = "Distribución de Valores Reales y Predichos") +
  scale_fill_manual(values = c("Real" = "blue", "Predicho" = "red", "Predicho sin data innecesaria" = "green"))

# Guardar la imagen con un tamaño específico
ggsave("mi_grafico.png", plot = p, width = 10, height = 6)


# Gráfico de residuos
Credit$residuals_sin_rating <- residuals(modelo_sin_rating)
ggplot(Credit, aes(x = Predicciones_sin_rating, y = residuals_sin_rating)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  labs(x = "Valores Predichos", y = "Residuos", title = "Valores Predichos vs Residuos (sin Rating)")
