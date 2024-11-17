Sys.setlocale("LC_CTYPE", "Spanish_Mexico.UTF-8")
# Instala y carga el paquete si no lo tienes
library(car)
# Reporte 2
lifeExpectancy <- read.csv("C:/Users/hamga/Documents/maestria/1er/MEB/codigosR/ProyectoMEB/LifeExpectancyData.csv")
lifeExpectancy
# Omitir filas donde haya valores NA
lifeExpectancy <- na.omit(lifeExpectancy)
colSums(is.na(lifeExpectancy))


# A
# Matriz de dispersión con ggpairs
ggpairs(lifeExpectancy)

cor_matrix <- cor(lifeExpectancy, use = "complete.obs")  # "complete.obs" para ignorar valores NA
print(cor_matrix)



#B
# Usar un modelo lineal para calcular el VIF
modelo <- lm(Life.expectancy ~ Adult.Mortality + Alcohol + BMI + 
               Total.expenditure + HIV.AIDS + GDP + 
               Income.composition.of.resources + Schooling, data = lifeExpectancy)
# Calcular VIF
vif_values <- vif(modelo)
print("Valores de VIF:")
print(vif_values)

summary(modelo)