Sys.setlocale("LC_CTYPE", "Spanish_Mexico.UTF-8")
set.seed(123)

library(car)
library(dplyr)

# Read Dataset
lifeExpectancy <- read.csv("C:/Users/hamga/Documents/maestria/1er/MEB/codigosR/ProyectoMEB/LifeExpectancyData.csv")
lifeExpectancy
lifeExpectancy <- na.omit(lifeExpectancy)
colSums(is.na(lifeExpectancy))
muestra <- lifeExpectancy[sample(nrow(lifeExpectancy), 100), ]
write.csv(muestra, file = "ProyectoMEB/graficas/reporte3/muestra100.csv", row.names = FALSE)

# A
# Matriz dispersión
png("ProyectoMEB/graficas/reporte3/matriz_dispersion.png", width = 1200, height = 1200)
plot(muestra,
     main = "Matriz de dispersión")
dev.off() 

cor_matrix <- cor(muestra)
print(cor_matrix)



#B
modelo <- lm(Life.expectancy ~ Adult.Mortality + Alcohol + Total.expenditure + BMI + HIV.AIDS + GDP + 
               Income.composition.of.resources + Schooling, data = muestra)
vif_values <- vif(modelo)
print(vif_values)


#C
summary(modelo)

confint(modelo)