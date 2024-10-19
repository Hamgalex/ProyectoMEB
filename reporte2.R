# Reporte 2
lifeExpectancy <- read.csv("C:/Users/hamga/Documents/maestria/1er/MEB/codigosR/ProyectoMEB/LifeExpectancyData.csv")
lifeExpectancy

# Omitir filas donde haya valores NA
lifeExpectancy <- na.omit(lifeExpectancy)
colSums(is.na(lifeExpectancy))

# Medidas descriptivas
for (col in colnames(lifeExpectancy)) {
  cat("\nMedidas descriptivas para columna", col, ":\n")
  print(summary(lifeExpectancy[[col]]))
}

# Histogramas
hist(lifeExpectancy$Life.expectancy,
     main = "Histograma de Life Expectancy",
     xlab = "Valores",
     ylab = "Frecuencia",
     col = "skyblue",
     border = "black",
     ylim = c(0, 700)
)

hist(lifeExpectancy$Adult.Mortality,
     main = "Histograma de Adult Mortality",
     xlab = "Valores",
     ylab = "Frecuencia",
     col = "skyblue",
     border = "black",
     xlim = c(0, 800)
)

hist(lifeExpectancy$Alcohol,
     main = "Histograma de Alcohol",
     xlab = "Valores",
     ylab = "Frecuencia",
     col = "skyblue",
     border = "black",
     ylim = c(0, 700),
     axes = FALSE
)
axis(2)
axis(1, at = seq(0, 18, by = 3))

hist(lifeExpectancy$BMI,
     main = "Histograma de BMI",
     xlab = "Valores",
     ylab = "Frecuencia",
     col = "skyblue",
     border = "black"
)

hist(lifeExpectancy$GDP,
     main = "Histograma de GDP",
     xlab = "Valores",
     ylab = "Frecuencia",
     col = "skyblue",
     border = "black",
     ylim = c(0, 2000)
)

hist(lifeExpectancy$HIV.AIDS,
     main = "Histograma de HIV AIDS",
     xlab = "Valores",
     ylab = "Frecuencia",
     col = "skyblue",
     border = "black",
     ylim = c(0, 2500),
     axes = FALSE
)
axis(2)
axis(1, at = seq(0, 55, by = 5))

hist(lifeExpectancy$Income.composition.of.resources,
     main = "Histograma de Income Composition of Resources",
     xlab = "Valores",
     ylab = "Frecuencia",
     col = "skyblue",
     border = "black",
     ylim = c(0, 600)
)

hist(lifeExpectancy$Schooling,
     main = "Histograma de Schooling",
     xlab = "Valores",
     ylab = "Frecuencia",
     col = "skyblue",
     border = "black",
     axes = FALSE
)
axis(2)
axis(1, at = seq(0, 22, by = 2))

hist(lifeExpectancy$Total.expenditure,
     main = "Histograma de Total Expenditure",
     xlab = "Valores",
     ylab = "Frecuencia",
     col = "skyblue",
     border = "black"
)

# Boxplot

boxplot(lifeExpectancy$Life.expectancy,
        main = "Boxplot de life expectancy",
        xlab = "Poblacion",
        ylab = "Esperanza de vida",
        col = "skyblue",
        border = "black"
)