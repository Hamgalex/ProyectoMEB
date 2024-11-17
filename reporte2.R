Sys.setlocale("LC_CTYPE", "Spanish_Mexico.UTF-8")
# Reporte 2
lifeExpectancy <- read.csv("C:/Users/hamga/Documents/maestria/1er/MEB/codigosR/ProyectoMEB/LifeExpectancyData.csv")
lifeExpectancy

# Omitir filas donde haya valores NA
lifeExpectancy <- na.omit(lifeExpectancy)
colSums(is.na(lifeExpectancy))

###################################### A ###########################################

# Medidas descriptivas
for (col in colnames(lifeExpectancy)) {
  cat("\nMedidas descriptivas para columna", col, ":\n")
  print(summary(lifeExpectancy[[col]]))
  mean_value <- mean(lifeExpectancy[[col]], na.rm = TRUE)
  median_value <- median(lifeExpectancy[[col]], na.rm = TRUE)
  variance_value <- var(lifeExpectancy[[col]], na.rm = TRUE)
  sd_value <- sd(lifeExpectancy[[col]], na.rm = TRUE)
  cat("Varianza:", variance_value, "\n")
  cat("Desviación estándar:", sd_value, "\n")
}

# LIFE EXPECTANCY
data <- lifeExpectancy$Life.expectancy
png("ProyectoMEB/graficas/LifeExpectancyHist.png", width = 800, height = 600)
histograma <- hist(data,
                   main = "Histograma de Life Expectancy",
                   xlab = "Esperanza de vida en años",
                   ylab = "Frecuencia",
                   col = "skyblue",
                   border = "black",
                   ylim = c(0, 700),
                   axes = FALSE
)
axis(2)
axis(1, at = seq(35, 90, by = 5))
dev.off()
frecuencias <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), tail(histograma$breaks, -1), sep = " - "),
  Frecuencia = histograma$counts
)
write.csv(frecuencias, "ProyectoMEB/tablas/LifeExpectancy.csv", row.names = FALSE)
png("ProyectoMEB/graficas/LifeExpectancyBoxPlot.png", width = 800, height = 600)
boxplot(data,
        main = "Boxplot de life expectancy",
        xlab = "Población",
        ylab = "Esperanza de vida en años",
        col = "skyblue",
        border = "black"
)
dev.off()

# ADULT MORTALITY
data <- lifeExpectancy$Adult.Mortality
png("ProyectoMEB/graficas/AdultMortalityHist.png", width = 800, height = 600)
histograma <- hist(data,
                   main = "Histograma de Adult Mortality",
                   xlab = "% de morir entre 15 y 60 años",
                   ylab = "Frecuencia",
                   col = "skyblue",
                   border = "black",
                   xlim = c(0, 800),
                   breaks = 30
)
frecuencias <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), tail(histograma$breaks, -1), sep = " - "),
  Frecuencia = histograma$counts
)
write.csv(frecuencias, "ProyectoMEB/tablas/AdultMortality.csv", row.names = FALSE)
dev.off()
png("ProyectoMEB/graficas/AdultMortalityBoxPlot.png", width = 800, height = 600)
boxplot(data,
        main = "Boxplot de Adult Mortality",
        xlab = "Población",
        ylab = "% de morir entre 15 y 60 años",
        col = "skyblue",
        border = "black",
        ylim = c(0, 800)
)
dev.off()

# ALCOHOL
data <- lifeExpectancy$Alcohol
png("ProyectoMEB/graficas/AlcoholHist.png", width = 800, height = 600)
histograma <- hist(data,
                   main = "Histograma de Alcohol",
                   xlab = "Consumo de alcohol per cápita",
                   ylab = "Frecuencia",
                   col = "skyblue",
                   border = "black",
                   ylim = c(0, 700),
                   axes = FALSE
)
axis(2)
axis(1, at = seq(0, 18, by = 3))
dev.off()
frecuencias <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), tail(histograma$breaks, -1), sep = " -- "),
  Frecuencia = histograma$counts
)
write.csv(frecuencias, "ProyectoMEB/tablas/Alcohol.csv", row.names = FALSE)
png("ProyectoMEB/graficas/AlcoholBoxPlot.png", width = 800, height = 600)
boxplot(data,
        main = "Boxplot de Alcohol",
        xlab = "Población",
        ylab = "Consumo de alcohol per cápita",
        col = "skyblue",
        border = "black",
        ylim = c(0, 20)
)
dev.off()

# BMI
data <- lifeExpectancy$BMI
png("ProyectoMEB/graficas/BMIHist.png", width = 800, height = 600)
histograma <- hist(data,
                   main = "Histograma de BMI",
                   xlab = "BMI de la población",
                   ylab = "Frecuencia",
                   col = "skyblue",
                   border = "black"
)
dev.off()
frecuencias <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), tail(histograma$breaks, -1), sep = " - "),
  Frecuencia = histograma$counts
)
write.csv(frecuencias, "ProyectoMEB/tablas/BMI.csv", row.names = FALSE)
png("ProyectoMEB/graficas/BMIBoxPlot.png", width = 800, height = 600)
boxplot(data,
        main = "Boxplot de BMI",
        xlab = "Población",
        ylab = "BMI de la población",
        col = "skyblue",
        border = "black"
)
dev.off()

# GDP
data <- lifeExpectancy$GDP
png("ProyectoMEB/graficas/GDPHist.png", width = 800, height = 600)
histograma <- hist(data,
                   main = "Histograma de GDP",
                   xlab = "Producto interno bruto per cápita (USD)",
                   ylab = "Frecuencia",
                   col = "skyblue",
                   border = "black",
                   ylim = c(0, 2000)
)
dev.off()
frecuencias <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), tail(histograma$breaks, -1), sep = " - "),
  Frecuencia = histograma$counts
)
write.csv(frecuencias, "ProyectoMEB/tablas/GDP.csv", row.names = FALSE)
png("ProyectoMEB/graficas/GDPBoxPlot.png", width = 800, height = 600)
boxplot(data,
        main = "Boxplot de GDP",
        xlab = "Población",
        ylab = "Producto interno bruto per cápita (USD)",
        col = "skyblue",
        border = "black"
)
dev.off()

# HIV AIDS
data <- lifeExpectancy$HIV.AIDS
png("ProyectoMEB/graficas/HIV_AIDSHist.png", width = 800, height = 600)
histograma <- hist(data,
                   main = "Histograma de HIV AIDS",
                   xlab = "Muertes por VIH por cada 1000 nacidos vivos",
                   ylab = "Frecuencia",
                   col = "skyblue",
                   border = "black",
                   ylim = c(0, 2500),
                   axes = FALSE
)
axis(2)
axis(1, at = seq(0, 55, by = 5))
dev.off()
frecuencias <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), tail(histograma$breaks, -1), sep = " - "),
  Frecuencia = histograma$counts
)
write.csv(frecuencias, "ProyectoMEB/tablas/VIH.csv", row.names = FALSE)
png("ProyectoMEB/graficas/VIH_AIDSBoxPlot.png", width = 800, height = 600)
boxplot(data,
        main = "Boxplot de VIH AIDS",
        xlab = "Población",
        ylab = "Muertes por VIH por cada 1000 nacidos vivos",
        col = "skyblue",
        border = "black"
)
dev.off()

# INCOME COMPOSITION OF RESOURCES
data <- lifeExpectancy$Income.composition.of.resources
png("ProyectoMEB/graficas/IncomeHist.png", width = 800, height = 600)
histograma <- hist(data,
                   main = "Histograma de Income Composition of Resources",
                   xlab = "Índice de desarrollo humano",
                   ylab = "Frecuencia",
                   col = "skyblue",
                   border = "black",
                   ylim = c(0, 600)
)
dev.off()
frecuencias <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), tail(histograma$breaks, -1), sep = " - "),
  Frecuencia = histograma$counts
)
write.csv(frecuencias, "ProyectoMEB/tablas/Income.csv", row.names = FALSE)
png("ProyectoMEB/graficas/IncomeBoxPlot.png", width = 800, height = 600)
boxplot(data,
        main = "Boxplot de Income Composition of Resources",
        xlab = "Población",
        ylab = "Índice de desarrollo humano",
        col = "skyblue",
        border = "black",
        ylim = c(0, 1)
)
dev.off()

# SCHOOLING
data <- lifeExpectancy$Schooling
png("ProyectoMEB/graficas/SchoolingHist.png", width = 800, height = 600)
histograma <- hist(data,
                   main = "Histograma de Schooling",
                   xlab = "Número de años de escolaridad",
                   ylab = "Frecuencia",
                   col = "skyblue",
                   border = "black",
                   axes = FALSE
)
axis(2)
axis(1, at = seq(0, 22, by = 2))
dev.off()
frecuencias <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), tail(histograma$breaks, -1), sep = " - "),
  Frecuencia = histograma$counts
)
write.csv(frecuencias, "ProyectoMEB/tablas/Schooling.csv", row.names = FALSE)
png("ProyectoMEB/graficas/SchoolingBoxPlot.png", width = 800, height = 600)
boxplot(data,
        main = "Boxplot de Schooling",
        xlab = "Población",
        ylab = "Número de años de escolaridad",
        col = "skyblue",
        border = "black",
        ylim = c(0, 25)
)
dev.off()

# TOTAL EXPENDITURE
data <- lifeExpectancy$Total.expenditure
png("ProyectoMEB/graficas/TotalExpenditureHist.png", width = 800, height = 600)
histograma <- hist(data,
                   main = "Histograma de Total Expenditure",
                   xlab = "% de gasto del gobierno en salud respecto al gasto total",
                   ylab = "Frecuencia",
                   col = "skyblue",
                   border = "black"
)
dev.off()
frecuencias <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), tail(histograma$breaks, -1), sep = " - "),
  Frecuencia = histograma$counts
)
write.csv(frecuencias, "ProyectoMEB/tablas/TotalExpenditure.csv", row.names = FALSE)
png("ProyectoMEB/graficas/TotalExpenditureBoxPlot.png", width = 800, height = 600)
boxplot(data,
        main = "Boxplot de Total Expenditure",
        xlab = "Población",
        ylab = "% de gasto del gobierno en salud respecto al gasto total",
        col = "skyblue",
        border = "black"
)
dev.off()


################################### B #########################################
# Life Expectancy
png("ProyectoMEB/graficas/LifeExpectancyDist.png", width = 1000, height = 500)
data <- lifeExpectancy$Life.expectancy
par(mfrow = c(1, 2))
histograma <- hist(data,
                   main = "Histograma de Life Expectancy",
                   xlab = "Esperanza de vida en años",
                   ylab = "Frecuencia",
                   col = "skyblue",
                   border = "black",
                   ylim = c(0, 700),
                   axes = FALSE
)
axis(2)
axis(1, at = seq(35, 90, by = 5))
shape <- 7
scale <- 11
x_values <- seq(30, 90, length.out = 100)
y_values <- dgamma(x_values, shape = shape, scale = scale)
plot(x_values, y_values,
     type = "l",
     col = "red",
     lwd = 2,
     xlab = "Experanza de vida en años",
     ylab = "Densidad",
     main = "Distribución Gamma"
)
par(mfrow = c(1, 1))
dev.off()

# Adult Mortality
png("ProyectoMEB/graficas/AdultMortalityDist.png", width = 1000, height = 500)
data <- lifeExpectancy$Adult.Mortality
par(mfrow = c(1, 2))
hist(data,
     main = "Histograma de Adult Mortality",
     xlab = "% de morir entre 15 y 60 años",
     ylab = "Frecuencia",
     col = "skyblue",
     border = "black",
     xlim = c(0, 800),
     breaks = 30)
lambda <- 1 / mean(data, na.rm = TRUE)
x_values <- seq(0, 800, length.out = 100)
y_values <- dexp(x_values, rate = lambda)
plot(x_values, y_values,
     type = "l", col = "red", lwd = 2,
     main = "Distribución Exponencial",
     xlab = "% de morir entre 15 y 60 años",
     ylab = "Densidad",
     ylim = c(0, max(y_values) * 1.2))
par(mfrow = c(1, 1))
dev.off()

# Alcohol
png("ProyectoMEB/graficas/AlcoholDist.png", width = 1000, height = 500)
data <- lifeExpectancy$Alcohol
par(mfrow = c(1, 2))
hist(data,
     main = "Histograma de Alcohol",
     xlab = "Consumo de alcohol per cápita",
     ylab = "Frecuencia",
     col = "skyblue",
     border = "black",
     ylim = c(0, 700),
     xlim = c(0,1000))
axis(2)
axis(1, at = seq(0, 18, by = 3))
lambda <- 1 / mean(data)
x_vals <- seq(0, max(data), length.out = 100)
y_vals <- dexp(x_vals, rate = lambda)
plot(x_vals, y_vals, type = "l",
     main = "Distribución Exponencial",
     xlab = "Consumo de alcohol per cápita",
     ylab = "Densidad",
     col = "red", lwd = 2)
par(mfrow = c(1, 1))
dev.off()

# GDP
png("ProyectoMEB/graficas/GDPDist.png", width = 1000, height = 500)
data <- lifeExpectancy$GDP
par(mfrow = c(1, 2))
hist(data,
     main = "Histograma de GDP",
     xlab = "Producto Interno Bruto per cápita (USD)",
     ylab = "Frecuencia",
     col = "skyblue",
     border = "black",
     ylim = c(0, 2000))
axis(2)
axis(1, at = seq(0, 18, by = 3))
lambda <- 1 / mean(data)
x_vals <- seq(0, max(data), length.out = 100)
y_vals <- dexp(x_vals, rate = lambda)
plot(x_vals, y_vals, type = "l",
     main = "Distribución Exponencial",
     xlab = "Producto Interno Bruto per cápita (USD)",
     ylab = "Densidad",
     col = "red", lwd = 2)
par(mfrow = c(1, 1))
dev.off()

# HIV AIDS
data <- lifeExpectancy$HIV.AIDS

png("ProyectoMEB/graficas/AIDSDist.png", width = 1000, height = 500)
par(mfrow = c(1, 2))
histograma <- hist(data,
                   main = "Histograma de HIV AIDS",
                   xlab = "Muertes por VIH por cada 1000 nacidos vivos",
                   ylab = "Frecuencia",
                   col = "skyblue",
                   border = "black",
                   ylim = c(0, 2500),
                   axes = FALSE
)
axis(2)
axis(1, at = seq(0, 55, by = 5))
lambda <- 1 / mean(data)
x_vals <- seq(0, max(data), length.out = 100)
y_vals <- dexp(x_vals, rate = lambda)
plot(x_vals, y_vals, type = "l",
     main = "Distribución Exponencial",
     xlab = "Muertes por VIH por cada 1000 nacidos vivos",
     ylab = "Densidad",
     col = "red", lwd = 2)
par(mfrow = c(1, 1))
dev.off()

# Income composition of resources
png("ProyectoMEB/graficas/IncomeDist.png", width = 1000, height = 500)
data <- lifeExpectancy$Income.composition.of.resources
hist_data <- hist(data, plot = FALSE)
max_freq <- max(hist_data$counts)
shape1 <- 4
shape2 <- 2
par(mfrow = c(1, 2))
histograma <- hist(data,
                   main = "Histograma de Income Composition of Resources",
                   xlab = "Índice de desarrollo humano",
                   ylab = "Frecuencia",
                   col = "skyblue",
                   border = "black",
                   ylim = c(0, max_freq),
                   breaks = "Sturges")
x_vals <- seq(min(data), max(data), length.out = 100)
y_vals <- dbeta((x_vals - min(data)) / (max(data) - min(data)), shape1, shape2)
plot(x_vals, y_vals, type = "l",
     main = "Distribución Beta",
     xlab = "Índice de desarrollo humano",
     ylab = "Densidad",
     col = "red", lwd = 2)
par(mfrow = c(1, 1))
dev.off()

# Schooling
png("ProyectoMEB/graficas/SchoolingDist.png", width = 1000, height = 500)
data <- lifeExpectancy$Schooling
hist_data <- hist(data, plot = FALSE)
max_freq <- max(hist_data$counts)
histograma <- hist(data,
                   par(mfrow = c(1, 2)),
                   main = "Histograma de Schooling",
                   xlab = "Número de años de escolaridad",
                   ylab = "Frecuencia",
                   col = "skyblue",
                   border = "black",
                   ylim = c(0, max_freq),
                   breaks = "Sturges",
                   axes = FALSE)

axis(2)
axis(1, at = seq(0, 22, by = 2))
mean_value <- mean(data)
sd_value <- sd(data)
x_vals <- seq(min(data), max(data), length.out = 100)
y_vals <- dnorm(x_vals, mean = mean_value, sd = sd_value)
plot(x_vals, y_vals, type = "l",
     main = "Distribución Normal",
     xlab = "Número de años de escolaridad",
     ylab = "Densidad",
     col = "red", lwd = 2)
par(mfrow = c(1, 1))
dev.off()

# Total Expenditure
png("ProyectoMEB/graficas/TotalExpenditureDist.png", width = 1000, height = 500)
data <- lifeExpectancy$Total.expenditure
hist_data <- hist(data, plot = FALSE)
max_freq <- max(hist_data$counts)
par(mfrow = c(1, 2))
histograma <- hist(data,
                   main = "Histograma de Total Expenditure",
                   xlab = "% de gasto del gobierno en salud respecto al gasto total",
                   ylab = "Frecuencia",
                   col = "skyblue",
                   border = "black",
                   ylim = c(0, max_freq),
                   breaks = "Sturges")
mean_value <- mean(data)
sd_value <- sd(data)
x_vals <- seq(min(data), max(data), length.out = 100)
y_vals <- dnorm(x_vals, mean = mean_value, sd = sd_value)
plot(x_vals, y_vals, type = "l",
     main = "Distribución Normal",
     xlab = "% de gasto del gobierno en salud respecto al gasto total",
     ylab = "Densidad",
     col = "red", lwd = 2)
par(mfrow = c(1, 1))
dev.off()


# Soluciones

#1
1- nrow(lifeExpectancy[lifeExpectancy$Life.expectancy <= 50, ]) / nrow(lifeExpectancy)


#2
1- nrow(lifeExpectancy[lifeExpectancy$Total.expenditure <= 10, ]) / nrow(lifeExpectancy)
1- pnorm(10,5.873,5.6220)

#3
nrow(lifeExpectancy[lifeExpectancy$BMI <= 25, ]) / nrow(lifeExpectancy) - nrow(lifeExpectancy[lifeExpectancy$BMI <= 18.5, ]) / nrow(lifeExpectancy) 

#4
1-nrow(lifeExpectancy[lifeExpectancy$HIV.AIDS <= 50, ]) / nrow(lifeExpectancy)

#5
1- nrow(lifeExpectancy[lifeExpectancy$Schooling <= 20, ]) / nrow(lifeExpectancy)


