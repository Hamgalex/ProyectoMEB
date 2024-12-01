Sys.setlocale("LC_CTYPE", "Spanish_Mexico.UTF-8")
set.seed(123)

lifeExpectancy <- read.csv("C:/Users/hamga/Documents/maestria/1er/MEB/codigosR/ProyectoMEB/graficas/reporte3/muestra100.csv")
lifeExpectancy


################################################################################################
############################# LIFE EXPECTANCY ##################################################
################################################################################################

datos <- lifeExpectancy$Life.expectancy

################# A)

histograma <- hist(datos)

tabla_frecuencias_histograma <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), "-", tail(histograma$breaks, -1)),
  Frecuencia = histograma$counts
)

write.csv(tabla_frecuencias_histograma, "C:/Users/hamga/Documents/maestria/1er/MEB/codigosR/ProyectoMEB/tablas/lifeexpectancy100.csv", row.names = FALSE)

hist(datos,
     main = "Hisograma de Life Expectancy",
     xlab = "Esperanza de vida en años",
     ylab = "frecuencia",
     ylim= c(0,35),
     col="skyblue",
     axes = FALSE
)
axis(2)
axis(1, at = seq(35, 85, by = 5))



par(mfrow = c(1, 2))
hist(datos,
     main = "Hisograma de Life Expectancy",
     xlab = "Esperanza de vida en años",
     ylab = "frecuencia",
     ylim= c(0,35),
     col="skyblue",
     axes = FALSE
)
axis(2)
axis(1, at = seq(35, 85, by = 5))
shape <- 7
scale <- 12
x_values <- seq(30, 90, length.out = 100)
y_values <- dgamma(x_values, shape = shape, scale = scale)
plot(x_values, y_values,
     type = "l",
     col = "red",
     lwd = 2,
     ylim = c(0,0.016),
     xlab = "Experanza de vida en años",
     ylab = "Densidad",
     main = "Distribución Gamma"
)

par(mfrow = c(1, 1))

####### B)

sum_x <- sum(datos)
sum_x2 <- sum(datos^2)
n <- length(datos)

alpha_hat <- (sum_x^2) / (n * sum_x2 - sum_x^2)
alpha_hat

theta_hat <- (n * sum_x2 - sum_x^2) / (n * sum_x)
theta_hat

############## C

ob <- hist(datos, plot = FALSE)$count

p <- c(
  pgamma(40, shape = alpha_hat, scale = theta_hat),
  pgamma(45, shape = alpha_hat, scale = theta_hat) - pgamma(40, shape = alpha_hat, scale = theta_hat),
  pgamma(50, shape = alpha_hat, scale = theta_hat) - pgamma(45, shape = alpha_hat, scale = theta_hat),
  pgamma(55, shape = alpha_hat, scale = theta_hat) - pgamma(50, shape = alpha_hat, scale = theta_hat),
  pgamma(60, shape = alpha_hat, scale = theta_hat) - pgamma(55, shape = alpha_hat, scale = theta_hat),
  pgamma(65, shape = alpha_hat, scale = theta_hat) - pgamma(60, shape = alpha_hat, scale = theta_hat),
  pgamma(70, shape = alpha_hat, scale = theta_hat) - pgamma(65, shape = alpha_hat, scale = theta_hat),
  pgamma(75, shape = alpha_hat, scale = theta_hat) - pgamma(70, shape = alpha_hat, scale = theta_hat),
  pgamma(80, shape = alpha_hat, scale = theta_hat) - pgamma(75, shape = alpha_hat, scale = theta_hat),
  1-  pgamma(80, shape = alpha_hat, scale = theta_hat)
)
sum(p)

es <- n * p
es

################################################################################################
############################# SCHOOLING ##################################################
################################################################################################


datos <- lifeExpectancy$Schooling


################# A)

histograma <- hist(datos)

tabla_frecuencias_histograma <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), "-", tail(histograma$breaks, -1)),
  Frecuencia = histograma$counts
)

write.csv(tabla_frecuencias_histograma, "C:/Users/hamga/Documents/maestria/1er/MEB/codigosR/ProyectoMEB/tablas/schooling100.csv", row.names = FALSE)

hist(datos,
     main = "Histograma de Schooling",
     xlab = "Años de escolaridad",
     ylab = "frecuencia",
     col="skyblue",
     ylim = c(0, 30),
     axes = FALSE
)
axis(2)
axis(1, at = seq(4, 20, by = 2))


par(mfrow = c(1, 2))
hist(datos,
     main = "Histograma de Schooling",
     xlab = "Años de escolaridad",
     ylab = "frecuencia",
     col="skyblue",
     ylim = c(0, 30),
     axes = FALSE
)
axis(2)
axis(1, at = seq(4, 20, by = 2))
mean_value <- mean(datos)
sd_value <- sd(datos)
x_vals <- seq(min(datos), max(datos), length.out = 100)
y_vals <- dnorm(x_vals, mean = mean_value, sd = sd_value)
plot(x_vals, y_vals, type = "l",
     main = "Distribución Normal",
     xlab = "Número de años de escolaridad",
     ylab = "Densidad",
     ylim=c(0,.14),
     col = "red",
     xlim=c(4,20)
)
axis(2)
axis(1, at = seq(4, 20, by = 2))
par(mfrow = c(1, 1))


####### B)

media <- mean(datos)
media

varianza <- mean(datos^2) - media^2
varianza


######## C)

ob <- hist(datos, plot = FALSE)$count

sd <- sqrt(varianza)

p <- c(
  pnorm(6,mean=media,sd=sd),
  pnorm(8,mean=media,sd=sd)-pnorm(6,mean=media,sd=sd),
  pnorm(10,mean=media,sd=sd)-pnorm(8,mean=media,sd=sd),
  pnorm(12,mean=media,sd=sd)-pnorm(10,mean=media,sd=sd),
  pnorm(14,mean=media,sd=sd)-pnorm(12,mean=media,sd=sd),
  pnorm(16,mean=media,sd=sd)-pnorm(14,mean=media,sd=sd),
  pnorm(18,mean=media,sd=sd)-pnorm(16,mean=media,sd=sd),
  1-pnorm(18,mean=media,sd=sd)
)
sum(p)

es <- n * p
es

chisq.test(data.frame(ob,es))  


################################################################################################
############################# TOTAL EXPENDITURE ##################################################
################################################################################################

datos <- lifeExpectancy$Total.expenditure

histograma <- hist(datos)

tabla_frecuencias_histograma <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), "-", tail(histograma$breaks, -1)),
  Frecuencia = histograma$counts
)

write.csv(tabla_frecuencias_histograma, "C:/Users/hamga/Documents/maestria/1er/MEB/codigosR/ProyectoMEB/tablas/totalexpend100.csv", row.names = FALSE)


histograma <- hist(datos,
                   main = "Histograma de Total Expenditure",
                   xlab = "% de gasto del gobierno en salud respecto al gasto total",
                   ylab = "Frecuencia",
                   col = "skyblue",
                   ylim = c(0,40))



par(mfrow = c(1, 2))
histograma <- hist(datos,
                   main = "Histograma de Total Expenditure",
                   xlab = "% de gasto del gobierno en salud respecto al gasto total",
                   ylab = "Frecuencia",
                   col = "skyblue",
                   ylim = c(0,40))
mean_value <- mean(datos)
sd_value <- sd(datos)
x_vals <- seq(min(datos), max(datos), length.out = 100)
y_vals <- dnorm(x_vals, mean = mean_value, sd = sd_value)
plot(x_vals, y_vals, type = "l",
     main = "Distribución Normal",
     xlab = "% de gasto del gobierno en salud respecto al gasto total",
     ylab = "Densidad",
     col = "red", lwd = 2)

################## B

media <- mean(datos)
media

varianza <- mean(datos^2) - media^2
varianza


############### C



ob <- hist(datos, plot = FALSE)$count

sd <- sqrt(varianza)

p <- c(
  pnorm(2,mean=media,sd=sd),
  pnorm(4,mean=media,sd=sd)-pnorm(2,mean=media,sd=sd),
  pnorm(6,mean=media,sd=sd)-pnorm(4,mean=media,sd=sd),
  pnorm(8,mean=media,sd=sd)-pnorm(6,mean=media,sd=sd),
  pnorm(10,mean=media,sd=sd)-pnorm(8,mean=media,sd=sd),
  pnorm(12,mean=media,sd=sd)-pnorm(10,mean=media,sd=sd),
  1-pnorm(12,mean=media,sd=sd)
)
sum(p)

es <- n * p
es

chisq.test(data.frame(ob,es))  

