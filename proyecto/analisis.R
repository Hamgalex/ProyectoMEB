Sys.setlocale("LC_CTYPE", "Spanish_Mexico.UTF-8")
set.seed(123)

lifeExpectancy <- read.csv("C:/Users/hamga/Documents/maestria/1er/MEB/codigosR/ProyectoMEB/graficas/reporte3/muestra100.csv")
lifeExpectancy



################################################################################################
############################# LIFE EXPECTANCY ##################################################
################################################################################################

datos <- lifeExpectancy$Life.expectancy

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


boxplot(datos,
        main = "Boxplot de life expectancy",
        xlab = "Población",
        ylab = "Esperanza de vida en años",
        col = "skyblue",
        border = "black",
        ylim= c(30,90)
)



################################################################################################
############################# ADULT MORTALITY ##################################################
################################################################################################

datos <- lifeExpectancy$Adult.Mortality

histograma <- hist(datos)

tabla_frecuencias_histograma <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), "-", tail(histograma$breaks, -1)),
  Frecuencia = histograma$counts
)

write.csv(tabla_frecuencias_histograma, "C:/Users/hamga/Documents/maestria/1er/MEB/codigosR/ProyectoMEB/tablas/adultMortality100.csv", row.names = FALSE)

hist(datos,
     main = "Histograma de Adult Mortality",
     xlab = "% de morir entre 15 y 60 años",
     ylab = "Frecuencia",
     col = "skyblue",
     border = "black",
     xlim = c(0, 800)
)

boxplot(datos,
        main = "Boxplot de Adult Mortality",
        xlab = "Población",
        ylab = "% de morir entre 15 y 60 años",
        col = "skyblue",
        border = "black",
        ylim = c(0, 800)
)


################################################################################################
############################# ALCOHOL ##########################################################
################################################################################################

datos <- lifeExpectancy$Alcohol

histograma <- hist(datos)

tabla_frecuencias_histograma <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), "-", tail(histograma$breaks, -1)),
  Frecuencia = histograma$counts
)

write.csv(tabla_frecuencias_histograma, "C:/Users/hamga/Documents/maestria/1er/MEB/codigosR/ProyectoMEB/tablas/Alcohol100.csv", row.names = FALSE)

hist(datos,
     main = "Histograma de Alcohol",
     xlab = "Consumo de alcohol per cápita",
     ylab = "Frecuencia",
     col = "skyblue",
     border = "black",
     ylim=c(0,50),
     xlim = c(0,16),
     axes = FALSE
)
axis(2)
axis(1, at = seq(0, 16, by = 2))

boxplot(datos,
        main = "Boxplot de Alcohol",
        xlab = "Población",
        ylab = "Consumo de alcohol per cápita",
        col = "skyblue",
        border = "black"
)

################################################################################################
############################# BMI#### ##########################################################
################################################################################################

datos <- lifeExpectancy$BMI

histograma <- hist(datos)

tabla_frecuencias_histograma <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), "-", tail(histograma$breaks, -1)),
  Frecuencia = histograma$counts
)

write.csv(tabla_frecuencias_histograma, "C:/Users/hamga/Documents/maestria/1er/MEB/codigosR/ProyectoMEB/tablas/BMI100.csv", row.names = FALSE)

histograma <- hist(datos,
                   main = "Histograma de BMI",
                   xlab = "BMI de la población",
                   ylab = "Frecuencia",
                   col = "skyblue",
                   border = "black",
                   ylim = c(0,30)
)

boxplot(datos,
        main = "Boxplot de BMI",
        xlab = "Población",
        ylab = "BMI de la población",
        col = "skyblue",
        border = "black",
        ylim=c(0,80)
)




################################################################################################
############################# GDP ##############################################################
################################################################################################

datos <- lifeExpectancy$BMI

histograma <- hist(datos)

tabla_frecuencias_histograma <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), "-", tail(histograma$breaks, -1)),
  Frecuencia = histograma$counts
)

write.csv(tabla_frecuencias_histograma, "C:/Users/hamga/Documents/maestria/1er/MEB/codigosR/ProyectoMEB/tablas/BMI100.csv", row.names = FALSE)

histograma <- hist(datos,
                   main = "Histograma de BMI",
                   xlab = "BMI de la población",
                   ylab = "Frecuencia",
                   col = "skyblue",
                   border = "black",
                   ylim = c(0,30)
)

boxplot(datos,
        main = "Boxplot de BMI",
        xlab = "Población",
        ylab = "BMI de la población",
        col = "skyblue",
        border = "black",
        ylim=c(0,80)
)