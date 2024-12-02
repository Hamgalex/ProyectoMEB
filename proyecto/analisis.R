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

datos <- lifeExpectancy$GDP

histograma <- hist(datos)

tabla_frecuencias_histograma <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), "-", tail(histograma$breaks, -1)),
  Frecuencia = histograma$counts
)

write.csv(tabla_frecuencias_histograma, "C:/Users/hamga/Documents/maestria/1er/MEB/codigosR/ProyectoMEB/tablas/gdp100.csv", row.names = FALSE)

hist(datos,
      main = "Histograma de GDP",
      xlab = "Producto interno bruto per cápita (USD)",
      ylab = "Frecuencia",
      col = "skyblue",
      border = "black",
     ylim = c(0,100)
)

boxplot(datos,
        main = "Boxplot de GDP",
        xlab = "Población",
        ylab = "Producto interno bruto per cápita (USD)",
        col = "skyblue",
        border = "black",
        ylim = c(0,140000)
)


################################################################################################
############################# VIH AIDS ##############################################################
################################################################################################

datos <- lifeExpectancy$HIV.AIDS

histograma <- hist(datos)

tabla_frecuencias_histograma <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), "-", tail(histograma$breaks, -1)),
  Frecuencia = histograma$counts
)

write.csv(tabla_frecuencias_histograma, "C:/Users/hamga/Documents/maestria/1er/MEB/codigosR/ProyectoMEB/tablas/hiv100.csv", row.names = FALSE)

hist(datos,
                   main = "Histograma de HIV AIDS",
                   xlab = "Muertes por VIH por cada 1000 nacidos vivos",
                   ylab = "Frecuencia",
                   col = "skyblue",
                   border = "black",
     ylim=c(0,100)
)

boxplot(datos,
        main = "Boxplot de VIH AIDS",
        xlab = "Población",
        ylab = "Muertes por VIH por cada 1000 nacidos vivos",
        col = "skyblue",
        border = "black"
)


################################################################################################
############################# INCOME COMPOSITIOTN OF RESOURCES ##############################################################
################################################################################################

datos <- lifeExpectancy$Income.composition.of.resources

histograma <- hist(datos)

tabla_frecuencias_histograma <- data.frame(
  Intervalo = paste(head(histograma$breaks, -1), "-", tail(histograma$breaks, -1)),
  Frecuencia = histograma$counts
)

write.csv(tabla_frecuencias_histograma, "C:/Users/hamga/Documents/maestria/1er/MEB/codigosR/ProyectoMEB/tablas/incomecomposition100.csv", row.names = FALSE)

hist(datos,
                   main = "Histograma de Income Composition of Resources",
                   xlab = "Índice de desarrollo humano",
                   ylab = "Frecuencia",
                   col = "skyblue",
                   border = "black",
     ylim = c(0,30)
)

boxplot(datos,
        main = "Boxplot de Income Composition of Resources",
        xlab = "Población",
        ylab = "Índice de desarrollo humano",
        col = "skyblue",
        border = "black",
        ylim = c(0,1)
)


################################################################################################
############################# SCHOOLING ##############################################################
################################################################################################

datos <- lifeExpectancy$Schooling


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


boxplot(datos,
        main = "Boxplot de Schooling",
        ylab = "Años de escolaridad",
        xlab = "Población",
        col="skyblue",
        border = "black",
        ylim = c(0,20)
)


################################################################################################
############################# TOTAL EXPENDITURE ################################################
################################################################################################


datos <- lifeExpectancy$Total.expenditure


histograma <- hist(datos,
                   main = "Histograma de Total Expenditure",
                   xlab = "% de gasto del gobierno en salud respecto al gasto total",
                   ylab = "Frecuencia",
                   col = "skyblue",
                   ylim = c(0,40))


boxplot(datos,
        main = "Boxplot de Total Expenditure",
        xlab = "Población",
        ylab = "% de gasto del gobierno en salud respecto al gasto total",
        col = "skyblue",
        border = "black",
        ylim = c(0,14)
)
