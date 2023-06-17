# Proyecto Estadística


# cargar paquetes
library(readr)
library(dplyr)
library(ggplot2)
library(fdth)

# importar WHO.csv
WHO <- read_csv("WHO.csv")

#vistas de las tablas
View(WHO)
View(analizar)

#se sacan las variables a trabajar de la base de datos
analizar <- select(WHO, 
                   Country, 
                   Cod, 
                   Year,
                   Status,
                   Percentage.expenditure,
                   Life.expectancy,
                   Infant.deaths
)
##################################################################
# Pruebas

# Filtrar los datos por los años 2014, 2015 y 2016
selected_years <- c(2014, 2015, 2016)
filtered_data <- analizar[analizar$Year %in% selected_years, ]


# Paises de America
paisesAmerica <- c("CAN", "USA", "MEX", "BLZ", "CRI", "GTM", "HND", "NIC", "PAN", "SLV",
                   "ATG", "BHS", "BRB", "CUB", "DOM", "GRD", "HTI", "JAM", "LCA", "VCT", "TTO",
                   "VEN", "URY", "PER",  "PRY", "GUY", "ECU", "COL", "CHL", "BRA", "BOL", "ARG"
                   )



filtroPaisesAmerica <- filtered_data[filtered_data$Cod %in% paisesAmerica, ]


###################################################################

## Gráficos

datos <- rbind(datosAmerica2014$Life.expectancy, 
               datosAmerica2015$Life.expectancy)


barplot(datos, beside = TRUE, col = c("blue", "green"),
        names.arg = c("Valor 1", "Valor 2", "Valor 3"),
        main = "Gráfico de Barras Comparativo",
        xlab = "Variables",
        ylab = "Valores")


bp <- boxplot(datosAmerica2014$Life.expectancy,
              datosAmerica2015$Life.expectancy,
              datosAmerica2016$Life.expectancy,
              main = "Diagrama de cajas",
              ylab = "Esperanza de vida",
              xlab = "Año",
              col = c("blue", "red", "green"))
axis(side = 1, at = 1:3, labels = c("2014", "2015", "2016"))


bp <- boxplot(datosAmerica2014$Infant.deaths,
              datosAmerica2015$Infant.deaths,
              datosAmerica2016$Infant.deaths,
              main = "Diagrama de cajas",
              ylab = "Muerte infantes",
              xlab = "Año",
              col = c("yellow", "brown", "palegreen"))
axis(side = 1, at = 1:3, labels = c("2014", "2015", "2016"))


bp <- boxplot(datosAmerica2014$Percentage.expenditure,
              datosAmerica2015$Percentage.expenditure,
              datosAmerica2016$Percentage.expenditure,
              main = "Diagrama de cajas",
              ylab = "Percentaje expenditure (%)",
              xlab = "Año",
              col = c("red", "blue", "pink"))
axis(side = 1, at = 1:3, labels = c("2014", "2015", "2016"))



medianaExpenditure2016 <- median(datosAmerica2016$Percentage.expenditure, na.rm = TRUE)



ojiva_esperanza_vida <- function() {
  # Obtener los datos de esperanza de vida para cada año
  datos_2014 <- datosAmerica2014$Life.expectancy
  datos_2015 <- datosAmerica2015$Life.expectancy
  datos_2016 <- datosAmerica2016$Life.expectancy
  
  # Calcular los acumulados de los datos
  acumulados_2014 <- cumsum(datos_2014)
  acumulados_2015 <- cumsum(datos_2015)
  acumulados_2016 <- cumsum(datos_2016)
  
  # Calcular los porcentajes acumulados
  porcentajes_2014 <- acumulados_2014 / sum(datos_2014) * 100
  porcentajes_2015 <- acumulados_2015 / sum(datos_2015) * 100
  porcentajes_2016 <- acumulados_2016 / sum(datos_2016) * 100
  
  # Graficar la ojiva
  plot(porcentajes_2014, type = "o", pch = 16, col = "blue",
       main = "Ojiva de esperanza de vida",
       ylab = "Porcentaje acumulado",
       xlab = "Número de países",
       xlim = c(1, length(datos_2014)),
       ylim = c(0, 100))
  
  lines(porcentajes_2015, type = "o", pch = 16, col = "red")
  lines(porcentajes_2016, type = "o", pch = 16, col = "green")
  
  legend("topright", legend = c("2014", "2015", "2016"),
         col = c("blue", "red", "green"), pch = 16)}



ojiva_esperanza_vida() 


#Tabla de frecuencia
nombre <- fdt(datosAmerica2014$Infant.deaths, breaks = "Sturges")

tablaFrecuenciaInfante2015 <- fdt(datosAmerica2015$Infant.deaths, breaks = "Sturges")

tablaFrecuenciaInfante2016 <- fdt(datosAmerica2016$Infant.deaths, breaks = "Sturges")


tablaFrecuenciaEsperanza2014 <- fdt(datosAmerica2016$Life.expectancy, breaks = "Sturges")

tablaFrecuenciaEsperanza2015 <- fdt(datosAmerica2016$Infant.deaths, breaks = "Sturges")
tablaFrecuenciaEsperanza201 <- fdt(datosAmerica2016$Infant.deaths, breaks = "Sturges")



tablaFrecuenciaExpenditura2014 <- fdt(datosAmerica2016$Percentage.expenditure, breaks = "Sturges")



## Tabla de comparaciones.

newdata <- filtroPaisesAmerica %>%
  group_by(Status) %>%
  summarize(mediaInfantes = mean(Infant.deaths, na.rm = TRUE),
            mediaEspVida = mean(Life.expectancy, na.rm = TRUE),
            mediaPerGasto = mean(Percentage.expenditure, na.rm = TRUE))


filtroAmerica2 <- filtroPaisesAmerica %>%
  mutate(Estado = recode(Status, "Developed" = "Desarrollado", "Developing" = "En Desarrollo")) %>%
  select(-Status)



# Crear un objeto para almacenar los datos resumidos
resumen <- filtroAmerica2 %>%
  group_by(Estado) %>%
  summarize(mediaInfantes = mean(Infant.deaths, na.rm = TRUE),
            mediaEspVida = mean(Life.expectancy, na.rm = TRUE),
            mediaPerGasto = mean(Percentage.expenditure, na.rm = TRUE))

View(resumen)

# Crear una variable de colores
colores <- c("blue", "steelblue")  # Definir colores para los graficos

# Graficar los datos en un gráfico de barras
ggplot(resumen, aes(x = Estado, y = mediaInfantes)) +
  geom_bar(stat = "identity", fill in colores) +
  labs(x = "Estado", y = "Media de Infantes Fallecidos") +
  ggtitle("Media de Infantes Fallecidos por Estado") +
  theme_minimal()

ggplot(resumen, aes(x = Estado, y = mediaEspVida)) +
  geom_bar(stat = "identity", fill in colores) +
  labs(x = "Estado", y = "Media de Infantes Fallecidos") +
  ggtitle("Media de Infantes Fallecidos por Estado") +
  theme_minimal()

ggplot(resumen, aes(x = Estado, y = mediaPerGasto)) +
  geom_bar(stat = "identity", fill in colores) +
  labs(x = "Estado", y = "Media de Infantes Fallecidos") +
  ggtitle("Media de Infantes Fallecidos por Estado") +
  theme_minimal()




# Graficar los datos en un gráfico de barras con colores diferentes
ggplot(resumen, aes(x = Estado, y = mediaInfantes, fill = Estado)) +
  geom_bar(stat = "identity") +
  labs(x = "Estado", y = "Media de Infantes Fallecidos") +
  ggtitle("Media de Infantes Fallecidos por Estado") +
  scale_fill_manual(values = colores) +  # Asignar los colores definidos a las barras
  theme_minimal()

# Graficar los datos en un gráfico de barras con colores diferentes
ggplot(resumen, aes(x = Estado, y = mediaEspVida, fill = Estado)) +
  geom_bar(stat = "identity") +
  labs(x = "Estado", y = "Media de la esperanza de vida") +
  ggtitle("Esperanza de Vida en años") +
  scale_fill_manual(values = colores) +  # Asignar los colores definidos a las barras
  theme_minimal()

# Graficar los datos en un gráfico de barras con colores diferentes
ggplot(resumen, aes(x = Estado, y = mediaPerGasto, fill = Estado)) +
  geom_bar(stat = "identity") +
  labs(x = "Estado", y = "Media del gato en salud (%)") +
  ggtitle("Gasto en Salud (%)") +
  scale_fill_manual(values = colores) +  # Asignar los colores definidos a las barras
  theme_minimal()
