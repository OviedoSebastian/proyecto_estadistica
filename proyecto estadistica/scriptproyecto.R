# cargar paquete readr
library(readr)
library(dplyr)

# importar WHO.csv
WHO <- read_csv("WHO.csv")

#vistas de las tablas
View(WHO)
View(analizar)
View(media_muertes.infantesXpais)
View(muertes.infantesXpais)

#se sacan las variables a trabajar de la base de datos
analizar <- select(WHO, 
                   Country, 
                   Cod, 
                   Year,
                   Status, 
                   Thinness.10.19.years,
                   Thinness.5.9.years,
                   BMI,
                   Life.expectancy,
                   Infant.deaths
)

#analisis sobre las muertes de infantes
muertes.infantesXpais <- select(WHO, 
                                Cod, 
                                Year,
                                Infant.deaths
                                 )

media_muertes.infantesXpais <- muertes.infantesXpais %>%
  group_by(Cod, Year) %>%
  summarize(mean_ht = mean(Infant.deaths, na.rm = TRUE) )
