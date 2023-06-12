# cargar paquete readr
library(readr)
library(dplyr)

# importar WHO.csv
WHO <- read_csv("WHO.csv")

#vistas de las tablas
View(WHO)
View(analizar)
View(muertes.infantesXpais_2011)
View(muertes.infantes)
View(listadoDePaises)
View(listadoDeanios)

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

#listado de paises sin repeticiones
listadoDePaises <- distinct(WHO, Cod )
listadoDeanios <- distinct(WHO, Year )

#analisis sobre las muertes de infantes
muertes.infantes <- select(WHO, 
                            Cod, 
                            Year,
                            Infant.deaths
                            )

muertes.infantesXpais_2010 <- muertes.infantes %>%
  filter(Year == "2010") %>%
  group_by(Cod) 


listadomuertesXanioYpais <- list()  # Lista para almacenar los resultados

for ( i in 1:nrow(listadoDeanios) ) {

  temporalnumber <- as.character(listadoDeanios[i,])
  
  temporaldata <- muertes.infantes %>%
  filter(Year == temporalnumber) %>%
  group_by(Cod)
  
  listadomuertesXanioYpais[[i]] <- temporaldata  # Guardar los resultados en la lista
}

# Visualizar cada elemento de la lista en vistas separadas
for (i in 1:length(listadomuertesXanioYpais) ) {
  View( listadomuertesXanioYpais[[i]] )
}
