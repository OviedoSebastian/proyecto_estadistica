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
                   Life.expectancy,
                   Infant.deaths
)


#analisis sobre las muertes de infantes
muertes.infantes <- select(WHO,
                           Country,
                           Cod,
                           Percentage.expenditure,
                           Year,
                           Infant.deaths
)


#listado de paises sin repeticiones
listadoDePaises <- distinct(WHO, Country )
listadoDeanios <- distinct(WHO, Year )

years <- c("2014", "2015", "2016")
paisesAmericaNorte <- c("CAN", "USA", "MEX")
paisesAmericaCentral <- c("BLZ", "CRI", "GTM", "HND", "NIC", "PAN", "SLV")
Caribe <- c( "ATG", "BHS", "BRB", "CUB", "DOM", "GRD", "HTI", "JAM", "LCA", "VCT", "TTO")
paisesAméricadelSur <- c("VEN", "URY", "PER",  "PRY", "GUY", "ECU", "COL", "CHL", "BRA", "BOL", "ARG")

muertes.infantesXAmericaNorte <- muertes.infantes %>%
  filter( Year %in% years & Cod %in%  paisesAmericaNorte) %>%
  group_by(Cod) 

muertes.infantesXAmericaCentral <- muertes.infantes %>%
  filter( Year %in% years & Cod %in%  paisesAmericaCentral) %>%
  group_by(Cod)

muertes.infantesXCaribe <- muertes.infantes %>%
  filter( Year %in% years & Cod %in%  Caribe) %>%
  group_by(Cod)

muertes.infantesXAméricadelSur <- muertes.infantes %>%
  filter( Year == '2014' & Cod %in%  paisesAméricadelSur) %>%
  group_by(Cod)

View(muertes.infantesXAmericaNorte)
View(muertes.infantesXAmericaCentral)
View(muertes.infantesXCaribe)
View(muertes.infantesXAméricadelSur)

hist(muertes.infantesXAméricadelSur$Year, 
     breaks = 10, main = "Numero de lactantes muerto por anio", 
     xlab = "Valores")


#fin del analisis momentaneo -- aqui vamos, ignoren lo de abajo


#otra cosa
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


