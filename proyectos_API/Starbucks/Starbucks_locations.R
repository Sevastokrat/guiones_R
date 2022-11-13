#' ---
#' title: "Extracción de ubicaciones de Starbucks empleando la API no-oficial"
#' author: "Sebastián Quintana Rosas"
#' date: "Noviembre 5 del 2022"
#' ---

library(tidyverse)
library(httr)
library(jsonlite)
library(glue)

# A traves de POSTMAN.com y la pagina de ubicaciones de Starbucks hemos conseguido lo siguiente:

# Los encabezados del GET que se utiliza en la pagina oficial de Starbucks MX para encontrar tu tienda mas cercana
headers = c(
  'User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:106.0) Gecko/20100101 Firefox/106.0',
  'Accept' = '*/*',
  'Accept-Language' = 'es-MX,es;q=0.8,en-US;q=0.5,en;q=0.3',
  'Accept-Encoding' = 'gzip, deflate, br',
  'Referer' = 'https://www.starbucks.com.mx/stores',
  'content-type' = 'application/json',
  'Connection' = 'keep-alive',
  'Cookie' = 'mx-mexico-ck=true',
  'Sec-Fetch-Dest' = 'empty',
  'Sec-Fetch-Mode' = 'cors',
  'Sec-Fetch-Site' = 'same-origin',
  'TE' = 'trailers'
)
# Definimos las variables que usan: coordenadas y radio de busqueda
crds <- "31.835855118041227,-116.59706881031494"
radio <- "200000"
glue("[{crds},{radio}]")

# Contruimos la llamada GET
res <- GET(url = "https://www.starbucks.com.mx/api/getStores",
           add_headers(headers,body = glue("[{crds},{radio}]") ))
sbcks <- as_tibble( fromJSON(content(res, 'text'), flatten = TRUE) )
sbcks
# Dos columnas tienen dataframes anidadas: Parecen ser un codigo de servicios disponible

test <- sbcks %>% mutate(store.hoursNext7Days.dailySchedule = NULL) %>%
                  unnest_wider(col = store.features.feature) %>% 
                  rename(feature.code = code, feature.name = name)
test
# NOTA: Una vez que se compile toda la lista, se deben extraer los codigos y nombres de las feature para generar su propia tabla
#### CODIGO para extraer tablas anidadas #### 
feature.name <- test %>% select(feature.code, feature.name) %>% 
                          unnest(cols= c(feature.name,feature.code)) %>%
                          distinct(feature.name)

feature.code <- test %>% select(feature.code, feature.name) %>% 
                          unnest(cols= c(feature.name,feature.code)) %>%
                          distinct(feature.code)

features <- tibble(feature.code, feature.name)

#### Transcripción a CSV ####

sbcks <- test %>% mutate(feature.code = NULL,feature.name = NULL)

write_excel_csv(sbcks, "Starbucks_locs.csv")

### PROBLEMA ###
# La api necesita coordenadas geograficas para ser utilizada y solo devuelve 50 maximo.
# 
# Solucion: Conseguir las coordenadas geograficas de todos los asentamientos urbanos o rurales
# El INEGI tiene un sistema de consulta que nos permitiría obtener dichos datos: https://www.inegi.org.mx/servicios/catalogounico.html

#### FLUJO para obtener los parámetros necesarios del servicio INEGI: las coordenadas de cada asentamiento en Mexico
# Consigamos primero los estados y sus claves
flux <- GET(url = "https://gaia.inegi.org.mx/wscatgeo/mgee/")
flux 

ts <-as_tibble( fromJSON( content(flux,"text") ) )
ts

AGEE <- ts %>% mutate(metadatos=NULL, numReg=NULL) %>% unnest(cols = datos) %>% transmute(cve_agee, nom_agee) %>% 
          rename(id=cve_agee, estado=nom_agee) #Tabla con codigo y nombre del estado
 
# Ahora consigamos los municipios de un estado: 01 - Aguascalientes
flux <- GET(url="https://gaia.inegi.org.mx/wscatgeo/mgem/01", verbose())
flux

ts <- as_tibble(fromJSON(content(flux,"text")))
ts

AGEM <- ts %>% mutate(metadatos=NULL, numReg=NULL) %>% unnest(cols = datos) %>% transmute(cve_agee, cve_agem, nom_agem) %>% 
          rename(estado=cve_agee, agem=cve_agem, municipio=nom_agem)

#Ahora podemos consultar las localidades y obtener sus coordenadas
flux <- GET(url="https://gaia.inegi.org.mx/wscatgeo/localidades/01/001")
flux

ts <- as_tibble(fromJSON(content(flux,"text")))
ts

LOCALIDAD <- ts %>% mutate(metadatos=NULL, numReg=NULL) %>% unnest(cols = datos) %>%
              transmute(nom_loc, ambito, latitud, longitud) %>% rename(Localidad = nom_loc)

# Para simplificar el código, guardaremos las url en diferentes variables
link_municipio <- "https://gaia.inegi.org.mx/wscatgeo/mgem/"
link_localidad <- "https://gaia.inegi.org.mx/wscatgeo/localidades/"

#### ITERACION MUNICIPIOS####
#Ahora debemos construir la iteracion que va a conseguir todas los municipios de la República
id_estados <- AGEE$id # Depositamos todos los id de los estados en una variable

assign("AGEM", tibble(id_estados, y=NA) )
for (i in 1:length(AGEE$id)) {
  
flux <- GET( url = glue(link_municipio, id_estados[i]) )
  ts <- as_tibble(fromJSON(content(flux,"text")))
    AGEM[i,2] <- ts %>% mutate(metadatos=NULL, numReg=NULL) %>% unnest(cols = datos) %>% transmute(cve_agee, cve_agem, nom_agem) %>% 
                  rename(estado=cve_agee, agem=cve_agem, municipio=nom_agem) %>% nest(mcp=c(estado:municipio))
}
AGEM_total <- AGEM %>% unnest(cols = y, keep_empty = TRUE) %>% mutate(id_estados=NULL)
AGEM_total

#### ITERACION LOCALIDADES ####
# Ahora se construye la que va a conseguir todas las localidades
# Podriamos reducir nuestra consulta al filtrar ambito = URBANO, para solo considerar ciudades
assign("LOCALIDAD", tibble(AGEM_total, y=NA))

for (i in 1:length(LOCALIDAD$agem)) {
flux <- GET(url= glue(link_localidad, AGEM_total[[i,1]],"/", AGEM_total[[i,2]]))
  ts <- as_tibble(fromJSON(content(flux,"text")))
    LOCALIDAD[i,4] <- ts %>% mutate(metadatos=NULL, numReg=NULL) %>% unnest(cols = datos) %>%
                  transmute(nom_loc, ambito, latitud, longitud) %>% rename(Localidad = nom_loc) %>% 
                  nest(mcp=everything())
}
LOCALIDAD_total <- LOCALIDAD %>% unnest(cols = y, keep_empty = TRUE)
LOCALIDAD_total

write_excel_csv(LOCALIDAD_total,"localidades.csv")

### Probaremos solo usar los ambitos URBANOS en nuestra busqueda de Starbucks
urbes <- LOCALIDAD_total %>% filter(ambito=="URBANO") %>% select(latitud,longitud) %>%
          mutate(coordenadas=paste(latitud,longitud, sep = ","), latitud=NULL, longitud=NULL)

STARBUCKS <- function(coordenadas) {
  res <- GET(url = "https://www.starbucks.com.mx/api/getStores",
             add_headers(headers,body = glue("[{coordenadas},199999]") ))
  flow <- as_tibble( fromJSON(content(res, 'text'), flatten = TRUE) )
  #Sys.sleep(3)
  return(flow)
}

assign("CAFES", list())
for (i in 1:length(urbes$coordenadas)) {
  CAFES[i] <- STARBUCKS(urbes[[i,1]]) %>% nest(data=everything())
}

### NOTA: La iteracion tarda mucho tiempo en juntar los datos.
### Aproximadamente tarda 0.5s en trabajar cada par de coordenadas por lo que: 4911 / 0.5s = 2455.5s / 60s = 40.925 min
### La iteración tarda 41 minutos en recolectar los datos asociados a las 4911 pares de coordenadas que tenemos en *urbes*

# Componemos nuestra tabla para luego desanidar las listas
CAFES_df <- CAFES %>% enframe(name = NULL) %>% unnest(cols=value) %>% unnest(cols=value)

# Generamos esa tabla de codigos que mencionamos en el principio
features <- CAFES_df %>% distinct(store.id, .keep_all = TRUE) %>% 
            unnest_wider(col = store.features.feature) %>% 
            rename(feature.code = code, feature.name = name) %>% 
            select(feature.code, feature.name) %>% unnest(cols = c(feature.code, feature.name)) %>% 
            distinct(feature.code, feature.name)

# Quitamos las columnas que no nos interesan
CAFES_df_final <- CAFES_df %>% transmute(store.id,store.address.city, store.coordinates.latitude,
                                   store.coordinates.longitude, store.name, store.financialInfo, store.operatingStatus.openDate, 
                                   store.operatingStatus.status, store.address.city, store.address.countrySubdivisionCode)

# Filtramos para obtener los valores unicos de store.id y asi eliminar duplicados
CAFES_df_final <- CAFES_df_final %>% distinct(store.id, .keep_all = TRUE)

# Despues podemos filtrar por estado segun el codigo o nombre
CAFES_df_final %>% distinct(store.address.countrySubdivisionCode)

# Cualquiera sea el caso, procedemos a escribir nuestras tablas generadas para documentar nuestra recolección
write_excel_csv(CAFES_df_final, "STARBUCKS_en_Mexico.csv")
write_excel_csv(features, "servicios_starbucks.csv")
