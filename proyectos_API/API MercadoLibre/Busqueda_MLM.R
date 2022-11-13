library(httr)
library(jsonlite)
library(tidyverse)
setwd("~/COURSERA/API projects/API MercadoLibre")

# client ID: 5593648658523354
# Client secret: Ab1l7jC8a6WSwrfPtjXlX0BIC7iHo5zr
# code=TG-632a97267ab3ca00018f1417-17423141

# API de mercado libre
api <- "https://api.mercadolibre.com/"
secret <- "Ab1l7jC8a6WSwrfPtjXlX0BIC7iHo5zr"
client_id <- "5593648658523354"

# Obteniendo el token de acceso y renovacion
test <- POST("https://api.mercadolibre.com/oauth/token",
             add_headers("accept"="application/json", "content-type"="application/x-www-form-urlencoded"),
             body = "grant_type=authorization_code&client_id=5593648658523354&client_secret=Ab1l7jC8a6WSwrfPtjXlX0BIC7iHo5zr&code=TG-632a97267ab3ca00018f1417-17423141&redirect_uri=https://fast.com/es/"
             )
flujo <- content(test)
acces_token <- flujo$access_token
refresh_token <- flujo$refresh_token
cabezal <- paste0("Authorization: Bearer ", acces_token)

### para renovar el token ###
# test <- POST("https://api.mercadolibre.com/oauth/token",
# add_headers("accept"="application/json", "content-type"="application/x-www-form-urlencoded"),
# body = paste0("grant_type=refresh_token&client_id=5593648658523354&client_secret=Ab1l7jC8a6WSwrfPtjXlX0BIC7iHo5zr&refresh_token=", refresh_token))
# flujo <- content(test)
# acces_token <- flujo$access_token
# refresh_token <- flujo$refresh_token


test <- GET(api,
            path = "sites",
            add_headers("Authorization" = "Bearer " ), verbose())

flujo <-  fromJSON(content(test, "text"), flatten = TRUE)
flujo
flujo <-flujo %>% filter(name == "Mexico")
# Se observa que el id de Mexico  es "MLM"

# Almacenamos el id
assign("site_id", flujo[1,2] )

#### BUSQUEDA ####
### /sites/$SITE_ID/search?q=Motorola%20G6 -> Obtener ítems de una consulta de búsqueda. ###
# Se ha construido una linea de codigo para que pueda ser reemplazado el termino de busqueda y avanzar las paginas con "offset"
busc <- "Samsung"
ts <- GET(api, 
          path = c("sites", site_id, "search"), query = paste0("q=", busc,"&offset=",0), 
          add_headers(cabezal),
          verbose())
flux <- fromJSON(content(ts, "text"), simplifyVector = FALSE, flatten = TRUE)
pr <- flux[["results"]]

#indices de listas que nos interesan dentro de la respuesta: 1 3 4 5 14 16 
# id, title, price, condition, thumbnail, category_id
#ili <- c(1,3,4,5,14,16)
ili <- c("id", "title", "price", "original_price", "seller", "condition", "thumbnail", "category_id")

# Almacenamos en forma de lista en una nueva variable
pr_df <- list(pr[[1]][ili])

          #### Iteracion 50 a 50 ####
# Creamos la iteracion para extraer registros de 50 en 50 ya que es el limite impuesto por la API
# Ejecutamos el GET una vez para obtener el total de entradas en la consulta: flux$paging$total 
ts <- GET(api, add_headers(cabezal),
          path = c("sites", site_id, "search"), query = paste0("q=", busc,"&offset=",j), verbose())
flux <- fromJSON(content(ts, "text"), simplifyVector = FALSE, flatten = TRUE)
total <- flux$paging$total
k <- 0
j <- 0
while (j <= total) { #El offset de paginacion no puede rebasar 1000, por lo que no podremos rescatar más de 1200 resultados
i <- 0
ts <- GET(api, add_headers(cabezal),
          path = c("sites", site_id, "search"), query = paste0("q=", busc,"&offset=",j), verbose())
flux <- fromJSON(content(ts, "text"), simplifyVector = FALSE, flatten = TRUE) 
pr <- flux[["results"]]

    for (i in 1:length(flux[["results"]])) {
      k <- k + 1
      pr_df[k] <- list(pr[[i]][ili])
      }

j <- j+50
}


#### LIMPIEZA FINAL ####
final_df <- pr_df %>% enframe(name = NULL) %>% unnest_wider(col ="value") %>% hoist(seller, id_vendedor = 1) %>%
                  mutate(seller = NULL)
glimpse(final_df)

final_df %>% count(!is.na(original_price), sort = TRUE)
# Se observa que hay articulos con precios "originales", lo que implica que existe alguna clase de descuento


exportacion <- final_df %>% 
  mutate(Descuento = (1 - price/original_price) * 100) %>% arrange(desc(Descuento)) %>%
  relocate(Descuento, .after = original_price)
glimpse(exportacion)

exportacion %>% group_by(Descuento, condition) %>% summarise(n()) %>% arrange(desc(Descuento))

# Determinamos cuantos son nuevos o usados
exportacion %>% count(condition, sort = TRUE) %>% recode(used="Usados", new="Nuevos")

# Determinamos la participacion del mercado
exportacion %>% count(id_vendedor, name = "productos_publicados" , sort = TRUE)



#### FUNCION DE BUSQUEDA ####
BUSCAR <- function(busc) {
  k <- 0
  j <- 0
  ts <- GET(api, add_headers(cabezal),
            path = c("sites", site_id, "search"), query = paste0("q=", busc,"&offset=",j))
  flux <- fromJSON(content(ts, "text"), simplifyVector = FALSE, flatten = TRUE)
  total <- flux$paging$total
   while (j <= total) { 
      i <- 0
      ts <- GET(api, add_headers(cabezal),
                path = c("sites", site_id, "search"), query = paste0("q=", busc,"&offset=",j) )
      flux <- fromJSON(content(ts, "text"), simplifyVector = FALSE, flatten = TRUE) 
      pr <- flux[["results"]]
      
          for (i in 1:length(flux[["results"]])) {
            k <- k + 1
            pr_df[k] <- list(pr[[i]][ili])
          
          }
      
      j <- j+50
  }
  final_df <- pr_df %>% enframe(name = NULL) %>% unnest_wider(col ="value") %>% hoist(seller, id_vendedor = 1) %>%
    mutate(seller = NULL)
  
  exportacion <- final_df %>% 
    mutate(Descuento = (1 - price/original_price) * 100) %>% arrange(desc(Descuento)) %>%
    relocate(Descuento, .after = original_price)
  print(exportacion)
return(exportacion)
}

index <- c("Samsung", "Motorola", "Apple", "Huawei", "Xiaomi", "ZTE")

# Iteracion para buscar los terminos de la variable index
df <- map(index, BUSCAR)

# Iteracion para depositar cada termino buscado en su propia dataframe
for (i in 1:length(index)) {
  nam <- paste0("df_", index[i])
  #Al mismo tiempo, se eliminan las filas que contengan NA y se añade una columna con la consulta empleada en caso de unir las tablas en el futuro
  assign( nam, df[i] %>% tibble() %>% unnest(cols = c(.)) %>% filter(!is.na(id)) %>% mutate(consulta=index[i]) )
}

#### Analisis Exploratorio ####
# Trabajaremos un solo dataframe de nuestras consultas, despues podremos combinarlas y aplicar el mismo codigo si se desea

# Determinamos cuantos son nuevos o usados
df_Huawei %>% count(condition, sort = TRUE) %>% 
  mutate(condition=recode(condition, used="Usados", new="Nuevos", not_specified="Sin especificar"))

# Determinamos la participacion en el mercado por los vendedores 
df_Huawei %>% count(id_vendedor, name = "productos_publicados" , sort = TRUE) %>% 
  mutate("margen_%"=productos_publicados/nrow(df_Huawei)*100)

# Nuestra busqueda incluye items de varias categorias, ¿cuales son?
# Importaremos una tabla con todas las categorias enlistadas
Nombres_Categorias_MLM <- read_csv("Nombres_Categorias_MLM.csv")

df_Huawei %>% select(category_id) %>% distinct() %>% left_join(Nombres_Categorias_MLM, by=c("category_id"="cat_id"))

# Observamos las categorias que dominan nuestra consulta
df_Huawei %>% count(category_id, sort = TRUE) %>% left_join(Nombres_Categorias_MLM, by=c("category_id"="cat_id"))

# Veamos cuantos items tienen descuento
df_Huawei %>% select(title, price, original_price, Descuento) %>% filter(Descuento > 0) %>% group_by(price, Descuento) %>% 
  arrange(desc(price), desc(Descuento))


