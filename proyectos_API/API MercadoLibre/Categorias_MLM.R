
library(httr)
library(jsonlite)
library(tidyverse)


# API de mercado libre
api <- "https://api.mercadolibre.com/"

# LLamada de prueba a la api para conseguir el codigo de la region Mexico
test <- GET(api,
            path = "sites",
            add_headers("Authorization" = "Bearer $ACCESS_TOKEN"), verbose())
flujo <-  fromJSON(content(test, "text"), flatten = TRUE)
flujo
flujo <-flujo %>% filter(name == "Mexico")
# Se observa que el id de Mexico  es "MLM"

# Almacenamos el id
assign("site_id", flujo[1,2] )

# Solicitamos las categorias existentes de Mercado Libre Mexico
ts <- GET(api,
          path = c("sites", site_id, "categories"), verbose())

# La transformamos en una tabla
categorias <- fromJSON(content(ts, "text"), flatten = TRUE)
categorias

# Ahora consultamos la descripción de cada una
ts <- GET(api,
          path = c("categories", categorias[1,1]), verbose())    
flux <- fromJSON(content(ts, "text"), flatten = TRUE)
flux

# Hay varios datos, lo más rescatable y significativo son el total de items y las subcategorias "children_categories"
SUB_CAT <- list()
SUB_CAT[[1]] <-  flux$children_categories

# Generamos la iteracion para jalar todas las subcategorias

for (i in 1:length(categorias$id)) {
  print(paste0("Numero de iteración: ",i))
  ts <- GET(api,
            path = c("categories", categorias[i,1]), verbose())    
  flux <- fromJSON(content(ts, "text"), flatten = TRUE)
  SUB_CAT[[i]] <-  flux$children_categories
}
# NOTA: Es posible que cada subcategoria tenga aun más categorias anidadas, se debe revisar.
# Al utilizar la funcion 'unnest', debemos indicar que conserve las filas vacias con "keep_empty =TRUE"
# porque puede suceder que una categoria ya no tenga más anidamientos.

# Conformamos nuestra tabla final con las categorias y sus subcategorias
final_df <- categorias %>% tibble() %>% mutate(SUB_CAT)
final_df
final_df <- final_df %>% rename(cat_id=id, categoria=name) %>%
            unnest(cols = SUB_CAT, keep_empty = TRUE) %>%
            mutate(total_items_in_this_category=NULL) %>% 
            rename(subcat_id_1=id, subcategoria_1=name)
final_df

# Revisamos si hay listas anidadas
hay <- SUB_CAT %>% enframe(name = NULL, value = "SUB_CAT") %>% count(SUB_CAT) %>% slice_head(n=3)
hay

#### ITERACION ABSOLUTA ####
# Se construye una iteracion que rescate todas las categorias existentes para no repetir el codigo
j <- 2
while ( !is.na(hay[2,2]) ) {
  
subcategorias <- final_df %>% select(last_col(1))
SUB_CAT <- list()
for (i in 1:nrow(subcategorias)) {
  print(paste0("Numero de iteración: ", j, "-", i))
  if(is.na(subcategorias[i,1])){
    SUB_CAT[[i]] <- list()
  } else {
    ts <- GET(api,
              path = c("categories", subcategorias[i,1]))    
    flux <- fromJSON(content(ts, "text"), flatten = TRUE)
    SUB_CAT[[i]] <-  flux$children_categories
    
  }
}

hay <- SUB_CAT %>% enframe(name = NULL, value = "SUB_CAT") %>% count(SUB_CAT) %>% slice_head(n=3)

final_df <- final_df %>% mutate(SUB_CAT) %>% 
  unnest_wider(col = SUB_CAT) %>%
  mutate(total_items_in_this_category=NULL) %>% 
  unnest(cols = c(id, name), keep_empty=TRUE) %>% 
  rename("subcat_id_{{j}}":=id, "subcategoria_{{j}}":=name)
  #Notese el uso de {} para encapsular la variable y := para asignarla correctamente
j <- j+1  
}  
  
###


# Exportamos un archivo CSV para archivar lo obtenido
write_excel_csv(final_df, "Categorias_MLM.csv")

#### LISTADO de CATEGORIAS ####
#Se desea crear una tabla de dos columnas que enliste el nombre y id de todas las categorias existentes
#Podría lograrse añadiendo una instrucción al final de cada llamada a la API, pero usaremos nuestra final_df 
#para extraerlos aqui

nombres <- colnames(final_df)
final_df %>% select(nombres[9], nombres[10]) %>% rename(cat_id=nombres[9], nombre=nombres[10]) %>% distinct()
## ITERACION ##
nombres_final_df <- tibble()
for (i in 1:7) {
  tmp <- final_df %>% select(nombres[i*2-1], nombres[i*2]) %>% rename(id=nombres[i*2-1], categorias=nombres[i*2]) %>% distinct()
  nombres_final_df <- bind_rows(nombres_final_df, tmp)
}
nombres_final_df

nombres_final_df %>% count(!is.na(id))

#Quitamos los NA que se acumularon
nombres_final_df <- nombres_final_df %>% filter(!is.na(id))

write_excel_csv(nombres_final_df, "Nombres_Categorias_MLM.csv")





#### APENDICE ####

### Instruccion para revisar si existe al menos un valor diferente de list() en las listas SUB_CAT_xx ###
hay <- SUB_CAT %>% enframe(name = NULL, value = "SUB_CAT") %>% count(SUB_CAT) %>% slice_head(n=3)
hay
hay[2,2]
is.na(hay[2,2])

if (is.na(hay[2,2])) {
  print("Ya no existen listas anidados")
}else{print("Aún existen listas anidadas")}

### Arbol de categorias ###

"https://api.mercadolibre.com/sites/MLM/categories/all > categoriesMLM.gz"

