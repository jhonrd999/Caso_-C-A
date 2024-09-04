# Instalar y cargar el paquete necesario (solo si es necesario)
if(!require(devtools)) install.packages("devtools")
devtools::install_github("centromagis/paqueteMODELOS", force =TRUE)

# Cargar el paquete y los datos
library(paqueteMODELOS)
library(tidyverse)
library(broom)
data("vivienda")
install.packages("stringi")
library(stringi)



#
#-----------------------------Vivienda 1 Zona Norte--------------------------
#Punto 1
# Filtrar los datos para incluir solo casas en la zona norte
vivienda_norte_casas <- subset(vivienda, zona == "Zona Norte" & tipo == "Casa")

# Mostrar los primeros 3 registros de la base de datos filtrada
print(head(vivienda_norte_casas, 3))

# Resumen de la base de datos filtrada
print(summary(vivienda_norte_casas))

# Conteo de observaciones por zona y tipo para verificar el filtro
print(table(vivienda_norte_casas$zona, vivienda_norte_casas$tipo))

# Instalar y cargar la librería necesaria para el mapa 
if(!require(leaflet)) install.packages("leaflet")
library(leaflet)

# Crear el mapa con las ubicaciones originales de las propiedades en la zona norte
mapa <- leaflet(vivienda_norte_casas) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitud,
    lat = ~latitud,
    radius = 5,
    color = "blue",
    fill = TRUE,
    fillOpacity = 0.7,
    popup = ~paste("Precio: $", round(preciom, 2)),
    label = ~paste("Precio: $", round(preciom, 2)) # Agrega etiquetas
  ) %>%
  setView(lng = mean(vivienda_norte_casas$longitud, na.rm = TRUE),
          lat = mean(vivienda_norte_casas$latitud, na.rm = TRUE),
          zoom = 13)  # Ajusta el zoom para ver mejor los puntos

# Mostrar el mapa
mapa

# Verificar si todos los registros filtrados pertenecen a la zona norte
print(unique(vivienda_norte_casas$zona))



------------------------------
  vivienda_norte_casas <- data.frame(
    barrio = c("acopi", "alameda del rio", "alamos", "atanasio girardot", "barranquilla", 
               "barrio tranquilo y", "base aerea", "berlin", "brisas de los", "brisas del guabito", 
               "Cali", "calibella", "calima", "calimio norte", "cambulos", "centenario", 
               "chapinero", "chipichape", "ciudad los alamos", "colinas del bosque", 
               "cristales", "el bosque", "el cedro", "el gran limonar", "el guabito", 
               "el sena", "el trebol", "evaristo garcia", "flora industrial", "floralia", 
               "gaitan", "granada", "jorge eliecer gaitan", "juanambu", "la base", 
               "la campina", "la esmeralda", "la flora", "la floresta", "la merced", 
               "la rivera", "la rivera i", "la rivera ii", "la riviera", "la villa del", 
               "las acacias", "las americas", "las ceibas", "las delicias", "las granjas", 
               "los andes", "los guaduales", "los guayacanes", "manzanares", "menga", 
               "metropolitano del norte", "nueva tequendama", "oasis de comfandi", 
               "occidente", "pacara", "parque residencial el", "paseo de los", 
               "paso del comercio", "poblado campestre", "popular", "portada de comfandi", 
               "portales de comfandi", "porvenir", "prados del norte", "quintas de salomia", 
               "rozo la torre", "salomia", "san luis", "san vicente", "santa barbara", 
               "santa monica", "santa monica", "santa monica norte", "santa monica residencial", 
               "santander", "tejares de san", "torres de comfandi", "union de vivienda", 
               "urbanizacion barranquilla", "urbanizacion la flora", "urbanizacion la merced", 
               "urbanizacion la nueva", "valle del lili", "versalles", "villa colombia", 
               "villa de veracruz", "villa del prado", "villa del sol", "villas de veracruz", 
               "vipasa", "zona norte", "zona oriente")
  )



# Cargar y filtrar los datos
vivienda_norte_casas <- subset(vivienda, zona == "Zona Norte" & tipo == "Casa")

# Eliminar acentos de los nombres de los barrios
vivienda_norte_casas$barrio_normalizado <- tolower(trimws(vivienda_norte_casas$barrio))
vivienda_norte_casas$barrio_normalizado <- iconv(vivienda_norte_casas$barrio_normalizado, from = "UTF-8", to = "ASCII//TRANSLIT")

# Realizar las sustituciones necesarias
vivienda_norte_casas$barrio_normalizado <- gsub("santa monica|santa monica", "santa monica", vivienda_norte_casas$barrio_normalizado)
vivienda_norte_casas$barrio_normalizado <- gsub("la flora|la.flora", "la flora", vivienda_norte_casas$barrio_normalizado)
vivienda_norte_casas$barrio_normalizado <- gsub("santa monica residencial|santa monica residencial", "santa monica residencial", vivienda_norte_casas$barrio_normalizado)
vivienda_norte_casas$barrio_normalizado <- gsub("villa del prado|villa del prado", "villa del prado", vivienda_norte_casas$barrio_normalizado)
vivienda_norte_casas$barrio_normalizado <- gsub("villas de veracruz|villas de veracruz", "villas de veracruz", vivienda_norte_casas$barrio_normalizado)



# Eliminar los barrios genéricos
vivienda_norte_casas <- subset(vivienda_norte_casas, !barrio_normalizado %in% c("zona norte", "zona oriente"))

# Calcular los valores máximos y mínimos de latitud y longitud
if ("latitud" %in% colnames(vivienda_norte_casas) & "longitud" %in% colnames(vivienda_norte_casas)) {
  
  latitud_min <- min(vivienda_norte_casas$latitud, na.rm = TRUE)
  latitud_max <- max(vivienda_norte_casas$latitud, na.rm = TRUE)
  longitud_min <- min(vivienda_norte_casas$longitud, na.rm = TRUE)
  longitud_max <- max(vivienda_norte_casas$longitud, na.rm = TRUE)
  
  print(paste("Rango de latitud:", latitud_min, "a", latitud_max))
  print(paste("Rango de longitud:", longitud_min, "a", longitud_max))
  
} else {
  print("Las columnas de latitud y longitud no existen en el DataFrame.")
}

# Ordenar el DataFrame por latitud y longitud
vivienda_norte_casas_ordenado <- vivienda_norte_casas[order(vivienda_norte_casas$latitud, vivienda_norte_casas$longitud), ]

# Generar el listado de barrios organizados por latitud y longitud
listado_barrios_ordenado <- data.frame(
  Barrio = vivienda_norte_casas_ordenado$barrio_normalizado,
  Latitud = vivienda_norte_casas_ordenado$latitud,
  Longitud = vivienda_norte_casas_ordenado$longitud
)

# Mostrar el listado ordenado
print(listado_barrios_ordenado)

barrios_a_actualizar <- data.frame(
  barrio_normalizado = c("valle del lili", "las granjas", "las ceibas", "el trebol", "las americas", 
                         "evaristo garcia", "chapinero", "la base", "urbanizacion la nueva", 
                         "cristales", "tejares de san", "occidente", "poblado campestre", 
                         "las delicias", "el sena", "brisas del guabito", "el guabito"),
  nueva_zona = c("Zona Sur", "Zona Sur", "Zona Sur", "Zona Sur", "Zona Sur", 
                 "Zona Sur", "Zona Sur", "Zona Centro-Sur", "Zona Sur", 
                 "Zona Oeste", "Zona Sur", "Zona Sur", "Zona Sur", 
                 "Zona Sur", "Zona Centro-Sur", "Zona Sur", "Zona Sur")
)

# Actualizar el campo 'zona' para los barrios que están en la lista
for (i in 1:nrow(barrios_a_actualizar)) {
  vivienda_norte_casas$zona[vivienda_norte_casas$barrio_normalizado == barrios_a_actualizar$barrio_normalizado[i]] <- barrios_a_actualizar$nueva_zona[i]
}

# Verificar que las zonas se hayan actualizado correctamente
print(unique(vivienda_norte_casas[vivienda_norte_casas$barrio_normalizado %in% barrios_a_actualizar$barrio_normalizado, c("barrio_normalizado", "zona")]))

# Eliminar los registros donde el barrio es "Cali"
vivienda_norte_casas <- subset(vivienda_norte_casas, barrio_normalizado != "cali")

# Verificar si todos los registros de "Cali" fueron eliminados
print(unique(vivienda_norte_casas$barrio_normalizado))

# Eliminar los registros donde el barrio es "santa mónica residencial"
vivienda_norte_casas <- subset(vivienda_norte_casas, barrio_normalizado != "santa mónica residencial")

# Verificar si aún existe el barrio en los datos
print(unique(vivienda_norte_casas$barrio_normalizado))


# Eliminar todas las posibles variantes de "santa mónica residencial"
vivienda_norte_casas <- subset(vivienda_norte_casas, !barrio_normalizado %in% c("santa monica residencial", "santa mónica residencial"))
# Eliminar cualquier barrio que contenga "santa monica residencial" en su nombre, incluso si hay variaciones
vivienda_norte_casas <- vivienda_norte_casas[!grepl("santa monica residencial", vivienda_norte_casas$barrio_normalizado, ignore.case = TRUE), ]

# Verificar si el barrio "santa mónica residencial" sigue presente
santa_monica_present <- vivienda_norte_casas[vivienda_norte_casas$barrio_normalizado == "santa monica residencial", ]
print(santa_monica_present)

# Eliminar todas las posibles variantes del nombre
vivienda_norte_casas <- subset(vivienda_norte_casas, !grepl("santa monica residencial", vivienda_norte_casas$barrio_normalizado, ignore.case = TRUE))


# Instalar y cargar la librería necesaria para el mapa 
if(!require(leaflet)) install.packages("leaflet")
library(leaflet)


# Asegúrate de estar utilizando el DataFrame actualizado
mapa <- leaflet(vivienda_norte_casas) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitud,
    lat = ~latitud,
    radius = 5,
    color = "blue",
    fill = TRUE,
    fillOpacity = 0.7,
    popup = ~paste("Barrio:", barrio_normalizado),
    label = ~paste("Barrio:", barrio_normalizado)
  ) %>%
  setView(lng = mean(vivienda_norte_casas$longitud, na.rm = TRUE),
          lat = mean(vivienda_norte_casas$latitud, na.rm = TRUE),
          zoom = 13)

# Mostrar el mapa
mapa




#Latitud y longitud rango zona norte
if ("latitud" %in% colnames(vivienda_norte_casas) & "longitud" %in% colnames(vivienda_norte_casas)) {
  
  # Calcular los valores máximos y mínimos de latitud y longitud para la zona norte
  latitud_min <- min(vivienda_norte_casas$latitud, na.rm = TRUE)
  latitud_max <- max(vivienda_norte_casas$latitud, na.rm = TRUE)
  longitud_min <- min(vivienda_norte_casas$longitud, na.rm = TRUE)
  longitud_max <- max(vivienda_norte_casas$longitud, na.rm = TRUE)
  
  # Imprimir los resultados
  print(paste("Rango de latitud:", latitud_min, "a", latitud_max))
  print(paste("Rango de longitud:", longitud_min, "a", longitud_max))
  
} else {
  print("Las columnas de latitud y longitud no existen en el DataFrame.")
}

# Imprimir las zonas de las viviendas restantes
print(unique(vivienda_norte_casas$zona))

# Contar cuántos barrios únicos quedaron en la zona norte
barrios_zona_norte <- unique(vivienda_norte_casas$barrio_normalizado)
cantidad_barrios_norte <- length(barrios_zona_norte)
print(paste("Cantidad de barrios en la zona norte:", cantidad_barrios_norte))


# Punto 2
install.packages("GGally")
install.packages("plotly")


library(GGally)
library(plotly)

# Selección de variables para el análisis
variables_seleccionadas <- vivienda_norte_casas[, c("preciom", "areaconst", "estrato", "banios", "habitaciones")]

# Generar el gráfico de pares (ggpairs) para las variables seleccionadas
ggpairs_plot <- ggpairs(variables_seleccionadas, 
                        title = "Correlación entre Precio y Variables Seleccionadas")

# Convertir el gráfico de ggpairs a un objeto plotly para interactividad
plotly_ggpairs <- ggplotly(ggpairs_plot)

# Mostrar el gráfico interactivo
plotly_ggpairs


#Punto 3 zona norte 

# 1. Identificación y Tratamiento de Valores Faltantes
# Ver cuántos valores NA hay en cada columna relevante
print(sapply(vivienda_norte_casas[, c("areaconst", "estrato", "habitaciones", 
                                      "parqueaderos", "banios")], 
             function(x) sum(is.na(x))))

# Función para calcular la moda
get_moda <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

# Calcular la moda de parqueaderos por barrio
moda_parqueaderos <- aggregate(parqueaderos ~ barrio_normalizado, data = vivienda_norte_casas, FUN = get_moda)

# Imputar los valores faltantes con la moda correspondiente al barrio
vivienda_norte_casas <- merge(vivienda_norte_casas, moda_parqueaderos, by = "barrio_normalizado", suffixes = c("", "_moda"))

# Reemplazar los NA en parqueaderos con la moda del barrio
vivienda_norte_casas$parqueaderos[is.na(vivienda_norte_casas$parqueaderos)] <- vivienda_norte_casas$parqueaderos_moda

# Eliminar la columna temporal de moda
vivienda_norte_casas$parqueaderos_moda <- NULL

# Verificar si se han imputado los valores faltantes
print(sapply(vivienda_norte_casas[, c("areaconst", "estrato", "habitaciones", 
                                      "parqueaderos", "banios")], function(x) sum(is.na(x))))

# Función para detectar outliers usando el método IQR
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(x < lower_bound | x > upper_bound)
}

# Identificación de outliers para cada variable
vivienda_norte_casas$outlier_areaconst <- detect_outliers(vivienda_norte_casas$areaconst)
vivienda_norte_casas$outlier_habitaciones <- detect_outliers(vivienda_norte_casas$habitaciones)
vivienda_norte_casas$outlier_parqueaderos <- detect_outliers(vivienda_norte_casas$parqueaderos)
vivienda_norte_casas$outlier_banios <- detect_outliers(vivienda_norte_casas$banios)

# Verificación del número de outliers detectados en cada variable
print(sapply(vivienda_norte_casas[, grep("outlier", colnames(vivienda_norte_casas))], sum))

# Convertir la variable 'estrato' a factor
vivienda_norte_casas$estrato <- as.factor(vivienda_norte_casas$estrato)

# Verificar la conversión
print(str(vivienda_norte_casas$estrato))

# Instalar los paquetes necesarios
install.packages("tidyverse")
install.packages("broom")

# Cargar las librerías necesarias
library(tidyverse)
library(broom)

# Estimación del modelo de regresión lineal múltiple con el nombre correcto de la variable precio
modelo <- lm(preciom ~ areaconst + estrato + habitaciones + parqueaderos + banios, data = vivienda_norte_casas)

# Resumen del modelo: Coeficientes y su significancia
modelo_summary <- summary(modelo)
print(modelo_summary)

# Resumen del modelo en un formato más ordenado usando broom
modelo_tidy <- tidy(modelo)
print(modelo_tidy)

# Interpretación del coeficiente R2
r2_value <- modelo_summary$r.squared
adjusted_r2_value <- modelo_summary$adj.r.squared

cat("El valor de R² es:", r2_value, "\n")
cat("El valor de R² ajustado es:", adjusted_r2_value, "\n")



#Punto 4
#Normalidad de los Residuos

# Visualización de los residuos del modelo para diagnóstico
par(mfrow = c(2, 2))
plot(modelo)

#Punto 5
#Particionar los Datos
set.seed(123)  # Fijar la semilla para la reproducibilidad

# Crear un índice aleatorio para seleccionar el 70% de los datos para el conjunto de entrenamiento
train_index <- sample(seq_len(nrow(vivienda_norte_casas)), size = 0.7 * nrow(vivienda_norte_casas))

# Crear los sets de entrenamiento y prueba
train_set <- vivienda_norte_casas[train_index, ]
test_set <- vivienda_norte_casas[-train_index, ]

#Estimar el Modelo Usando el Conjunto de Entrenamiento
# Estimar el modelo con el conjunto de entrenamiento
modelo_train <- lm(preciom ~ areaconst + estrato + habitaciones + parqueaderos + banios, data = train_set)

# Mostrar un resumen del modelo
summary(modelo_train)

#Punto 6
# Paso 1: Realizar predicciones sobre el conjunto de prueba
predicciones <- predict(modelo, newdata = test_set)

# Paso 2: Combinar las predicciones con los valores reales
resultados <- data.frame(
  Real = test_set$preciom,
  Prediccion = predicciones
)

# Mostrar las primeras filas de las predicciones
print(head(resultados))

# Paso 3: Visualización de las predicciones vs los valores reales
library(ggplot2)

ggplot(resultados, aes(x = Real, y = Prediccion)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicciones vs Valores Reales",
       x = "Valor Real",
       y = "Predicción") +
  theme_minimal()



#Punto 7
# Calcular el Error Cuadrático Medio (MSE)
mse <- mean((test_set$preciom - predicciones)^2)

# Calcular el Error Absoluto Medio (MAE)
mae <- mean(abs(test_set$preciom - predicciones))

# Calcular el R2
r2 <- cor(test_set$preciom, predicciones)^2

# Mostrar los resultados
cat("Error Cuadrático Medio (MSE):", mse, "\n")
cat("Error Absoluto Medio (MAE):", mae, "\n")
cat("Coeficiente de Determinación (R2):", r2, "\n")

#Punto 8
library(leaflet)
library(dplyr)

# Definir los valores objetivo de la vivienda 1
area_objetivo <- 200
parqueaderos_objetivo <- 1
banios_objetivo <- 2
habitaciones_objetivo <- 4
estrato_objetivo <- c(4, 5)
precio_maximo <- 350  # Crédito preaprobado en millones

# Filtrar por precio, estrato y calcular la diferencia con los valores objetivo
ofertas_cercanas <- test_set %>%
  filter(preciom <= precio_maximo & estrato %in% estrato_objetivo) %>%
  mutate(
    score = abs(areaconst - area_objetivo) + 
      abs(parqueaderos - parqueaderos_objetivo) + 
      abs(banios - banios_objetivo) + 
      abs(habitaciones - habitaciones_objetivo)
  ) %>%
  arrange(score) %>%
  head(5)  # Seleccionar las 5 ofertas más cercanas

# Verificar si hay propiedades que cumplan con los criterios
if (nrow(ofertas_cercanas) == 0) {
  cat("No se encontraron propiedades que cumplan con los criterios de la vivienda 1.\n")
} else {
  # Crear el mapa interactivo con las 5 ofertas seleccionadas
  mapa_ofertas_norte <- leaflet(ofertas_cercanas) %>%
    addTiles() %>%
    addCircleMarkers(
      lng = ~longitud,
      lat = ~latitud,
      radius = 8,
      color = "blue",
      fill = TRUE,
      fillOpacity = 0.6,
      popup = ~paste(
        "Barrio:", barrio, "<br>",
        "Precio Estimado: $", round(preciom, 2), "millones<br>",
        "Área Construida:", areaconst, "m2<br>",
        "Estrato:", estrato, "<br>",
        "Habitaciones:", habitaciones, "<br>",
        "Parqueaderos:", parqueaderos, "<br>",
        "Baños:", banios
      ),
      label = ~paste("Precio: $", round(preciom, 2), "millones")
    ) %>%
    setView(lng = mean(ofertas_cercanas$longitud, na.rm = TRUE),
            lat = mean(ofertas_cercanas$latitud, na.rm = TRUE),
            zoom = 13)
  
  # Mostrar el mapa
  print(mapa_ofertas_norte)
  
  # Generar la tabla con las características de las casas seleccionadas
  tabla_ofertas <- ofertas_cercanas %>%
    select(areaconst, parqueaderos, banios, habitaciones, estrato, zona, barrio, preciom)
  
  # Mostrar la tabla
  print(tabla_ofertas)
}


--------------------------------------------------------------------------------
  #-----------------------------Vivienda 2 Zona Sur--------------------------
------------------------------------------------------------------------------------------
#Punto 1.
  
# Filtrar los datos para incluir solo apartamentos en la zona sur
vivienda_sur_aptos <- subset(vivienda, zona == "Zona Sur" & tipo == "Apartamento")  

# Resumen de la base de datos filtrada
print(summary(vivienda_sur_aptos))
  
# Crear el mapa con las ubicaciones originales de los apartamentos en la zona sur
mapa <- leaflet(vivienda_sur_aptos) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitud,
    lat = ~latitud,
    radius = 5,
    color = "blue",
    fill = TRUE,
    fillOpacity = 0.7,
    popup = ~paste("Precio: $", round(preciom, 2)),
    label = ~paste("Precio: $", round(preciom, 2))
  ) %>%
  setView(lng = mean(vivienda_sur_aptos$longitud, na.rm = TRUE),
          lat = mean(vivienda_sur_aptos$latitud, na.rm = TRUE),
          zoom = 13)

# Mostrar el mapa
mapa

# Obtener un listado único de los barrios en la zona sur
listado_barrios_unicos <- vivienda_sur_aptos %>%
  distinct(barrio) %>%
  arrange(barrio)

# Mostrar el listado de barrios únicos
print(listado_barrios_unicos, n = nrow(listado_barrios_unicos))

# Normalizar los nombres de los barrios
vivienda_sur_aptos <- vivienda_sur_aptos %>%
  mutate(barrio_normalizado = case_when(
    barrio %in% c("Valle Del Lili", "valle del lili", "valle de lili") ~ "Valle del Lili",
    barrio %in% c("Melendez", "melendez", "meléndez", "mel√©ndez") ~ "Meléndez",
    barrio %in% c("Ingenio", "El Ingenio", "ingenio", "ingenio ii", "El Ingenio II", "ingenio 2") ~ "El Ingenio",
    barrio %in% c("Gran Limonar", "El Gran Limonar", "gran limonar", "el gran limonar") ~ "El Gran Limonar",
    barrio %in% c("Ciudad Melendez", "ciudad melendez", "Ciudad Mel√©ndez", "Ciudad Meléndez") ~ "Ciudad Meléndez",
    barrio %in% c("Ciudad Jardín", "ciudad jardin", "Ciudad Jardin", "Ciudad Jardín Pance", "ciudad jardín") ~ "Ciudad Jardín",
    barrio %in% c("Alferez Real", "alferez real", "alf√©rez real") ~ "Alférez Real",
    barrio == "Cali" ~ "Cali",
    TRUE ~ barrio  # Mantener el nombre original si no coincide con ninguna de las condiciones anteriores
  ))

# Verificar los primeros registros para asegurar que la normalización se ha realizado correctamente
print(head(vivienda_sur_aptos[, c("barrio", "barrio_normalizado")]))

# Integrar la columna normalizada a la columna original de barrios
vivienda_sur_aptos <- vivienda_sur_aptos %>%
  mutate(barrio = barrio_normalizado) %>%
  select(-barrio_normalizado)  # Eliminar la columna barrio_normalizado si ya no es necesaria

# Verificar los primeros registros para asegurar que la integración se ha realizado correctamente
print(head(vivienda_sur_aptos[, c("barrio", "longitud", "latitud")]))


library(dplyr)

# Listado de barrios a retirar
barrios_retirar <- c("Zona Sur", "Cali", "Belisario Caicedo", "Cataya Real", "Fuentes de la", 
                     "Pampa linda", "La Cascada", "San Fernando", "Alameda", "Colseguros", 
                     "Colseguros Andes", "Cuarto de Legua", "El Lido", "Panamericano", 
                     "San Bosco", "San Fernando Nuevo", "San Fernando Viejo", "Acopi", 
                     "La Flora", "Versalles", "Vipasa", "Aguablanca", "Buenos Aires", 
                     "Caldas", "Aguacatal", "Alto Jordán", "Altos de Guadalupe", 
                     "Arboleda", "Bella Suiza", "Cerro Cristales", "Cristales", 
                     "Los Farallones", "Miraflores", "Normandía", "Santa Teresita", 
                     "Sector Aguacatal", "Seminario")

# Filtrar los datos para excluir los barrios en la lista de retiro
vivienda_sur_aptos_filtrados <- vivienda_sur_aptos %>%
  filter(!barrio %in% barrios_retirar)

# Verificar los primeros registros del conjunto de datos filtrado
print(head(vivienda_sur_aptos_filtrados[, c("barrio", "longitud", "latitud")]))


# Crear el mapa con las ubicaciones originales de los apartamentos en la zona sur después del filtrado
mapa_filtrado <- leaflet(vivienda_sur_aptos_filtrados) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitud,
    lat = ~latitud,
    radius = 5,
    color = "blue",
    fill = TRUE,
    fillOpacity = 0.7,
    popup = ~paste("Barrio:", barrio, "<br>Precio: $", round(preciom, 2)),
    label = ~paste("Barrio:", barrio)
  ) %>%
  setView(lng = mean(vivienda_sur_aptos_filtrados$longitud, na.rm = TRUE),
          lat = mean(vivienda_sur_aptos_filtrados$latitud, na.rm = TRUE),
          zoom = 13)

# Mostrar el mapa
mapa_filtrado

#Punto 2
# Instalar y cargar las librerías necesarias (solo si es necesario)
if(!require(GGally)) install.packages("GGally")
if(!require(plotly)) install.packages("plotly")

library(GGally)
library(plotly)
# Selección de variables para el análisis en la zona sur
variables_seleccionadas_sur <- vivienda_sur_aptos_filtrados[, c("preciom", "areaconst", "estrato", "banios", "habitaciones")]

# Generar el gráfico de pares (ggpairs) para las variables seleccionadas
ggpairs_plot_sur <- ggpairs(variables_seleccionadas_sur, 
                            title = "Correlación entre Precio y Variables Seleccionadas - Zona Sur")

# Convertir el gráfico de ggpairs a un objeto plotly para interactividad
plotly_ggpairs_sur <- ggplotly(ggpairs_plot_sur)

# Mostrar el gráfico interactivo
plotly_ggpairs_sur

#Punto 3

# Identificación y Tratamiento de Valores Faltantes

# Ver cuántos valores NA hay en cada columna relevante en la zona sur
na_counts_sur <- sapply(vivienda_sur_aptos_filtrados[, c("areaconst", "estrato", "habitaciones", 
                                                         "parqueaderos", "banios")], 
                        function(x) sum(is.na(x)))

# Mostrar la cantidad de valores NA en cada columna
print(na_counts_sur)


# Asegurarse de que la columna barrio_normalizado no se elimina antes de realizar la imputación
vivienda_sur_aptos_filtrados <- vivienda_sur_aptos %>%
  filter(!barrio %in% barrios_retirar)

# Función para calcular la moda
get_moda <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

# Calcular la moda de parqueaderos por barrio en la zona sur
moda_parqueaderos_sur <- aggregate(parqueaderos ~ barrio, data = vivienda_sur_aptos_filtrados, FUN = get_moda)

# Imputar los valores faltantes con la moda correspondiente al barrio
vivienda_sur_aptos_filtrados <- merge(vivienda_sur_aptos_filtrados, moda_parqueaderos_sur, by = "barrio", suffixes = c("", "_moda"))

# Reemplazar los NA en parqueaderos con la moda del barrio
vivienda_sur_aptos_filtrados$parqueaderos[is.na(vivienda_sur_aptos_filtrados$parqueaderos)] <- vivienda_sur_aptos_filtrados$parqueaderos_moda

# Eliminar la columna temporal de moda
vivienda_sur_aptos_filtrados$parqueaderos_moda <- NULL

# Verificar si se han imputado los valores faltantes
na_counts_sur_updated <- sapply(vivienda_sur_aptos_filtrados[, c("areaconst", "estrato", "habitaciones", 
                                                                 "parqueaderos", "banios")], 
                                function(x) sum(is.na(x)))
print(na_counts_sur_updated)


# Función para detectar outliers usando el método IQR
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(x < lower_bound | x > upper_bound)
}

# Identificación de outliers para cada variable en la zona sur
vivienda_sur_aptos_filtrados$outlier_areaconst <- detect_outliers(vivienda_sur_aptos_filtrados$areaconst)
vivienda_sur_aptos_filtrados$outlier_habitaciones <- detect_outliers(vivienda_sur_aptos_filtrados$habitaciones)
vivienda_sur_aptos_filtrados$outlier_parqueaderos <- detect_outliers(vivienda_sur_aptos_filtrados$parqueaderos)
vivienda_sur_aptos_filtrados$outlier_banios <- detect_outliers(vivienda_sur_aptos_filtrados$banios)

# Verificación del número de outliers detectados en cada variable
print(sapply(vivienda_sur_aptos_filtrados[, grep("outlier", colnames(vivienda_sur_aptos_filtrados))], sum))

# Convertir la variable 'estrato' a factor
vivienda_sur_aptos_filtrados$estrato <- as.factor(vivienda_sur_aptos_filtrados$estrato)

# Verificar la conversión
print(str(vivienda_sur_aptos_filtrados$estrato))

# Cargar las librerías necesarias
library(tidyverse)
library(broom)

# Estimación del modelo de regresión lineal múltiple 
modelo_sur <- lm(preciom ~ areaconst + estrato + habitaciones + parqueaderos + banios, data = vivienda_sur_aptos_filtrados)

# Resumen del modelo: Coeficientes y su significancia
modelo_sur_summary <- summary(modelo_sur)
print(modelo_sur_summary)

# Resumen del modelo en un formato más ordenado usando broom
modelo_sur_tidy <- tidy(modelo_sur)
print(modelo_sur_tidy)

# Interpretación del coeficiente R²
r2_value_sur <- modelo_sur_summary$r.squared
adjusted_r2_value_sur <- modelo_sur_summary$adj.r.squared

cat("El valor de R² es:", r2_value_sur, "\n")
cat("El valor de R² ajustado es:", adjusted_r2_value_sur, "\n")

#Punto 4
# Visualización de los residuos del modelo para diagnóstico
par(mfrow = c(2, 2))
plot(modelo_sur)

# Punto 5
# Punto 5: Particionar los Datos
set.seed(123)  # Fijar la semilla para la reproducibilidad

# Crear un índice aleatorio para seleccionar el 70% de los datos para el conjunto de entrenamiento
train_index_sur <- sample(seq_len(nrow(vivienda_sur_aptos_filtrados)), size = 0.7 * nrow(vivienda_sur_aptos_filtrados))

# Crear los sets de entrenamiento y prueba
train_set_sur <- vivienda_sur_aptos_filtrados[train_index_sur, ]
test_set_sur <- vivienda_sur_aptos_filtrados[-train_index_sur, ]

# Estimar el Modelo Usando el Conjunto de Entrenamiento
# Estimar el modelo con el conjunto de entrenamiento
modelo_train_sur <- lm(preciom ~ areaconst + estrato + habitaciones + parqueaderos + banios, data = train_set_sur)

# Mostrar un resumen del modelo
summary(modelo_train_sur)

# Punto 6: Realizar predicciones y visualización
# Paso 1: Realizar predicciones sobre el conjunto de prueba
predicciones_sur <- predict(modelo_train_sur, newdata = test_set_sur)

# Paso 2: Combinar las predicciones con los valores reales
resultados_sur <- data.frame(
  Real = test_set_sur$preciom,
  Prediccion = predicciones_sur
)

# Mostrar las primeras filas de las predicciones
print(head(resultados_sur))

# Paso 3: Visualización de las predicciones vs los valores reales
library(ggplot2)

ggplot(resultados_sur, aes(x = Real, y = Prediccion)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicciones vs Valores Reales - Zona Sur",
       x = "Valor Real",
       y = "Predicción") +
  theme_minimal()

# Punto 7: Evaluación del modelo
# Calcular el Error Cuadrático Medio (MSE)
mse_sur <- mean((test_set_sur$preciom - predicciones_sur)^2)

# Calcular el Error Absoluto Medio (MAE)
mae_sur <- mean(abs(test_set_sur$preciom - predicciones_sur))

# Calcular el R2
r2_sur <- cor(test_set_sur$preciom, predicciones_sur)^2

# Mostrar los resultados
cat("Error Cuadrático Medio (MSE) - Zona Sur:", mse_sur, "\n")
cat("Error Absoluto Medio (MAE) - Zona Sur:", mae_sur, "\n")
cat("Coeficiente de Determinación (R2) - Zona Sur:", r2_sur, "\n")


#Punto 8
library(leaflet)
library(dplyr)

# Definir los valores objetivo de la vivienda 2
area_objetivo <- 300
parqueaderos_objetivo <- 3
banios_objetivo <- 3
habitaciones_objetivo <- 5
estrato_objetivo <- c(5, 6)
precio_maximo <- 850  # Crédito preaprobado en millones

# Filtrar por precio, estrato y calcular la diferencia con los valores objetivo
ofertas_cercanas <- test_set_sur %>%
  filter(preciom <= precio_maximo & estrato %in% estrato_objetivo) %>%
  mutate(
    score = abs(areaconst - area_objetivo) + 
      abs(parqueaderos - parqueaderos_objetivo) + 
      abs(banios - banios_objetivo) + 
      abs(habitaciones - habitaciones_objetivo)
  ) %>%
  arrange(score) %>%
  head(5)  # Seleccionar las 5 ofertas más cercanas

# Verificar si hay propiedades que cumplan con los criterios
if (nrow(ofertas_cercanas) == 0) {
  cat("No se encontraron propiedades que cumplan con los criterios de la vivienda 2.\n")
} else {
  # Crear el mapa interactivo con las 5 ofertas seleccionadas
  mapa_ofertas_sur <- leaflet(ofertas_cercanas) %>%
    addTiles() %>%
    addCircleMarkers(
      lng = ~longitud,
      lat = ~latitud,
      radius = 8,
      color = "blue",
      fill = TRUE,
      fillOpacity = 0.6,
      popup = ~paste(
        "Barrio:", barrio, "<br>",
        "Precio Estimado: $", round(preciom, 2), "millones<br>",
        "Área Construida:", areaconst, "m2<br>",
        "Estrato:", estrato, "<br>",
        "Habitaciones:", habitaciones, "<br>",
        "Parqueaderos:", parqueaderos, "<br>",
        "Baños:", banios
      ),
      label = ~paste("Precio: $", round(preciom, 2), "millones")
    ) %>%
    setView(lng = mean(ofertas_cercanas$longitud, na.rm = TRUE),
            lat = mean(ofertas_cercanas$latitud, na.rm = TRUE),
            zoom = 13)
  
  # Mostrar el mapa
  print(mapa_ofertas_sur)
  
  # Generar la tabla con las características de los apartamentos seleccionados
  tabla_ofertas <- ofertas_cercanas %>%
    select(areaconst, parqueaderos, banios, habitaciones, estrato, zona, barrio, preciom)

  # Mostrar la tabla
  print(tabla_ofertas)
}
