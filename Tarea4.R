# Carga de bibliotecas

library(raster)
library(sf)
library(spData)
library(leaflet)
library(dplyr)
library(RColorBrewer)


# Carga de datos

# Datos de orquideas

orquideas <-
  st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/gbif/orchidaceae-cr-registros.csv",
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    ), 
    quiet = TRUE
  )

# Datos de Areas Protegidas Silvestres

asp <-
  st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/sinac/asp/asp-wgs84.geojson",
    quiet = TRUE
  )

# Carga de la capa de cantones
cantones <-
  st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/ign/delimitacion-territorial-administrativa/cr_cantones_simp_wgs84.geojson",
    quiet = TRUE
  )

# Carga de la capa de provincias
provincias <-
  st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/ign/delimitacion-territorial-administrativa/cr_provincias_simp_wgs84.geojson",
    quiet = TRUE
  )


# Asignación del sistema de coordenadas
st_crs(orquideas) = 4326
st_crs(asp) = 4326

 

# (15%) En el conjunto de datos de registros de presencia, elimine los registros con un valor mayor que 1000 (mil) en el campo coordinateUncertaintyInMeters. 

orquideas <-
  orquideas %>%
  mutate(coordinateUncertaintyInMeters = as.numeric(coordinateUncertaintyInMeters)) %>%
  mutate(eventDate = as.Date(eventDate, "%Y-%m-%d"))

cat("Cantidad original de registros: ", nrow(orquideas))

# Descartar los registros que poseen una alta incertidumbre en la ubicación

orquideas <-
  orquideas %>%
  dplyr::filter(!is.na(coordinateUncertaintyInMeters) & coordinateUncertaintyInMeters <= 1000)

cat("Cantidad de registros después de descartar los de alta incertidumbre en la ubicación: ", nrow(orquideas))


# (15%) En el conjunto de datos de registros de presencia, elimine los registros con valor vacío o NA en el campo species.

# Descartar registros con especie = NA


orquideas <-
  orquideas %>%
  dplyr::filter(!is.na(species) & 
                  species != "")


cat("Cantidad de registros después de descartar los que no tienen especie: ", nrow(orquideas))

# (15%) En el conjunto de datos de ASP, elimine los registros con valor de “Area Marina de Manejo” o “Area marina protegida” en el campo descripcio (esto es para no incluir estas ASP en los análisis ni en el mapa y considerar solo las ASP terrestres).

asp <-
  asp %>%
  dplyr::filter(!is.na(descripcio) & 
                  descripcio != "Area Marina de Manejo")
asp <-
  asp %>%
  dplyr::filter(!is.na(descripcio) & 
                  descripcio != "Area marina protegida")


# Recolección de los datos de orquideas en asp


asp_registros <-
  asp %>%
  st_make_valid() %>%
  st_join(orquideas) %>%
  group_by(nombre_asp) %>%
  summarize(species = n())

# En un mapa Leaflet, muestre las siguientes capas y controles:


# Paleta de colores

registros_colores <-
  colorNumeric(palette = "YlOrBr",
               domain = asp_registros$species,
               na.color = "transparent")

# Obtencion de capa de altitud
 
alt <- getData(
  "worldclim",
  var = "alt",
  res = .5,
  lon = -84,
  lat = 10
)

altitud <- crop(alt, extent(-86, -82.3, 8, 11.3))
altitud <-
  alt %>%
  crop(provincias) %>%
  mask(provincias)

# popups


pas <- paste0("<b>", "Area protegia: ","</b>",
               (asp_registros$nombre_asp),"<br>",
               "<b>", "Numero de especies: ","</b>",
               (asp_registros$species),"<br>",
               "<b>", "Descripcion del Area Silvestre Protegida: ","</b>",
               (asp$descripcio))

leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Stamen Toner Lite") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Imágenes de ESRI") %>%
  addRasterImage(
    altitud,
    opacity = 0.8,
    col = brewer.pal(5, "YlGn"),
    group = "Altitud"
  ) %>%
  addPolygons(
    data = asp_registros,
    fillColor = ~ registros_colores(asp_registros$species),
    fillOpacity = 0.7,
    stroke = TRUE,
    color = "black",
    weight = 1,
    popup = pas,
    group = "ASP - especies de orquideas"
  ) %>%
  addLayersControl(baseGroups = c("OpenStreetMap","Stamen Toner Lite", "Imágenes de ESRI"),
                   overlayGroups = c("ASP - especies de orquideas", "Altitud")) %>%
  addLegend(
    position = "bottomleft",
    pal = registros_colores,
    values = asp_registros$species,
    group = "ASP - especies de orquideas",
    title = "Cantidad de especies") %>%
addMiniMap(
  tiles = providers$Stamen.OpenStreetMap.Mapnik,
  position = "bottomleft",
  toggleDisplay = TRUE
)



