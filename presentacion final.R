# Instalar y cargar la libreria de igraph si no está instalado
if(!require(igraph)) install.packages("igraph", dependencies=TRUE)
library(igraph)
# Instalar y cargar la libreria de leaflet si no está instalado
if(!require(leaflet)) install.packages("leaflet", dependencies=TRUE)
library(leaflet)
# Instalar y cargar la libreria de geosphere si no está instalado
if(!require(geosphere)) install.packages("geosphere", dependencies=TRUE)
library(geosphere)

# Definir los distritos y sus coordenadas
distritos_coords <- data.frame(
  Distrito = c("San Isidro", "Miraflores", "San Borja", "La Molina", "Surco", "Lince", "Barranco", "Pueblo Libre",
               "San Miguel", "Jesús María", "Rímac", "Breña", "Comas", "Magdalena del Mar", 
               "San Juan de Lurigancho", "Ate", "Villa El Salvador", "Chorrillos"),
  Latitud = c(-12.09655, -12.1191, -12.1036, -12.0695, -12.1348, -12.0840, -12.1439, -12.0769, 
              -12.0762, -12.0805, -12.0278, -12.0561, -11.9450, -12.0908, 
              -11.9812, -12.0566, -12.1537, -12.1712),
  Longitud = c(-77.04258, -77.0341, -77.0010, -76.9378, -76.9814, -77.0317, -77.0200, -77.0606, 
               -77.0833, -77.0494, -77.0283, -77.0502, -77.0591, -77.0689, 
               -76.9943, -76.9365, -76.9980, -77.0209)
)

# Crear el mapa interactivo inicial
map <- leaflet(distritos_coords) %>%
  addTiles() %>%
  setView(lng = -77.04258, lat = -12.09655, zoom = 12) %>%
  addMarkers(
    lat = ~Latitud,
    lng = ~Longitud,
    popup = ~Distrito
  )

# Mostrar el mapa inicial
print(map)

# Calcular las distancias en kilómetros entre todas las ubicaciones
distancias <- matrix(NA, nrow = nrow(distritos_coords), ncol = nrow(distritos_coords))
rownames(distancias) <- distritos_coords$Distrito
colnames(distancias) <- distritos_coords$Distrito

for (i in 1:nrow(distritos_coords)) {
  for (j in 1:nrow(distritos_coords)) {
    distancias[i, j] <- distHaversine(c(distritos_coords$Longitud[i], distritos_coords$Latitud[i]), c(distritos_coords$Longitud[j], distritos_coords$Latitud[j])) / 1000
  }
}

# Crear el grafo ponderado utilizando las distancias como pesos de las aristas
grafito <- graph.adjacency(as.matrix(distancias), weighted = TRUE, mode = "undirected")

# Añadir las aristas del grafo original al mapa en color gris
for (edge in E(grafito)) {
  vertices <- ends(grafito, edge)
  distrito1 <- vertices[1]
  distrito2 <- vertices[2]
  lat1 <- distritos_coords[distritos_coords$Distrito == distrito1, "Latitud"]
  lon1 <- distritos_coords[distritos_coords$Distrito == distrito1, "Longitud"]
  lat2 <- distritos_coords[distritos_coords$Distrito == distrito2, "Latitud"]
  lon2 <- distritos_coords[distritos_coords$Distrito == distrito2, "Longitud"]
  
  map <- map %>% addPolylines(lng = c(lon1, lon2), lat = c(lat1, lat2), color = "gray", weight = 1, opacity = 0.5)
}

# Encontrar el MST utilizando el algoritmo de Prim
Mst_prim <- minimum.spanning.tree(grafito, algorithm = "prim")

# Añadir los caminos del MST al mapa en color azul con las distancias
for (edge in E(Mst_prim)) {
  vertices <- ends(Mst_prim, edge)
  distrito1 <- vertices[1]
  distrito2 <- vertices[2]
  lat1 <- distritos_coords[distritos_coords$Distrito == distrito1, "Latitud"]
  lon1 <- distritos_coords[distritos_coords$Distrito == distrito1, "Longitud"]
  lat2 <- distritos_coords[distritos_coords$Distrito == distrito2, "Latitud"]
  lon2 <- distritos_coords[distritos_coords$Distrito == distrito2, "Longitud"]
  distancia <- round(distancias[distrito1, distrito2], 2)
  
  map <- map %>% addPolylines(lng = c(lon1, lon2), lat = c(lat1, lat2), color = "blue", weight = 2, opacity = 0.8, label = paste0(distancia, " km"))
}

# Mostrar el mapa con el MST
print(map)

# Dibujar el MST con etiquetas de distancias
E(Mst_prim)$label <- paste0(round(E(Mst_prim)$weight, 2), " km")
plot(Mst_prim, vertex.label = V(Mst_prim)$name, edge.label = E(Mst_prim)$label, main = "Árbol de expansión mínima utilizando el algoritmo PRIM")
