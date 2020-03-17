#este script trabaja con el CPV2017 (que viene resumido por manzanas y con las manzanas de menos de 30hab agrupadas
#para preservar los datos personales) para:
# 1. importar los resumenes del censo por manzanas (.xlsx) e importar el shapefile correspondiente
# 2. calcular el porcentaje de población de 60 años a más
# 3. imputar el porcentaje correspondiente para las manzanas con menos de 30hab 
# 4. graficarlas usando tmap

library( readxl)
library( dplyr)
library( sf)
library( leaflet)
library( beepr)
library( tmap)

#importar bases del cpv en excel
#cpv2017pob <- read_excel( "~/Dropbox/Inputs/BD/CPV2017/BASE_DATOS_CPV2017_POBLACION_MANZANA.xlsx")
#d_pob      <- read_excel( "~/Dropbox/Inputs/BD/CPV2017/DICCIONARIO_CPV2017_POBLACION_MANZANA.xlsx")

#save(cpv2017pob, file = "~/Dropbox/RProjects/covid19/data/cpv2017pob_gedad.RData")
#abrir cpv2017pob guardado
load("~/Dropbox/RProjects/covid19/data/cpv2017pob_gedad.RData")

#seleccionar variables de pob necesarias (POB_TOTAL, además de los ID necesarios para matchear las 
#manzanas con menos de 30 habitantes que están agrupadas) y filtrar solo lima metropolitana. 
cpv2017pob <- cpv2017pob %>% 
  filter( CCDD == "07" | (CCDD == "15" & CCPP == "01")) %>% 
  select( IDMANZANA:C5_P8_6)

#construir IDZONA para poder imputar luego las manzanas que faltan. Tres pasos: (1) transformar ZONA_A en numérico,
#(2)pegar el código, (3) ordenar y quedar con lo que necesitamos.
cpv2017pob <- cpv2017pob %>% 
  mutate( ZONA_N = recode( ZONA_A, A="01", B="02", C="03", D="04", E="05", F="06", G="07", H="08", .missing="00" )) %>% 
  mutate( IDZONA = paste0( UBIGEO, ZONA_ID, ZONA_N)) %>%
  mutate( grupo60mas = GRUPOS_QUIN_13 + GRUPOS_QUIN_14 + GRUPOS_QUIN_15 + GRUPOS_QUIN_16 + GRUPOS_QUIN_17) %>% 
  select( IDMANZANA, IDZONA, POB_TOTAL, grupo60mas, C5_P2_1:GRUPOS_EDAD_5, C5_P8_1:C5_P8_6)

#calcular proporción de abuelos
cpv17_p_abuelos <- cpv2017pob %>% 
  transmute( IDMANZANA    = IDMANZANA,
             IDZONA       = IDZONA,
             POB_TOTAL    = POB_TOTAL,
             p_pob_vul    = grupo60mas/POB_TOTAL)

#importar shapefile
shp2017sf <- read_sf( "~/Google Drive/expansion intercensal/data_output/LMM_CPV2017.shp")

#crear IDZONASHP para poder hacer el match, y ordenar tabla   
shp2017sf <- shp2017sf %>% 
  mutate( IDZONASHP = substr( IDMANZANA, 1, 11)) %>% 
  select( OBJECTID:MANZANA, IDZONASHP, geometry)

#juntar pegar atributos en el shapefile (solo la parte que corersponde se pegará)
join <- left_join( shp2017sf, cpv17_p_abuelos, by = "IDMANZANA")

#dividir la base juntada
join_a <- join[ is.na( join$IDZONA), ]  #la parte a son manzanas del shp que no tienen correspondencia en cpv2017 (mznaas con menos de 30h)
join_b <- join[ !is.na( join$IDZONA), ] #la parte b está bien pegada

join_a <- join_a %>% select( OBJECTID:MANZANA, IDZONASHP, geometry) #se borra de joina lo que se pegó sin correspondencia para dejar espacio

#join_a debe completarse con la info de zonas_menos_30 (que tiene el p_pob_vul en las manzanas -30h de cada zona)
zonas_menos_30 <- cpv17_p_abuelos[ cpv17_p_abuelos$IDMANZANA == "MANZANAS CON MENOS DE 30 HABITANTES", ]
zonas_menos_30 <- zonas_menos_30 %>% select( IDZONA:p_pob_vul)

#pegamos el p_pobl_vul en cada manzana de join_a, según la zona en la que está
join_a <- left_join( join_a, zonas_menos_30, by = c( "IDZONASHP" = "IDZONA"))

#eliminar IDZONA de join_b
join_b <- join_b %>% select( OBJECTID:IDZONASHP, POB_TOTAL:geometry)

#juntar de nuevo join_a con join_b
join <- rbind( join_a, join_b)

#transformar el decimal en porcentaje
join <- join %>% mutate( porc_mayores_de_60 = round( p_pob_vul*100, 2))

#capear mayores de 30 a 30,renombrar y seleccionar las variables necesarias
#filtrar playas del sur para reducir tamaño del archivo
manzanas_lmm <- join %>% 
  mutate( porcentaje_pob_vulnerable = case_when(porc_mayores_de_60 >= 30 ~ 30,
                                                porc_mayores_de_60 <= 30 ~ porc_mayores_de_60))

#quitar objetos innecesarios
rm( cpv17_p_abuelos, cpv2017pob, join, join_a, join_b, shp2017sf, zonas_menos_30)

#separar el norte y el sur

manzanas_sur <-   manzanas_lmm %>% 
  filter( substr( UBIGEO, start = 1, stop = 2) != "07") %>%
  filter( UBIGEO != 150102 & UBIGEO != 150106 & UBIGEO != 150110 & UBIGEO != 150112 & UBIGEO != 150117 & 
          UBIGEO != 150125 & UBIGEO != 150135 & UBIGEO != 150139)

manzanas_norte <-   manzanas_lmm %>% 
  filter( substr( UBIGEO, start = 1, stop = 2) != "07") %>%
  filter( UBIGEO != 150108 & UBIGEO != 150119 & UBIGEO != 150123 & UBIGEO != 150124 & UBIGEO != 150126 & 
          UBIGEO != 150127 & UBIGEO != 150129 & UBIGEO != 150133 & UBIGEO != 150138 & UBIGEO != 150142 &
          UBIGEO != 150143)

#  Seleccionar solo las variables necesarias
manzanas_sur   <- manzanas_sur    %>%  select( IDMANZANA, UBIGEO, DISTRITO, porc_mayores_de_60, porcentaje_pob_vulnerable, geometry)
manzanas_norte <- manzanas_norte  %>%  select( IDMANZANA, UBIGEO, DISTRITO, porc_mayores_de_60, porcentaje_pob_vulnerable, geometry)


#distrito prueba
distrito <- manzanas_sur %>% filter( UBIGEO == "150101")

#tmap. definir modo
current.mode <- tmap_mode( "view")

#crear objetos de mapas sur y norte
m_lima_sur <- tm_shape( manzanas_sur) +
  tm_basemap(server = NULL) +
  tm_fill( col = "porcentaje_pob_vulnerable",
           title = "Mayores de 60 años (%)",
           style = "cont",
           breaks = c(0, 10, 20, 30),
           showNA = FALSE,
           id = "IDMANZANA",
           popup.vars = c( "porc_mayores_de_60", "DISTRITO")) +
  tm_tiles( leaflet::providers$Stamen.Toner, alpha = 0.15) +
  tm_layout( title = "POBLACIÓN VULNERABLE AL COVID-19: MAYORES DE 60 AÑOS. (Norte de Lima, fuente: CPV 2017, elaboración: @gonzalotalavera)",
             title.position = "center") +
  tm_view( view.legend.position=c("right", "bottom"),
           set.view = 11,
           set.zoom.limits = c(10, 18)) 

m_lima_norte <- tm_shape( manzanas_norte) +
  tm_basemap(server = NULL) +
  tm_fill( col = "porcentaje_pob_vulnerable",
           title = "Mayores de 60 años (%)",
           style = "cont",
           breaks = c(0, 10, 20, 30),
           showNA = FALSE,
           id = "IDMANZANA",
           popup.vars = c( "porc_mayores_de_60", "DISTRITO")) +
  tm_tiles( leaflet::providers$Stamen.Toner, alpha = 0.15) +
  tm_layout( title = "POBLACIÓN VULNERABLE AL COVID-19: MAYORES DE 60 AÑOS. (Norte de Lima, fuente: CPV 2017, elaboración: @gonzalotalavera)",
             title.position = "center") +
  tm_view( view.legend.position=c("right", "bottom"),
           set.view = 11,
           set.zoom.limits = c(10, 18)) 

#guardar objetos en html
tmap_save( m_lima_norte, filename = "lima_norte.html")
tmap_save( m_lima_sur,   filename = "lima_sur.html")

map<-leaflet()%>%
  addTiles()%>%
  
  addPolygons(data = plotMerge,
              fillColor = ~pal(plotMerge$incomePerCapita),
              color = "#000000", 
              fillOpacity = 0.8,
              weight = 0.2,
              popup=popup)%>%
  saveWidget(map, file="map1.html", selfcontained=FALSE)

