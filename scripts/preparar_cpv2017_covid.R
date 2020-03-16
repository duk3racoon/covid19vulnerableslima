#este script trabaja con el CPV2017 (que viene resumido por manzanas y con las manzanas de menos de 30hab agrupadas
#para preservar los datos personales) para:
# 1. importar los resumenes del censo por manzanas (.xlsx) e importar el shapefile correspondiente
# 2. agregar el dato de población total al resumen de viviends
# 3. crear un IDZONA compatible con la cartografía
# 4. calcular las proporciones sobre las variables de vivienda
# 5. hacer un join de atributos con la cartografía
# 6. imputar a las manzanas con menos de 30hab el valor correspondiente al promedio en la zona correspondiente

library( readxl)
library( dplyr)
library( sf)
library( leaflet)

#importar bases del cpv en excel
#cpv2017pob <- read_excel( "~/Dropbox/Inputs/BD/CPV2017/BASE_DATOS_CPV2017_POBLACION_MANZANA.xlsx")
d_pob      <- read_excel( "~/Dropbox/Inputs/BD/CPV2017/DICCIONARIO_CPV2017_POBLACION_MANZANA.xlsx")

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
             porcentaje_pob_vulnerable    = grupo60mas/POB_TOTAL)

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
zonas_menos_30 <- zonas_menos_30 %>% select( IDZONA:porcentaje_pob_vulnerable)

#pegamos el p_pobl_vul en cada manzana de join_a, según la zona en la que está
join_a <- left_join( join_a, zonas_menos_30, by = c( "IDZONASHP" = "IDZONA"))

#eliminar IDZONA de join_b
join_b <- join_b %>% select( OBJECTID:IDZONASHP, POB_TOTAL:geometry)

#juntar de nuevo join_a con join_b
manzanas_lima <- rbind( join_a, join_b)
a <- manzanas_lima %>% 

#st_write(shp_abuelos, "/data_output/shp_abuelos.shp")

#probar con un distrito
vmt <- shp_abuelos %>% filter( UBIGEO == "150143")

#tmap
m_lima <-   tm_basemap(leaflet::providers$Stamen.Toner, alpha = 0.1)+
    tm_shape( manzanas_lima) +
    tm_fill( col = "porcentaje_pob_vulnerable")+
  tm_layout( title = "Lima Metrpolitana 2017: Porcentaje de población de 60 años a más",
            title.position = "center")+
  tm_view( view.legend.position=c("right", "bottom"))
  
current.mode <- tmap_mode( "view")

m_lima

tmap_save(m_lima, filename = "index.html")

