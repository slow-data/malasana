#===========================================
# @Project: Malasana
# @Name: viz
# @author: jprimav
# @date: 2020/05
#===========================================

rm(list=ls())

# ------------------------
# 1. Setup
# ------------------------

# libraries
library(here);packageVersion("here") # 0.1
library(sf);packageVersion("sf") # 0.9.2
library(ggplot2);packageVersion("ggplot2") # 3.3.0.9000
library(ggrepel);packageVersion("ggrepel") # 0.8.2
sessionInfo()
# R version 3.5.3 (2019-03-11)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)

# ------------------------
# 2. Prepare Geometries
# ------------------------

# Spain OSM excerpt downloaded from Geofabrik, processed to obtain only features within Madrid bounding box, stored in /shapes subfolder.

# Read Madrid Barrios
barrios <- st_read(dsn = here("shapes", "BARRIOS_ETRS89", "BARRIOS.shp"))
barrios <- st_transform(barrios, 3857)
universidad <- barrios[barrios$CODBAR=="015",]

# Read building (areas)
bldg_mad <- st_read(dsn = here("shapes", "bldg_mad", "bldg_mad.shp"))
bldg_mad <- st_transform(bldg_mad, 3857)
# clip building to barrio Universidad
bldg_universidad <- st_intersection(bldg_mad, universidad)

# Read landuse
landuse_mad <- st_read(dsn = here("shapes", "landuse_mad", "landuse_mad.shp"))
landuse_mad <- st_transform(landuse_mad, 3857)
# clip landuse to barrio Universidad
landuse_universidad <- st_intersection(landuse_mad, universidad)

# Subset landuse to extract only parks
parks_mad <- landuse_mad[landuse_mad$fclass=="park",]
parks_mad <- parks_mad[!st_is_empty(parks_mad),,drop=FALSE]
# clip parks_mad to barrio Universidad
parks_universidad <- st_intersection(parks_mad, universidad)

# Read bodies of water
waterbody_mad <- st_read(dsn = here("shapes", "waterbody_mad", "waterbody_mad.shp"))
waterbody_mad <- st_transform(waterbody_mad, 3857)
# clip bodies of water to barrio Universidad
waterbody_universidad <- st_intersection(waterbody_mad, universidad)

# Read waterways
waterway_mad <- st_read(dsn = here("shapes", "waterway_mad", "waterway_mad.shp"))
waterway_mad <- st_transform(waterway_mad, 3857)
# clip waterways to barrio Universidad
waterway_universidad <- st_intersection(waterway_mad, universidad)

# Read roads (lines)
roads_mad <- st_read(dsn = here("shapes", "roads_mad", "roads_mad.shp"))
roads_mad <- st_transform(roads_mad, 3857)

# Subset roads_mad to extract only certain roads
fuencarral <- roads_mad[roads_mad$name=="Calle de Fuencarral",]
fuencarral <- fuencarral[!st_is_empty(fuencarral),,drop=FALSE]

san_bernardo <- roads_mad[roads_mad$name=="Calle de San Bernardo",]
san_bernardo <- san_bernardo[!st_is_empty(san_bernardo),,drop=FALSE]

gran_via <- roads_mad[roads_mad$name=="Gran Vía",]
gran_via <- gran_via[!st_is_empty(gran_via),,drop=FALSE]
gran_via_clean <- gran_via[gran_via$osm_id!=254044768,]

bilbao <- roads_mad[roads_mad$name=="Glorieta de Bilbao",]
bilbao <- bilbao[!st_is_empty(bilbao),,drop=FALSE]

ruiz_jimenez <- roads_mad[roads_mad$name=="Glorieta de Ruiz Jiménez",]
ruiz_jimenez <- ruiz_jimenez[!st_is_empty(ruiz_jimenez),,drop=FALSE]

princesa <- roads_mad[roads_mad$name=="Calle de la Princesa",]
princesa <- princesa[!st_is_empty(princesa),,drop=FALSE]

aguilera <- roads_mad[roads_mad$name=="Calle de Alberto Aguilera",]
aguilera <- aguilera[!st_is_empty(aguilera),,drop=FALSE]

pl_domingo <- roads_mad[roads_mad$name=="Plaza de Santo Domingo",]
pl_domingo <- pl_domingo[!st_is_empty(pl_domingo),,drop=FALSE]
pl_domingo_clean <- pl_domingo[pl_domingo$osm_id %in% c(443396463, 45757392),]

cuesta_domingo <- roads_mad[roads_mad$name=="Cuesta de Santo Domingo",]
cuesta_domingo <- cuesta_domingo[!st_is_empty(cuesta_domingo),,drop=FALSE]
cuesta_domingo_clean <- cuesta_domingo[cuesta_domingo$osm_id == 82280715,]

costanilla_angeles <- roads_mad[roads_mad$name=="Costanilla de los Ángeles",]
costanilla_angeles <- costanilla_angeles[!st_is_empty(costanilla_angeles),,drop=FALSE]

pl_espana <- roads_mad[roads_mad$name=="Plaza de España",]
pl_espana <- pl_espana[!st_is_empty(pl_espana),,drop=FALSE]
pl_espana_connect <- pl_espana[pl_espana$osm_id %in% c(238829520, 238829521, 4308596, 545103682, 545103684),]

carranza <- roads_mad[roads_mad$name=="Calle de Carranza",]
carranza <- carranza[!st_is_empty(carranza),,drop=FALSE]

sagasta <- roads_mad[roads_mad$name=="Calle de Sagasta",]
sagasta <- sagasta[!st_is_empty(sagasta),,drop=FALSE]

marques_urquijo <- roads_mad[roads_mad$name=="Calle del Marqués de Urquijo",]
marques_urquijo <- marques_urquijo[!st_is_empty(marques_urquijo),,drop=FALSE]

luchana <- roads_mad[roads_mad$name=="Calle de Luchana",]
luchana <- luchana[!st_is_empty(luchana),,drop=FALSE]

montera <- roads_mad[roads_mad$name=="Calle de la Montera",]
montera <- montera[!st_is_empty(montera),,drop=FALSE]


## PoIs

# la via lactea
geometry <- st_sfc(st_point(c(-3.702952,40.426874)), crs = 4326)
via_lactea <- st_sf(name="La Via Lactea", geometry)
via_lactea <- st_transform(via_lactea, 3857)
via_lactea_bldg <- bldg_mad[via_lactea,]

# conde duque
bldg_mad[grep("Conde Duque", bldg_mad$name),]
conde_duque <- bldg_mad[bldg_mad$name=="Centro Cultural Conde Duque",]
conde_duque <- conde_duque[!st_is_empty(conde_duque),,drop=FALSE]

# dos de mayo
landuse_mad[grep("Dos de Mayo", landuse_mad$name),]
dos_de_mayo <- landuse_mad[landuse_mad$name=="Plaza del Dos de Mayo",]
dos_de_mayo <- dos_de_mayo[!st_is_empty(dos_de_mayo),,drop=FALSE]
dos_de_mayo

# Casa Camacho
geometry <- st_sfc(st_point(c(-3.703818, 40.425615)), crs = 4326)
casa_camacho <- st_sf(name="Casa Camacho", geometry)
casa_camacho <- st_transform(casa_camacho, 3857)
casa_camacho_bldg <- bldg_mad[casa_camacho,]
casa_camacho_bldg

# BarCo
geometry <- st_sfc(st_point(c(-3.702234, 40.423182)), crs = 4326)
barco <- st_sf(name="Barco", geometry)
barco <- st_transform(barco, 3857)
barco_bldg <- bldg_mad[barco,]
barco_bldg

# Sala Siroco
geometry <- st_sfc(st_point(c(-3.707737, 40.426909)), crs = 4326)
siroco <- st_sf(name="Sala Siroco", geometry)
siroco <- st_transform(siroco, 3857)
siroco
siroco_bldg <- bldg_mad[siroco,]
siroco_bldg

# Plaza de las comendadores
pl_comendadores <- roads_mad[roads_mad$name=="Plaza de las Comendadoras",]
pl_comendadores <- pl_comendadores[!st_is_empty(pl_comendadores),,drop=FALSE]
pl_comendadores
plot(pl_comendadores)
pl_comendadores_poly <- st_cast(pl_comendadores, "POLYGON")

# Teatro alfil
geometry <- st_sfc(st_point(c(-3.704523, 40.423235)), crs = 4326)
alfil <- st_sf(name="Teatro Alfil", geometry)
alfil <- st_transform(alfil, 3857)
alfil
alfil_bldg <- bldg_mad[alfil,]
alfil_bldg

# noviciado
geometry <- st_sfc(st_point(c(-3.707420, 40.425060)), crs = 4326)
noviciado <- st_sf(name="Noviciado", geometry)
noviciado <- st_transform(noviciado, 3857)

# bilbao
geometry <- st_sfc(st_point(c(-3.702189, 40.429063)), crs = 4326)
bilbao_poi <- st_sf(name="Bilbao", geometry)
bilbao_poi <- st_transform(bilbao_poi, 3857)

# calle san bernardo poi
geometry <- st_sfc(st_point(c(-3.707399, 40.424396)), crs = 4326)
sanbern_poi <- st_sf(name="C. San Bernardo", geometry)
sanbern_poi <- st_transform(sanbern_poi, 3857)

# mercado de san ildefonso
geometry <- st_sfc(st_point(c(-3.700834, 40.424197)), crs = 4326)
mercado_ilde <- st_sf(name="Mercado de San Ildefonso", geometry)
mercado_ilde <- st_transform(mercado_ilde, 3857)
mercado_ilde
mercado_ilde_bldg <- bldg_mad[mercado_ilde,]
mercado_ilde_bldg

# Plaza de Espana
geometry <- st_sfc(st_point(c(-3.712158, 40.423491)), crs = 4326)
pl_espana_poi <- st_sf(name="Pl. de Espana", geometry)
pl_espana_poi <- st_transform(pl_espana_poi, 3857)

# gran via
geometry <- st_sfc(st_point(c(-3.709096, 40.422322)), crs = 4326)
granvia_poi <- st_sf(name="Gran Via", geometry)
granvia_poi <- st_transform(granvia_poi, 3857)

# princesa poi
geometry <- st_sfc(st_point(c(-3.713624, 40.427010)), crs = 4326)
princesa_poi <- st_sf(name="C. Princesa", geometry)
princesa_poi <- st_transform(princesa_poi, 3857)

# Corte Ingles
geometry <- st_sfc(st_point(c(-3.715265, 40.430169)), crs = 4326)
corte_ingles <- st_sf(name="Corte Ingles", geometry)
corte_ingles <- st_transform(corte_ingles, 3857)
corte_ingles
corte_ingles_bldg <- bldg_mad[corte_ingles,]
corte_ingles_bldg

# fuencarral poi
geometry <- st_sfc(st_point(c(-3.702809, 40.430180)), crs = 4326)
fuencarral_poi <- st_sf(name="C. Fuencarral", geometry)
fuencarral_poi <- st_transform(fuencarral_poi, 3857)

# Callap poi
geometry <- st_sfc(st_point(c(-3.705740, 40.420245)), crs = 4326)
callao_poi <- st_sf(name="Callao", geometry)
callao_poi <- st_transform(callao_poi, 3857)

# Alberto Aguilera
geometry <- st_sfc(st_point(c(-3.711346, 40.430156)), crs = 4326)
alb_agui_poi <- st_sf(name="Alberto Aguilera", geometry)
alb_agui_poi <- st_transform(alb_agui_poi, 3857)



# ---------------------------------
# 3. Prepare plot style and annotations
# ---------------------------------

# set fonts
windowsFonts(
  Impact=windowsFont("Impact"),
  Times=windowsFont("TT Times New Roman"),
  ArialBlack=windowsFont("Arial Black"),
  BookmanOldStyle=windowsFont("Bookman Old Style"),
  ComicSansMS=windowsFont("Comic Sans MS"),
  OldEngText=windowsFont("Old English Text MT"),
  Matura=windowsFont("Matura MT Script Capitals"),
  Kunstler=windowsFont("Kunstler Script"),
  HighTower=windowsFont("High Tower Text"),
  Goudy=windowsFont("Goudy Old Style"),
  Symbol=windowsFont("Symbol"),
  GillUltra=windowsFont("Gill Sans Ultra Bold"),
  Ravie=windowsFont("Ravie"),
  Forte=windowsFont("Forte")
)


# colors from https://www.schemecolor.com/disco-dance.php


# -----------------------------------------------------------
# Plot
# -----------------------------------------------------------

# set plot zoom
s1 <- st_bbox(universidad)["xmin"] - 100
s2 <- st_bbox(universidad)["xmax"] + 100
s3 <- st_bbox(universidad)["ymin"] - 100
s4 <- st_bbox(universidad)["ymax"] + 100

# Crop geometries to plot window
roads_bb <- st_crop(roads_mad, y=c(s1, s2, s3, s4))
bldg_bb <- st_crop(bldg_mad, y=c(s1, s2, s3, s4))
parks_bb <- st_crop(parks_mad, y=c(s1, s2, s3, s4))
waterbody_bb <- st_crop(waterbody_mad, y=c(s1, s2, s3, s4))
waterway_bb <- st_crop(waterway_mad, y=c(s1, s2, s3, s4))
fuencarral_bb <- st_crop(fuencarral, y=c(s1, s2, s3, s4))
san_bernardo_bb <- st_crop(san_bernardo, y=c(s1, s2, s3, s4))
gran_via_clean_bb <- st_crop(gran_via_clean, y=c(s1, s2, s3, s4))
bilbao_bb <- st_crop(bilbao, y=c(s1, s2, s3, s4))
ruiz_jimenez_bb <- st_crop(ruiz_jimenez, y=c(s1, s2, s3, s4))
princesa_bb <- st_crop(princesa, y=c(s1, s2, s3, s4))
aguilera_bb <- st_crop(aguilera, y=c(s1, s2, s3, s4))
pl_domingo_clean_bb <- st_crop(pl_domingo_clean, y=c(s1, s2, s3, s4))
costanilla_angeles_bb <- st_crop(costanilla_angeles, y=c(s1, s2, s3, s4))
pl_espana_bb <- st_crop(pl_espana, y=c(s1, s2, s3, s4))
pl_espana_connect_bb <- st_crop(pl_espana_connect, y=c(s1, s2, s3, s4))
carranza_bb <- st_crop(carranza, y=c(s1, s2, s3, s4))
sagasta_bb <- st_crop(sagasta, y=c(s1, s2, s3, s4))
marques_urquijo_bb <- st_crop(marques_urquijo, y=c(s1, s2, s3, s4))
luchana_bb <- st_crop(luchana, y=c(s1, s2, s3, s4))
montera_bb <- st_crop(montera, y=c(s1, s2, s3, s4))

# Set margins
m1 <- 50
m2 <- 50
m3 <- 150
m4 <- 50

# plot window limits
mxmin <- s1 - m1
mxmax <- s2 + m2
mymin <- s3 - m3
mymax <- s4 + m4

coord <- coord_equal(xlim = c(mxmin,mxmax), ylim = c(mymin,mymax), expand = FALSE)
asp <- coord$aspect(list(x.range = range(c(mxmin,mxmax)), y.range = range(c(mymin,mymax))))
asp # e.g. Instagram maximum Height/Width ratio is 1.25

p <- ggplot() +
  # Parks
  geom_sf(data = parks_bb, fill = "#F1F1F1", col = NA) +
  # buildings
  geom_sf(data = bldg_bb, fill = "#F1F1F1", col = NA) +
  # Roads
  geom_sf(data = roads_bb, size=.2, col = "#E7E7E7") +
  # bodies of water Malasana
  geom_sf(data = waterbody_universidad, fill = "#6E95CC", col = NA) +
  # Parks Malasana
  geom_sf(data = parks_universidad, fill = "#ABBD9D", col = NA) +
  # building malasana
  geom_sf(data = bldg_universidad, fill = "#E7C9E3", col = NA) + # #B0ABCA
  # Highlights Malasaña perimeter and key streets
  geom_sf(data = fuencarral_bb, size = 4, col = "black") +
  geom_sf(data = fuencarral_bb, size = 1, col = "#FA1085") +
  geom_sf(data = san_bernardo_bb, size = 4, col = "black") +
  geom_sf(data = san_bernardo_bb, size = 1, col = "#FA1085") +
  geom_sf(data = gran_via_clean_bb, size = 4, col = "black") +
  geom_sf(data = gran_via_clean_bb, size = 1, col = "#FA1085") +
  geom_sf(data = bilbao_bb, size = 4, col = "black") +
  geom_sf(data = bilbao_bb, size = 1, col = "#FA1085") +
  geom_sf(data = ruiz_jimenez_bb, size = 4, col = "black") +
  geom_sf(data = ruiz_jimenez_bb, size = 1, col = "#FA1085") +
  geom_sf(data = princesa_bb, size = 4, col = "black") +
  geom_sf(data = princesa_bb, size = 1, col = "#FA1085") +
  geom_sf(data = aguilera_bb, size = 4, col = "black") +
  geom_sf(data = aguilera_bb, size = 1, col = "#FA1085") +
  geom_sf(data = pl_espana_connect_bb, size = 4, col = "black") +
  geom_sf(data = pl_espana_connect_bb, size = 1, col = "#FA1085") +
  geom_sf(data = carranza_bb, size = 4, col = "black") +
  geom_sf(data = carranza_bb, size = 1, col = "#FA1085") +
  geom_sf(data = sagasta_bb, size = 4, col = "black") +
  geom_sf(data = sagasta_bb, size = 1, col = "#FA1085") +
  geom_sf(data = marques_urquijo_bb, size = 4, col = "black") +
  geom_sf(data = marques_urquijo_bb, size = 1, col = "#FA1085") +
  geom_sf(data = montera_bb, size = 4, col = "black") +
  geom_sf(data = montera_bb, size = 1, col = "#FA1085") +
  geom_sf(data = pl_domingo_clean_bb, size = 4, col = "black") +
  geom_sf(data = pl_domingo_clean_bb, size = 1, col = "#FA1085") +
  geom_sf(data = costanilla_angeles_bb, size = 4, col = "black") +
  geom_sf(data = costanilla_angeles_bb, size = 1, col = "#FA1085") +
  ## PoIs
  # Conde Duque
  geom_sf(data = conde_duque, fill = "#FA1085", col = "black", size = 1) +
  # Pl. 2 de Mayo
  geom_sf(data = dos_de_mayo, fill = "#FA1085", col = "black", size = 1) +
  # Casa Camacho
  geom_sf(data = casa_camacho_bldg, fill = "#FA1085", col = "black", size = 1) +
  # Sala Siroco
  geom_sf(data = siroco_bldg, fill = "#FA1085", col = "black", size = 1) +
  # Teatro Alfil
  geom_sf(data = alfil_bldg, fill = "#FA1085", col = "black", size = 1) +
  # Mercado de San Ildefonso
  geom_sf(data = mercado_ilde_bldg, fill = "#FA1085", col = "black", size = 1) +
  # Cortes Ingles
  geom_sf(data = corte_ingles_bldg, fill = "#FA1085", col = "black", size = 1) +
  ## PoIs Annotations
  geom_text_repel(data = as.data.frame(st_coordinates(st_centroid(conde_duque))),
                   fill = "white",
                   col = "black",
                   aes(x = X, y = Y), label = "Conde Duque",
                   size = 4, fontface="italic", family = "BookmanOldStyle", nudge_x = -160, nudge_y = 60, force=0) +
  geom_text_repel(data = as.data.frame(st_coordinates(st_centroid(dos_de_mayo))),
                   fill = "white",
                   col = "black",
                   aes(x = X, y = Y), label = "Pl. Dos de Mayo",
                   size = 4, fontface="italic", family = "BookmanOldStyle", nudge_y = 70, force=0) +
  geom_text_repel(data = as.data.frame(st_coordinates(st_centroid(casa_camacho_bldg))),
                   fill = "white",
                   col = "black",
                   aes(x = X, y = Y), label = "Casa Camacho",
                   size = 4, fontface="italic", family = "BookmanOldStyle", nudge_y = -30, force=0) +
  geom_text_repel(data = as.data.frame(st_coordinates(st_centroid(siroco_bldg))),
                   fill = "white",
                   col = "black",
                   aes(x = X, y = Y), label = "Sala Siroco",
                   size = 4, fontface="italic", family = "BookmanOldStyle", nudge_y = 30, force=0) +
  geom_text_repel(data = as.data.frame(st_coordinates(st_centroid(alfil_bldg))),
                   fill = "white",
                   col = "black",
                   aes(x = X, y = Y), label = "Teatro Alfil",
                   size = 4, fontface="italic", family = "BookmanOldStyle", nudge_y = -80, force=0) +
  geom_text_repel(data = as.data.frame(st_coordinates(st_centroid(mercado_ilde_bldg))),
                   fill = "white",
                   col = "black",
                   aes(x = X, y = Y), label = "Mercado S. Ildefonso",
                   size = 4, fontface="italic", family = "BookmanOldStyle", nudge_x = -170, nudge_y = -35, force=0) +
  geom_text_repel(data = as.data.frame(st_coordinates(sanbern_poi)),
                   fill = "white",
                   col = "black",
                   aes(x = X, y = Y), label = "C/San Bernardo",
                   size = 4, fontface="italic", family = "BookmanOldStyle", nudge_x = -180, nudge_y = 30, force=0) +
  geom_text_repel(data = as.data.frame(st_coordinates(pl_espana_poi)),
                   fill = "white",
                   col = "black",
                   aes(x = X, y = Y), label = "Pl. Espa\u00f1a",
                   size = 4, fontface="italic", family = "BookmanOldStyle", force=0) +
  geom_text_repel(data = as.data.frame(st_coordinates(bilbao_poi)),
                   fill = "white",
                   col = "black",
                   aes(x = X, y = Y), label = "Gl.ta Bilbao",
                   size = 4, fontface="italic", family = "BookmanOldStyle", nudge_y = 50, nudge_x = 90, force=0) +
  geom_text_repel(data = as.data.frame(st_coordinates(granvia_poi)),
                   fill = "white",
                   col = "black",
                   aes(x = X, y = Y), label = "Gran V\u00eda",
                   size = 4, fontface="italic", family = "BookmanOldStyle", nudge_y = -60, nudge_x = -50, force=0) +
  geom_text_repel(data = as.data.frame(st_coordinates(princesa_poi)),
                   fill = "white",
                   col = "black",
                   aes(x = X, y = Y), label = "C/Princesa",
                   size = 4, fontface="italic", family = "BookmanOldStyle", nudge_x = -130, nudge_y = -30, force=0) +
  geom_text_repel(data = as.data.frame(st_coordinates(callao_poi)),
                   fill = "white",
                   col = "black",
                   aes(x = X, y = Y), label = "Callao",
                   size = 4, fontface="italic", family = "BookmanOldStyle", force=0, nudge_y = -30) +
  geom_text_repel(data = as.data.frame(st_coordinates(fuencarral_poi)),
                   fill = "white",
                   col = "black",
                   aes(x = X, y = Y), label = "C/Fuencarral",
                   size = 4, fontface="italic", family = "BookmanOldStyle", nudge_x = 155, force=0) +
  geom_text_repel(data = as.data.frame(st_coordinates(alb_agui_poi)),
                   fill = "white",
                   col = "black",
                   aes(x = X, y = Y), label = "C/Aguilera",
                   size = 4, fontface="italic", family = "BookmanOldStyle", nudge_y = 50, force=0) +
  geom_text_repel(data = as.data.frame(st_coordinates(st_centroid(corte_ingles_bldg))),
                   fill = "white",
                   col = "black",
                   aes(x = X, y = Y), label = "Corte Ingl\u00e9s",
                   size = 4, fontface="italic", family = "BookmanOldStyle", nudge_y = 110, nudge_x = 20, force=0) +
  # set coordinates
  coord_sf(xlim = c(mxmin,mxmax), ylim = c(mymin,mymax), expand = F) +
  # Theme
  theme(
    axis.title = element_blank(),
    axis.text =  element_blank(), 
    axis.ticks= element_blank(),
    axis.ticks.length = unit(0, "pt"),
    legend.box.spacing = unit(0, "mm"),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    plot.margin=grid::unit(c(0, 0, 0, 0), "mm"),
    plot.background = element_rect(fill="green"),
    panel.background = element_rect(fill="white"),
    panel.border = element_blank()
  ) +
  # # Title
  geom_text(data = as.data.frame(list(x = mxmin+(mxmax-mxmin)/2, y = mymin+((s3-10)-(mymin+5))/2)),
            aes(x = x, y = y),
            hjust = "center",
            vjust = "center",
            label = "MALASA\u00d1A",
            family = "Forte", size=16, fontface = "bold", col = "black")

p

# Save plot in PNG
w <- 3500
r <- 400
png(here("out", "malasana.png"), bg = "white", width = w, height = w*asp, res = r, units = "px") 
print(p)
dev.off()