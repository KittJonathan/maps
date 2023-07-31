# City maps
# 2023-07-31

# Links ----

# http://estebanmoro.org/post/2020-10-19-personal-art-map-with-r/
# https://ggplot2tutor.com/tutorials/streetmaps

# Load packages ----

library(sf)
library(tidyverse)
library(osmdata)

# Map of Yzeure ----

yzeure_points <- tibble(
  address = c("22 rue Clara Malraux",
              "3 rue Flora Tristan"),
  x = c(3.35741,
        3.36091),
  y = c(46.57196,
        46.55975)
)

yzeure <- osmdata::opq(bbox = c(3.30, 46.54, 3.40, 46.60)) |>
  osmdata::add_osm_feature(key = "highway") |>
  osmdata::osmdata_sf()

p <- ggplot() +
  geom_sf(data = yzeure$osm_lines,
          size = 0.1,
          alpha = 0.1,
          colour = "#b3cde0") +
  geom_point(data = yzeure_points,
             aes(x, y),
             colour = "#005b96", size = 5) +
  xlim(c(3.30, 3.40)) +
  ylim(c(46.54, 46.60)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#011f4b"))

ggsave("../maps/figs/yzeure.png", p, dpi = 320, width = 12, height = 6)

big_streets <- getbb("Asheville United States")%>%
  opq()

uk <- opq_osm_id (id = 62149) %>%
  opq_string () %>%
  osmdata_sf ()

ggplot() +
  geom_sf(data = uk$osm_lines,
          size = 0.4,
          alpha = 0.65) +
  theme_void()

# Glenrothes map ----

bbx <- sf::getbb("Glenrothes")

highways <- bbx |>
  opq() |>
  add_osm_feature(key = "highway") |>
  osmdata_sf()

ggplot() +
  geom_sf(data = highways$osm_lines,
          aes(colour = highway),
          size = 0.4,
          alpha = 0.65) +
  theme_void()

# Le Havre map ----

bbx <- getbb("Le Havre") |>
  opq()

lh_points <- tibble(
  address = c("6 rue Pierre Faure",
              "465 rue de Verdun"),
  x = c(0.10368,
        0.18170),
  y = c(49.49590,
        49.50245)
)

highways <- bbx |>
  opq() |>
  add_osm_feature(key = "highway",
                  value = c("motorway", "trunk", "primary", "secondary",
                            "tertiary", "motorway_link", "trunk_link",
                            "primary_link", "secondary_link", "tertiary_link")) |>
  osmdata_sf()

streets <- bbx |>
  opq() |>
  add_osm_feature(key = "highway",
                  value = c("residential", "living_street",
                            "service", "unclassified",
                            "pedestrian", "footway", "track", "path")) |>
  osmdata_sf()

# highways <- bbx |>
#   opq() |>
#   add_osm_feature(key = "highway") |>
#   osmdata_sf()

lh <- ggplot() +
  # geom_sf(data = highways$osm_lines,
  #         size = 0.4,
  #         alpha = 0.65) +
  geom_sf(data = streets$osm_lines,
          size = 0.4,
          alpha = 0.65) +
  geom_point(data = lh_points,
             aes(x = x, y = y),
             colour = "red") +
  theme_void() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

ggsave("../maps/maps/lh.png", lh, dpi = 320, width = 12, height = 6)

# Villefranche-sur-Saône map ----

bbx <- getbb("Villefranche-sur-Saône")

vilf_points <- tibble(
  address = "1184 route de Riottier",
  x = 4.73369,
  y = 45.97959
)

highways <- bbx |>
  opq() |>
  add_osm_feature(key = "highway",
                  value = c("motorway", "trunk", "primary", "secondary",
                            "tertiary", "motorway_link", "trunk_link",
                            "primary_link", "secondary_link", "tertiary_link")) |>
  osmdata_sf()

streets <- bbx |>
  opq() |>
  add_osm_feature(key = "highway",
                  value = c("residential", "living_street",
                            "service", "unclassified",
                            "pedestrian", "footway", "track", "path")) |>
  osmdata_sf()

vilf <- ggplot() +
  geom_sf(data = highways$osm_lines,
          size = 0.4,
          alpha = 0.65) +
  geom_sf(data = streets$osm_lines,
          size = 0.4,
          alpha = 0.65) +
  geom_point(data = vilf_points,
             aes(x = x, y = y),
             colour = "red") +
  theme_void() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

ggsave("../maps/maps/vilf.png", vilf, dpi = 320, width = 12, height = 6)
