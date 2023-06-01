#' ---
#' title: "Get hex grid"
#' author: "Steffi LaZerte"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     theme: cosmo
#'     toc: true
#'     toc_float:
#'       collapsed: false
#' ---
#'
#' <!--- This style block is to tweak the formatting of the data tables in the report --->
#' <style>
#'   table {
#'     display: block;
#'     overflow: auto;
#'   }
#'
#' </style>

#' R Code to run and save this report:
#+ eval = FALSE
# rmarkdown::render(input = "Scripts/02_initial_data_hex.R",
#                   output_dir = 'Results/',
#                   output_file = paste0('02_initial_data_hex_', Sys.Date(), '.html'),
#                   envir = new.env())

#' # Download/Create hex grid
#'
#' In this script we'll download/filter/acquire spatial hex grid from
#' http://www.discreteglobalgrids.org
#'
#' Apparently they have an R package for accessing their grids:
#' <https://github.com/r-barnes/dggridR>
#'
#' # Setup
#'
#' To get Hex grid
#+ message = FALSE
library(tidyverse)
library(dggridR)
library(sf)
library(lwgeom)
library(rnaturalearth)
library(here)

locs <- ne_countries(continent = c("North America", "South America", "Central America"),
                     returnclass = "sf") %>%
  st_union() %>%
  st_transform("+proj=laea +lat_0=7.6 +lon_0=-97.7") %>%
  st_make_valid()

#' What resolution of hex to get closest to Supp et al. 12,452 km2?
dggs <- dgconstruct(res = 10)
dg_closest_res_to_area(dggs, 49811, round = "nearest") # La Sorte et al. 2016
dg_closest_res_to_area(dggs, 20000, round = "nearest")
dg_closest_res_to_area(dggs, 12452, round = "nearest") # Supp et al.

#' Let's make two, res = 6 and res = 7
dggs <- dgconstruct(res = 6)
grid <- dgrectgrid(dggs, minlat = -45, maxlat = 60, minlon = -135, maxlon = -50, frame = FALSE) %>%
  st_as_sf() %>%
  st_transform(st_crs(locs)) %>%
  st_make_valid() %>%
  st_crop(st_bbox(locs)) %>%
  mutate(hexID = 1:n())

ggplot() +
  geom_sf(data = locs, fill = "pink") +
  geom_sf(data = grid, fill = NA, size = 0.2)

write_rds(grid, here("./Data/Datasets/hex_res6.rds"))

dggs <- dgconstruct(res = 7)
grid <- dgrectgrid(dggs, minlat = -45, maxlat = 60, minlon = -135, maxlon = -50, frame = FALSE) %>%
  st_as_sf() %>%
  st_transform(st_crs(locs)) %>%
  st_make_valid() %>%
  st_crop(st_bbox(locs)) %>%
  mutate(hexID = 1:n())

ggplot() +
  geom_sf(data = locs, fill = "pink") +
  geom_sf(data = grid, fill = NA, size = 0.2)

write_rds(grid, here("Data/Datasets/hex_res7.rds"))


#' ## Create background map
#'
americas <- ne_countries(continent = c("North America", "Central America", "South America"),
                    returnclass = "sf") %>%
  pull(name) %>%
  # Rename US to correctly pull out states
  str_replace("United States", "United States of America") %>%
  ne_states(country = ., returnclass = "sf") %>%
  # Remove Hawaii
  filter(name !="Hawaii") %>%
  group_by(admin) %>%
  st_make_valid() %>%
  summarize() %>%
  st_transform(st_crs(grid)) %>%
  st_make_valid()

write_rds(americas, here("Data/Datasets/americas.rds"))

#+ echo = FALSE
# Reproducible ---------------------------------------------------------------
#' # Reproducible
#+ results = "asis"

print(citation("dggridR"), style = "html")
print(citation("ggplot2"), style = "html")
print(citation("rnaturalearth"), style = "html")

#' ## Session Info
#+ R.options = list(width = 100)
devtools::session_info()
