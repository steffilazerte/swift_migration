#' ---
#' title: "Publication Figures"
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
#'
#' R Code to run and save this report:
#+ eval = FALSE
# rmarkdown::render(input = "Scripts/09_figures.R",
#                   output_dir = 'Results/',
#                   output_file = paste0('09_figures_', Sys.Date(), '.html'),
#                   envir = new.env())
#'
#'
#+ echo = FALSE
# Setup -------------------------------------------------------------------
#' # Setup
#+ message = FALSE
library(tidyverse)
library(sf)
library(patchwork) # For combining figures
library(here)
library(rnaturalearth)

#+ echo = FALSE
# Load Data ---------------------------------------------------------------
#' Load data
birds_predicted <- read_csv(here("Data/Datasets/birds_03_mean.csv")) |> filter(type == "swift")
migration_details <- read_csv(here("Data/Datasets/migration_details.csv")) |> filter(type == "swift")
migration <- read_csv(here("Data/Datasets/birds_04_migration.csv")) |> filter(type == "swift")

americas <- read_rds(here("Data/Datasets/americas.rds"))
map_limits <- read_rds(here("Data/Datasets/species_bbox.rds"))[c(1,4)]

#+ echo = FALSE
# Figures ---------------------------------------------------------------
#' # Figures

#+ echo = FALSE
## Figure 1 - Combo Map ---------------------------------
#' ## Figure 1 - Combo Map

#' ### Summarize for plotting
plot_dates <- migration_details %>%
  select(species, year, spring_begin_yday, spring_end_yday, fall_begin_yday,
         fall_end_yday) %>%
  pivot_longer(names_to = "event", values_to = "yday", cols = c(-species, -year)) %>%
  mutate(event = factor(
    event,
    levels = c("spring_begin_yday", "spring_end_yday",
               "fall_begin_yday", "fall_end_yday"),
    labels = c("Spring start",
               "Spring end",
               "Fall start",
               "Fall end")))

p <- birds_predicted %>%
  select(-contains("sdl"), -contains("_se"), -numcells, -numobs, -numlists) %>%
  gather(type, value, center_lon, center_lat, pred_lon, pred_lat) %>%
  separate(type, into = c("type", "coord"), sep = "_", remove = TRUE) %>%
  spread(coord, value) %>%
  arrange(species, type, year, yday) %>%
  filter(!is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = st_crs(americas)) %>%
  group_by(species, type, year, month) %>%
  summarize(n = n(), do_union = FALSE) %>%
  ungroup() %>%
  rename(Month = month)


#' ### Bounding boxes and geo types

cs <- st_bbox(c(xmin = -135, xmax = -65, ymin = 5, ymax = 60), crs = st_crs(4326)) %>%
  st_as_sfc() %>%
  st_transform(st_crs(p)) %>%
  st_bbox()

vs <- st_bbox(c(xmin = -135, xmax = -65, ymin = 5, ymax = 60), crs = st_crs(4326)) %>%
  st_as_sfc() %>%
  st_transform(st_crs(p)) %>%
  st_bbox()

p$geometry[p$type == "pred"] <- st_cast(p$geometry[p$type == "pred"], "LINESTRING")

p <- mutate(p, type = factor(type, labels = c("Average location", "GAM predicted location")))

#' ### Panels
#'
g1 <- filter(p, species == "Chimney Swift") %>%
  ggplot() +
  geom_sf(data = st_crop(americas, cs), fill = "white") +
  geom_sf(aes(colour = year, size = type, linewidth = type, alpha = factor(year))) +
  scale_color_gradientn(name = "Year", colours = fields::tim.colors(12)) +
  facet_wrap(~ type) +
  scale_size_manual(values = c("Average location" = 1.5, "GAM predicted location" = 1), guide = "none") +
  scale_linewidth_manual(values = c("Average location" = 1.5, "GAM predicted location" = 1), guide = "none") +
  scale_alpha_manual(values = seq(1,0.5, length.out = 10), guide = "none") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(name = "Chimney Swift", expand = c(0.001, 0.001))

g2 <- filter(p, species == "Vaux's Swift") %>%
  ggplot() +
  geom_sf(data = st_crop(americas, vs), fill = "white") +
  geom_sf(aes(colour = year, size = type, linewidth = type, alpha = factor(year))) +
  scale_color_gradientn(name = "Year", colours = fields::tim.colors(12)) +
  facet_wrap(~ type)+
  scale_size_manual(values = c("Average location" = 1.5, "GAM predicted location" = 1), guide = "none") +
  scale_linewidth_manual(values = c("Average location" = 1.5, "GAM predicted location" = 1), guide = "none") +
  scale_alpha_manual(values = seq(1, 0.5, length.out = 10), guide = "none") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(name = "Vaux's Swift", expand = c(0.001, 0.001))

#' ### Combine
g_map <- g1 / g2  &
  theme(legend.title = element_text(vjust = 0.7),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)))

g_map

ggsave("Results/Figures/figure1.png", plot = g_map, dpi = 1000, width = 9, height = 9)

# Because Chimney Swift data during the non-breeding period was too sparse,
# we omitted ordinal dates below 75 and above 300 for this species, prior to modelling.

