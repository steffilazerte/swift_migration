#' ---
#' title: "Explore data samples"
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
# rmarkdown::render(input = "Scripts/04_ebird_samples.R",
#                   output_dir = 'Results/',
#                   output_file = paste0('04_ebird_samples_', Sys.Date(), '.html'),
#                   envir = new.env())

#' # Explore data samples
#'
#+ echo = FALSE
# Setup -------------------------------------------------------------------
#' # Setup
#+ message = FALSE
library(tidyverse)
library(sf)
library(here)
library(patchwork)

#+ echo = FALSE
# Load Data -------------------------------------------------------------------
#' ## Load Data

#' Load hex polygons and birds counts

birds <- read_rds(here("Data/Datasets/birds_02_hex_sum.rds"))
hex <- read_rds(here("Data/Datasets/hex_res7.rds"))
americas <- read_rds(here("Data/Datasets/americas.rds"))

#' Create base map that will be the same for each hex figure
base_map <- ggplot() +
  theme_bw() +
  geom_sf(data = americas, fill = "white", colour = "grey", size = 0.2) +
  scale_fill_viridis_c(option = "inferno", direction = -1, na.value = NA)


#+ echo = FALSE
# Checklists samples ------------------------------------------------------
#' # Checklist samples

#' > **Spatial bias**: most participants in citizen science surveys sample near their homes (Luck et al. 2004), in easily accessible areas such as roadsides (Kadmon, Farber, and Danin 2004), or in areas and habitats of known high biodiversity (Prendergast et al. 1993). A simple method to reduce the spatial bias is to create an equal area grid over the region of interest, and sample a given number of checklists from within each grid cell.
#'
#' > **Temporal bias**: participants preferentially sample when they are available, such as weekends (Courter et al. 2013), and at times of year when they expect to observe more birds, notably during spring migration (Sullivan et al. 2014). To address the weekend bias, we recommend using a temporal scale of a week or multiple weeks for most analyses.
#'
#' For our purposes we want to remove outlier observations... but nothing more drastic.
#'
#' **Identifying outlier observations**
#'
#' For chimney swifts, this is pretty much just observations during the winter. These observations are usually in the northern hemisphere because there is poor coverage in South America. Therefore these observations have a disproportionately large effect.
#'
#' > Limit chimney swift observations by date:

limits <- tibble(species = "Chimney Swift",
                 species_year = paste0(2009:2018, " - Chimney Swift"),
                 yday_min = 75,
                 yday_max = 300)

#+ echo = FALSE
# Prep Data ---------------------------------------------------------------
#' # Prep Data
birds_daily <- birds %>%
  filter(!is.na(observation_date)) %>%
  group_by(species, year, yday, observation_date) %>%
  summarize(total_checklists = sum(total_checklists),
            total_obs = sum(total_obs),
            presence = total_obs > 0) %>%
  mutate(species_year = paste0(year, " - ", species))

n <- birds_daily %>%
  group_by(species_year) %>%
  summarize(n = sum(presence))

samples <- birds %>%
  filter(!is.na(observation_date)) %>%
  group_by(species, year) %>%
  summarize(total = sum(total_checklists),
            presence = sum(total_obs),
            prop = presence/total)

#' Checklists per year (total) checklists with an observation per year (presence)
samples

write_csv(samples, "Results/samples.csv")


samples_hex <- birds %>%
  filter(!is.na(observation_date), total_checklists > 0) %>%
  group_by(species, hexID) %>%
  summarize(total = sum(total_checklists),
            presence = sum(hexID[total_obs > 0])) %>%
  group_by(species) %>%
  summarize(total = sum(hexID),
            presence = sum(presence>0),
            prop = presence/total)

#' Hexes with a checklist vs hexes with an observation
samples_hex

#' Summarize counts as monthly totals for each species in each year
birds_monthly <- birds %>%
  filter(!is.na(observation_date)) %>%
  # Filter out Chimney Swift outliers
  filter(!(species == "Chimney Swift" &
             yday < limits$yday_min[1] &
             yday > limits$yday_max[1])) %>%
  mutate(prop_obs = total_obs/total_checklists) %>%
  group_by(hexID, species, year, month) %>%
  summarize(total_checklists = sum(total_checklists),
            total_obs = sum(total_obs),
            prop_obs = total_obs/total_checklists) %>%
  ungroup() %>%
  # Set zeros as NA, so that we can clearly differentiate those observations
  # in the colour scale
  mutate(total_obs = if_else(total_obs == 0, as.integer(NA), total_obs),
         prop_obs = if_else(prop_obs == 0, as.numeric(NA), prop_obs),
         month = factor(month.abb[month], levels = month.abb)) %>%
  # Now join to spatial hex data
  left_join(hex, ., by = "hexID")

#' Summarize counts as monthly totals for each species in each year
birds_yearly <- birds %>%
  filter(!is.na(observation_date)) %>%
  # Filter out Chimney Swift outliers
  filter(!(species == "Chimney Swift" &
             yday < limits$yday_min[1] &
             yday > limits$yday_max[1])) %>%
  mutate(prop_obs = total_obs/total_checklists) %>%
  group_by(hexID, species, year) %>%
  summarize(total_checklists = sum(total_checklists),
            total_obs = sum(total_obs),
            prop_obs = total_obs/total_checklists) %>%
  ungroup() %>%
  # Set zeros as NA, so that we can clearly differentiate those observations
  # in the colour scale
  mutate(total_obs = if_else(total_obs == 0, as.integer(NA), total_obs),
         prop_obs = if_else(prop_obs == 0, as.numeric(NA), prop_obs)) %>%
  # Now join to spatial hex data
  left_join(hex, ., by = "hexID")

birds_total <- birds_yearly %>%
  group_by(species, hexID) %>%
  summarize(total_checklists = sum(total_checklists, na.rm = TRUE),
            total_obs = sum(total_obs, na.rm = TRUE)) %>%
  # Set zeros as NA, so that we can clearly differentiate those observations
  # in the colour scale
  mutate(total_obs = if_else(total_obs == 0, as.integer(NA), total_obs))


#+ echo = FALSE
# Plot Checklists ------------------------------------------------------------
#' # Plot Checklists
#'
#' ## Checklists per day-of-year
#'
#' - These show the number of checklists with an individual observed
#' - Note that pink means NO individuals observed
#' - Note that the Chimney Swift data hasn't been limited yet, but the black lines show where the limits will be

#+ fig.asp = 0.5
ggplot(data = birds_daily,
       aes(x = yday, y = total_obs)) +
  theme_bw() +
  geom_point(aes(colour = presence)) +
  stat_smooth() +
  geom_vline(data = limits, aes(xintercept = yday_min)) +
  geom_vline(data = limits, aes(xintercept = yday_max)) +
  facet_wrap(~species, scales = "free_y") +
  labs(subtitle = "Black lines show Chimney swift limits")

#+ fig.asp = 3, fig.width = 12
ggplot(data = birds_daily,
       aes(x = yday, y = total_obs)) +
  theme_bw() +
  geom_point(aes(colour = presence)) +
  geom_vline(data = limits, aes(xintercept = yday_min)) +
  geom_vline(data = limits, aes(xintercept = yday_max)) +
  geom_text(data = n,
            x = -Inf, y = Inf, hjust = 0, vjust = 1,
            aes(label = paste0("n = ", n))) +
  facet_wrap(~ species_year, scales = "free_y", ncol = 2) +
  labs(subtitle = "Black lines show Chimney swift limits")


#+ echo = FALSE
# Plot Yearly Hex ----------------------------------------------

#' # Yearly Hex map
#+ fig.asp = 0.2, fig.width = 20
base_map +
  facet_grid( ~ year) +
  geom_sf(data = filter(birds_yearly, !is.na(total_checklists), species == "Chimney Swift"),
          aes(fill = total_obs), size = 0.1) +
  labs(title = "Chimney Swift - Filtered by date limits",
       subtitle = "Hexes have at least 1 checklists for that year (with/without birds); white hexes have no checklists with swifts",
       fill = "Total no. checklists\nwith a bird")

base_map +
  facet_grid( ~ year) +
  geom_sf(data = filter(birds_yearly, !is.na(total_checklists), species == "Vaux's Swift"),
          aes(fill = total_obs), size = 0.1) +
  labs(title = "Vaux's Swift",
       subtitle = "Hexes have at least 1 checklists for that year (with/without birds); white hexes have no checklists with swifts",
       fill = "Total no. checklists\nwith a bird")


#+ echo = FALSE
# By Species overall ------------------------------------------------------
#' # Swifts overall
g1 <- base_map +
  geom_sf(data = filter(birds_total, !is.na(total_checklists),
                        species == "Chimney Swift"),
          aes(fill = total_obs), size = 0.1) +
  labs(title = "Chimney Swifts - Filtered",
       fill = "Total no. checklists\nwith a swift")

g2 <- base_map +
  geom_sf(data = filter(birds_total, !is.na(total_checklists),
                        species == "Vaux's Swift"),
          aes(fill = total_obs), size = 0.1) +
  labs(title = "Vaux's Swifts",
       fill = "Total no. checklists\nwith a swift")

#+ fig.width = 12, fig.asp = 0.3
g1 + g2 + plot_annotation(
  caption = "Hexes have at least 1 checklist from 2009 to 2018 (with/without swifts); white hexes have no checklists with swifts"
)


#+ echo = FALSE
# Reproducible ---------------------------------------------------------------
#' # Reproducible

#' ## Session Info
#+ R.options = list(width = 100)
devtools::session_info()
