#' ---
#' title: "Migration Distances and Speed"
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
# rmarkdown::render(input = "Scripts/07_migration_dist_speed.R",
#                   output_dir = 'Results/',
#                   output_file = paste0('07_migration_dist_speed_', Sys.Date(), '.html'),
#                   envir = new.env())

#'
#+ echo = FALSE
# Setup -------------------------------------------------------------------
#' # Setup
#+ message = FALSE
library(tidyverse)
library(here)
library(geosphere)

#+ echo = FALSE
# Load Data -------------------------------------------------------------------
#' ## Load Data

migration <- read_csv(here("Data/Datasets/birds_04_migration.csv"), guess_max = 20000)
details <- read_csv(here("Data/Datasets/migration_details.csv")) %>%
  select(-contains("speed")) # Remove values from previous runs

#+ echo = FALSE
# Distance and Speed-------------------------------------------------------------------
#' ## Distance

#' Custom function to pull out distances from a data frame
get_dist <- function(df) {
  df <- as.matrix(df[, c("pred_lon", "pred_lat")])
  dist <- distVincentyEllipsoid(df)
  # Each dist is to previous point
  # Add an NA at the beginning, because don't know the distance
  c(NA, dist)
}

#' Calculate Great Circle distance assuming elliptical earth (accurate)
dist <- migration %>%
  nest(data = c(-species, -year)) %>%
  # Use custom function to get distance
  mutate(dist = map(data, ~get_dist(.))) %>%
  unnest(c(data, dist)) %>%
  mutate(dist = dist/1000, # Convert to kilometres
         season = factor(season, levels = c("non-breeding", "spring", "breeding", "fall")),
         season2 = factor(season2, levels = c("spring", "breeding", "fall")))

#' ## Speed
#' Calculate median of 5 fastest speeds per species, season, year
#'
#' Seasons:
#'
#' - Spring: New Year to End of Spring Migration
#' - Breeding: End of Spring Migration to Start of Fall Migration
#' - Fall: Start of Fall Migration to New Year
#'
#' Note that Spring and Fall both include non-breeding period movements. However,
#' it is expected that movements during migration will be much faster (farther
#' distances per day). Therefore the median of the top 5 distances should all
#' fall during the migration periods
#'
#' However, there's something weird with Western Blubirds in 2015. The population
#' centroid moved longitudinally in December. So let's filter out december migration dates.
#'
#' See plots at bottom of distance vs. yday

speed <- dist %>%
  group_by(species, year, season2) %>%
  summarize(daily_speed = median(sort(dist[yday < 340], decreasing = TRUE)[1:5]),
            timing_speed = median(yday[order(dist[yday < 340], decreasing = TRUE)[1:5]])) %>%
  ungroup()

#' ## Visual Calculation of speed
#'
#' - Boxplots show distances per day
#' - Red dot is median of top 5 distances, i.e. speed for that season
#'


#+ fig.width = 12, fig.asp = 0.5
ggplot(data = dist, aes(x = season2, y = dist)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  geom_boxplot() +
  geom_point(data = speed,
             aes(y = daily_speed, colour = "Calculated Speed")) +
  facet_grid(species ~ year, scales = "free") +
  scale_colour_manual(name = "", values = "red") +
  labs(y = "Daily Distance (km) / Speed (km/day)",
       x = "Season")


#+ echo = FALSE
# Check time of speed ----------------------------------------------------------
#' ## Check timing of speed

#+ fig.width = 12, fig.asp = 0.5
ggplot(data = dist, aes(x = yday, y = dist)) +
  geom_line() +
  facet_grid(species ~ year, scales = "free") +
  geom_point(data = filter(speed, season2 != "breeding"),
             aes(x = timing_speed, y = daily_speed), colour = "red", size = 3)

#+ echo = FALSE
# Add speed to details ----------------------------------------------------------
#' ## Add speed to details
#'
all <- speed %>%
  select(-timing_speed) %>%
  mutate(season2 = paste0("speed_", season2)) %>%
  pivot_wider(names_from = season2, values_from = daily_speed) %>%
  left_join(details, by = c("species", "year"))

write_csv(all, here("Data/Datasets/migration_details.csv"))

#+ echo = FALSE
# Reproducible ---------------------------------------------------------------
#' # Reproducible

#' ## Session Info
#+ R.options = list(width = 100)
devtools::session_info()
