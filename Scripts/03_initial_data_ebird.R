#' ---
#' title: "Get e-bird data by hex"
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
# rmarkdown::render(input = "Scripts/03_initial_data_ebird.R",
#                   output_dir = 'Results/',
#                   output_file = paste0('03_initial_data_ebird_', Sys.Date(), '.html'),
#                   envir = new.env())

#' # Download all large data sets
#'
#' In this script we'll download/filter/acquire large ebird datasets
#'
#' This script will take time to run!
#'
#+ echo = FALSE
# Setup -------------------------------------------------------------------
#' # Setup

library(auk)
library(tidyverse)
library(lubridate)
library(sf)
library(here)

#' # Filter ebird data
#'
#' - E-bird data must be requested [here](https://ebird.org/data/download/)
#'     - I made 2 special requests: Chimney swifts, Vaux's swifts
#'     - Once the request is approved you'll get an email and can download the data
#'      (for me, it took between 2 hours and 5 days)
#' - You'll also need to download the complete checklist data (currently not available as a special request)
#' - Extract all the files (including the internal compressed files) into the same folder
#'   - You'll have to move the 3 main files into the same folder after they've been extracted
#'     (e.g., ebd_sampling_relOct-2019.txt and each species file, like ebd_chiswi_relOct-2019.txt)
#' - Tell `auk` where that folder is (below) and restart R.

ebird_files <- "/media/steffi/7faab516-92aa-457e-a16b-b33c5b1a3c40/ebird/"
#auk_set_ebd_path(path = ebird_files, overwrite = TRUE)
americas <- read_rds(here("Data/Datasets/americas.rds"))

#+ echo = FALSE
# Best Practices --------------------------------------------------------------
#' # Best Practices
#' - Cite ebird and ebird data: <https://ebird.org/science/citation>
#'   Sullivan, B.L., C.L. Wood, M.J. Iliff, R.E. Bonney, D. Fink, and S. Kelling. 2009. eBird: a citizen-based bird observation network in the biological sciences. Biological Conservation 142: 2282-2292.
#'
#'   eBird Basic Dataset. Versions: ebd_easblu_200501_201901_relNov-2019, ebd_moublu_200501_201901_relNov-2019, ebd_wesblu_200501_201901_relNov-2019. Cornell Lab of Ornithology, Ithaca, New York. Nov 2019.
#'
#' - [E-bird Data Best Practices](https://cornelllabofornithology.github.io/ebird-best-practices/) suggests keeping only standard protocols of "traveling" and "stationary counts.
#'
#'
#' ## Species one at a time
#'
#' - Using species-specific data sets provided by ebird
#' - Using full sampling data set downloaded from ebird
#' - This is **MUCH** faster than using the complete observation dataset and
#'   also results in smaller files which makes it easier to work with in R
#'
#'   List all the data files.
#'   **NOTE:** These are the names of MY files, if you redownload the data you
#'   will have different dates and possibly different date ranges
#'   (note that I restricted the dates of the files from Jan 2005 to Jan 2019).
#'   Make sure these files names match the names of your files.

list.files(ebird_files, pattern = "relNov-2019.txt")

for(s in c("chiswi", "vauswi")) {
  filters <- auk_ebd(paste0("ebd_", s, "_200501_201901_relNov-2019.txt"),
                     file_sampling = "ebd_sampling_relNov-2019.txt") %>%
    # Keep only standard protocols (Best Practices)
    auk_protocol(protocol = c("Stationary", "Traveling")) %>%
    # Restrict to suggested limits (Best Practices)
    auk_duration(c(0, 60*5)) %>%
    auk_distance(distance = c(0, 5)) %>%
    # Complete data (for getting a proxy of effort)
    auk_complete()

  #' Specific filters by year
  #' - Restrict to last 10 years (2019 isn't a complete year)
  for(y in 2009:2018) {
    if(!file.exists(here(paste0("./Data/Intermediate/", s, "_", y, "_obs.txt")))) {
      filters <- auk_date(filters, date = c(paste0(y, "-01-01"), paste0(y, "-12-31")))

      #' Run the filters and save the output
      auk_filter(filters,
                 file = here(paste0("./Data/Intermediate/", s, "_", y, "_obs.txt")),
                 file_sampling = here(paste0("./Data/Intermediate/", y, "_checklists.txt")),
                 overwrite = TRUE) # Only need to keep one copy of sampling per year
    }
  }
}

#+ echo = FALSE
# Summarize by hex -------------------------------------------------------------
#' ## Summarize data by hex grid
#'
#' - Get counts of checklists for each data for each hex
#' - n checklists total
#' - n checklists with a bird detected
#'
#' Also need to:
#' - roll up species (i.e. no subspecies)
#' - de-duplicate group checklists (Also a la Supp et al.)
#' - **This takes time to run!** So it might be faster to just read in the .rds later

#' Hex grid
hex <- read_rds(here("./Data/Datasets/hex_res7.rds"))

info <- data.frame()
for(s in c("chiswi", "vauswi")) {
  for(y in 2009:2018) {
    if(!file.exists(here(paste0("Data/Intermediate/", s, "_", y, "_hex_res7.rds")))) {
      auk_zerofill(here(paste0("Data/Intermediate/", s, "_", y, "_obs.txt")),
                   here(paste0("Data/Intermediate/", y, "_checklists.txt")),
                   collapse = TRUE) %>%
        mutate(effort_distance_km = replace_na(effort_distance_km, 0)) %>%
        filter(effort_distance_km <= 5) %>%
        select(checklist_id, scientific_name, latitude, longitude,
               observation_date, species_observed) %>%
        st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
        st_transform(st_crs(hex)) %>%
        st_join(hex, .) %>%
        group_by(observation_date, hexID) %>%
        summarize(total_checklists = sum(!is.na(observation_date)),
                  total_obs = sum(species_observed, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(species = case_when(s == "chiswi" ~ "Chimney Swift",
                                   s == "vauswi" ~ "Vaux's Swift")) %>%
        write_rds(here(paste0("Data/Intermediate/", s, "_", y, "_hex_res7.rds")))

    }
  }
}

#+ echo = FALSE
# Bind hex -----------------------------------------------------------------
#' ## Bind hex files together
#' Load and bind all hex files together for later

#' Get hex file names
hex_files <- list.files(here("Data/Intermediate"),
                        pattern = "_hex_res7.rds",
                        full.names = TRUE)

#' Load and bind together into a single file
birds <- do.call("rbind", lapply(hex_files, read_rds)) %>%
  mutate(year = year(observation_date),
         month = month(observation_date),
         week = week(observation_date),
         yday = yday(observation_date))

#' Save as geo file (for sharing) and rds (for speed in loading)
#st_write(birds, here("./Data/Datasets/birds_01_hex_all.gpkg"))
write_rds(birds, here("./Data/Datasets/birds_01_hex_all.rds"))

birds <- read_rds(here("./Data/Datasets/birds_01_hex_all.rds"))

#' Get with hex centroids as lat/lon in non-spatial data file
hex_centroid <- hex %>%
  st_centroid() %>%
  st_transform(crs = 4326) %>%
  st_coordinates() %>%
  as.data.frame() %>%
  cbind(st_set_geometry(hex, NULL)) %>%
  rename(lon = X, lat = Y)

birds_centroid <- birds %>%
  st_set_geometry(NULL) %>%
  left_join(hex_centroid, by = "hexID")

#' Save as csv (for sharing) and rds (for speed in loading)
#write_csv(birds_centroid, here("./Data/Datasets/birds_02_hex_sum.csv"))
write_rds(birds_centroid, here("Data/Datasets/birds_02_hex_sum.rds"))


#+ echo = FALSE
# Bounding box for each species -------------------------------------------
#' ## Bounding Boxes
map_limits <- birds_centroid %>%
  filter(!(species == "Chimney Swift" & (yday < 75 | yday > 300))) %>%
  filter(total_obs > 0) %>%
  group_by(species) %>%
  summarize(xmin = min(lon), ymin = min(lat),
            xmax = max(lon), ymax = max(lat)) %>%
  gather(type, value, -species) %>%
  separate(type, into = c("loc", "type"), sep = 1) %>%
  spread(loc, value) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  st_transform(crs = st_crs(americas)) %>%
  group_by(species) %>%
  summarize(do_union = FALSE)

map_limits <- map(map_limits$geometry, st_bbox) %>%
  set_names(unique(birds_centroid$species))
write_rds(map_limits, here("./Data/Datasets/species_bbox.rds"))

#+ echo = FALSE
# Reproducible ---------------------------------------------------------------
#' # Reproducible

#' ## Session Info
#+ R.options = list(width = 100)
devtools::session_info()

