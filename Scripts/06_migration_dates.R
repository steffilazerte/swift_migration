#' ---
#' title: "Estimate migration dates"
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
# rmarkdown::render(input = "Scripts/06_migration_dates.R",
#                   output_dir = 'Results/',
#                   output_file = paste0('06_migration_dates_', Sys.Date(), '.html'),
#                   envir = new.env())
#'
#'
#+ echo = FALSE
# Setup -------------------------------------------------------------------
#' # Setup
library(tidyverse)
library(patchwork) # for combining figures
library(here)
#library(segmented) # For conducting segmented regression to get timing
# Use segmented::segmented, because otherwise loads MASS which conflicts with
# dplyr::select()

#+ echo = FALSE
# Load Data -------------------------------------------------------------------
#' ## Load Data

#' Load hex polygons and birds count
birds <- read_csv(here("Data/Datasets/birds_03_mean.csv"))
hex <- read_rds(here("Data/Datasets/hex_res7.rds"))

#+ echo = FALSE
# Estimate Dates ----------------------------------------------------------
#' # Estimate Dates
#'
#' ## Background
#' For each species and each year use GAM to predict timing from weighted
#' lat/lon averages GAM model was already create in previous step
#'
#'
#' 1. **Threshold Latitudes**
#'   Dates are defined by first date the bulk GAM data crosses a threshold lat
#'     - **Northern-centric** End of Spring and Start of Fall
#'       - Upper latitude threshold (use northern, summer, breeding lats)
#'    - **Southern-centric** Start of Spring and End of Fall
#'       - Lower latitude threshold (use southern, winter, non-breeding lats)
#'
#' 2. **Segmented Regression**
#'   Fine-tune the exact date selection with segmented regression (see below)
#'
#' ### 1. Defining the threshold latitude
#'
#' **For Southern-centric (i.e. start of spring, end of fall):**
#'
#' **Which seems to be the 'normal' method for calculating migration dates**
#'
#' > *From La Sorte et al. 2013 Ecology:*
#'
#' > We used the MINIMUM of the UPPER limit from the 99% confidence band of the
#' > predicted daily occurrence as a threshold to define WINTER (NON-BREEDING)
#' > season occurrence before SPRING (NORTHERN) migration and WINTER(NON-BREEDING)
#' > season occurrences after AUTUMN (SOUTHERN) migration.
#'
#' **For Northern-centric (i.e. end of spring, start of fall):**
#'
#' Use the MAXIMUM of the LOWER limit from the 99% confidence
#' band of the predicted daily occurence as a threshold to define BREEDING
#' seasonal occurrence before SOUTHERN migration and BREEDING seasonal
#' occurrences after NORTHERN migration (for EACH YEAR).
#'
#'
#' ### 2. Segmented Regression
#'
#' #### Problems
#'
#' - Movement during the breeding season is a bit more bumpy than during the non-breeding season
#' - **What do we consider the start/end of Migration?**
#' - Should we use the two humps at the start and end of the breeding season?
#' - What if there are no little humps?
#'
#' #### Solutions
#'
#' In Supp et al., they reference use of the segmented regression approach to
#' further fine-tune the analysis.
#'
#' If we use this here, and limit the range to be the date the threshold is
#' reached, +/- 60 days, the segmented regression will find the 'best' break
#' point in that range, which fortuitously, generally looks good.
#'
#' Importantly, this a) matches a visual inspection of when migration
#' ends/begins, but also, is systematic and relies on an algorithm and the data,
#' not us eyeballing things :)
#'
#'
#' ## Calculate Latitudinal Thresholds
#'
#' Establish what latitudes the populations are at right after the southward
#' migration and right before the northward migration
#'
#' Assume we consider the following dates for each period
spring_begin <- c(1, 80)
fall_end <- c(285, 345)

spring_end <- c(80, 175)
fall_begin <- c(225, 285)


#' Calculate the thresholds for each season/year/species

birds <- birds %>%
  # Add in missing Chimney Swift dates as NAs (needed for indexing, below)
  complete(nesting(species), nesting(year, yday)) %>%
  group_by(species, year) %>%
  mutate(ci99_upper = pred_lat + pred_lat_se * 2.58,
         ci99_lower = pred_lat - pred_lat_se * 2.58,
         spring_begin_thresh = min(ci99_upper[yday > spring_begin[1] &
                                                yday < spring_begin[2]], na.rm = TRUE),
         fall_end_thresh = min(ci99_upper[yday > fall_end[1] &
                                            yday < fall_end[2]], na.rm = TRUE),
         spring_end_thresh = max(ci99_lower[yday > spring_end[1] &
                                              yday < spring_end[2]], na.rm = TRUE),
         fall_begin_thresh = max(ci99_lower[yday > fall_begin[1] &
                                              yday < fall_begin[2]], na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(spring_begin_thresh = if_else(species == "Chimney Swift", as.numeric(NA), spring_begin_thresh),
         fall_end_thresh = if_else(species == "Chimney Swift", as.numeric(NA), fall_end_thresh))

limits <- cbind(spring_begin, fall_end, spring_end, fall_begin) %>%
  as_tibble() %>%
  mutate(range = c("lim1", "lim2")) %>%
  pivot_longer(cols = -range, names_to = "type", values_to = "lim") %>%
  mutate(season = str_extract(type, "spring|fall"),
         time = str_extract(type, "begin|end")) %>%
  pivot_wider(names_from = "range", values_from = "lim")

thresh_plot <- birds %>%
  select(species, year, contains("thresh")) %>%
  distinct() %>%
  pivot_longer(cols = contains("thresh"), names_to = "type", values_to = "thresh") %>%
  mutate(season = str_extract(type, "spring|fall"),
         time = str_extract(type, "begin|end")) %>%
  left_join(limits, by = c("season", "time")) %>%
  mutate(type = paste0(season, "-", time))

g <- list()
for(s in c("Chimney Swift", "Vaux's Swift")) {
  g[[s]] <- ggplot(data = filter(birds, species == s),
                   aes(x = yday, y = pred_lat)) +
    theme_bw() +
    geom_rect(data = filter(thresh_plot, species == s),
              aes(xmin = lim1, xmax = lim2, group = type, x = NULL, y = NULL),
              ymin = -Inf, ymax = +Inf,
              fill = "blue", alpha = 0.2, colour = "black") +
    geom_point(aes(y = center_lat), shape = 21, colour = "grey40",
               fill = "grey70", na.rm = TRUE) +
    geom_ribbon(aes(ymin = pred_lat - 2.58 * pred_lat_se,
                    ymax = pred_lat + 2.58 * pred_lat_se,
                    fill = "Predicted Latitude\nfrom GAM (99% CI)",
                    group = year),
                alpha = 0.5, colour = "black") +
    geom_line() +
    geom_segment(data = filter(thresh_plot, species == s),
                 aes(y = thresh, yend = thresh, colour = type,
                     x = lim1, xend = lim2), size = 1.5) +
    facet_grid(year ~ ., scales = "free_y") +
    scale_colour_manual(name = "Latitude Thresholds",
                        values = c("orange", "orange", "cadetblue", "cadetblue")) +
    labs(title = s)
}

#' ### Visual of Thresholds Calculations
#'
#' This figure is only so we have a visual representation of how the thresholds
#' are being assessed
#'
#' > - Pink ribbon = 99% Confidence interval of latitudes predicted from GAM model
#' > - Black lines in the ribbon are the upper limit and lower limit, the middle
#' line is the predicted latitude (from GAM model)
#' > - Transparent blue rectangles indicate the date ranges used to establish
#' the latitudes just after and just before migration
#' (i.e. begining *and* ending of spring migration).
#' > - Blue horizontal lines represents the latitude threshold for spring migration (begin/end)
#' > - Orange horzontal lines represents the latitude threshold for fall migration (begin/end)
#'
#' Right-click > Show/view imgage to get a closer look
#'
#' > **Note:** Chimney Swifts don't have a spring-begin or a fall-end threshold
#'
#+ fig.asp = 1, fig.width = 15
wrap_plots(g) + plot_layout(guides = "collect", nrow = 1)


#' ## Getting Dates from Thresholds
#'
#' - First use threshold to get approximate date of spring and fall departures
#'   and arrivals
#' - Next use segmented regression to get breakpoint of relationship between lat
#'   and yday, using approximate date as starting point. This is the official date
#'   of migration that we'll use


birds <- nest(birds, data = c(-species, -year))

dates <- data.frame()
for(i in 1:nrow(birds)) {
  data <- birds$data[[i]]

  # Dates we expect migration to occur in
  spring_index <- spring_begin[1]:spring_end[2]
  fall_index <- fall_begin[1]:fall_end[2]

  # What are the min latitudes during spring and fall?
  spring_min <- spring_index[which.min(data$pred_lat[spring_index])]
  fall_min <- fall_index[which.min(data$pred_lat[fall_index])]

  # Get spring migration by starting at min lat and working way forwards until hit
  # spring threshold latitude for migration (Omit Chimney Swifts from South as we don't have the data)

  if(birds$species[i] != "Chimney Swift") {
    index <- spring_min
    while(data$pred_lat[index] < data$spring_begin_thresh[1]){
      if(index == nrow(data)) break
      index <- index + 1
    }
    spring_begin1 <- index - 1
  } else spring_begin1 <- NA

  index <- spring_min
  while(data$pred_lat[index] < data$spring_end_thresh[1]){
    if(index == nrow(data)) break
    index <- index + 1
  }
  spring_end1 <- index - 1

  # Get fall migration by starting at min lat and working way backwards until hit
  # fall threshold latitude for migration
  index <- fall_min
  while(data$pred_lat[index] < data$fall_begin_thresh[1]){
    if(index == 1) break
    index <- index - 1
  }
  fall_begin1 <- index + 1

  if(birds$species[i] != "Chimney Swift") {
    index <- fall_min
    while(data$pred_lat[index] < data$fall_end_thresh[1]){
      if(index == 1) break
      index <- index - 1
    }
    fall_end1 <- index + 1
  } else fall_end1 <- NA

  # Get max lat, but constrain to summer
  max_lat_yday <- data$yday[100:275][which.max(data$pred_lat[100:275])]
  max_lat <- max(data$pred_lat[100:275])

  # Get min lat, but constrain to winter
  min_lat_yday <- data$yday[c(1:100, 275:365)][which.min(data$pred_lat[c(1:100, 275:365)])]
  min_lat <- min(data$pred_lat[c(1:100, 275:365)])

  # Use segmented approach to get final dates
  if(birds$species[i] != "Chimney Swift") {
    m <- lm(pred_lat ~ yday,
            data = filter(data, yday > (spring_begin1 - 60), yday < (spring_begin1 + 60)))
    s <- segmented::segmented(m, psi = spring_begin1,
                              control = segmented::seg.control(digits = 0))
    spring_begin2 <- s$psi[1, "Est."]
  } else spring_begin2 <- NA

  m <- lm(pred_lat ~ yday,
          data = filter(data, yday > (spring_end1 - 60), yday < (spring_end1 + 60)))
  s <- segmented::segmented(m, psi = spring_end1,
                            control = segmented::seg.control(digits = 0))
  spring_end2 <- s$psi[1, "Est."]

  # # For troubleshooting:
  # ggplot(data = filter(data, yday > spring_index[1], yday < last(spring_index)),
  #        aes(x = yday, y = pred_lat)) +
  #   geom_point() +
  #   geom_vline(xintercept = spring_begin1, colour = "red") +
  #   geom_vline(xintercept = spring_begin2, colour = "blue")

  m <- lm(pred_lat ~ yday,
          data = filter(data, yday > (fall_begin1 - 60), yday < (fall_begin1 + 60)))
  s <- segmented::segmented(m, psi = fall_begin1,
                            control = segmented::seg.control(digits = 0))
  fall_begin2 <- s$psi[1, "Est."]

  if(birds$species[i] != "Chimney Swift") {
    m <- lm(pred_lat ~ yday,
            data = filter(data, yday > (fall_end1 - 60), yday < (fall_end1 + 60)))
    s <- segmented::segmented(m, psi = fall_end1,
                              control = segmented::seg.control(digits = 0))
    fall_end2 <- s$psi[1, "Est."]
  } else fall_end2 <- NA

  # # For troubleshooting:
  # ggplot(data = filter(data, yday > fall_index[1], yday < last(fall_index)),
  #        aes(x = yday, y = pred_lat)) +
  #   geom_point() +
  #   geom_vline(xintercept = fall_end1, colour = "red") +
  #   geom_vline(xintercept = fall_end2, colour = "blue")

  dates <- select(birds[i,], -data) %>%
    cbind(spring_begin1, spring_begin2,
          spring_end1, spring_end2,
          max_lat, max_lat_yday,
          min_lat, min_lat_yday,
          fall_begin1, fall_begin2,
          fall_end1, fall_end2) %>%
    rbind(dates)
}

dates <- select(dates, species, year,
                spring_begin_yday = spring_begin2,
                spring_end_yday = spring_end2,
                fall_begin_yday = fall_begin2,
                fall_end_yday = fall_end2,
                max_lat_yday, max_lat,
                min_lat, min_lat_yday)

migration <- unnest(birds, data) %>%
  left_join(dates, by = c("species", "year")) %>%
  mutate(season = case_when(yday < spring_begin_yday | yday > fall_end_yday ~ "non-breeding",
                            yday > spring_end_yday & yday < fall_begin_yday ~ "breeding",
                            yday <= spring_end_yday ~ "spring",
                            yday >= fall_begin_yday ~ "fall"),
         season = factor(season, levels = c("non-breeding", "spring", "breeding", "fall")),
         season2 = case_when(yday > spring_end_yday & yday < fall_begin_yday ~ "breeding",
                             yday <= spring_end_yday ~ "spring",
                             yday >= fall_begin_yday ~ "fall"),
         season2 = factor(season2, levels = c("spring", "breeding", "fall")))

plot_dates <- dates %>%
  select(-max_lat, -max_lat_yday, -min_lat, -min_lat_yday) %>%
  pivot_longer(names_to = "event", values_to = "yday",
               cols = c(-species, -year)) %>%
  mutate(event = factor(event,
                         levels = c("spring_begin_yday", "spring_end_yday",
                                    "fall_begin_yday", "fall_end_yday"),
                         labels = c("Date of the start of Spring Migration (spring_begin_yday)",
                                    "Date of the end of Spring Migration (spring_end_yday)",
                                    "Date of the start of Fall Migration (fall_begin_yday)",
                                    "Date of the end of Fall Migration (fall_end_yday)")))



#+ echo = FALSE
# Plot migration dates ----------------------------------------------------
#' # Plot Migration Dates
#'
#+ fig.asp = 0.5
ggplot(data = migration,
       aes(x = yday, y = pred_lat, group = year)) +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "vertical") +
  geom_line() +
  geom_vline(data = plot_dates,
             aes(xintercept = yday, colour = event), size = 0.25) +
  scale_colour_viridis_d(name = "Events", end = 0.8) +
  scale_fill_manual(name = "", values = "black") +
  facet_wrap(~species, scales = "free_y")

#' ## By year
g <- list()
for(s in c("Chimney Swift", "Vaux's Swift")) {
  g[[s]] <- ggplot(data = filter(migration, species == s),
                   aes(x = yday, y = pred_lat, group = year)) +
    theme_bw() +
    theme(legend.position = "bottom", legend.direction = "vertical") +
    geom_ribbon(aes(ymin = pred_lat - 2.58 * pred_lat_se,
                    ymax = pred_lat + 2.58 * pred_lat_se,
                    fill = "Predicted Lat +/- 99CI", group = year),
                alpha = 0.1) +
    geom_line(size = 0.5) +
    geom_vline(data = filter(plot_dates, species == s),
               aes(xintercept = yday, colour = event), size = 1) +
    scale_colour_viridis_d(name = "Events", end = 0.8) +
    scale_fill_manual(name = "", values = "black") +
    facet_wrap(~year, scales = "free_y", ncol = 1) +
    labs(title = s)
}

#' > - Black line is predicted latitude
#' > - Grey ribbon is 99% CI
#' > - Blue line is end of spring migration
#' > - Green line is max breeding latitude
#' > - Orange lines is start of fall migration
#+ fig.asp = 2, fig.width = 8, out.width = "50%"
wrap_plots(g[1:2]) + plot_layout(guides = "collect", nrow = 1)

#+ echo = FALSE
# Save Data ---------------------------------------------------------------
#' # Save Data
write_csv(dates, here("Data/Datasets/migration_details.csv"))
write_csv(migration, here("Data/Datasets/birds_04_migration.csv"))

#+ echo = FALSE
# Reproducible ---------------------------------------------------------------
#' # Reproducible

#' ## Session Info
#+ R.options = list(width = 100)
devtools::session_info()
