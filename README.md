[![DOI](https://zenodo.org/badge/648380978.svg)](https://zenodo.org/badge/latestdoi/648380978)
![In Review](https://img.shields.io/badge/status-review-orange)

# Shifts in breeding distribution, migration timing, and migration routes of two North American swift species

*[Submitted to Journal of Field Ornithology]()*

Erik D. Prytula<sup>1</sup>, Matthew W. Reudink<sup>1</sup>, [Steffi LaZerte](https://steffilazerte.ca)<sup>2</sup>, and Ann E. McKellar<sup>3, \*</sup>

<sup>\*</sup> Corresponding Author <ann.mckellar@ec.gc.ca>
<sup>1</sup> Department of Biological Sciences, Thompson Rivers University, Kamloops, BC V2C 0C8, Canada  
<sup>2</sup> Department of Biology, Brandon University, Brandon, MB R7A 6A9, Canada  
<sup>3</sup> Wildlife Research Division, Environment and Climate Change Canada, Saskatoon, SK S7N 0X4, Canada  


# Contents

This repository contains the code used to summarize, transform, and visualize 
eBird data for this manuscript as well as the final data set (`migration_details.csv`).

We are extremely grateful to Dr. Sarah Supp and colleagues for sharing their [well-annotated code](https://github.com/sarahsupp/hb-migration) from their publication [Supp et al. (2015)](https://esajournals.onlinelibrary.wiley.com/doi/full/10.1890/ES15-00239.1), 
which we used as a guide for our analyses.

> Note that this workflow is very similar to a [related workflow](https://github.com/steffilazerte/bluebird_migration) we created to 
> explore migration in bluebird species.

## Format

The scripts are stored int the `Scripts` folder and can be run in sequence. 
Note that scripts are designed to produce formatted
RMarkdown reports, so can be run interactively (by hand) or with the `rmarkdown::render()` 
code contained at the top of each script. If running this code, an html report
of the script will be produced in the `Results` folder. 
Data produced will be saved to a `Data` folder. 
The final dataset, `migration_details.csv` is stored in the `Data/Datasets` folder.

If possible, [`renv`](https://rstudio.github.io/renv) will restore the packages
used and automatically install them for you. Otherwise see `01_setup.R`.
Please contact [@steffilazerte](https://github.com/steffilazerte)
if you run into any problems.

The following sections describe the various folders that are *produced* by the
scripts in the *Scripts* folder, ending with a description of the scripts 
themselves.


### `Data` folder

Subfolders contain the following:

- `Intermediate` - Intermediate data files created in the process of summarizing
ebird data
- `Datasets` - Full datasets created by the scripts. Although not in the
intermediate folder, some of these datasets are intermediate in nature. For
example, the `birds_01_hex_all.rds` file contains all observations separately
linked to hex. But we will only every used the data which is summarized by hex. 
However, this data may be useful for other things so it's stored here.

> Note that the original, eBird data is not stored here, but can be obtained from
> eBird directly. See details in `Scripts/03_initial_data_ebird.R`.

### `Results` folder

Compiled scripts will be saved to the `Results` folder and dated.

Figures are usually embedded in the Report, but final manuscript figures may be
saved to the `Results/Figures` folder.

### `Scripts` folder

All the scripts used to summarize the data and conduct the analyses are
contained in the **Scripts** folder. The scripts are numbered in the order that
they are meant to be compiled. For example, `04_ebird_samples.R` creates figures
exploring the data samples and relies on data created by
`03_initial_data_ebird.R`.

#### `01_setup.R`
This script makes sure the folder structure is set up and that you have all the packages you need to run the analysis.

#### `02_initial_data_hex.R`
This script creates the hex grid we use for summarizing the eBird data and
prepares a map of North and South America for plotting.

**Output:**

- Spatial hex grids (`hex_res6.rds`, `hex_res7.rds`)
- Map of the Americas (`americas.rds`)

#### `03_initial_data_ebird.R`
This script takes the eBird data on checklists and sampling and filters it, and
summarizes it by hex. The eBird data needs to be downloaded from [eBird](https://ebird.org) and is in
the format of, for example: `ebd_chiswi_200501_201901_relNov-2019.txt`

**Output:**

- The complete data file with all observations aligned by hex (`birds_01_hex_all.rds`)
- The summarized data file with summed checklists by hex by species (`birds_02_hex_sum.rds`)

#### `04_ebird_samples.R`
This script explores the data samples. There is no output for this script (just figures in the Report)

#### `05_gam.R`
This script calculates mean population locations based on hex locations weighted by the proportion of birds observed. 
It then calculates a GAM model of the population migration routes and predicts daily locations. 

**Output:**

- For each day in each year for each species, average population location, and predicted location from the GAM model (`birds_03_mean.csv`)

#### `06_migration_dates.R`
This script uses the predicted GAM locations to calculate dates of spring and fall migrations.
BOTH the start and end of spring and the start and end of fall are calculated 
A threshold method is used to find an approximate date, and then a segmented 
regression method is used to find a breakpoint (i.e. the final date).

**Output:**

- Dates and seasonal categories are added ot the average/predicted population locations 
  (`birds_04_migration.csv`)
- A simple data set of migration dates for each species in each year 
  (`migration_details.csv`)
    - Start and End dates of spring migration (`spring_begin_yday`, `spring_end_yday`)
    - Start and End dates of fall migration (`fall_begin_yday`, `fall_end_yday`)
    - Date the first time the maximum latitude was reached (`max_lat_yday`)
    - Maximum latitude (`max_lat`) reached

#### `07_migration_dist_speed.R`
This script calculates migration distances and daily speeds for each seasonal category. 

**Output:**

- Speeds for each seasonal category are added to the migration details file
  (`migration_details`)
  - Median of the top 5 Daily speeds (km/day) for each spring, breeding and fall
    (`speed_spring`, `speed_breeding`, `speed_fall`)

#### `08_migration_routes.R`
This script looks at migration routes and calculates longitudinal variation

**Output:**

- Different measures of seasonal longitude are added to the migration details file
  (`migration_details`)
  - Min, Mean, Median, Mid-point, and Max longitude for non-breeding, spring, 
  breeding, and fall 
   (`non-breeding_min_lon`, `non-breeding_mean_lon`, `non-breeding_max_lon`, etc.)

#### `09_figures.R`
- Code to produce Figure 1 of the manuscript

