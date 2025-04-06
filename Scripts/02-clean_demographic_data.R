#### Preamble ####
# Purpose: Initial cleaning of demographic data from Sevi (2021) & Johnston et al. (2021)
# Author: Inessa De Angelis
# Date: 16 January 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Note: Use these cleaned datasets to manually match up with my YouTube info dataset

#### Workspace setup ####
library(tidyverse)

sevi <- read_csv("2024_2/Inputs/Data/Demographic/sevi.csv")
johnson <- read_csv("2024_2/Inputs/Data/Demographic/johnson.csv")

#### Clean datasets ####
sevi_cleaned <- sevi |>
  filter(year == 2021) |>
  filter(elected == "Elected") |>
  select(candidate_name, gender, birth_year, country_birth, lgbtq2_out, indigenousorigins)

johnson_cleaned <- johnson |>
  filter(year == 2019) |>
  filter(status == "Elected") |>
  select(riding, gender, background, occupation, age, political_affiliation)

#### Save datasets ####
write_csv(sevi_cleaned, "2024_2/Outputs/Data/Demographic/sevi.csv")
write_csv(johnson_cleaned, "2024_2/Outputs/Data/Demographic/johnson.csv")