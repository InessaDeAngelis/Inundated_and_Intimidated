#### Preamble ####
# Purpose: Prepares and models all 2024 data
# Author: Inessa De Angelis
# Date: 18 February 2025
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT

#### Workspace setup ####
library(tidyverse)
library(modelsummary)
library(marginaleffects)
library(betareg)
library(kableExtra)

all_data <- read_csv("2024_full/Inputs/all_data.csv")

#### Prepare data for modeling ####
analysis_data <- all_data |>
  mutate(gender = case_when(
    gender == "Woman" ~ 0,
    gender == "Man" ~ 1),
    background = case_when(
      background == "White" ~ 0,
      background == "Racialized" ~ 1,
      background == "Indigenous" ~ 2),
    lgbtq_out = case_when(
      lgbtq_out ==  "Not out" ~ 0,
      lgbtq_out ==  "Out" ~ 1),
    political_affiliation = case_when(
      political_affiliation == "Liberal" ~ 0,
      political_affiliation == "NDP" ~ 1,
      political_affiliation == "Conservative" ~ 2,
      political_affiliation == "Green Party" ~ 3,
      political_affiliation == "Independent" ~ 4)) |>
  rename(severity_of_harassment = toxicity_score) |>
  select(Comment, Name, severity_of_harassment, gender, background, lgbtq_out, political_affiliation)

## Factor independent variables ##
analysis_data$gender <- factor(analysis_data$gender)
analysis_data$background <- factor(analysis_data$background)
analysis_data$lgbtq_out <- factor(analysis_data$lgbtq_out)
analysis_data$political_affiliation <- factor(analysis_data$political_affiliation)

#### Test data ####
## Check class of all variables ##
class(analysis_data$gender) == "factor"
class(analysis_data$background) == "factor"
class(analysis_data$lgbtq_out) == "factor"
class(analysis_data$political_affiliation) == "factor"
class(analysis_data$severity_of_harassment) == "numeric"

## Check that all severity of harassment scores are between 0 and 1 ##
all(analysis_data$severity_of_harassment >= 0 & analysis_data$severity_of_harassment <= 1)

#### Make beta regression model ####
set.seed(16)
model_beta <- betareg(severity_of_harassment ~ gender + background + political_affiliation + lgbtq_out | 
                      gender + background + political_affiliation + lgbtq_out,
                      data = analysis_data, 
                      link = "logit")

## Summarize ##
summary(model_beta)

## Save model ##
saveRDS(model_beta, "2024_full/Outputs/model_beta.rds")
