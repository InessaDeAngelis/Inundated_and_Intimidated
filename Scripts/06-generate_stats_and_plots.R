#### Preamble ####
# Purpose: Generates summary statistics, tables, and figures shown in the paper
# Author: Inessa De Angelis
# Date: 18 March 2025
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT

#### Workspace setup ####
library(tidyverse)
library(kableExtra)
library(modelsummary)
library(marginaleffects)
library(broom)
library(ggplot2)
library(scales)

all_data <- read_csv("2024_full/Inputs/all_data.csv")
vid_info <- read.csv("2024_full/Outputs/Info/video_info_2024_all.csv")
model_beta <- read_rds("2024_full/Outputs/model_beta.rds")

all_data <- all_data |> rename(severity_of_harassment = toxicity_score)

#### Number of videos posted / with comments statistics ####
## Count all videos by political affiliation ##
video_info_stats <- vid_info |>
  group_by(Political_affiliation) |>
  summarise(Has_comments = n()) 
video_info_stats

## Count only videos with comments by political affiliation ##
video_info_comments <- vid_info |>
  filter(Has_Comments == "Yes") |>
  group_by(Political_affiliation) |>
  summarise(Has_comments = n())
video_info_comments

#### Comments by gender and political affiliation table ####
scores_by_gender <- all_data |>  
  count(gender, political_affiliation, sort = TRUE) |>
  mutate(proportion = round(n / sum(n) * 100, 1)) |>
  rename(number = n, percent = proportion) |>
  kable(col.names = c("Gender", "Political Affiliation", "Number of Comments", "Percent"),
        booktabs = TRUE)
scores_by_gender

#### Number of comments and severity of harassment scores figure ####
data_viz <- all_data |>
  mutate(severity_bin = cut(severity_of_harassment, breaks = seq(0, 1, by = 0.1))) |>
  group_by(severity_bin) |> summarise(count = n()) 

data_viz <- data_viz |>  
  mutate(severity_bin = as.character(severity_bin), 
         severity_bin = case_when(
           severity_bin == "(0,0.1]" ~ "0-0.1",
           severity_bin == "(0.1,0.2]" ~ "0.1-0.2",
           severity_bin == "(0.2,0.3]" ~ "0.2-0.3",
           severity_bin == "(0.3,0.4]" ~ "0.3-0.4",
           severity_bin == "(0.4,0.5]" ~ "0.4-0.5",
           severity_bin == "(0.5,0.6]" ~ "0.5-0.6",
           severity_bin == "(0.6,0.7]" ~ "0.6-0.7",
           severity_bin == "(0.7,0.8]" ~ "0.7-0.8",
           severity_bin == "(0.8,0.9]" ~ "0.8-0.9",
           severity_bin == "(0.9,1]" ~ "0.9-1"))

ggplot(data_viz, aes(x = severity_bin, y = count)) +
  geom_col(fill = "darkseagreen4") +
  labs(x = "Severity of Harassment", y = "Number of Comments") +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +  
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme_minimal()

#### Severity of harassment by gender table ####
scores_by_politician <- all_data |>  
  group_by(gender) |>
  summarize(
    severity_of_harassment = (sum(severity_of_harassment >= 0.7) / n()) * 100) |>
  select(gender, severity_of_harassment) |>
  arrange(desc(severity_of_harassment)) |>
  mutate(across(where(is.numeric), ~ round(., 2))) |>
  arrange(desc(severity_of_harassment)) |>
  kable(col.names = c("Gender", "Severity of harassment"),
    booktabs = TRUE)
scores_by_politician

#### Severity of harassment by background table ####
scores_by_politician <- all_data |>  
  group_by(background) |>
  summarize(
    severity_of_harassment = (sum(severity_of_harassment >= 0.7) / n()) * 100) |>
  select(background, severity_of_harassment) |>
  arrange(desc(severity_of_harassment)) |>
  mutate(across(where(is.numeric), ~ round(., 2))) |>
  arrange(desc(severity_of_harassment)) |>
  kable(col.names = c("Background", "Severity of harassment"),
    booktabs = TRUE)
scores_by_politician

#### Severity of harassment by gender and background table ####
scores_by_politician <- all_data |>  
  group_by(gender, background) |>
  summarize(
    severity_of_harassment = (sum(severity_of_harassment >= 0.7) / n()) * 100) |>
  select(gender, background, severity_of_harassment) |>
  arrange(desc(severity_of_harassment)) |>
  mutate(across(where(is.numeric), ~ round(., 2))) |>
  arrange(desc(severity_of_harassment)) |>
  kable(col.names = c("Gender", "Background", "Severity of harassment"),
    booktabs = TRUE)
scores_by_politician

#### Severity of harassment by political affiliation table ####
scores_by_politician <- all_data |>  
  group_by(political_affiliation) |>
  summarize(
    severity_of_harassment = (sum(severity_of_harassment >= 0.7) / n()) * 100) |>
  select(political_affiliation, severity_of_harassment) |>
  arrange(desc(severity_of_harassment)) |>
  mutate(across(where(is.numeric), ~ round(., 2))) |>
  arrange(desc(severity_of_harassment)) |>
  kable(col.names = c("Political Affiliation", "Severity of harassment"),
    booktabs = TRUE)
scores_by_politician

#### Severity of harassment by LGBTQ2+ community membership table ####
scores_by_politician <- all_data |>  
  group_by(lgbtq_out) |>
  summarize(
    severity_of_harassment = (sum(severity_of_harassment >= 0.7) / n()) * 100) |>
  select(lgbtq_out, severity_of_harassment) |>
  arrange(desc(severity_of_harassment)) |>
  mutate(across(where(is.numeric), ~ round(., 2))) |>
  arrange(desc(severity_of_harassment)) |>
  kable(col.names = c("LGBTQ2+", "Severity of harassment"),
    booktabs = TRUE)
scores_by_politician

#### Severity of harassment by gender and political affiliation table ####
scores_by_politician <- all_data |>  
  group_by(gender, political_affiliation) |>
  summarize(
    severity_of_harassment = (sum(severity_of_harassment >= 0.7) / n()) * 100) |>
  select(gender, political_affiliation, severity_of_harassment) |>
  arrange(desc(severity_of_harassment)) |>
  mutate(across(where(is.numeric), ~ round(., 2))) |>
  arrange(desc(severity_of_harassment)) |>
  kable(col.names = c("Gender", "Political Affiliation", "Severity of harassment"),
    booktabs = TRUE)
scores_by_politician

#### Severity of harassment by background and political affiliation table ####
scores_by_politician <- all_data |>  
  group_by(background, political_affiliation) |>
  summarize(
    severity_of_harassment = (sum(severity_of_harassment >= 0.7) / n()) * 100) |>
  select(background, political_affiliation, severity_of_harassment) |>
  arrange(desc(severity_of_harassment)) |>
  mutate(across(where(is.numeric), ~ round(., 2))) |>
  arrange(desc(severity_of_harassment)) |>
  kable(col.names = c("Background", "Political Affiliation", "Severity of harassment"),
    booktabs = TRUE)
scores_by_politician

#### Beta Regression model (mean component) predictions table ####
comp <- slopes(model_beta, newdata = "median") |>
  select(term, contrast, estimate, p.value) |>
  arrange(match(term, c('gender', 'background', 'political_affiliation', 'lgbtq_out'))) |>
  mutate(contrast = case_when(
    contrast == "1 - 0" & term == "background" ~ "Racialized - White",
    contrast == "2 - 0" & term == "background" ~ "Indigenous - White",
    contrast == "1 - 0" & term == "gender" ~ "Man - Woman", 
    contrast == "1 - 0" & term == "lgbtq_out" ~ "LGBTQ out - LGBTQ not out",
    contrast == "1 - 0" & term == "political_affiliation" ~ "NDP - Liberal",
    contrast == "2 - 0" & term == "political_affiliation" ~ "Conservative - Liberal",
    contrast == "3 - 0" & term == "political_affiliation" ~ "Green Party - Liberal",
    contrast == "4 - 0" & term == "political_affiliation" ~ "Independent - Liberal")) |>
  mutate(term = case_when(
    term == "gender" ~ "Gender",
    term == "background" ~ "Background",
    term == "political_affiliation" ~ "Political affiliation",
    term == "lgbtq_out" ~ "LGBTQ out")) |>
  kable(col.names = c("Term", "Contrast", "Estimate", "Pr(>|z|)"),
        digits = 3, booktabs = TRUE)

comp |> kable()

#### Appendix: Beta Regression model (mean and precision components) table ####
tidy_model <- tidy(model_beta)
tidy_model |> kable(col.names = c("Component", "Term", "Estimate", "Standard Error", "Statistic", "Pr(>|z|)"),
                    digits = 3, booktabs = TRUE)
