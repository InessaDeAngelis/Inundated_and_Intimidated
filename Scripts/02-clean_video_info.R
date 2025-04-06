#### Preamble ####
# Purpose: Cleans and calculates video info stats for all of 2024
# Author: Inessa De Angelis
# Date: 26 March 2025
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT

#### Workspace setup ####
library(tidyverse)

#### Clean LPC data ####
## Read in datasets ##
LPC_files <- list.files(path = "2024_2/Inputs/Info/LPC", recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)

LPC_files <- readr::read_csv(LPC_files, id = "Political_affiliation")

## Fix political affiliation column ##
LPC_files$Political_affiliation <- str_extract(LPC_files$Political_affiliation, "(?<=LPC/)(.*?)(?=_)")

## Update options for "Has_Comments" ##
LPC_video_info_all <- LPC_files |>
  mutate("Has_Comments" = case_when(
    Has_Comments == "Yes" ~ "Yes",
    Has_Comments == "No" ~ "No",
    Has_Comments == "Unknown" ~ "Disabled")) |>
  mutate("Political_affiliation" = case_when(
    Political_affiliation == "LPC" ~ "Liberal"))
LPC_video_info_all

## save CSV ##
write_csv(LPC_video_info_all, "2024_2/Outputs/Info/LPC_video_info_all.csv")

#### Clean CPC data ####
## Read in datasets ##
CPC_files <- list.files(path = "2024_2/Inputs/Info/CPC", recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)

CPC_files <- readr::read_csv(CPC_files, id = "Political_affiliation")

## Fix political affiliation column ##
CPC_files$Political_affiliation <- str_extract(CPC_files$Political_affiliation, "(?<=CPC/)(.*?)(?=_)")

## Update options for "Has_Comments" ##
CPC_video_info_all <- CPC_files |>
  mutate("Has_Comments" = case_when(
    Has_Comments == "Yes" ~ "Yes",
    Has_Comments == "No" ~ "No",
    Has_Comments == "Unknown" ~ "Disabled")) |>
  mutate("Political_affiliation" = case_when(
    Political_affiliation == "CPC" ~ "Conservative"))
CPC_video_info_all

## save CSV ##
write_csv(CPC_video_info_all, "2024_2/Outputs/Info/CPC_video_info_all.csv")

#### Clean NDP data ####
## Read in datasets ##
NDP_files <- list.files(path = "2024_2/Inputs/Info/NDP", recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)

NDP_files <- readr::read_csv(NDP_files, id = "Political_affiliation")

## Fix political affiliation column ##
NDP_files$Political_affiliation <- str_extract(NDP_files$Political_affiliation, "(?<=NDP/)(.*?)(?=_)")

## Update options for "Has_Comments" ##
NDP_video_info_all <- NDP_files |>
  mutate("Has_Comments" = case_when(
    Has_Comments == "Yes" ~ "Yes",
    Has_Comments == "No" ~ "No",
    Has_Comments == "Unknown" ~ "Disabled")) 
NDP_video_info_all

## save CSV ##
write_csv(NDP_video_info_all, "2024_2/Outputs/Info/NDP_video_info_all.csv")

#### Clean Green data ####
## Read in datasets ##
GPC_files <- list.files(path = "2024_2/Inputs/Info/Other", recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)

GPC_files <- readr::read_csv(GPC_files, id = "Political_affiliation")

## Fix political affiliation column ##
GPC_files$Political_affiliation <- str_extract(GPC_files$Political_affiliation, "(?<=Other/)(.*?)(?=_)")

## Update options for "Has_Comments" ##
GPC_video_info_all <- GPC_files |>
  mutate("Has_Comments" = case_when(
    Has_Comments == "Yes" ~ "Yes",
    Has_Comments == "No" ~ "No",
    Has_Comments == "Unknown" ~ "Disabled")) |>
  mutate("Political_affiliation" = case_when(
    Political_affiliation == "Other" ~ "Green Party"))
GPC_video_info_all

## Save CSV ##
write_csv(GPC_video_info_all, "2024_2/Outputs/Info/GPC_video_info_all.csv")

#### Combine all 2024_2 datasets ####
video_info_2_all <- rbind(LPC_video_info_all, CPC_video_info_all, NDP_video_info_all, GPC_video_info_all)

write_csv(video_info_2_all, "2024_2/Outputs/Info/video_info_2_all.csv")

#### Combine 2024_1 and 2024_2 info datasets ####
video_info_all <- read_csv("2024_1/Outputs/Data/Info/video_info_all.csv")
video_info_1_all <- video_info_all[,c(2,1,3,4,5,6)]

vid_info <- rbind(video_info_1_all, video_info_2_all)

vid_info <- vid_info |>
  filter(grepl('2024', Publish_Date)) 

write_csv(vid_info, "2024_full/Outputs/Info/video_info_2024_all.csv")
