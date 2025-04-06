#### Preamble ####
# Purpose: Cleans all of the 2024 YouTube comment data
# Author: Inessa De Angelis
# Date: 13 February 2025
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Note: Encompasses January 1 - December 31, 2024

#### Workspace setup ####
library(tidyverse)
library(cld2)

#### 2024-1: First six months ####
#### LPC Dataset cleaning ####
## Read in datasets ##
LPC_files <- list.files(path = "2024_1/Inputs/Main/Data/LPC", recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)

LPC_files <- readr::read_csv(LPC_files, id = "Name")

## Fix MP name column ##
LPC_files$Name <- str_extract(LPC_files$Name, "(?<=LPC/)(.*?)(?=_)")

## Detect comment language ##
# Rename column #
LPC_files <- LPC_files |>
  rename(text = Comment)

# Detect languages #
language_results <- detect_language(LPC_files$text)

print(language_results)

# Put language categorizations into df #
df <- data.frame(language_results)

# Combine language categorizations with the rest of the data #
LPC_df <- cbind(LPC_files, df)

## Remove text in other languages, re-name + drop columns ##
LPC_df_updated <- LPC_df |>
  filter(grepl('en',language_results)) |>
  rename(Comment = text) |>
  select(-c(Name, language_results, AuthorProfileImageUrl, AuthorChannelUrl, UpdatedAt))
LPC_df_updated 

# Get rid of "@" before some usernames #
LPC_df_updated$Username <- str_remove(LPC_df_updated$Username, "^@") 

## Save dataset ##
write_csv(LPC_df_updated, "2024_1/Outputs/Data/LPC/LPC_2024_1_cleaned.csv")

#### NDP Dataset cleaning ####
## Read in datasets ##
NDP_files <- list.files(path = "2024_1/Inputs/Main/Data/NDP", recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)

NDP_files <- readr::read_csv(NDP_files, id = "Name")

## Fix MP name column ##
NDP_files$Name <- str_extract(NDP_files$Name, "(?<=NDP/)(.*?)(?=_)")

## Detect comment language ##
# Rename column #
NDP_files <- NDP_files |>
  rename(text = Comment)

# Detect languages #
language_results_NDP <- detect_language(NDP_files$text)

print(language_results_NDP)

# Put language categorizations into df #
df2 <- data.frame(language_results_NDP)

# Combine language categorizations with the rest of the data #
NDP_df <- cbind(NDP_files, df2)

## Remove text in other languages, re-name + drop columns ##
NDP_df_updated <- NDP_df |>
  filter(grepl('en',language_results_NDP)) |>
  rename(Comment = text) |>
  select(-c(Name, language_results_NDP, AuthorProfileImageUrl, AuthorChannelUrl, UpdatedAt))

# Get rid of "@" before some usernames #
NDP_df_updated$Username <- str_remove(NDP_df_updated$Username, "^@") 

## Save dataset ##
write_csv(NDP_df_updated, "2024_1/Outputs/Data/NDP/NDP_2024_1_cleaned.csv")

#### Other Dataset cleaning ####
## Read in datasets ##
Other_files <- list.files(path = "2024_1/Inputs/Main/Data/Other", recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)

Other_files <- readr::read_csv(Other_files, id = "Name")

## Fix MP name column ##
Other_files$Name <- str_extract(Other_files$Name, "(?<=Other/)(.*?)(?=_)")

## Detect comment language ##
# Rename column #
Other_files <- Other_files |>
  rename(text = Comment)

# Detect languages #
language_results_Other <- detect_language(Other_files$text)

print(language_results_Other)

# Put language categorizations into df #
df3 <- data.frame(language_results_Other)

# Combine language categorizations with the rest of the data #
Other_df <- cbind(Other_files, df3)

## Remove text in other languages, re-name + drop columns ##
Other_df_updated <- Other_df |>
  filter(grepl('en', language_results_Other)) |>
  rename(Comment = text) |>
  select(-c(Name, language_results_Other, AuthorProfileImageUrl, AuthorChannelUrl, UpdatedAt))

# Get rid of "@" before some usernames #
Other_df_updated$Username <- str_remove(Other_df_updated$Username, "^@") 

## Save dataset ##
write_csv(Other_df_updated, "2024_1/Outputs/Data/Other/Other_2024_1_cleaned.csv")

#### CPC Dataset cleaning ####
CPC_files <- list.files(path = "2024_1/Inputs/Main/Data/CPC", recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)

CPC_files <- readr::read_csv(CPC_files, id = "Name")

## Fix MP name column ##
CPC_files$Name <- str_extract(CPC_files$Name, "(?<=CPC/)(.*?)(?=_)")

## Detect comment language ##
# Rename column #
CPC_files <- CPC_files |>
  rename(text = Comment)

# Detect languages #
language_results_CPC <- detect_language(CPC_files$text)

print(language_results_CPC)

# Put language categorizations into df #
df4 <- data.frame(language_results_CPC)

# Combine language categorizations with the rest of the data #
CPC_df <- cbind(CPC_files, df4)

## Remove text in other languages, re-name + drop columns ##
CPC_df_updated <- CPC_df |>
  filter(grepl('en',language_results_CPC)) |>
  rename(Comment = text) |>
  select(-c(Name, language_results_CPC, AuthorProfileImageUrl, AuthorChannelUrl, UpdatedAt))

# Get rid of "@" before some usernames #
CPC_df_updated$Username <- str_remove(CPC_df_updated$Username, "^@") 

## Save full dataset ##
write_csv(CPC_df_updated, "2024_1/Outputs/Data/CPC/CPC_2024_1_cleaned.csv")

## Split dataset into multiple files to commit to Git ##
# Just Pierre #
CPC_df_updated2 = CPC_df_updated |>
  filter(Name == "Pierre Poilievre")

# Everyone but Pierre #
CPC_df_updated3 = CPC_df_updated |>
  filter(!Name == "Pierre Poilievre")

## Save seperate datasets ##
write_csv(CPC_df_updated, "2024_1/Outputs/Data/CPC/CPC_2024_1_cleaned2.csv")
write_csv(CPC_df_updated3, "2024_1/Outputs/Data/CPC/CPC_2024_1_cleaned3.csv")

#### 2024-2: Next six months ####
#### LPC Dataset cleaning ####
## Read in datasets ##
LPC_files <- list.files(path = "2024_2/Inputs/Data/LPC", recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)

LPC_files <- readr::read_csv(LPC_files, id = "Name")

## Fix MP name column ##
LPC_files$Name <- str_extract(LPC_files$Name, "(?<=LPC/)(.*?)(?=_)")

## Detect comment language ##
# Rename column #
LPC_files <- LPC_files |>
  rename(text = Comment)

# Detect languages #
language_results <- detect_language(LPC_files$text)

print(language_results)

# Put language categorizations into df #
df <- data.frame(language_results)

# Combine language categorizations with the rest of the data #
LPC_df <- cbind(LPC_files, df)

## Remove text in other languages, re-name + drop columns ##
LPC_df_updated <- LPC_df |>
  filter(grepl('en',language_results)) |>
  rename(Comment = text) |>
  select(-c(Name, language_results, AuthorProfileImageUrl, AuthorChannelUrl, UpdatedAt))
LPC_df_updated 

# Get rid of "@" before some usernames #
LPC_df_updated$Username <- str_remove(LPC_df_updated$Username, "^@") 

## Save dataset ##
write_csv(LPC_both, "2024_2/Outputs/Data/LPC/LPC_2024_2_cleaned.csv")

#### NDP Dataset cleaning ####
## Read in datasets ##
NDP_files <- list.files(path = "2024_2/Inputs/Data/NDP", recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)

NDP_files <- readr::read_csv(NDP_files, id = "Name")

## Fix MP name column ##
NDP_files$Name <- str_extract(NDP_files$Name, "(?<=NDP/)(.*?)(?=_)")

## Detect comment language ##
# Rename column #
NDP_files <- NDP_files |>
  rename(text = Comment)

# Detect languages #
language_results_NDP <- detect_language(NDP_files$text)

print(language_results_NDP)

# Put language categorizations into df #
df2 <- data.frame(language_results_NDP)

# Combine language categorizations with the rest of the data #
NDP_df <- cbind(NDP_files, df2)

## Remove text in other languages, re-name + drop columns ##
NDP_df_updated <- NDP_df |>
  filter(grepl('en',language_results_NDP)) |>
  rename(Comment = text) |>
  select(-c(Name, language_results_NDP, AuthorProfileImageUrl, AuthorChannelUrl, UpdatedAt))

# Get rid of "@" before some usernames #
NDP_df_updated$Username <- str_remove(NDP_df_updated$Username, "^@") 

## Save dataset ##
write_csv(NDP_both, "2024_2/Outputs/Data/NDP/NDP_2024_2_cleaned.csv")

#### Other Dataset cleaning ####
## Read in datasets ##
Other_files <- list.files(path = "2024_2/Inputs/Data/Other", recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)

Other_files <- readr::read_csv(Other_files, id = "Name")

## Fix MP name column ##
Other_files$Name <- str_extract(Other_files$Name, "(?<=Other/)(.*?)(?=_)")

## Detect comment language ##
# Rename column #
Other_files <- Other_files |>
  rename(text = Comment)

# Detect languages #
language_results_Other <- detect_language(Other_files$text)

print(language_results_Other)

# Put language categorizations into df #
df3 <- data.frame(language_results_Other)

# Combine language categorizations with the rest of the data #
Other_df <- cbind(Other_files, df3)

## Remove text in other languages, re-name + drop columns ##
Other_df_updated <- Other_df |>
  filter(grepl('en',language_results_Other)) |>
  rename(Comment = text) |>
  select(-c(Name, language_results_Other, AuthorProfileImageUrl, AuthorChannelUrl, UpdatedAt))

# Get rid of "@" before some usernames #
Other_df_updated$Username <- str_remove(Other_df_updated$Username, "^@") 

## Save dataset ##
write_csv(Other_both, "2024_2/Outputs/Data/Other/Other_2024_2_cleaned.csv")

#### CPC Dataset cleaning ####
CPC_files <- list.files(path = "2024_2/Inputs/Data/CPC", recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)

CPC_files <- readr::read_csv(CPC_files, id = "Name")

## Fix MP name column ##
CPC_files$Name <- str_extract(CPC_files$Name, "(?<=CPC/)(.*?)(?=_)")

## Detect comment language ##
# Rename column #
CPC_files <- CPC_files |>
  rename(text = Comment)

# Detect languages #
language_results_CPC <- detect_language(CPC_files$text)

print(language_results_CPC)

# Put language categorizations into df #
df4 <- data.frame(language_results_CPC)

# Combine language categorizations with the rest of the data #
CPC_df <- cbind(CPC_files, df4)

## Remove text in other languages, re-name + drop columns ##
CPC_df_updated <- CPC_df |>
  filter(grepl('en',language_results_CPC)) |>
  rename(Comment = text) |>
  select(-c(Name, language_results_CPC, AuthorProfileImageUrl, AuthorChannelUrl, UpdatedAt))

# Get rid of "@" before some usernames #
CPC_df_updated$Username <- str_remove(CPC_df_updated$Username, "^@") 

## Save dataset ##
write_csv(CPC_both, "2024_2/Outputs/Data/CPC/CPC_2024_2_cleaned.csv")

## Split dataset into multiple files to commit to Git ##
# Just Pierre #
CPC_both2 <- CPC_both |>
  filter(Name == "Pierre Poilievre")

# Everyone but Pierre #
CPC_both3 <- CPC_both |>
  filter(!Name == "Pierre Poilievre")

## Save separate datasets ##
write_csv(CPC_both2, "2024_2/Outputs/Data/CPC/CPC_both2.csv")
write_csv(CPC_both3, "2024_2/Outputs/Data/CPC/CPC_both3.csv")
