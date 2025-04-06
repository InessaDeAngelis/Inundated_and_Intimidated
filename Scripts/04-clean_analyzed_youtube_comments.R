#### Preamble ####
# Purpose: Clean up and rejoin analyzed datasets
# Author: Inessa De Angelis
# Date: 18 February 2025
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT

#### Workspace setup ####
library(tidyverse)

## Read in 2024_2 YouTube comment data ##
LPC_analyzed <- read_csv("2024_2/Outputs/Data/Perspective/LPC_analyzed_2024_2.csv")
NDP_analyzed <- read_csv("2024_2/Outputs/Data/Perspective/NDP_analyzed_2024_2.csv")
Other_analyzed <- read_csv("2024_2/Outputs/Data/Perspective/Other_analyzed_2024_2.csv")

list_of_files <- list.files(path = "2024_2/Outputs/Data/Perspective/CPC/", recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)

CPC_analyzed <- readr::read_csv(list_of_files, id = "file_name") |>
  drop_na(Comment) |>
  select(-c(file_name, PublishedAt, ReplyCount, LikeCount))

## Read in and tidy demographic data ##
usernames <- read_csv("2024_2/Inputs/Data/Demographic/Cdn_Politicians_YouTube_2025-1.csv")
usernames <- unite(usernames, Name, c(first_name, last_name), sep = " ")
usernames <- usernames |> select(-c(honorific_title, profile_URL))

## Read in 2024-1 Perspective data ##
all_analyzed <- read_csv("2024_1/Outputs/Data/Perspective/all_analyzed.csv")

#### Clean 2024_2 datasets ####
## LPC ##
LPC_analyzed <- LPC_analyzed |>
  drop_na(Comment) |>
  select(-c(PublishedAt, ReplyCount, LikeCount))

## NDP ##
NDP_analyzed <- NDP_analyzed |>
  drop_na(Comment) |>
  select(-c(PublishedAt, ReplyCount, LikeCount))

## Other ##
Other_analyzed <- Other_analyzed |>
  drop_na(Comment) |>
  select(-c(PublishedAt, ReplyCount, LikeCount))

## CPC ##
# Just Pierre #
CPC_analyzed2 <- CPC_analyzed |>
  filter(Name == "Pierre Poilievre")
CPC_analyzed2

# Everyone but Pierre #
CPC_analyzed3 <- CPC_analyzed |>
  filter(!Name == "Pierre Poilievre")
CPC_analyzed3

## Combine datasets ##
all_analyzed2 <- rbind(LPC_analyzed, NDP_analyzed, Other_analyzed, CPC_analyzed)

write_csv(all_analyzed2, "2024_2/Outputs/Data/Perspective/Cleaned/all_analyzed2.csv")

#### Combine 2024-1 and 2024-2 datasets ####
## Only select "toxicity" classification column ##
all_analyzed_final <- all_analyzed |>
  mutate(Name = case_when(
    Name == "Mark Garretsen" ~ "Mark Gerretsen",
    TRUE ~ Name)) |>
  select(-c(Status, Gender, Political_affiliation, severe_toxicity_score, insult_score, sexually_explicit_score,
            profanity_score, threat_score, flirtation_score))

all_analyzed_final2 <- all_analyzed2 |>
  mutate(Name = case_when(
    is.na(Name) ~ "Jenny Kwan",
    TRUE ~ Name)) |>
  select(-c(status, gender, political_affiliation, background, lgbtq_out, constituency, province_territory, Username,
            severe_toxicity_score, insult_score, sexually_explicit_score, profanity_score, threat_score, flirtation_score)) |>
  rename(AuthorChannelID = AuthorChannelId, CommentID = CommentId, ParentID = ParentId, VideoID = VideoId)

## filter out the few comments from 2025! ##
all_data <- all_data |> filter(VideoDate <= as.Date('2024-12-31'))

#### Join in demographic data ####
all_analyzed_final <- all_analyzed_final |> 
  left_join(usernames, by = join_by(Name==Name)) |>
  select(-c(username, age))

all_analyzed_final2 <- all_analyzed_final2 |> 
  left_join(usernames, by = join_by(Name==Name)) |>
  select(-c(username, age))

all_data <- rbind(all_analyzed_final, all_analyzed_final2)

## Save final dataset ##
write_csv(all_data, "2024_full/Inputs/all_data.csv")
