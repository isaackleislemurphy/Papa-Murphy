# setup, calling packages
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(ggimage)

# init helpers
source("helpers.R")

# read in data
nfl2020_raw <- read.csv("NFL_Rankings_2020.csv")



#create wrangled NFL 2020 data frame
nfl2020_wrangled <- nfl2020_raw%>% #start with nfl2020_raw dataframe
  mutate(Week0=PreSeason)%>% #duplicate PreSeason column with name Week0
  pivot_longer(., paste0("Week",0:18), names_to = "Week", values_to = "Rank") %>% #pivot weeks to long form
  filter(!is.na(Rank)) %>% #filter out any row where the Rank column is NA
  mutate(Week=as.numeric(str_replace_all(Week, "Week", "")), #replace "WeekX" in each cell with just "X", converted to a numeric value
         MainColor = paste0("#",toupper(MainColor)), #specifies the hex color for each team
         png_local = paste0(TeamCode,".png")) # create a column of .png file names for each team's logo




filtered_df = subset_teams(df = nfl2020_wrangled,conference = "AFC", division = NULL, playoff_seed = 0)

scatterplot = compare_preseason(df=filtered_df, wk=1) # number after comma is week compared to preseason

swimchart = ranking_by_week(df=filtered_df)