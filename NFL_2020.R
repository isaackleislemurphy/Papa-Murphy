# setup, calling packages
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(ggimage)

nfl2020_raw <- read.csv("NFL_Rankings_2020.csv")

#create wrangled NFL 2020 data frame

nfl2020_wrangled <- nfl2020_raw%>% #start with nfl2020_raw dataframe
  mutate(Week0=PreSeason)%>% #duplicate PreSeason column with name Week0
  pivot_longer(., paste0("Week",0:18), names_to = "Week", values_to = "Rank") %>% #pivot weeks to long form
  filter(!is.na(Rank)) %>% #filter out any row where the Rank column is NA
  mutate(Week=as.numeric(str_replace_all(Week, "Week", "")), #replace "WeekX" in each cell with just "X", converted to a numeric value
        MainColor = toupper(MainColor), #specifies the hex color for each team
        png_local = paste0(TeamCode,".png")) # create a column of .png file names for each team's logo


# create functions to select a subset of teams

subset_teams <- function(df,conference=NULL, division=NULL, playoff_seed=0:6){
  
  playoff_df <- df%>%
    filter(X2019PlayoffSeed %in% playoff_seed)

# filter for playoff status  
  
  # if(playoff_seed>0){
  #   playoff_df <- df%>%
  #     filter()
  #     # filter(X2019PlayoffSeed>=1, X2019PlayoffSeed<=playoff_seed)
  # 
  # }else{
  #   
  #   playoff_df <- df #this creates a "playoff df" to be picked up by the next filters
  #   
  #   
  # }
  
    
# if/else filter for conference or division
  
  if(is.null(conference)){
    
    return(playoff_df)
    
  }else{
    
    conference_df <- playoff_df%>%
      filter(Conference==conference)
    
    if(is.null(division)){
      
      return(conference_df)
    }else{
      
      confdiv_df <- conference_df%>%
        filter(Division==division)
      
      return(confdiv_df)
        
    }
    
    
    
    
  }
  
}





filtered_df = subset_teams(df = nfl2020_wrangled,conference = NULL, division = NULL, playoff_seed = (0:6))

# CALL THE PLOTTING HELPER FUNCTIONS

source("NFL_2020_plotfunctions.R")

scatterplot = compare_preseason(df=filtered_df, wk=3) # number after comma is week compared to preseason

swimchart = ranking_by_week(df=filtered_df)

