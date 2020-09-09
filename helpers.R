subset_teams <- function(df, conference=NULL, division=NULL, playoff_seed=0:6){
  
  # filter for playoff status  
  playoff_df <- df %>%
    filter(X2019PlayoffSeed %in% c(playoff_seed))
  # if(playoff_seed>0){
  #   playoff_df <- df%>%
  #     filter(X2019PlayoffSeed>=1, X2019PlayoffSeed<=playoff_seed)
  # }else{
  #   playoff_df <- df #this creates a "playoff df" to be picked up by the next filters
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






# Create a function for a scatterplot of a specified week

compare_preseason <- function(df,wk){
  #@param df: a dataframe, containing preseason and week by week rankings from ESPN
  #@param wk: an integer specifying the desired week to compare against the preseason
  #@return: a ggplot scatterplot
  
  # create a dataframe for Week n ggplot
  nfl2020_plot <- df%>%
    filter(Week==wk)
  
  # create scatterplot of preseason vs Week1
  
  
  scatter_teams = ggplot(nfl2020_plot, aes(x=PreSeason, y=Rank)) + 
    
    geom_image(aes(image=png_local)) +
    labs(title="ESPN 2020 NFL Power Rankings",
         x="Pre-season rank", 
         y=paste0("Week ",wk," Rank")) +
    
    scale_x_reverse(limits=c(32,1),breaks=32:1, minor_breaks = 32:1) +
    scale_y_reverse(limits=c(32,1),breaks=32:1, minor_breaks = 32:1) +
    theme(panel.grid.minor = element_line(size = 0))
  
  
  
  return (scatter_teams)
  
  
  
}


# Create a function to plot a "swimlane" or ranking line chart

ranking_by_week <- function(df) { 
  
  swimlane <- ggplot() 
  
  
  for (i in unique(df$TeamCode)){
    # filter for single team
    singleteam_df <- df%>%
      filter(TeamCode==i)
    team_hexcolor <- singleteam_df$MainColor[1]
    team_logo <- singleteam_df$LogoPath[1]
    
    swimlane <- swimlane +
      geom_path(data=singleteam_df, mapping=aes(x=Week, y=Rank),
                color=team_hexcolor, size=2)  
    
  }
  
  logoweek <- max(df$Week)
  maxweek_df <- df%>%
    filter(Week==logoweek)
  
  swimlane <- swimlane +
    geom_image(data = maxweek_df,
               mapping = aes(x = Week, y = Rank, image = png_local),
               asp=1, size=.032)
  
  swimlane <- swimlane +
    labs(title="ESPN 2020 NFL Power Rankings") +
    scale_y_reverse(limits=c(32,1),breaks=32:1, minor_breaks = 32:1) +
    theme(panel.grid.minor = element_line(size = 0))
  
  return(swimlane)
  
}

