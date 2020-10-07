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
                color=team_hexcolor, size=1.5)  
    
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

