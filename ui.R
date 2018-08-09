#
# This is the user interface for the "FPL stat attack" Shiny application.
# written by; Christopher Hay
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


# Define UI
shinyUI(fluidPage(
    titlePanel("FPL stat attack!"),
    tabsetPanel(
        type = "tabs",
        tabPanel("2017/18 Player analysis",

    sidebarLayout(
        sidebarPanel(
            tags$body(
                p("This tab contains player data from the 2017/18 season.
                  Players can be ranked by various statistics on this tab, controlling for
              gameweek, cost and position.")
            ),
  # Application title

  sliderInput("min_gw","Gameweek filter",1,max(unlist_json_players$round),c(value=max(unlist_json_players$round)-5,max(unlist_json_players$round)),step=1),
  
  #numericInput("no_players","Number of players to show",value=20,min=1),
  selectInput("stat","Stat to calculate",var_labels_out,selected=c("Total points")),
  selectInput("pos","Position filter",position_listy),
  selectInput("tm","Team filter",team_filter_1718),
  sliderInput("cost","Price band filter",0,15,value=c(0,15),step=0.5),
  downloadButton("downloadData", "Download")
  
  
   # Sidebar with a slider input for number of bins 

    ),
    
    # output table based on the filters
    mainPanel(

       DT::dataTableOutput("fpl_table")
        )
    )
    ),
  tabPanel("Fixture analysis",
           sidebarLayout(
               sidebarPanel(
                   tags$body(
                       p("Use this tab to determine the teams with the easiest average fixtures
                         as measured by the FPL fixture difficulty ranking. Control for different
                         gameweek periods using the slider.
                         Smaller fixture difficulty rating means easier fixtures!")
                       ),
                   #sliderInput("team_gw","Gameweek filter",1,38,c(value=max(unlist_json_players$round),max(unlist_json_players$round)+4),step=1)
                   #CHANGE THIS BACK AFTER GW1 
                   sliderInput("team_gw","Gameweek filter",1,38,c(value=1,5),step=1)
                   
                   ),
               
               mainPanel( DT::dataTableOutput("team_table"))
               
      
  )
  )
  )
  
)
)
