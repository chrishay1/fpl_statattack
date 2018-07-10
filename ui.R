#
# This is the user interface for the "FPL stat attack" Shiny application.
# written by; Christopher Hay
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(curl)
library(jsonlite)
library(dplyr)
json_input <- fromJSON("https://fantasy.premierleague.com/drf/element-summary/8")
json_input_hist<-json_input$history
#position list
position_listx <- as.data.frame(c("Goalkeeper","Defender","Midfielder","Forward"))
position_list <- cbind(position_listx,c(1:4))
colnames(position_list) <- c("position","element_type")
#variable names
var_names <- colnames(json_input$history)
var_labels <-c("ICT index","Total points","Value","Transfers balance",
               "Influence","Creativity","Threat","Selected","Transfers in","Transfers out","Minutes",
               "Goals scored","Assists","Clean sheets","Goals conceded","Own goals","Penalties saved",
               "Penalties missed","Yellow cards","Red cards","Saves","Bonus points","Bonus points system",
                "open play crosses", "big chances created",
               "Clearances blocks interceptions","Recoveries",
               "Key passes","tackles","winning_goals", "attempted_passes",
               "Completed passes","Penalties conceded","Big chances missed","Errors leading to goal",
               "Errors leading to goal attempts","Tackled","Offsides","Target missed","Fouls","Dribbles","element",
               "fixture","opponent_team","id","kickoff_time","kickoff_time_formatted","Home team score",
               "Away team score","Was home","round","ea_index","Loaned in","Loaned out"
               )
var_name_labels <-cbind(var_names,var_labels)

position_listy <- c("","Goalkeeper","Defender","Midfielder","Forward")
# Define UI
shinyUI(fluidPage(
    titlePanel("FPL stat attack!"),
    tabsetPanel(
        type = "tabs",
        tabPanel("Player analysis",

    sidebarLayout(
        sidebarPanel(
            tags$body(
                p("Use this tab to determine the top players by various statistics, controlling for
              gameweek, cost and position.")
            ),
  # Application title

  sliderInput("min_gw","Gameweek filter",1,nrow(json_input_hist),c(value=nrow(json_input_hist)-5,nrow(json_input_hist)),step=1),
  numericInput("no_players","Number of players to show",value=20,min=1),
  selectInput("stat","Stat to calculate",var_labels,selected=c("Total points")),
  selectInput("pos","Position filter",position_listy) ,
  sliderInput("cost","Price band filter",0,15,value=c(0,15),step=0.5)
  # Sidebar with a slider input for number of bins 

    ),
    
    # output table based on the filters
    mainPanel(

       tableOutput("fpl_table")
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
                   sliderInput("team_gw","Gameweek filter",1,38,c(value=nrow(json_input_hist),nrow(json_input_hist)+4),step=1)
                   ),
               
               mainPanel(tableOutput("team_table"))
               
      
  )
  )
  )
  
)
)
