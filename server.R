    #
    # This is the server logic for the FPl stat attack Shiny application. 
    # Written by; Christopher Hay
    #
    # 
    # 
    #    
    #
    #json_player_list <- list(0)
    
    
    shinyServer(function(input, output) {
        
        stat_table <- reactive({
            
            fpl_stat_use <-var_names[match(input$stat,var_labels)]
            
    
            
            #list of positions
            position_listx <- as.data.frame(c("Goalkeeper","Defender","Midfielder","Forward"))
            position_list <- cbind(position_listx,c(1:4))
            colnames(position_list) <- c("position","element_type")
            
            
            nrow(unlist_json_players)
            
            
            fpl_start <- input$min_gw[1]
            fpl_end <- input$min_gw[2]
            
            player_data2 <- unlist_json_players[unlist_json_players$round>=fpl_start&unlist_json_players$round<=fpl_end,]
            
            player_team_index <- json_players_det[,c("team","id","web_name","element_type","now_cost")]
            colnames(player_team_index)[2] <-c("newid")
            colnames(player_team_index)[5] <-c("cost")
            player_team_index$cost <- player_team_index$cost/10
            player_data_x <- player_data2[,c("newid",fpl_stat_use)]
            colnames(player_data_x) <- c("newid","stat")
            player_data3 <- player_data_x %>% group_by(newid) %>% summarise(mean = mean(stat),total=sum(stat)) %>% 
                arrange(desc(mean)) %>% inner_join(player_team_index) %>% inner_join(fpl_team_list) %>%
                inner_join(position_list)
            player_data3$mean <- round(player_data3$mean,2)
            colnames(player_data3)[2] <- paste0("Average ",fpl_stat_use)
            
            colnames(player_data3)[3] <- paste0("Total ",fpl_stat_use)
           player_data4 <- player_data3[,-c(1,4,6)]
           player_data5 <- player_data4[,c(3,4,5,6,1,2)]
            colnames(player_data5)[c(1:4)] <- c("Player name","Cost","Team name","Position")
            player_data6 <- player_data5[player_data5$Cost >=input$cost[1]&player_data5$Cost <= input$cost[2],]
            if (input$pos != ""){
                player_data6 <- player_data6 %>%filter(`Position`==input$pos)   
            }
            if (input$tm != ""){
                player_data6 <- player_data6 %>%filter(`Team name`==input$tm)   
            }
            

                        return(player_data6)
        })  
        output$fpl_table <- DT::renderDataTable(stat_table(),rownames=FALSE,options=list(searching=FALSE,encoding="UTF-8",pageLength=20,lengthMenu=c(10,20,50,100)))
    
        team_table <- reactive({
            #this part creates the team ranking
            json_teams<- json_teams[json_teams$gw>=input$team_gw[1]&json_teams$gw<=input$team_gw[2],]
            home_diff <- json_teams %>% group_by(home_team) %>% summarise(home_diff_sum=sum(home_diff),home_count=n())
            away_diff <- json_teams %>% group_by(away_team) %>% summarise(away_diff_sum=sum(away_diff),away_count=n())
            total_diff <- full_join(home_diff,away_diff,by=c("home_team"="away_team")) 
            total_diff[is.na(total_diff)] <-0
            total_diff <- total_diff %>% 
                mutate(total_diff_sum=home_diff_sum+away_diff_sum,total_count=home_count+away_count)%>%
                mutate(diff_rank = total_diff_sum / total_count)
            colnames(total_diff)[1] <- c("team")
            total_diff <- full_join(total_diff,fpl_team_list18) %>% arrange(diff_rank)
            total_diff_x <- total_diff[,c("team_name","diff_rank","total_count","home_count","away_count")]
            colnames(total_diff_x) <- c("Team name","Average difficulty ranking","Total count","Home game count","Away games count")
            total_diff_x[is.na(total_diff_x)] <- 0
            
            ##this part determines the list of fixtures for each team
           json_teams2 <- select(json_teams,home_team,away_team,home_diff,away_diff,gw)
            json_teams3 <- json_teams2
            colnames(json_teams3) <-c("team","opponent","home_diff","away_diff","gw")
            json_teams3$home <- c(" (H) ")
            json_teams4 <- json_teams2
            colnames(json_teams4) <-c("opponent","team","home_diff","away_diff","gw")
            json_teams4$home <- c(" (A) ")
            
            team_games <-rbind(json_teams3,json_teams4) %>%arrange(gw) %>% inner_join(fpl_team_list18) %>%
                inner_join(fpl_team_list182) %>%mutate(opp_home=paste0(opponent_name,home))
            
            team_list <- lapply(fpl_team_list18[,1],function(x){
                temp_data_new <-team_games[team_games$team_name==x,]
                game_list = toString(temp_data_new$opp_home)
                return(game_list)
            })
            
            team_list2 <- as.data.frame(unlist(team_list))
            team_list3 <- cbind(team_list2,fpl_team_list18[,1])
            colnames(team_list3) <-c("Opponent list","Team name")
            total_diff_x2 <- total_diff_x %>%inner_join(team_list3)
            return(total_diff_x2)
            
        })
        output$team_table <-DT::renderDataTable(team_table(),rownames=FALSE,
                                                options=list(searching=FALSE,paging=FALSE))
    })
        
     
      
    
