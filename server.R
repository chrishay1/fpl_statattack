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
        #2017/18 player statistics
        stat_table <- reactive({
            
            fpl_stat_use <-var_names[match(input$stat,var_labels)]
            
    
            
            #list of positions
            position_listx <- as.data.frame(c("Goalkeeper","Defender","Midfielder","Forward"))
            position_list <- cbind(position_listx,c(1:4))
            colnames(position_list) <- c("position","element_type")
            
            
            
            fpl_start <- input$min_gw[1]
            fpl_end <- input$min_gw[2]
            
            player_data2 <- unlist_json_players[unlist_json_players$round>=fpl_start&unlist_json_players$round<=fpl_end,]
            
            player_team_index <- json_players_det[,c("team","id","web_name","element_type","now_cost")]
            colnames(player_team_index)[2] <-c("newid")
            colnames(player_team_index)[5] <-c("cost")
            player_team_index$cost <- player_team_index$cost/10
            player_data_x <- player_data2[,c("newid",fpl_stat_use,"minutes")]
            colnames(player_data_x) <- c("newid","stat","minutes")
            player_data3 <- player_data_x %>% mutate(Appearances=if_else(minutes > 0,1,0))%>%
                group_by(newid) %>% summarise(mean = mean(stat),total=sum(stat),Appearances=sum(Appearances)) %>% 
                mutate(stats_per_app = total/Appearances) %>%arrange(desc(mean)) %>% 
                inner_join(player_team_index) %>% inner_join(fpl_team_list) %>%
                inner_join(position_list)
            player_data3$mean <- round(player_data3$mean,2)
            colnames(player_data3)[2] <- paste0("Average ",input$stat)
            
            colnames(player_data3)[3] <- paste0(input$stat)
            
            colnames(player_data3)[5] <- paste0("Average ",input$stat," per appearance")
            player_data3[,5] <- round(player_data3[,5],2)
            player_data4 <- player_data3[,-c(1,6,8)]
            
            player_data5 <- player_data4[,c(5,6,7,8,1,2,3,4)]
            #apply some filters
            colnames(player_data5)[c(1:4)] <- c("Player name","Cost","Team name","Position")
            player_data6 <- player_data5[player_data5$Cost >=input$cost[1]&player_data5$Cost <= input$cost[2],]
            if (input$pos != "None"){
                player_data6 <- player_data6 %>%filter(`Position`==input$pos)   
            }
            if (input$tm != "None"){
                player_data6 <- player_data6 %>%filter(`Team name`==input$tm)   
            }
            

                        return(player_data6)
        })  
        output$fpl_table <- DT::renderDataTable(stat_table(),rownames=FALSE,options=list(searching=FALSE,encoding="UTF-8",pageLength=20,lengthMenu=c(10,20,50,100)))
    
        output$downloadData <- downloadHandler(
            filename = function() {
                paste(input$stat, ".csv", sep = "")
            },
            content = function(file) {
                write.csv(stat_table(), file, row.names = FALSE)
            }
        )
        
        #2018/19 player statistics
        
        stat_table_1819 <- reactive({
            
            fpl_stat_use <-var_names[match(input$stat_new,var_labels)]
            
            
            
            #list of positions
            position_listx <- as.data.frame(c("Goalkeeper","Defender","Midfielder","Forward"))
            position_list <- cbind(position_listx,c(1:4))
            colnames(position_list) <- c("position","element_type")
            
            
           
            
            fpl_start <- input$min_gw_new[1]
            fpl_end <- input$min_gw_new[2]
            
            player_data2 <- unlist_json_players_1819[unlist_json_players_1819$round>=fpl_start&unlist_json_players_1819$round<=fpl_end,]
            
            player_team_index <- json_players_det_1819[,c("team","id","web_name","element_type","now_cost")]
            colnames(player_team_index)[2] <-c("newid")
            colnames(player_team_index)[5] <-c("cost")
            player_team_index$cost <- player_team_index$cost/10
            player_data_x <- player_data2[,c("newid",fpl_stat_use,"minutes")]
            colnames(player_data_x) <- c("newid","stat","minutes")
            player_data3 <- player_data_x %>% mutate(Appearances=if_else(minutes > 0,1,0))%>%
                group_by(newid) %>% summarise(mean = mean(stat),total=sum(stat),Appearances=sum(Appearances)) %>% 
                mutate(stats_per_app = total/Appearances) %>%arrange(desc(mean)) %>% 
                inner_join(player_team_index) %>% inner_join(fpl_team_list18) %>%
                inner_join(position_list)
            player_data3$mean <- round(player_data3$mean,2)
            colnames(player_data3)[2] <- paste0("Average ",input$stat_new)
            
            colnames(player_data3)[3] <- paste0(input$stat_new)
            
            colnames(player_data3)[5] <- paste0("Average ",input$stat_new," per appearance")
            player_data3[,5] <- round(player_data3[,5],2)
            player_data4 <- player_data3[,-c(1,6,8)]
            
            player_data5 <- player_data4[,c(5,6,7,8,1,2,3,4)]
            #apply some filters
            colnames(player_data5)[c(1:4)] <- c("Player name","Cost","Team name","Position")
            player_data6 <- player_data5[player_data5$Cost >=input$cost_new[1]&player_data5$Cost <= input$cost_new[2],]
            if (input$pos_new != "None"){
                player_data6 <- player_data6 %>%filter(`Position`==input$pos_new)   
            }
            if (input$tm_new != "None"){
                player_data6 <- player_data6 %>%filter(`Team name`==input$tm_new)   
            }
            
            
            return(player_data6)
        })  
        output$fpl_table_1819 <- DT::renderDataTable(stat_table_1819(),rownames=FALSE,options=list(searching=FALSE,encoding="UTF-8",pageLength=20,lengthMenu=c(10,20,50,100)))
        
        
        output$downloadData_new <- downloadHandler(
            filename = function() {
                paste(input$stat_new, ".csv", sep = "")
            },
            content = function(file) {
                write.csv(stat_table_1819(), file, row.names = FALSE)
            }
        )
        #team difficulty statistics
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
            total_diff_x$diff_rank <- round(total_diff_x$diff_rank,2)
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
   

    #player predictions        
    player_pred_table <- reactive({
        #need to multiply the clean sheet etc likelihood by the likelihood of playing
        
        player_preds$adjud_cs_likelihood <- player_preds$play_likelihood * player_preds$cs_likelihood
        player_preds$adjud_goal_likelihood <- player_preds$play_likelihood * player_preds$goal_likelihood
        player_preds$adjud_assist_likelihood <- player_preds$play_likelihood * player_preds$assist_likelihood
        
        
                
        highest_points <- player_preds %>%filter(event>=input$pred_filter[1] &event<=input$pred_filter[2])%>%
            group_by(newid,web_name,team_name,position,value)%>%
            summarise(expected_points=sum(expected_points)/n(),
                      play_likelihood=sum(play_likelihood)/n(),
                      cs_likelihood= sum(adjud_cs_likelihood)/n(),
                      goal_likelihood=sum(adjud_goal_likelihood)/n(),
                      assist_likelihood=sum(adjud_assist_likelihood)/n()) %>%
            mutate(points_per_value=expected_points/value) %>%
           arrange(desc(expected_points))
        
        highest_points$expected_points <- round(highest_points$expected_points,2)
        highest_points$play_likelihood <- round(highest_points$play_likelihood,2)
        highest_points$cs_likelihood <- round(highest_points$cs_likelihood,2)
        highest_points$goal_likelihood <- round(highest_points$goal_likelihood,2)
        highest_points$assist_likelihood <- round(highest_points$assist_likelihood,2)
        highest_points$points_per_value <- round(highest_points$points_per_value*100,2)
        highest_points$value <- highest_points$value/10
        highest_points <- highest_points[,-1]
        
     
         colnames(highest_points) <- c("Name","Team name","Position","Cost","Average expected points",
                                       "Average likelihood of playing","Average likelihood of a clean sheet",
                                       "Average likelihood of a goal","Average likelihood of an assist",
                                       "Average expected points per cost")
         
         highest_points <- highest_points[highest_points$Cost >=input$pred_cost[1]&highest_points$Cost <= input$pred_cost[2],]
         if (input$pred_pos != "None"){
             highest_points <- highest_points %>%filter(`Position`==input$pred_pos)   
         }
         if (input$pred_tm != "None"){
             highest_points <- highest_points %>%filter(`Team name`==input$pred_tm)   
         }
         highest_points  <- datatable(highest_points,
         rownames=FALSE,
         options=list(searching=FALSE,encoding="UTF-8",
                      pageLength=20,lengthMenu=c(10,20,50,100)))
        highest_points <- formatPercentage(highest_points,c( 'Average likelihood of playing','Average likelihood of a clean sheet',
                                                             'Average likelihood of a goal','Average likelihood of an assist'),1)

        return(highest_points)  
        
    })
    output$player_preds <-DT::renderDataTable(player_pred_table())
    })
    
