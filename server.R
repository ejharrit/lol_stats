library(shiny)
library(gghighlight)
library(fmsb)
library(gridExtra)
library(tidyverse)
library(utf8)
source("dis_theme.R")
source("datainstate_cols.R")
lol_data <- read_csv(paste("shinyappdata")) %>%
  mutate_if(is.character, utf8_encode)

server <- function(input, output, session) {
  observeEvent(
    input$player,
    updateSelectInput(session, "role", "select your player's role", 
                      choices = unique(lol_data %>%
                                         filter(name == input$player)%>%
                                         pull(role)))
  )
  observeEvent(
    input$role,
    updateSelectInput(session, "opp_name", "select your player's opponent",
                      choices = unique(lol_data %>%
                                         filter(name == input$player &
                                                  role == input$role)%>%
                                         pull(opp_name)))
  )
  observeEvent(
    input$opp_name,
    updateSelectInput(session, "team", "select your player's team",
                      choices = unique(lol_data %>%
                                         filter(name == input$player &
                                                  role == input$role &
                                                  opp_name == input$opp_name)%>%
                                         pull(team)))
  )
  observeEvent(
    input$team,
    updateSelectInput(session, "team_vs", "select your player's opposing team",
                      choices = unique(lol_data %>%
                                         filter(name == input$player &
                                                  role == input$role &
                                                  opp_name == input$opp_name &
                                                  team == input$team)%>%
                                         pull(team_vs)))
  )
  output$radar <- renderPlot({
    r1_plot <- lol_data %>%
      filter(name == input$player | name == input$opp_name) %>%
      select(kills, deaths, assists, cs, gold, team_kills, team_gold,
             points, name) %>% 
      mutate(kda = ifelse((kills+assists)/deaths == Inf, (kills+assists), 
                          (kills+assists)/deaths), 
             ks_pct = kills/team_kills, gld_pct = gold/team_gold) %>%
      select(kills, deaths, assists, cs, gold, points, kda, ks_pct,
             gld_pct, name) %>%
      group_by(name) %>%
      summarize(kills = mean(kills, na.rm = T)/5.9, 
                deaths = mean(deaths, na.rm = T)/6, 
                assists = mean(assists, na.rm = T)/11.3, 
                kda = mean(kda, na.rm = T)/8.75,
                ks_pct = mean(ks_pct, na.rm = T)/0.4, 
                cs = mean(cs, na.rm = T)/337.9, 
                gold = mean(gold, na.rm = T)/15500, 
                gld_pct = mean(gld_pct, na.rm = T)/.3, 
                points = mean(points, na.rm = T)/35.1) %>%
      gather(key = "stat", value = "value",-name) %>%
      arrange(stat) %>%
      ggplot(aes(x = stat, y = value, group = name, fill = name, color = name))+
      scale_fill_datainstate()+
      scale_color_datainstate()+
      theme_set(theme_dis)+
      geom_point()+
      geom_polygon(alpha = 0.5, rule = "winding")+
      coord_polar()+
      theme(axis.title = element_blank(),
            legend.position = "none",
            axis.text.y = element_blank())+
      ggtitle("Career average")
    
    r2_plot <- lol_data %>%
      filter((name == input$player & current_year == TRUE) | 
               (name == input$opp_name & current_year == TRUE)) %>%
      select(kills, deaths, assists, cs, gold, team_kills, team_gold,
             points, name) %>% 
      mutate(kda = ifelse((kills+assists)/deaths == Inf, (kills+assists), 
                          (kills+assists)/deaths), 
             ks_pct = kills/team_kills, gld_pct = gold/team_gold) %>%
      select(kills, deaths, assists, cs, gold, points, kda, ks_pct,
             gld_pct, name) %>%
      group_by(name) %>%
      summarize(kills = mean(kills, na.rm = T)/5.9, 
                deaths = mean(deaths, na.rm = T)/6, 
                assists = mean(assists, na.rm = T)/11.3, 
                kda = mean(kda, na.rm = T)/8.75,
                ks_pct = mean(ks_pct, na.rm = T)/0.4, 
                cs = mean(cs, na.rm = T)/337.9, 
                gold = mean(gold, na.rm = T)/15500, 
                gld_pct = mean(gld_pct, na.rm = T)/.3, 
                points = mean(points, na.rm = T)/35.1) %>%
      gather(key = "stat", value = "value",-name) %>%
      arrange(stat) %>%
      ggplot(aes(x = stat, y = value, group = name, fill = name, color = name))+
      scale_fill_datainstate()+
      scale_color_datainstate()+
      theme_set(theme_dis)+
      geom_point()+
      geom_polygon(alpha = 0.5, rule = "winding")+
      coord_polar()+
      theme(axis.title = element_blank(),
            legend.position = "none",
            axis.text.y = element_blank())+
      ggtitle("2020 average")
    
    r3_plot <- lol_data %>%
      filter((name == input$player & opp_name == input$opp_name) | 
               (name == input$opp_name & opp_name == input$player)) %>%
      select(kills, deaths, assists, cs, gold, team_kills, team_gold,
             points, name) %>% 
      mutate(kda = ifelse((kills+assists)/deaths == Inf, (kills+assists), 
                          (kills+assists)/deaths), 
             ks_pct = kills/team_kills, gld_pct = gold/team_gold) %>%
      select(kills, deaths, assists, cs, gold, points, kda, ks_pct,
             gld_pct, name) %>%
      group_by(name) %>%
      summarize(kills = mean(kills, na.rm = T)/5.9, 
                deaths = mean(deaths, na.rm = T)/6, 
                assists = mean(assists, na.rm = T)/11.3, 
                kda = mean(kda, na.rm = T)/8.75,
                ks_pct = mean(ks_pct, na.rm = T)/0.4, 
                cs = mean(cs, na.rm = T)/337.9, 
                gold = mean(gold, na.rm = T)/15500, 
                gld_pct = mean(gld_pct, na.rm = T)/.3, 
                points = mean(points, na.rm = T)/35.1) %>%
      gather(key = "stat", value = "value",-name) %>%
      arrange(stat) %>%
      ggplot(aes(x = stat, y = value, group = name, fill = name, color = name))+
      scale_fill_datainstate()+
      scale_color_datainstate()+
      theme_set(theme_dis)+
      geom_point()+
      geom_polygon(alpha = 0.5, rule = "winding")+
      coord_polar()+
      theme(axis.title = element_blank(),
            axis.text.y = element_blank())+
      ggtitle("Vs average")
    
    
    grid.arrange(grobs = list(r1_plot, r2_plot, r3_plot), ncol = 3)
  })  
  
  output$histogram <- renderPlot({
    lol_data %>% 
      filter(name == input$player) %>%
      mutate(opp = ifelse(opp_name == input$opp_name, TRUE, FALSE)) %>%
      ggplot(aes(x = points, fill = opp))+
      geom_histogram() +
      scale_fill_datainstate(palette = "grn_ylw")+
      gghighlight(opp_name == input$opp_name)+
      theme_set(theme_dis)
  })
  
  output$table <- renderTable({
    
    player_avg_stats <-lol_data %>% 
      filter(name == input$player) %>% 
      select(kills, deaths, assists, cs, gold, team_kills, team_gold,
             points) %>% 
      mutate(kda = ifelse((kills+assists)/deaths == Inf, (kills+assists), 
                          (kills+assists)/deaths), 
             kp_pct = (kills+assists)/team_kills,
             ks_pct = kills/team_kills, gld_pct = gold/team_gold) %>%
      summarize(Stat = "Career Avg",
                points = mean(points, na.rm = T),
                kills = mean(kills, na.rm = T), 
                deaths = mean(deaths, na.rm = T), 
                assists = mean(assists, na.rm = T), kda = mean(kda, na.rm = T),
                kp_pct = mean(kp_pct, na.rm = T), 
                ks_pct = mean(ks_pct, na.rm = T), cs = mean(cs, na.rm = T), 
                gold = mean(gold, na.rm = T), 
                gld_pct = mean(gld_pct, na.rm = T), 
                team_kills = mean(team_kills, na.rm = T))
    
    player_avg_stats_yr <-lol_data %>% 
      filter(name == input$player & current_year == T) %>% 
      select(kills, deaths, assists, cs, gold, team_kills, team_gold,
             points) %>% 
      mutate(kda = ifelse((kills+assists)/deaths == Inf, (kills+assists), 
                          (kills+assists)/deaths), 
             kp_pct = (kills+assists)/team_kills,
             ks_pct = kills/team_kills, gld_pct = gold/team_gold) %>%
      summarize(Stat = "2020 Avg",
                points = mean(points, na.rm = T),
                kills = mean(kills, na.rm = T), 
                deaths = mean(deaths, na.rm = T), 
                assists = mean(assists, na.rm = T), kda = mean(kda, na.rm = T),
                kp_pct = mean(kp_pct, na.rm = T), 
                ks_pct = mean(ks_pct, na.rm = T), cs = mean(cs, na.rm = T), 
                gold = mean(gold, na.rm = T), 
                gld_pct = mean(gld_pct, na.rm = T), 
                team_kills = mean(team_kills, na.rm = T))
    
    opp_avg_stats_yr <-lol_data %>% 
      filter(name == input$opp_name & current_year == T) %>% 
      select(kills, deaths, assists, cs, gold, team_kills, team_gold,
             points) %>% 
      mutate(kda = ifelse((kills+assists)/deaths == Inf, (kills+assists), 
                          (kills+assists)/deaths), 
             kp_pct = (kills+assists)/team_kills,
             ks_pct = kills/team_kills, gld_pct = gold/team_gold) %>%
      summarize(Stat = "Opp 2020 Avg",
                points = mean(points, na.rm = T),
                kills = mean(kills, na.rm = T), 
                deaths = mean(deaths, na.rm = T), 
                assists = mean(assists, na.rm = T), kda = mean(kda, na.rm = T),
                kp_pct = mean(kp_pct, na.rm = T), 
                ks_pct = mean(ks_pct, na.rm = T), cs = mean(cs, na.rm = T), 
                gold = mean(gold, na.rm = T), 
                gld_pct = mean(gld_pct, na.rm = T), 
                team_kills = mean(team_kills, na.rm = T))
    
    playervs_avg_stats <-lol_data %>% 
      filter(name == input$player & opp_name == input$opp_name) %>% 
      select(kills, deaths, assists, cs, gold, team_kills, team_gold,
             points) %>% 
      mutate(kda = ifelse((kills+assists)/deaths == Inf, (kills+assists), 
                          (kills+assists)/deaths), 
             kp_pct = (kills+assists)/team_kills,
             ks_pct = kills/team_kills, gld_pct = gold/team_gold) %>%
      summarize(Stat = "Avg vs Opp",
                points = mean(points, na.rm = T),
                kills = mean(kills, na.rm = T), 
                deaths = mean(deaths, na.rm = T), 
                assists = mean(assists, na.rm = T), kda = mean(kda, na.rm = T),
                kp_pct = mean(kp_pct, na.rm = T), 
                ks_pct = mean(ks_pct, na.rm = T), cs = mean(cs, na.rm = T), 
                gold = mean(gold, na.rm = T), 
                gld_pct = mean(gld_pct, na.rm = T), 
                team_kills = mean(team_kills, na.rm = T))
    
    stat_table <- bind_rows(player_avg_stats, player_avg_stats_yr,
                            opp_avg_stats_yr, playervs_avg_stats)
    
  })
  
}