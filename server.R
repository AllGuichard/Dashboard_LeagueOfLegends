setwd("C:/Users/remil/Desktop/M2/shiny/app_lol/LoLProject/")
source('share_load.R')


server <- function(input, output, session) {
  
  
  
  
  data2 <- reactive({
    if (input$choix_classe == "All") {
      df_stat
    } else {
      df_stat %>%
        filter(ligue == input$choix_classe)
    }
    
  })
  
  
  output$nbGame <- renderValueBox({
    valueBox(sum(data2()$nb_game), 
             "Nombre total de parties jouÃ©es", 
             icon = icon("book", verify_fa = FALSE, fill = TRUE),
             color = "yellow")
  })
  
  output$meanKD <- renderValueBox({
    valueBox(round(median(data2()$kd_rate),2), 
             "Median du ratio K/D", 
             icon = icon("skull", verify_fa = FALSE, fill = TRUE), 
             color = "yellow")
  })
  
  output$meanwinrate <- renderValueBox({
    valueBox(round(mean(data2()$winrate),2), 
             "Moyenne du % de victoire", 
             icon = icon("percentage", verify_fa = FALSE, fill = TRUE), 
             color = "yellow")
  })
  
  output$globalTable <- function() {
    df_global %>% 
      group_by(champion) %>% 
      summarise(Rate = .data[[input$select_df]]) %>% 
      arrange(desc(Rate)) %>% 
      head(10) %>% 
      knitr::kable("html", col.names = c("Champion", "Rate in %")) %>% 
      kable_styling("striped", full_width = F)
  }
  
  output$finalPlot <- renderPlot({
    
    df_global %>% 
      group_by(champion) %>% 
      summarise(rate = .data[[input$select_y]]) %>% 
      arrange(desc(rate)) %>% 
      head(10) %>%
      ggplot(df_global, mapping = aes(reorder(champion, rate), rate)) + 
      geom_bar(stat = "identity",
               fill = "#feb24c", 
               alpha = 0.7, 
               width = 0.8) + 
      xlab("") + ylab(input$select_y) +
      ggtitle("Top 10 rated legends \n in percentage") +
      coord_flip() +
      theme_minimal() +
      theme(
        axis.title = element_text(size = (13)),
        axis.text.y = element_text(size = (12)),
        axis.text.x = element_text(size = (12), face = "bold")
      )
    
    
  })
  
  df <- reactive({
    if (input$season_choose == "All") {
      df_summoners
    } else {
      df_summoners %>%
        filter(saison == input$season_choose)
    }
    
  })
  
  output$v1box <- renderValueBox({
    valueBox(
      value = sum(df()$nb_games),
      subtitle = paste("Games played")
    )
  })
  
  output$v2box <- renderValueBox({
    valueBox(value = paste(round(mean(df()$avg_kills), 1), 
                           round(mean(df()$avg_deaths), 1), 
                           round(mean(df()$avg_assists), 1), 
                           sep = " / "),
             subtitle = "Average K/D/A")
  })
  
  
  output$v3box <- renderValueBox({
    valueBox(
      value = sum(df()[df()$champion == "Lux",]$nb_games),
      subtitle = "Games played"
    )
  })
  
  output$summary <- function() {
    if (input$filter_choose == "Best winrates") {
      df() %>% 
        group_by(champion) %>% 
        filter(nb_games >= 5) %>% 
        summarise(Games = sum(nb_games), 
                  Winrate = round(mean(winrate), 2),
                  AverageKills = round(mean(avg_kills),2),
                  AverageDeaths = round(mean(avg_deaths),2),
                  AverageAssists = round(mean(avg_assists),2)
        ) %>% 
        arrange(desc(Winrate)) %>% 
        head(10) %>% 
        knitr::kable("html") %>% 
        kable_styling("striped", full_width = F)
    } else {
      df() %>% 
        group_by(champion) %>% 
        summarise(Games = sum(nb_games), 
                  Winrate = round(mean(winrate), 2),
                  AverageKills = round(mean(avg_kills),2),
                  AverageDeaths = round(mean(avg_deaths),2),
                  AverageAssists = round(mean(avg_assists),2)
        ) %>% 
        arrange(desc(Games)) %>% 
        head(10) %>% 
        knitr::kable("html") %>% 
        kable_styling("striped", full_width = F)
    }
  }
  
  output$plot <- renderPlot({
    if(input$filter_choose == "Best winrates") {
      df() %>% 
        group_by(champion) %>%
        filter(nb_games >= 5) %>% 
        summarise(nb_games = sum(nb_games), winrate = mean(winrate), avg = mean(.data[[input$ycol]])) %>% 
        arrange(desc(winrate)) %>% 
        head(10) %>% 
        ggplot(., mapping = aes(reorder(champion, avg), avg)) +
        geom_bar(stat = 'identity', width = 0.5) + 
        labs(y = input$ycol, x = "Champion") + 
        coord_flip() +
        theme_minimal()
    } else {
      df() %>% 
        group_by(champion) %>%
        filter(nb_games >= 5) %>% 
        summarise(nb_games = sum(nb_games), winrate = mean(winrate), avg = mean(.data[[input$ycol]])) %>% 
        arrange(desc(nb_games)) %>% 
        head(10) %>% 
        ggplot(., mapping = aes(reorder(champion, avg), avg)) +
        geom_bar(stat = 'identity', width = 0.5) + 
        labs(y = input$ycol, x = "Champion") + 
        coord_flip() +
        theme_minimal()
    }
    
  })
  
  output$radar <- renderPlot({
    p <- df() %>% 
      group_by(role) %>% 
      summarise(tot_games = sum(nb_games)) %>% 
      select(role, tot_games)
    
    maxg <- sum(df()$nb_games)
    
    p <- p %>% 
      pivot_wider(names_from = role, values_from = tot_games)
    
    max_min <- data.frame(
      Bot = c(maxg, 0), Jungle = c(maxg, 0), 
      Mid = c(maxg, 0), Support = c(maxg, 0),
      Top = c(maxg, 0)
    )
    
    p1 <- rbind(max_min, p)
    
    rownames(p1) <- c("Max", "Min", "Value")
    radarchart(p1)
  })
  
  
  output$radarcomp <- renderPlot({
    
    test <- df() %>% 
      filter(champion == input$champ_choose) %>% 
      summarise(sbire = mean(nb_kill_mobs), gold = mean(gold), kd_rate = mean(kd_rate), winrate = mean(winrate)) %>% 
      select(sbire, gold, kd_rate, winrate)
    
    max_min2 <- data.frame(
      sbire = c(max(df_summoners$nb_kill_mobs), 0), gold = c(max(df_summoners$gold), 0), 
      kd_rate = c(max(df_summoners$kd_rate), 0), winrate = c(100, 0)
    )
    
    diam <- df_stat %>% 
      filter(champion == input$champ_choose & ligue == "Diamond") %>% 
      summarise(sbire = mean(nb_kill_mobs), gold = mean(gold), kd_rate = mean(kd_rate), winrate = mean(winrate)) %>% 
      select(sbire, gold, kd_rate, winrate)
    
    master <- df_stat %>% 
      filter(champion == input$champ_choose & ligue == "Master") %>% 
      summarise(sbire = mean(nb_kill_mobs), gold = mean(gold), kd_rate = mean(kd_rate), winrate = mean(winrate)) %>% 
      select(sbire, gold, kd_rate, winrate)
    
    test <- rbind(max_min2, test, diam, master)
    
    rownames(test) <- c("Max", "Min", "You", "Diamond", "Master")
    
    colors <- c("#00AFBB", "#E7B800", "#FC4E07")
    titles <- c("You", "Diamond", "Master")
    
    op <- par(mar = c(1, 1, 1, 1))
    par(mfrow = c(1,3))
    
    for(i in 1:3){
      create_beautiful_radarchart(
        data = test[c(1, 2, i+2), ],
        color = colors[i], title = titles[i],
        vlabels = c("Sbire", "Gold", "KDA", "WR")
      )
    }
    par(op)
    
  })
  
  data <- reactive({
    subset(
      df_stat,
      ligue == input$choixLigue
    )
  })    
  
  output$firstwinrate <- renderValueBox({
    valueBox(max(data()$winrate), 
             "Best winrated champion", 
             icon = icon("percent", verify_fa = FALSE, fill = TRUE),
             color = "yellow")
  })
  
  output$firstnbgame <- renderValueBox({
    valueBox(max(data()$nb_game), 
             "Most played champion", 
             icon = icon("fire", verify_fa = FALSE, fill = TRUE),
             color = "yellow")
  })
  
  output$firstkdrate <- renderValueBox({
    valueBox(max(data()$kd_rate), 
             "Best K/D champion's ratio", 
             icon = icon("skull-crossbones", verify_fa = FALSE, fill = TRUE),
             color = "yellow")
  })
  
  output$firstgold <- renderValueBox({
    valueBox(max(data()$gold), 
             "Best gold farming champion", 
             icon = icon("coins", verify_fa = FALSE, fill = TRUE),
             color = "yellow")
  })
  
  output$firstKillmob <- renderValueBox({
    valueBox(max(data()$nb_kill_mobs), 
             "Best mobs farming champion", 
             icon = icon("optin-monster", verify_fa = FALSE, fill = TRUE),
             color = "yellow")
  })
  
  output$rankedPlot <- renderPlot({
    data() %>% 
      group_by(champion) %>% 
      summarise(rate = .data[[input$select_var]]) %>% 
      arrange(desc(rate)) %>% 
      head(10) %>% 
      ggplot(data(), mapping = aes(reorder(champion, rate), rate)) +
      geom_bar(stat = "identity",
               fill = "#feb24c", 
               alpha = 0.7,
               width = 0.4,
      ) + 
      xlab("") + ylab(input$select_var) +
      ggtitle("Top 10 rated champions") +
      coord_flip() +
      theme_minimal() +
      theme(
        axis.title = element_text(size = (13)),
        axis.text.y = element_text(size = (12)),
        axis.text.x = element_text(size = (12), face = "bold")
      )
  })
  
  output$bubblePlot <- renderPlot({
    data() %>% 
      ggplot(., aes(x = gold, y = nb_kill_mobs, size = winrate)) + 
      geom_point(alpha = 0.5,
                 colour = "orange") + 
      scale_color_viridis(discrete = TRUE, guide = "none") + 
      xlab("Montant d'Or obtenue") + ylab("Nombre de sbire tuÃ©") +
      ggtitle("CorrrÃ©lation entre le taux de victoire \n et le nombre de sbire tuÃ© ainsi que les gold rÃ©coltÃ©s") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = (13)),
        axis.text.y = element_text(size = (12)),
        axis.text.x = element_text(size = (12)),
      )
  })
  
  df2 <- reactive({
    if (input$season_choose_2 == "All") {
      df_summoners
    } else {
      df_summoners %>% 
        filter(saison == input$season_choose_2)
    }
  })
  
  output$v3box <- renderValueBox({
    valueBox(
      value = sum(df2()[df2()$champion == input$champ_choose,]$nb_games),
      subtitle = "Games played"
    )
  })
  
  
  output$v4box <- renderValueBox({
    df_diamond <- df_stat %>% 
      filter(ligue == "Diamond") 
    valueBox(
      value = paste(round(mean(df2()[df2()$champion == input$champ_choose,]$kd_rate), 2),
                    df_diamond[df_diamond$champion == input$champ_choose,]$kd_rate, 
                    sep = " vs "), 
      subtitle = "You vs Diamond average K/D/A"
    )
  })
  
  output$v5box <- renderValueBox({
    df_diamond <- df_stat %>% 
      filter(ligue == "Diamond") 
    valueBox(
      value = paste(round(mean(df2()[df2()$champion == input$champ_choose,]$winrate), 2),
                    df_diamond[df_diamond$champion == input$champ_choose,]$winrate,
                    sep = " vs "), 
      subtitle = "You vs Diamon winrate"
    )
  })
  
  output$picture <- renderImage({
    champ_image <- normalizePath(file.path('C:/Users/remil/Desktop/M2/shiny/app_lol/LoLProject/www/',
                                           paste(input$champ_choose, '.png', sep='')))
    
    
    
    list(src = champ_image,
         width = 150,
         height = 150)}, deleteFile = FALSE)
  
  output$info_champ <- renderUI({
    
    str1 <- paste("<FONT SIZE = '5pt'><B>Name:<B>", input$champ_choose, sep = " ")
    str2 <- paste("<B>Race:<B>", df_bio[df_bio$champion == input$champ_choose,]$race, sep = " ")
    str3 <- paste("<B>Type:<B>", df_bio[df_bio$champion == input$champ_choose,]$type, sep = " ")
    str4 <- paste("<B>Region:<B>", df_bio[df_bio$champion == input$champ_choose,]$region, sep = " ")
    
    HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
    
  })
  
  output$bio_champ <- renderUI({
    HTML(paste("<FONT SIZE = '3pt'><B>Biography:<B>", df_bio[df_bio$champion == input$champ_choose,]$bio, sep = '<br/>'))
    
  })
  
}

