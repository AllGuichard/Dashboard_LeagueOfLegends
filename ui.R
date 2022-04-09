setwd("C:/Users/remil/Desktop/M2/shiny/app_lol/LoLProject/")
source('share_load.R')


sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebar",
    menuItem("Global", tabName = "global", icon = icon("globe")),
    div(id = 'sidebar_cr',
        conditionalPanel("input.sidebar === 'global'",
                         selectInput("choix_classe",
                                     "Choisissez une ligue",
                                     choices = c("All", levels(df_stat$ligue)),
                                     selected = "All",
                                     multiple = FALSE))),
    
    menuItem("Summoners", tabName = "summoners", icon = icon("address-card")),
    div(id = 'sidebar_cr',
        conditionalPanel("input.sidebar === 'summoners'",
                         selectInput("filter_choose",
                                     "Select filter", 
                                     choices =  c("Best winrates", "Most played"), 
                                     selected = "Best winrates",  width = "200px"),
                         selectInput("season_choose",
                                     "Choose season", 
                                     choices = c("All", levels(df_summoners$saison)),
                                     selected = "All",
                                     multiple = FALSE))),
    
    menuItem("Ranked", tabName = "ranked", icon = icon("trophy")),
    div(id = "sidebar_cr",
        conditionalPanel("input.sidebar === 'ranked'",
                         selectInput(inputId = "choixLigue",
                                     label = "Ranked League",
                                     choices = unique(df_stat$ligue)
                         ),
                         selectInput(inputId = "select_var", 
                                     label = "Choose the graphic", 
                                     choices = colnames(df_stat[, c(3:7)]),
                                     selected = "winrate")
                         
        )),
    
    menuItem("Champion", tabName = "champ"),
    div(id = 'sidebar_cr',
        conditionalPanel("input.sidebar === 'champ'",
                         selectInput("champ_choose",
                                     "Select a champion", 
                                     choices =  unique(df_summoners$champion), 
                                     width = "200px",
                                     selected = "Akshan"),
                         selectInput("season_choose_2",
                                     "Choose season",
                                     choices = c("All", levels(df_summoners$saison)),
                                     selected = "All",
                                     multiple = FALSE)
        )
    )
    
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "global",
            h2("Global content on LOL champions"), 
            fluidRow(
              valueBoxOutput("nbGame", width = 4),
              valueBoxOutput("meanKD", width = 4),
              valueBoxOutput("meanwinrate", width = 4)
            ),
            fluidRow(
              box(
                width = 6,
                selectInput(inputId = "select_y", 
                            label = "Select a variable", 
                            choices = colnames(df_global[, c(2,3)]),
                            selected = "pickrate")
              ),
              box(
                width = 5,
                selectInput(inputId = "select_df", 
                            label = "Choose your order", 
                            choices = colnames(df_global[, c(2,3)]),
                            selected = "pickrate")
              ),
              
            ), 
            fluidRow(
              box(
                width = 6, status = "info", solidHeader = FALSE, 
                plotOutput("finalPlot")
                
              ),
              
              box(
                width = 5, status = "info", solidHeader = FALSE,
                tableOutput("globalTable")
              ),
            )
    ),
    
    tabItem(tabName = "summoners",
            fluidRow(
              valueBoxOutput("v1box"),
              valueBoxOutput("v2box"),
              valueBox(value = "1,276", 
                       subtitle = "Hours spent on LoL", 
                       icon = icon("chess-king"))
            ),
            
            fluidRow(
              column(width = 8,
                     box(width = NULL,
                         status = "info",
                         solidHeader = FALSE,
                         plotOutput("plot"),
                         dropdownButton(
                           tags$h3("Inputs"),
                           circle = TRUE, 
                           status = "danger", 
                           icon = icon("cog"), 
                           width = "300px",
                           selectInput(inputId = "ycol", 
                                       label = "Y variable", 
                                       choices = names(df_summoners[c(7:11, 14, 15)]),
                                       selected = "avg_kills",
                                       multiple = FALSE
                                       
                           ))
                     )
              ),
              
              column(width = 4,
                     box(width = NULL,
                         title = "Summary",
                         tableOutput("summary")
                     )
              )
            ),
            
            fluidRow(
              column(width = 6,
                     box(width = NULL,
                         status = "info",
                         solidHeader = FALSE,
                         ggplotly(
                           df_summoners %>% 
                             group_by(saison) %>% 
                             summarise(Winrate = mean(winrate)) %>% 
                             ggplot(., aes(x = saison, y = Winrate, group = 1)) + 
                             geom_line() + 
                             geom_point() + 
                             theme_minimal()
                         )
                         
                         
                     )
              ),
              column(width = 6,
                     box(
                       width = NULL,
                       status = "info",
                       solidHeader = FALSE,
                       plotOutput('radar')
                     )
              )
            )
    ),
    
    tabItem(tabName = "ranked",
            h2("Ranked content on LOL legends"),
            column(width = 3,
                   fluidRow(
                     valueBoxOutput("firstwinrate", width = 13)
                   ),
                   fluidRow(
                     valueBoxOutput("firstnbgame", width = 13)
                   ),
                   fluidRow(
                     valueBoxOutput("firstkdrate", width = 13)
                   ),
                   fluidRow(
                     valueBoxOutput("firstgold", width = 13)
                   ),
                   fluidRow(
                     valueBoxOutput("firstKillmob", width = 13)
                   )
            ),
            column(width = 9,
                   box(
                     width = 12, status = "info", solidHeader = FALSE,
                     plotOutput("rankedPlot")
                   )
            ),
            fluidRow(
              box(width = 12, status = "info", solidHeader = FALSE,
                  plotOutput("bubblePlot")
              )
              
            )
    ),
    
    
    tabItem(tabName = "champ",
            h2("Content on LOL champions"),
            fluidRow(
              valueBoxOutput("v3box"),
              valueBoxOutput("v4box"),
              valueBoxOutput("v5box"),
            ),
            fluidRow(
              column(width = 5,
                     box(title = "Champion's infos", solidHeader = TRUE, status = "info", width = NULL,
                         fluidRow(
                           column(width = 3,
                                  imageOutput("picture")),
                           column(width = 6,
                                  htmlOutput("info_champ"),
                                  htmlOutput("bio_champ")
                           )
                           
                         )
                     )
              ),
              column(width = 7,
                     box(title = "dddd", solidHeader = TRUE, status = "info", width = NULL,
                         plotOutput('radarcomp')
                     )
              ) 
            )
    )
  )
)

ui <- dashboardPage(
  dashboardHeader(title = "League of Legends dashboard \n Global stats"),
  sidebar,
  body
)