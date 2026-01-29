
source("data.R")

theme <- bs_theme_update(
    bslib::bs_theme(), secondary = "#648FBE",
    base_font = font_google("Pangolin"),
    code_font = font_google("Courgette"),
    bootswatch = "united")


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = theme,
    

    # Application title
    navbarPage(
        paste0("NBA: 1975 - ", current_year),
        id = "main_title",
        
        tabPanel(
            "Franchise History", icon = icon("landmark"),
            sidebarLayout(
                sidebarPanel(
                    width = 4,
                    
                    withLoader(reactableOutput("NBA_Team"), type = "html", loader = "loader3"),
                    
                    pickerInput("FH_team_history", label = NULL,
                                choices = sort(unique(game_data$nameTeam)),
                                selected = "Boston Celtics",
                                inline = TRUE,
                                options = list(
                                  size = 3.5,
                                  `live-search`= TRUE
                                ),
                                multiple = FALSE),
                    
                    checkboxGroupInput("FH_type_Season", label = NULL,
                                       choices = NULL,
                                       selected = c("Regular Season", "Playoffs"))
                    
                ),
                mainPanel(width = 8,
                  tabsetPanel(
                    tabPanel(div(fa("house-user"), "Leaders"),
                             fluidRow(
                               fluidRow(width = 12,
                                        column(width = 4, htmlOutput("FH_leters_max_pts")),
                                        column(width = 4, htmlOutput("FH_leters_max_ast")),
                                        column(width = 4, htmlOutput("FH_leters_max_reb"))
                               ),
                               fluidRow(width = 12,
                                        column(width = 4,
                                               reactableOutput("FH_photo_max_pts")
                                        ),
                                        column(width = 4,
                                               withLoader(reactableOutput("FH_photo_max_ast"), type = "html", loader = "loader7")
                                        ),
                                        column(width = 4,
                                               reactableOutput("FH_photo_max_reb")
                                        )
                               ),
                               br(),
                               fluidRow(width = 12,
                                        column(width = 4, htmlOutput("FH_leters_max_games")),
                                        column(width = 4, htmlOutput("FH_leters_max_blk")),
                                        column(width = 4, htmlOutput("FH_leters_max_stl"))
                               ),
                               fluidRow(width = 12,
                                        column(width = 4, 
                                               reactableOutput("FH_photo_max_games")
                                        ),
                                        column(width = 4, 
                                               reactableOutput("FH_photo_max_blk")
                                        ),
                                        column(width = 4, 
                                               reactableOutput("FH_photo_max_stl")
                                        )
                               )
                             )
                    ),
                    tabPanel(div(fa("hubspot"), "Graphics"),
                             fluidRow(
                               width = 12,
                               column(
                                 width = 3,
                                 h5(strong(fa("id-card", fill = "orange"), "X")),
                                 pickerInput("Graphics_Franchise_var_x", label = NULL,
                                             choices = list("fgm", "fga",
                                                            "pctFG", "pctFT",
                                                            "minutes", "ftm",
                                                            "fta", "oreb",
                                                            "dreb", "treb",
                                                            "ast", "stl",
                                                            "blk", "pf",
                                                            "pts", "tov",
                                                            "fpts", "fg3m",
                                                            "fg3a",  "pctFG3",
                                                            "fg2m", "fg2a",
                                                            "pctFG2",  "plus minus"="plusminus",
                                                            "double fig"="double_fig"),
                                             selected = c("pts"),
                                             width = "150px",
                                             inline = TRUE,
                                             options = list(
                                               `actions-box` = TRUE,
                                               size = 10,
                                               `selected-text-format` = "count > 2"
                                             ),
                                             multiple = FALSE),
                               ),
                               column(
                                 width = 3,
                                 h5(strong(fa("id-card", fill = "orange"), "Y")),
                                 pickerInput("Graphics_Franchise_var_y", label = NULL,
                                             choices = list("fgm", "fga",
                                                            "pctFG", "pctFT",
                                                            "minutes", "ftm",
                                                            "fta", "oreb",
                                                            "dreb", "treb",
                                                            "ast", "stl",
                                                            "blk", "pf",
                                                            "pts", "tov",
                                                            "fpts", "fg3m",
                                                            "fg3a",  "pctFG3",
                                                            "fg2m", "fg2a",
                                                            "pctFG2",  "plus minus"="plusminus",
                                                            "double fig"="double_fig"),
                                             selected = c("ast"),
                                             width = "150px",
                                             inline = TRUE,
                                             options = list(
                                               `actions-box` = TRUE,
                                               size = 10,
                                               `selected-text-format` = "count > 2"
                                             ),
                                             multiple = FALSE),
                               ),
                               column(
                                 width = 3,
                                 h5(strong(fa("id-card", fill = "orange"), "Year season")),
                                 pickerInput("Graphics_Franchise_Season", label = NULL,
                                             choices = NULL,
                                             inline = TRUE,
                                             options = list(
                                               `actions-box` = TRUE, 
                                               size = 10,
                                               `selected-text-format` = "count > 2",
                                               `live-search`= TRUE
                                             ),
                                             multiple = TRUE)
                               ),
                               column(
                                 width = 3,
                                 h5(strong(fa("id-card", fill = "orange"), "Opps")),
                                 pickerInput("Graphics_Franchise_Opps", label = NULL,
                                             choices = NULL,
                                             inline = TRUE,
                                             options = list(
                                               `actions-box` = TRUE, 
                                               size = 10,
                                               `selected-text-format` = "count > 2",
                                               `live-search`= TRUE
                                             ),
                                             multiple = TRUE)
                               )
                             ),
                             plotlyOutput("Graphics_Franchise")
                    )
                  )
                )
            )
        ),
        
        tabPanel(
          "Big 3", icon = icon("users"),
          mainPanel(
            width = 12,
            fluidRow(
              column(width = 4,
                     h5(strong(fa("id-card", fill = "orange"), "Player 1: ")),
                     pickerInput("B3_player_1", label = NULL,
                                 choices = sort(unique(game_data$name_idPlayer)),
                                 inline = TRUE,
                                 selected = "LeBron James - 2544",
                                 options = list(
                                   size = 5,
                                   `live-search`= TRUE
                                 ),
                                 multiple = FALSE),
              ),
              column(width = 4,
                     h5(strong(fa("user-friends", fill = "orange"), "Player 2: ")),
                     pickerInput("B3_player_2", label = NULL,
                                 choices = NULL,
                                 inline = TRUE,
                                 selected = NULL,
                                 options = list(
                                   size = 5,
                                   `live-search`= TRUE
                                 ),
                                 multiple = FALSE)
              ),
              column(width = 4,
                     h5(strong(fa("dice-three", fill = "orange"), "Player 3: ")),
                     pickerInput("B3_player_3", label = NULL,
                                 choices = NULL,
                                 inline = TRUE,
                                 selected = NULL,
                                 options = list(
                                   size = 5,
                                   `live-search`= TRUE
                                 ),
                                 multiple = FALSE)
              )
            ),
            column(
              width = 12, 
              htmlOutput("B3_leters_1")
            ),
            fluidRow(
              column(width = 12,
                     withLoader(reactableOutput("B3_table_1"), type = "html", loader = "loader1")
              )
            ),
            fluidRow(
              column(
                width = 12, 
                htmlOutput("B3_leters_2")
              )
              # ,
              # column(width = 6,
              #        h4(strong(fa("drafting-compass", fill = "orange"), "Player 1 Stats playing without Player 2: "), align="center")
              # )
            ),
            fluidRow(
              column(width = 12,
                     withLoader(reactableOutput("B3_table_2"), type = "html", loader = "loader1")
              )
              # ,
              # column(width = 6,
              #        withLoader(reactableOutput("B3_table_4"), type = "html", loader = "loader1")
              # )
            ),
            fluidRow(
              column(
                width = 12, 
                htmlOutput("B3_leters_3")
              )
              # ,
              # column(width = 6,
              #        h4(strong(fa("drafting-compass", fill = "orange"), "Player 1 & 2 Stats playing without Player 3: "), align="center")
              # )
            ),
            fluidRow(
              column(width = 12,
                     withLoader(reactableOutput("B3_table_3"), type = "html", loader = "loader1")
              )
              # ,
              # column(width = 6,
              #        withLoader(reactableOutput("B3_table_5"), type = "html", loader = "loader1")
              # )
            )
          )
        ),
        
        tabPanel(
          "Records", icon = icon("medal"),
          mainPanel(
            width = 12,
            fluidRow(
              width = 12,
              column(
                width = 3,
                h5(strong(fa("id-card", fill = "orange"), "Points")),
                noUiSliderInput(
                  inputId = "Record_pts",
                  behaviour = "snap", 
                  tooltips = TRUE,
                  min = 0, max = 100, step = 1, format = wNumbFormat(decimals = 0),
                  value = c(0, 81), margin = 0, inline = TRUE, 
                  color = "#F1C10F",
                  width = "50%"
                ),
                h5(strong(fa("id-card", fill = "orange"), "Player")),
                pickerInput("Records_player", label = NULL,
                            choices = sort(unique(game_data$name_idPlayer)),
                            selected = "Russell Westbrook - 201566",
                            inline = TRUE,
                            options = list(
                              `actions-box` = TRUE, 
                              size = 10,
                              `selected-text-format` = "count > 2",
                              `live-search`= TRUE
                            ),
                            multiple = TRUE)
              ),
              column(
                width = 3,
                h5(strong(fa("id-card", fill = "orange"), "Assists")),
                noUiSliderInput(
                  inputId = "Record_ast",
                  behaviour = "snap", 
                  tooltips = TRUE,
                  min = 0, max = 40, step = 1, format = wNumbFormat(decimals = 0),
                  value = c(0, 40), margin = 0, inline = TRUE, 
                  color = "#F1C10F",
                  width = "50%"
                ),
                h5(strong(fa("id-card", fill = "orange"), "Triple doble")),
                prettyCheckboxGroup(inputId = "Record_double_fig",
                                    label = NULL,
                                    icon = icon("check"),
                                    choices = c("0", "1", "2", "3", "4"),
                                    selected = c("0", "1", "2", "3", "4"),
                                    animation = "rotate",
                                    shape = c("curve"),
                                    status = "warning",
                                    inline = TRUE),
              ),
              column(
                width = 3,
                h5(strong(fa("id-card", fill = "orange"), "Rebounds")),
                noUiSliderInput(
                  inputId = "Record_treb",
                  behaviour = "snap", 
                  tooltips = TRUE,
                  min = 0, max = 40, step = 1, format = wNumbFormat(decimals = 0),
                  value = c(0, 40), margin = 0, inline = TRUE, 
                  color = "#F1C10F",
                  width = "50%"
                ),
                h5(strong(fa("id-card", fill = "orange"), "Minutes")),
                noUiSliderInput(
                  inputId = "Record_minutes",
                  behaviour = "snap", 
                  tooltips = TRUE,
                  min = 0, max = 69, step = 1, format = wNumbFormat(decimals = 0),
                  value = c(0, 69), margin = 0, inline = TRUE, 
                  color = "#F1C10F",
                  width = "50%"
                )
              ),
              column(
                width = 3,
                br(),
                dropdownButton(
                  inputId = "mydropdown",
                  label = "Other Filters",
                  icon = icon("sliders"),
                  status = "primary",
                  circle = FALSE,
                  width = "280px",
                  size = "sm",
                  noUiSliderInput(
                    inputId = "Field_Goal_Percentage",
                    label =  h5(strong("Field Goal Percentage")),
                    behaviour = "snap",
                    tooltips = TRUE,
                    min = 0, max = 1, step = 0.1, format = wNumbFormat(decimals = 1),
                    value = c(0, 1),
                    margin = 0,
                    inline = TRUE,
                    color = "#F1C10F",
                    width = "225px"
                  ),
                  prettyCheckboxGroup(
                    inputId = "Record_isWin",
                    label = h5(strong("Match Result")),
                    choiceNames = c("Win", "Lost"),
                    choiceValues = c("TRUE", "FALSE"),
                    selected = c("TRUE", "FALSE"),
                    icon = icon("check"),
                    inline = TRUE,
                    animation = "jelly"
                  ),
                  prettyCheckboxGroup(
                    inputId = "Record_typeSeason",
                    label =  h5(strong("Type Season")),
                    choiceNames = c("Regular Season", "Playoffs", "All Star"),
                    choiceValues = c("Regular Season", "Playoffs", "All Star"),
                    selected = c("Regular Season", "Playoffs", "All Star"),
                    icon = icon("check"),
                    inline = FALSE,
                    animation = "jelly"
                  )
                ),
                br(),
                br(),
                br(),
                actionButton("Search", "Search")
              )
            ),
            fluidRow(
              column(width = 12,
                     withLoader(reactableOutput("Record_table"), type = "html", loader = "loader1")
              )
            )
          )
        ),
        
        navbarMenu("Connect",
                   icon = icon("info-circle"),
                   tabPanel(
                     tags$a(href="https://es.linkedin.com/in/daniel-alejo-rodriguez-2601ba161", icon("linkedin"), "Linkedin")
                   ),
                   tabPanel(
                     tags$a(href="https://github.com/dalerodr", icon("github"), "Github")
                   )
        )
    )
)


# Define server
server <- function(input, output, session){
  
    cleave()   # to manage errors
    
    # Franchise History -------------------------------------------------------
    
    ##### Leaders  #####
    
    FH_team <- reactive({
        req(input$FH_team_history)
        filter(game_data, nameTeam == input$FH_team_history)
    })
    observeEvent(FH_team(), {
        choice <- sort(unique(FH_team()$typeSeason))
        updateCheckboxGroupInput(session, "FH_type_Season",
                                 choiceNames = choice,
                                 choiceValues = choice,
                                 selected = choice[1])
    })
    
    FH_Season_type <- reactive({
      req(input$FH_type_Season)
      filter(FH_team(), typeSeason %in% input$FH_type_Season)
    })
    observeEvent(FH_Season_type(), {
      choice <- sort(unique(FH_Season_type()$slugSeason))
      updatePickerInput(session, "Graphics_Franchise_Season",
                        choices = choice,
                        selected = choice[1])
    })
    
    FH_Season_year <- reactive({
      req(input$Graphics_Franchise_Season)
      FH_Season_type() %>% 
        filter(slugSeason %in% input$Graphics_Franchise_Season)
    })
    observeEvent(FH_Season_year(), {
      choice <- sort(unique(FH_Season_year()$slugOpponent))
      updatePickerInput(session, "Graphics_Franchise_Opps",
                        choices = choice,
                        selected = choice)
    })
    
    output$NBA_Team <- renderReactable({
      photo_team <- FH_team() %>% select(nameTeam) %>% unique() %>% as.character()
      teams_logos_NT %>% filter(nameTeam == photo_team) %>% select(Team) %>% head(1) %>%
        reactable(columns = list(Team = colDef(cell = embed_img(., height = "250", 
                                                                  width = "250"),
                                                 maxWidth = 290)
        ),
        striped = TRUE)
    })
    
    FH_max <- reactive({
      req(input$FH_team_history)
      franch_lead <- FH_Season_type() %>% 
                          group_by(name_idPlayer, namePlayer, nameTeam) %>% 
                          summarise(games = n(),
                                    pts = sum(pts, na.rm = TRUE),
                                    treb = sum(treb, na.rm = TRUE),
                                    ast = sum(ast, na.rm = TRUE),
                                    blk = sum(blk, na.rm = TRUE),
                                    stl = sum(stl, na.rm = TRUE),
                                    urlPlayerThumbnail = unique(urlPlayerThumbnail), .groups = "keep")
    })
    ## MAX GAMES ##
    observeEvent(FH_max(), {
      data <- FH_max() %>%
        select(urlPlayerThumbnail, name_idPlayer, namePlayer, nameTeam, games,
               pts, ast, treb, blk, stl) %>% 
        dplyr::arrange(., desc(games)) %>%
        head(3)
      
      output$FH_leters_max_games <- renderUI({
        name <- data$namePlayer[1]
        games <- data$games[1]
        h5(strong(fa("basketball-ball", fill = "orange"), paste("Games:", games, "-", name)))
      })
      
      output$FH_photo_max_games <- renderReactable({
        data %>% mutate(Player = urlPlayerThumbnail) %>% select(Player, games) %>% 
          reactable(columns = list(Player = colDef(cell = embed_img(., height = "50", 
                                                                  width = "50"),
                                                 minWidth = 57),
                                   namePlayer = colDef(show = FALSE),
                                   games = colDef(show = TRUE, minWidth = 62),
                                   name_idPlayer = colDef(show = FALSE)
          ),
          striped = TRUE)
      })
      
      })
    
    ## MAX BLOCKS ##
    observeEvent(FH_max(), {
      data <- FH_max() %>%
        select(urlPlayerThumbnail, name_idPlayer, namePlayer, nameTeam, games,
               pts, ast, treb, blk, stl) %>% 
        dplyr::arrange(., desc(blk)) %>%
        head(3)
      
    output$FH_leters_max_blk <- renderUI({
      name <- data$namePlayer[1]
      blk <- data$blk[1]
      h5(strong(fa("basketball-ball", fill = "orange"), paste("Blocks:", blk, "-", name)))
    })
    
    output$FH_photo_max_blk <- renderReactable({
      data %>% mutate(Player = urlPlayerThumbnail) %>% select(Player, blk) %>% 
        reactable(columns = list(Player = colDef(cell = embed_img(., height = "50", 
                                                                  width = "50"),
                                                 minWidth = 57),
                                 namePlayer = colDef(show = FALSE),
                                 blk = colDef(show = TRUE, minWidth = 62),
                                 name_idPlayer = colDef(show = FALSE)
        ),
        striped = TRUE)
    })
    
    })
    
    ## MAX STEALS ##
    observeEvent(FH_max(), {
      data <- FH_max() %>%
        select(urlPlayerThumbnail, name_idPlayer, namePlayer, nameTeam, games,
               pts, ast, treb, blk, stl) %>% 
        dplyr::arrange(., desc(stl)) %>%
        head(3)
      
      output$FH_leters_max_stl <- renderUI({
        name <- data$namePlayer[1]
        stl <- data$stl[1]
        h5(strong(fa("basketball-ball", fill = "orange"), paste("Steals:",stl, "-", name)))
      })
      
      output$FH_photo_max_stl <- renderReactable({
        data %>% mutate(Player = urlPlayerThumbnail) %>% select(Player, stl) %>% 
          reactable(columns = list(Player = colDef(cell = embed_img(., height = "50", 
                                                                    width = "50"),
                                                   minWidth = 57),
                                   namePlayer = colDef(show = FALSE),
                                   stl = colDef(show = TRUE, minWidth = 62),
                                   name_idPlayer = colDef(show = FALSE)
          ),
          striped = TRUE)
      })
      
    })
    
    ## MAX POINTS ##
    observeEvent(FH_max(), {
      data <- FH_max() %>%
        select(urlPlayerThumbnail, name_idPlayer, namePlayer, nameTeam, games,
               pts, ast, treb, blk, stl) %>% 
        dplyr::arrange(., desc(pts)) %>%
        head(3)
      
      output$FH_leters_max_pts <- renderUI({
        name <- data$namePlayer[1]
        pts <- data$pts[1]
        h5(strong(fa("basketball-ball", fill = "orange"), paste("Points:", pts, "-", name)))
      })
      
      output$FH_photo_max_pts <- renderReactable({
        data %>% mutate(Player = urlPlayerThumbnail) %>% select(Player, pts) %>% 
          reactable(columns = list(Player = colDef(cell = embed_img(., height = "50", 
                                                                    width = "50"),
                                                   minWidth = 57),
                                   namePlayer = colDef(show = FALSE),
                                   pts = colDef(show = TRUE, minWidth = 62),
                                   name_idPlayer = colDef(show = FALSE)
          ),
          striped = TRUE)
      })
      
    })
    
    ## MAX ASSISTS ##
    observeEvent(FH_max(), {
      data <- FH_max() %>%
        select(urlPlayerThumbnail, name_idPlayer, namePlayer, nameTeam, games,
               pts, ast, treb, blk, stl) %>% 
        dplyr::arrange(., desc(ast)) %>%
        head(3)
      
      output$FH_leters_max_ast <- renderUI({
        name <- data$namePlayer[1]
        ast <- data$ast[1]
        h5(strong(fa("basketball-ball", fill = "orange"), paste("Assists:", ast, "-", name)))
      })
      
      output$FH_photo_max_ast <- renderReactable({
        data %>% mutate(Player = urlPlayerThumbnail) %>% select(Player, ast) %>% 
          reactable(columns = list(Player = colDef(cell = embed_img(., height = "50", 
                                                                    width = "50"),
                                                   minWidth = 57),
                                   namePlayer = colDef(show = FALSE),
                                   ast = colDef(show = TRUE, minWidth = 62),
                                   name_idPlayer = colDef(show = FALSE)
          ),
          striped = TRUE)
      })
      
    })
    
    ## MAX REBOUNDS ##
    observeEvent(FH_max(), {
      data <- FH_max() %>%
        select(urlPlayerThumbnail, name_idPlayer, namePlayer, nameTeam, games,
               pts, ast, treb, blk, stl) %>% 
        dplyr::arrange(., desc(treb)) %>%
        head(3)
      
      output$FH_leters_max_reb <- renderUI({
        name <- data$namePlayer[1]
        reb <- data$treb[1]
        h5(strong(fa("basketball-ball", fill = "orange"), paste("Rebounds:", reb, "-", name)))
      })
      
      output$FH_photo_max_reb <- renderReactable({
        data %>% mutate(Player = urlPlayerThumbnail) %>% select(Player, treb) %>% 
          reactable(columns = list(Player = colDef(cell = embed_img(., height = "50", 
                                                                    width = "50"),
                                                   minWidth = 57),
                                   namePlayer = colDef(show = FALSE),
                                   treb = colDef(show = TRUE, minWidth = 62),
                                   name_idPlayer = colDef(show = FALSE)
          ),
          striped = TRUE)
      })
      
    })
    
    ##### Graphics  #####
    
    Plots_franchise_data <- reactive({
      req(input$Graphics_Franchise_Opps)
      FH_Season_year() %>% 
        filter(slugOpponent %in% input$Graphics_Franchise_Opps) %>% 
        group_by(dateGame) %>% 
        summarise_at(c("fgm", "fga",
                       "pctFG", "pctFT",
                       "minutes", "ftm",
                       "fta", "oreb",
                       "dreb", "treb",
                       "ast", "stl",
                       "blk", "pf",
                       "pts", "tov",
                       "fpts", "fg3m",
                       "fg3a",  "pctFG3",
                       "fg2m", "fg2a",
                       "pctFG2",  "plusminus",
                       "double_fig"), sum, na.rm = TRUE) %>% 
        select(input$Graphics_Franchise_var_x, input$Graphics_Franchise_var_y)
    })
    observeEvent(Plots_franchise_data(), {
      
      data <- as.data.frame(Plots_franchise_data())
      
      output$Graphics_Franchise <- renderPlotly({
        plot_ly(x = data[,input$Graphics_Franchise_var_x],
                y = data[,input$Graphics_Franchise_var_y],
                text = ~paste0(input$Graphics_Franchise_var_x, ': ', data[,input$Graphics_Franchise_var_x], 
                              '<br>', input$Graphics_Franchise_var_y, ': ', data[,input$Graphics_Franchise_var_y]),
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 10,
                              color = 'rgba(255, 182, 193, .9)',
                              line = list(color = 'rgba(152, 0, 0, .8)', width = 2))) %>% 
          layout(title = paste(input$FH_team_history, 'Scatter Plot'),
                 plot_bgcolor = "#e5ecf6",
                 yaxis = list(title = input$Graphics_Franchise_var_y,
                              face = "bold.italic"),
                 xaxis = list(title = input$Graphics_Franchise_var_x,
                              face = "bold.italic"))
      })
      
    })
    
    # Big 3 -------------------------------------------------------------------
    
    ## Stats Players 1 --------------------------------------------------------
    B3_Player1 <- reactive({
      req(input$B3_player_1)
      filter(game_data, name_idPlayer == input$B3_player_1)
    })
    observeEvent(B3_Player1(), {
      output$B3_leters_1 <- renderUI({
        h3(strong(fa("star", fill = "orange"), paste(unique(B3_Player1()$namePlayer), " Stats")), align="center")
      })
      
      choices_1 <- B3_Player1() %>%
        inner_join(game_data, by = c("idGame","idTeam")) %>%
        select(name_idPlayer.y) %>% 
        filter(name_idPlayer.y != input$B3_player_1)
      choice <- sort(unique(choices_1$name_idPlayer.y))
      updatePickerInput(session, "B3_player_2",
                        choices = choice,
                        selected = choice[1])
      
      output$B3_table_1 <- renderReactable({
        data <- B3_Player1() %>%
          select(namePlayer, urlPlayerThumbnail, nameTeam, isWin, fgm, fga, pts,
                 ast, treb, blk, tov, plusminus) %>%
          group_by(namePlayer, nameTeam) %>%
          summarise(urlPlayerThumbnail= first(urlPlayerThumbnail),
                    nameTeam = unique(nameTeam),
                    isWin = sum(isWin), games = n(), "% wins" = round((isWin/games), digits = 2), fgm = round(mean(fgm, na.rm = TRUE), digits = 2),
                    fga = round(mean(fga, na.rm = TRUE), digits = 2), pts = round(mean(pts, na.rm = TRUE), digits = 2),
                    ast = round(mean(ast, na.rm = TRUE), digits = 2), treb = round(mean(treb, na.rm = TRUE), digits = 2),
                    blk = round(mean(blk, na.rm = TRUE), digits = 2), tov = round(mean(tov, na.rm = TRUE), digits = 2),
                    plusminus = round(mean(plusminus, na.rm = TRUE), digits = 2), .groups = "rowwise") %>%
          rename(Player = urlPlayerThumbnail,
                 wins = isWin) %>% 
          left_join(teams_logos_NT, by=c("nameTeam")) %>% 
          select(-nameTeam) %>% 
          relocate(Team, .before = Player)
        
        
        reactable(data, 
                  groupBy = "namePlayer",
                  defaultExpanded = TRUE,
                  defaultSorted = "games",
                  defaultSortOrder = "desc",
                  columns = list(
                    namePlayer = colDef(show = TRUE, maxWidth = 175),
                    Player = colDef(show = FALSE),
                    Team = colDef(cell = embed_img(data, height = "60", width = "80"),
                                  maxWidth = 115),
                    wins = colDef(aggregate = "sum",
                                  maxWidth = 60),
                    games = colDef(aggregate = "sum",
                                   maxWidth = 80),
                    namePlayer = colDef(show = TRUE),
                    "% wins"   = colDef(format = colFormat(percent = TRUE, digits = 2),
                                        align = "center",
                                        cell = color_tiles(data,
                                                           colors = c("brown1", "chocolate1", "chartreuse2"),
                                                           number_fmt = scales::percent),
                                        minWidth = 75,
                                        maxWidth = 100),
                    fgm = colDef(format = colFormat(digits = 2), maxWidth = 60),
                    fga = colDef(format = colFormat(digits = 2), maxWidth = 60),
                    pts = colDef(format = colFormat(digits = 2), maxWidth = 60),
                    ast = colDef(format = colFormat(digits = 2), maxWidth = 60),
                    treb = colDef(format = colFormat(digits = 2), maxWidth = 60),
                    blk = colDef(format = colFormat(digits = 2), maxWidth = 60),
                    tov = colDef(format = colFormat(digits = 2), maxWidth = 60),
                    plusminus = colDef(format = colFormat(digits = 2), maxWidth = 100)))
      })
    })
    
    ## Stats Players 1 & 2 playing together -----------------------------------
    B3_Player2 <- reactive({
      req(input$B3_player_2)
      B3_Player1() %>%
        inner_join(game_data, by = c("idGame","idTeam")) %>%
        filter(name_idPlayer.y %in% c(input$B3_player_2))
    })
    observeEvent(B3_Player2(), {
      output$B3_leters_2 <- renderUI({
        h4(strong(fa("people-carry", fill = "orange"), paste(unique(B3_Player1()$namePlayer), " & ", unique(B3_Player2()$namePlayer.y), "Stats playing together")), align="center")
      })
      
      choices_2 <- B3_Player2() %>%
        inner_join(game_data, by = c("idGame","idTeam")) %>% 
        select(name_idPlayer) %>%
        filter(name_idPlayer != input$B3_player_1,
               name_idPlayer != input$B3_player_2)
      choice <- sort(unique(choices_2$name_idPlayer))
      updatePickerInput(session, "B3_player_3",
                        choices = choice,
                        selected = choice[1])
      P1 <- B3_Player2() %>% 
        select("idGame", "idTeam", contains(".x")) %>% 
        rename_with(~ gsub(".x", "", .x, fixed = TRUE))
      
      P2 <- B3_Player2() %>% 
        select("idGame", "idTeam", contains(".y")) %>% 
        rename_with(~ gsub(".y", "", .x, fixed = TRUE))
      
      P1_P2 <- plyr::rbind.fill(P1, P2)
      
      output$B3_table_2 <- renderReactable({
        data <- P1_P2 %>%
          select(namePlayer, urlPlayerThumbnail, nameTeam, isWin, fgm, fga, pts,
                 ast, treb, blk, tov, plusminus) %>%
          group_by(namePlayer, nameTeam) %>%
          summarise(urlPlayerThumbnail= first(urlPlayerThumbnail),
                    nameTeam = unique(nameTeam),
                    isWin = sum(isWin), games = n(), "% wins" = round((isWin/games), digits = 2), fgm = round(mean(fgm, na.rm = TRUE), digits = 2),
                    fga = round(mean(fga, na.rm = TRUE), digits = 2), pts = round(mean(pts, na.rm = TRUE), digits = 2),
                    ast = round(mean(ast, na.rm = TRUE), digits = 2), treb = round(mean(treb, na.rm = TRUE), digits = 2),
                    blk = round(mean(blk, na.rm = TRUE), digits = 2), tov = round(mean(tov, na.rm = TRUE), digits = 2),
                    plusminus = round(mean(plusminus, na.rm = TRUE), digits = 2), .groups = "rowwise") %>%
          rename(Player = urlPlayerThumbnail,
                 wins = isWin) %>% 
          left_join(teams_logos_NT, by=c("nameTeam")) %>% 
          select(-nameTeam) %>% 
          relocate(Team, .after = Player)
        
        reactable(data, columns = list(
          nameTeam = colDef(show = FALSE),
          namePlayer = colDef(show = TRUE, maxWidth = 150),
          Player = colDef(cell = embed_img(data, height = "60", width = "80"),
                          maxWidth = 125),
          Team = colDef(cell = embed_img(data, height = "60", width = "80"),
                        maxWidth = 115),
          wins = colDef(maxWidth = 60),
          games = colDef(maxWidth = 80),
          "% wins"   = colDef(
            style = function(value) {
              if (value >= 0.5) {
                color <- "#008000"
              } else if (value < 0.5) {
                color <- "#e00000"
              } else {
                color <- "#777"
              }
              list(color = color, fontWeight = "bold")
            },
            format = colFormat(percent = TRUE)
          ),
          fgm = colDef(format = colFormat(digits = 2), maxWidth = 60),
          fga = colDef(format = colFormat(digits = 2), maxWidth = 60),
          pts = colDef(format = colFormat(digits = 2), maxWidth = 80),
          ast = colDef(format = colFormat(digits = 2), maxWidth = 60),
          treb = colDef(format = colFormat(digits = 2), maxWidth = 60),
          blk = colDef(format = colFormat(digits = 2), maxWidth = 60),
          tov = colDef(format = colFormat(digits = 2), maxWidth = 60),
          plusminus = colDef(format = colFormat(digits = 2), maxWidth = 100)))
      })
    })
    
    ## Stats Players 1, 2 & 3 playing together --------------------------------
    
    B3_Player3 <- reactive({
      req(input$B3_player_3)
      B3_Player2() %>%
        inner_join(game_data, by = c("idGame","idTeam")) %>%
        filter(name_idPlayer %in% c(input$B3_player_3))
    })
    observeEvent(B3_Player3(), {
      output$B3_leters_3 <- renderUI({
        h4(strong(fa("people-carry", fill = "orange"), paste(unique(B3_Player1()$namePlayer), ", ", unique(B3_Player2()$namePlayer.y), " & ", unique(B3_Player3()$namePlayer), "Stats playing together")), align="center")
      })
      
      P1 <- B3_Player3() %>% 
        select("idGame", "idTeam", contains(".x")) %>% 
        rename_with(~ gsub(".x", "", .x, fixed = TRUE))
      
      P2 <- B3_Player3() %>% 
        select("idGame", "idTeam", contains(".y")) %>% 
        rename_with(~ gsub(".y", "", .x, fixed = TRUE))
      
      P3 <- B3_Player3() %>% 
        select(!contains(c(".x", ".y")))
      
      P1_P2_P3 <- plyr::rbind.fill(P1, P2, P3)
      
      output$B3_table_3 <- renderReactable({
        data <- P1_P2_P3 %>%
          select(namePlayer, urlPlayerThumbnail, nameTeam, isWin, fgm, fga, pts,
                 ast, treb, blk, tov, plusminus) %>%
          group_by(namePlayer, nameTeam) %>%
          summarise(urlPlayerThumbnail= first(urlPlayerThumbnail),
                    nameTeam = unique(nameTeam),
                    isWin = sum(isWin), games = n(), "% wins" = round((isWin/games), digits = 2), fgm = round(mean(fgm, na.rm = TRUE), digits = 2),
                    fga = round(mean(fga, na.rm = TRUE), digits = 2), pts = round(mean(pts, na.rm = TRUE), digits = 2),
                    ast = round(mean(ast, na.rm = TRUE), digits = 2), treb = round(mean(treb, na.rm = TRUE), digits = 2),
                    blk = round(mean(blk, na.rm = TRUE), digits = 2), tov = round(mean(tov, na.rm = TRUE), digits = 2),
                    plusminus = round(mean(plusminus, na.rm = TRUE), digits = 2), .groups = "rowwise") %>%
          rename(Player = urlPlayerThumbnail,
                 wins = isWin) %>% 
          left_join(teams_logos_NT, by=c("nameTeam")) %>% 
          select(-nameTeam) %>% 
          relocate(Team, .after = Player)
        
        reactable(data, columns = list(
          nameTeam = colDef(show = FALSE),
          namePlayer = colDef(show = TRUE, maxWidth = 150),
          Player = colDef(cell = embed_img(data, height = "60", width = "80"),
                          maxWidth = 125),
          Team = colDef(cell = embed_img(data, height = "60", width = "80"),
                        maxWidth = 115),
          wins = colDef(maxWidth = 60),
          games = colDef(maxWidth = 80),
          "% wins"   = colDef(
            style = function(value) {
              if (value >= 0.5) {
                color <- "#008000"
              } else if (value < 0.5) {
                color <- "#e00000"
              } else {
                color <- "#777"
              }
              list(color = color, fontWeight = "bold")
            },
            format = colFormat(percent = TRUE)
          ),
          fgm = colDef(format = colFormat(digits = 2), maxWidth = 60),
          fga = colDef(format = colFormat(digits = 2), maxWidth = 60),
          pts = colDef(format = colFormat(digits = 2), maxWidth = 80),
          ast = colDef(format = colFormat(digits = 2), maxWidth = 60),
          treb = colDef(format = colFormat(digits = 2), maxWidth = 60),
          blk = colDef(format = colFormat(digits = 2), maxWidth = 60),
          tov = colDef(format = colFormat(digits = 2), maxWidth = 60),
          plusminus = colDef(format = colFormat(digits = 2), maxWidth = 100)))
      })
    })
    
    ## Stats Player 1 playing without player 2 -------------------------------
    
    # B3_Player1 <- reactive({
    #   req(input$B3_player_1)
    #   filter(game_data, name_idPlayer == input$B3_player_1)
    # })
    # observeEvent(B3_Player1(), {
    #   P1 <- B3_Player1() %>% 
    #     select(idGame)
    #   
    #   P2 <- game_data %>% filter(name_idPlayer == input$B3_player_1)
    #   
    #   P1_w <- B3_Player1() %>% filter(idGame)
    #   
    #   output$B3_table_4 <- renderReactable({
    #     data <- P1_P2 %>%
    #       select(namePlayer, urlPlayerThumbnail, nameTeam, isWin, fgm, fga, pts,
    #              ast, treb, blk, tov, plusminus) %>%
    #       group_by(namePlayer, nameTeam) %>%
    #       summarise(urlPlayerThumbnail= first(urlPlayerThumbnail),
    #                 nameTeam = unique(nameTeam),
    #                 isWin = sum(isWin), games = n(), "% wins" = round((isWin/games), digits = 2), fgm = round(mean(fgm, na.rm = TRUE), digits = 2),
    #                 fga = round(mean(fga, na.rm = TRUE), digits = 2), pts = round(mean(pts, na.rm = TRUE), digits = 2),
    #                 ast = round(mean(ast, na.rm = TRUE), digits = 2), treb = round(mean(treb, na.rm = TRUE), digits = 2),
    #                 blk = round(mean(blk, na.rm = TRUE), digits = 2), tov = round(mean(tov, na.rm = TRUE), digits = 2),
    #                 plusminus = round(mean(plusminus, na.rm = TRUE), digits = 2)) %>%
    #       rename(Player = urlPlayerThumbnail,
    #              wins = isWin) %>% 
    #       left_join(teams_logos_NT, by=c("nameTeam")) %>% 
    #       select(-nameTeam) %>% 
    #       relocate(Team, .after = Player)
    #     
    #     reactable(data, columns = list(
    #       namePlayer = colDef(show = TRUE, maxWidth = 150),
    #       Player = colDef(cell = embed_img(data, height = "60", width = "80"),
    #                       maxWidth = 125),
    #       Team = colDef(cell = embed_img(data, height = "60", width = "80"),
    #                     maxWidth = 115),
    #       wins = colDef(maxWidth = 60),
    #       games = colDef(maxWidth = 80),
    #       "% wins"   = colDef(
    #         style = function(value) {
    #           if (value >= 0.5) {
    #             color <- "#008000"
    #           } else if (value < 0.5) {
    #             color <- "#e00000"
    #           } else {
    #             color <- "#777"
    #           }
    #           list(color = color, fontWeight = "bold")
    #         },
    #         format = colFormat(percent = TRUE)
    #       ),
    #       fgm = colDef(format = colFormat(digits = 2), maxWidth = 60),
    #       fga = colDef(format = colFormat(digits = 2), maxWidth = 60),
    #       pts = colDef(format = colFormat(digits = 2), maxWidth = 80),
    #       ast = colDef(format = colFormat(digits = 2), maxWidth = 60),
    #       treb = colDef(format = colFormat(digits = 2), maxWidth = 60),
    #       blk = colDef(format = colFormat(digits = 2), maxWidth = 60),
    #       tov = colDef(format = colFormat(digits = 2), maxWidth = 60),
    #       plusminus = colDef(format = colFormat(digits = 2), maxWidth = 100)))
    #   })
    # })
    # 
    # B3_Player1_w <- reactive({
    #   B3_Player2() %>%
    #     inner_join(game_data, by = c("idGame","idTeam")) %>%
    #     filter(name_idPlayer %in% c(input$B3_player_3))
    # })
    
    ## Stats Players 1 & 2 playing without player 3 ---------------------------
    
    
    # Records -----------------------------------------------------------------
    
    Record <- eventReactive(input$Search, {
      if(is.null(input$Record_double_fig)){
        data <- game_data %>% filter(between(pts, input$Record_pts[1], input$Record_pts[2]),
                                     between(minutes, input$Record_minutes[1], input$Record_minutes[2]),
                                     isWin %in% input$Record_isWin,
                                     name_idPlayer %in% input$Records_player)
      }else{
        data <- game_data %>% filter(double_fig %in% input$Record_double_fig,
                                     between(pts, input$Record_pts[1], input$Record_pts[2]),
                                     between(minutes, input$Record_minutes[1], input$Record_minutes[2]),
                                     isWin %in% input$Record_isWin,
                                     name_idPlayer %in% input$Records_player)
      }
      
      if((input$Record_ast[1]) == 0){
        data <- data %>% filter(is.na(ast) | between(ast, input$Record_ast[1], input$Record_ast[2]))
      }else{
        data <- data %>% filter(between(ast, input$Record_ast[1], input$Record_ast[2]))
      }
      
      if((input$Record_treb[1]) == 0){
        data <- data %>% filter(is.na(treb) | between(treb, input$Record_treb[1], input$Record_treb[2]))
      }else{
        data <- data %>% filter(between(treb, input$Record_treb[1], input$Record_treb[2]))
      }
      
      if((input$Field_Goal_Percentage[1]) == 0){
        data <- data %>% filter(is.na(pctFG) | between(pctFG, input$Field_Goal_Percentage[1], input$Field_Goal_Percentage[2]))
      }else{
        data <- data %>% filter(between(pctFG, input$Field_Goal_Percentage[1], input$Field_Goal_Percentage[2]))
      }
      
      if(is.null(input$Record_typeSeason)){
        data <- data
      }else{
        data <- data %>% filter(typeSeason %in% input$Record_typeSeason)
      }
      
    })
    output$Record_table <- renderReactable({
      data <- Record() %>%
        head(2000) %>%
        select(urlPlayerThumbnail, namePlayer, nameTeam, dateGame, slugOpponent, minutes, fgm, fga, pctFG, fg3m, fg3a,
               pts, ast, treb, blk, stl, tov, plusminus) %>%
        rename(Player = urlPlayerThumbnail,
               OppTeam = slugOpponent)
      
      reactable(data,
                searchable = TRUE,
                highlight = TRUE, height = 600,
                columns = list(
                  namePlayer = colDef(show = TRUE, maxWidth = 150),
                  Player = colDef(cell = embed_img(data, height = "60", width = "80"),
                                  maxWidth = 125),
                  nameTeam = colDef(show = TRUE, maxWidth = 150),
                  dateGame = colDef(show = TRUE, minWidth = 130),
                  OppTeam = colDef(show = TRUE, maxWidth = 80),
                  minutes = colDef(show = TRUE, maxWidth = 80),
                  fgm = colDef(maxWidth = 60),
                  fga = colDef(maxWidth = 60),
                  pctFG = colDef(
                    format = colFormat(percent = TRUE),
                    maxWidth = 80,
                    na = "0 %"
                  ),
                  pts = colDef(maxWidth = 60, style = list(fontWeight = "bold")),
                  fg3m = colDef(maxWidth = 60, na = "0"),
                  fg3a = colDef(maxWidth = 60, na = "0"),
                  ast = colDef(maxWidth = 60, na = "0"),
                  treb = colDef(maxWidth = 60, na = "0"),
                  blk = colDef(maxWidth = 60, na = "0"),
                  stl = colDef(maxWidth = 60, na = "0"),
                  tov = colDef(maxWidth = 60, na = "0"),
                  plusminus = colDef(maxWidth = 90, na = "0")),
                showPageSizeOptions = TRUE, pageSizeOptions = c(24, 50, 100), defaultPageSize = 24)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
# runApp(shinyApp(ui, server), launch.browser = TRUE)
