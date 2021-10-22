library(lattice)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinyhelper)
library(dplyr)
library(tm)
library(wordcloud)
library(purrr)
library(readr)
library(stringr)
library(magrittr)
library(reactable)
library(NLP)
library(RColorBrewer)
library(twitteR)
library(gcookbook) 
library(viridis)  
library(forcats)
library(gridExtra)
library(grid)
library(scales)
library(caret)
library(MLeval)
library(ggrepel)

options(warn=-1)

#save(tweets.text,file="C:/Users/ayush/Documents/BAIM - Purdue 2021/Summer 2021 - Courses/Module 3/MGMT 590RA - Using R for Analytics/Project Upload/tweets_10K_clean.RData")
#setwd("C:/Users/ayush/Documents/BAIM - Purdue 2021/Summer 2021 - Courses/Module 3/MGMT 590RA - Using R for Analytics/Project Upload/")

#### Team List ####
team_list <- c("Boston Celtics", "Brooklyn Nets", "New York Knicks", "Philadelphia 76ers", "Toronto Raptors",
               "Chicago Bulls","Cleveland Cavaliers","Detroit Pistons","Indiana Pacers","Milwaukee Bucks",
               "Atlanta Hawks","Charlotte Hornets","Washington Wizards","Miami Heat","Orlando Magic",
               "Denver Nuggets","Minnesota Timberwolves","Oklahoma City Thunder","Portland TrailBlazers","Utah Jazz",
               "Golden State Warriors","Los Angeles Clippers","Los Angeles Lakers","Phoenix Suns","Sacramento Kings",
               "Dallas Mavericks","Houston Rockets","Memphis Grizzlies","New Orleans Pelicans","San Antonio Spurs")

team_list_img <- c("Boston Celtics"="bostonceltics", "Brooklyn Nets" ="brooklynnets", "New York Knicks"="newyorkknicks",
                   "Philadelphia 76ers"="philadelphia76ers", "Toronto Raptors"="torontoraptors","Chicago Bulls"="chicagobulls"
                   ,"Cleveland Cavaliers"="clevelandcavaliers","Detroit Pistons"="detroitpistons","Indiana Pacers"="indianapacers",
                   "Milwaukee Bucks"="milwaukeebucks","Atlanta Hawks"="atlantahawks","Charlotte Hornets"="charlottehornet",
                   "Washington Wizards"="washingtonwizards","Miami Heat"="miamiheat","Orlando Magic"="orlandomagic",
                   "Denver Nuggets"="denvernuggets","Minnesota Timberwolves"="minnesotatimberwolves",
                   "Oklahoma City Thunder"="oklahomacitythunder","Portland Trail Blazers"="portlandtrailblazers",
                   "Utah Jazz"="utahjazz","Golden State Warriors"="goldenstatewarriors","Los Angeles Clippers"="losangelesclippers",
                   "Los Angeles Lakers"="losangeleslakers","Phoenix Suns"="phoenixsuns","Sacramento Kings"="sacramentokings",
                   "Dallas Mavericks"="dallasmavericks","Houston Rockets"="houstonrockets","Memphis Grizzlies"="memphisgrizzlies",
                   "New Orleans Pelicans"="neworleanspelicans","San Antonio Spurs"="sanantoniospurs")
data.frame(team_list_img)

team_id <- c("Boston Celtics"="1610612738", "Brooklyn Nets"="1610612751", "New York Knicks"="1610612752",
             "Philadelphia 76ers"="1610612755", "Toronto Raptors"="1610612761","Chicago Bulls"="1610612741"
             ,"Cleveland Cavaliers"="1610612739","Detroit Pistons"="1610612765","Indiana Pacers"="1610612754",
             "Milwaukee Bucks"="1610612749","Atlanta Hawks"="1610612737","Charlotte Hornets"="1610612766",
             "Washington Wizards"="1610612764","Miami Heat"="1610612748","Orlando Magic"="1610612753",
             "Denver Nuggets"="1610612743","Minnesota Timberwolves"="1610612750","Oklahoma City Thunder"="1610612760",
             "Portland TrailBlazers"="1610612757","Utah Jazz"="1610612762","Golden State Warriors"="1610612744",
             "Los Angeles Clippers"="1610612746","Los Angeles Lakers"="1610612747","Phoenix Suns"="1610612756",
             "Sacramento Kings"="1610612758","Dallas Mavericks"="1610612742","Houston Rockets"="1610612745",
             "Memphis Grizzlies"="1610612763","New Orleans Pelicans"="1610612740","San Antonio Spurs"="1610612759")

################### Data Load for Prediction #####################

##################################################################

###################
##### Loading Twitter Data ############################

#load("tweets_10K.RData") #Direct twitter feed downloaded using TwitteR library and searchTwitter function hitting the Twitter API. Account got blocked as we hit the Twitter API 10K times.
#tweets.text <- sapply(tweets_10K, function(x) x$getText())  #List of Tweets is more than 32Mb and uploading it on server caused issues so had to clean it and store it separately
#save(tweets.text,file="./Data/tweets_10K_clean.RData")
tweets <- read_tsv("./Data/twitterfeed.tsv")
load("./Data/tweets_10K_clean.RData")
tweets.text <- gsub("rt", "", tweets.text)
tweets.text <- gsub("@\\w+", "", tweets.text)
tweets.text <- gsub("[[:punct:]]", "", tweets.text)
tweets.text <- gsub("http\\w+", "", tweets.text)
tweets.text <- gsub("[ |\t]{2,}", "", tweets.text)
tweets.text <- gsub("^ ", "", tweets.text)
tweets.text <- gsub(" $", "", tweets.text)
tweets.text.corpus <- Corpus(VectorSource(tweets.text))
tweets.text.corpus <- tm_map(tweets.text.corpus, function(x)removeWords(x,stopwords())) #Removing stopwords from the data
######


###### Team Profile Data Load ########################
teams = read.csv(file = "./Data/teams.csv", header = 1, sep = ",")
team_last_five = read.csv(file = "./Data/team_last_five.csv", header = 1, sep = ",")
player_stats1 = read.csv(file = "./Data/player_stats1.csv", header = 1, sep = ",")
roster = read.csv(file = "./Data/roster.csv", header = 1, sep = ",")
salary = read.csv(file = "./Data/salary.csv", header = 1, sep = ",")
numeric_player_stats = player_stats1 %>% select(age:points)
numeric_last_five = team_last_five %>% select(Season, Wins, Losses, SRS, Pace, Rel_Pace, ORtg, DRtg, Rel_DRtg)
############################################


###### Player Profile Data Processing #######
player_data = read.csv("./Data/all_seasons.csv")
#filtering data - taking records of last 5 years
player_data <- player_data %>% filter(season == "2016-17" | season == "2017-18" | season == "2018-19" | season == "2019-20" | season == "2020-21")
# selecting only active payers, will use variable active_players_data henceforth
active_players <- player_data %>% filter(season == '2020-21')
active_players_names <- active_players$player_name
rm(active_players)
player_data$active <- ifelse(player_data$player_name %in% active_players_names,1,0)
active_players_data <- player_data %>% filter(active == 1)
acn <- active_players_data %>% group_by(player_name) %>%summarise(most_gp_ios = max(gp),gp = sum(gp))%>% arrange(desc(player_name))%>%filter(gp > 100 | most_gp_ios > 50 ) 
specific_player_name = ""
#############################################

##### Twitter Timeline Display Functions #####

users <- tweets %>%
  pull(screen_name) %>%
  unique() %>%
  sort()

make_links <- function(urls, text, icon = NULL) {
  if (is.na(urls)) return("")
  split_urls <- unlist(str_split(urls, ","))
  if (length(split_urls) > 1) {
    text <- paste(text, 1:length(split_urls))
  }
  names(split_urls) <- text 
  links <- imap(split_urls, ~ tags$a(.y, href = .x, target = "_blank"))
  c(icon, links)
}


embed_tweet <- function(tweet_url) {
  paste0(
    "<blockquote class='twitter-tweet'>",
    "<p lang='en' dir='ltr'>",
    "Loading...",
    "<a href='", tweet_url, "'></a>",
    "</p>",
    "</blockquote>"
  )
}

make_embedded_tweets_table <- function(tweets) {
  reactable(
    tweets,
    showPageInfo = FALSE,
    defaultPageSize = 1,
    sortable = FALSE,
    width = 500,
    height = 600,
    columns = list(status_url = colDef(cell = embed_tweet, html = TRUE)),
    rowClass = JS("function() {twttr.widgets.load()}")
  )
}

########################

title <- tags$p(tags$img(src="NBA.png", height="50", width="50"),"Analytics Dashboard")

dashHeader <- dashboardHeader(title = title,
                              titleWidth= 300
)

dashSidebar <- dashboardSidebar(
  width=300,
  sidebarMenu(id="my_sidebarmenu",
    menuItem('Home', tabName = "HomeTab", icon=icon('dashboard')
    ),
    menuItem('Social Listening',tabName = 'SocialListening',
             icon=icon('twitter')
    ),
    menuItem('Team Profile',tabName = 'TeamProfile',
             icon=icon('users')
    ),
    menuItem('Players Profile',tabName = 'PlayerProfile',
             icon=icon('basketball-ball')
    ),
    menuItem('Cluster Analysis',tabName = 'Clustering',
             icon=icon('users-cog')
    ),
    menuItem('Predict Winner',tabName = 'Prediction',
             icon=icon('trophy')
    )
  )
)

dashBody <- dashboardBody(
  
  tabItems(
    tabItem(tabName = 'HomeTab',
            h1('NBA Analytics Dashboard', style="text-align:center"),
            p("The National Basketball Association (NBA) is a professional basketball league in North America. The league is composed of 30 teams (29 in the United States and 1 in Canada) and is one of the four major professional sports leagues in the United States and Canada. It is the premier men's professional basketball league in the world.The league was founded in New York City on June 6, 1946, as the Basketball Association of America (BAA). It changed its name to the National Basketball Association on August 3, 1949, after merging with the competing National Basketball League (NBL). The NBA's regular season runs from October to April, with each team playing 82 games. The league's playoff tournament extends into June. As of 2020, NBA players are the world's best paid athletes by average annual salary per player.",
              style="font-size:125%"),
            p('Source: Wikipedia',style="text-align:right;font-style:italic;"),
            fluidRow(
              column(width=12,tags$iframe(width="560",height="315",src="https://www.youtube.com/embed/W8ioaqx4HeM?controls=0", title="YouTube video player",frameborder="0",allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",allowfullscreen=NA))
            ),
            fluidRow( box(width="100%",
                          background = "navy",
                          style = "text-align:center;font-weight:bold;",
                          "Eastern Conference"
            )),
            fluidRow(background="red",
                     column(width=2,box(width=10,height=100,br(),"Atlantic",style="text-align:center;font-weight:bold;font-size:125%",background = "red")),
                     column(width=2,tags$figure(img(src = "bostonceltics.png",width="100",height="100"),tags$figcaption("Boston Celtics",style="font-weight:bold;"))),
                     column(width=2,tags$figure(img(src = "brooklynnets.png",width="100",height="100"),tags$figcaption("Brooklyn Nets",style="font-weight:bold;"))),
                     column(width=2,tags$figure(img(src = "newyorkknicks.png",width="100",height="100"),tags$figcaption("New York Knicks",style="font-weight:bold;"))),
                     column(width=2,tags$figure(img(src = "philadelphia76ers.png",width="100",height="100"),tags$figcaption("Philadelphia 76ers",style="font-weight:bold;"))),
                     column(width=2,tags$figure(img(src = "torontoraptors.png",width="100",height="100"),tags$figcaption("Toronto Raptors",style="font-weight:bold;")))
            ), tags$br(),
            fluidRow(
              column(width=2, box(width=10,height=100,br(),"Central",style="text-align:center;font-weight:bold;font-size:125%", background ="yellow")),
              column(width=2,tags$figure(img(src = "chicagobulls.png",width="100",height="100"),tags$figcaption("Chicago Bulls",style="font-weight:bold;"))),
              column(width=2,tags$figure(img(src = "clevelandcavaliers.png",width="100",height="100"),tags$figcaption("Cleveland Cavaliers",style="font-weight:bold;"))),
              column(width=2,tags$figure(img(src = "detroitpistons.png",width="100",height="100"),tags$figcaption("Detroit Pistons",style="font-weight:bold;"))),
              column(width=2,tags$figure(img(src = "indianapacers.png",width="100",height="100"),tags$figcaption("Indiana Pacers",style="font-weight:bold;"))),
              column(width=2,tags$figure(img(src = "milwaukeebucks.png",width="100",height="100"),tags$figcaption("Milwaukee Bucks",style="font-weight:bold;")))
            ),tags$br(),
            fluidRow(
              column(width=2, box(width=10,height=100,br(),"SouthEast",style="text-align:center;font-weight:bold;font-size:125%",background="teal")),
              column(width=2,tags$figure(img(src = "atlantahawks.png",width="100",height="100"),tags$figcaption("Atlanta Hawks",style="font-weight:bold;"))),
              column(width=2,tags$figure(img(src = "charlottehornet.png",width="100",height="100"),tags$figcaption("Charlotte Hornets",style="font-weight:bold;"))),
              column(width=2,tags$figure(img(src = "washingtonwizards.png",width="100",height="100"),tags$figcaption("Washington Wizards",style="font-weight:bold;"))),
              column(width=2,tags$figure(img(src = "miamiheat.png",width="100",height="100"),tags$figcaption("Miami Heat",style="font-weight:bold;"))),
              column(width=2,tags$figure(img(src = "orlandomagic.png",width="100",height="100"),tags$figcaption("Orlando Magic",style="font-weight:bold;")))
            ),tags$br(),
            fluidRow( box(width="100%",
                          background = "maroon",
                          style = "text-align:center;font-weight:bold;",
                          "Western Conference"
            )),
            fluidRow(
              column(width=2, box(width=10,height=100,br(),"Northwest",style="text-align:center;font-weight:bold;font-size:125%",background="lime")),
              column(width=2,tags$figure(img(src = "denvernuggets.png",width="100",height="100"),tags$figcaption("Denver Nuggets"))),
              column(width=2,tags$figure(img(src = "minnesotatimberwolves.png",width="100",height="100"),tags$figcaption("Minnesota Timberwolves",style="font-weight:bold;"))),
              column(width=2,tags$figure(img(src = "oklahomacitythunder.png",width="100",height="100"),tags$figcaption("Oklahoma City Thunder",style="font-weight:bold;"))),
              column(width=2,tags$figure(img(src = "portlandtrailblazers.png",width="100",height="100"),tags$figcaption("Portland TrailBlazers",style="font-weight:bold;"))),
              column(width=2,tags$figure(img(src = "utahjazz.png",width="100",height="100"),tags$figcaption("Utah Jazz",style="font-weight:bold;")))
            ),tags$br(),
            fluidRow(
              column(width=2, box(width=10,height=100,br(),"Pacific",style="text-align:center;font-weight:bold;font-size:125%",background="purple")),
              column(width=2,tags$figure(img(src = "goldenstatewarriors.png",width="100",height="100"),tags$figcaption("Golden State Warriors",style="font-weight:bold;"))),
              column(width=2,tags$figure(img(src = "losangelesclippers.png",width="100",height="100"),tags$figcaption("Los Angeles Clippers",style="font-weight:bold;"))),
              column(width=2,tags$figure(img(src = "losangeleslakers.png",width="100",height="100"),tags$figcaption("Los Angeles Lakers",style="font-weight:bold;"))),
              column(width=2,tags$figure(img(src = "phoenixsuns.png",width="100",height="100"),tags$figcaption("Phoenix Suns",style="font-weight:bold;"))),
              column(width=2,tags$figure(img(src = "sacramentokings.png",width="100",height="100"),tags$figcaption("Sacramento Kings",style="font-weight:bold;")))
            ),tags$br(),
            fluidRow(
              column(width=2, box(width=10,height=100,br(),"Southwest",style="text-align:center;font-weight:bold;font-size:125%",background="fuchsia")),
              column(width=2,tags$figure(img(src = "dallasmavericks.png",width="100",height="100"),tags$figcaption("Dallas Mavericks",style="font-weight:bold;"))),
              column(width=2,tags$figure(img(src = "houstonrockets.png",width="100",height="100"),tags$figcaption("Houston Rockets",style="font-weight:bold;"))),
              column(width=2,tags$figure(img(src = "memphisgrizzlies.png",width="100",height="100"),tags$figcaption("Memphis Grizzlies",style="font-weight:bold;"))),
              column(width=2,tags$figure(img(src = "neworleanspelicans.png",width="100",height="100"),tags$figcaption("New Orleans Pelicans",style="font-weight:bold;"))),
              column(width=2,tags$figure(img(src = "sanantoniospurs.png",width="100",height="100"),tags$figcaption("San Antonio Spurs",style="font-weight:bold;")))
            )
            
    ),
    tabItem(tabName = 'TeamProfile',
            fluidRow(column(width=3,selectInput(inputId = "choose_team", label = "Select Your Home Team:", choices = team_id)),
                     column(width=5, h2(textOutput("team_name"), style="font-weight:bold;text-align:center;"))), 
                     br(),
            tabsetPanel(type ="tab",
                                 tabPanel("Team Information", 
                                          br(),
                                          fluidRow(column(width=7,box(title="Team Overview",height="150",width="150",solidHeader=T,tableOutput("new_teams"), status="info"),
                                                          box(title="Season Results (Last 5 Seasons)",
                                                              solidHeader=T, status="info", width="150",
                                                              tableOutput("last_five_table1"))),
                                                   
                                                   column(width=3, align="center",box(title="Team Logo",solidHeader=T, height="440",
                                                                                     width="100%",status="info",br(), br(),imageOutput("teamimage", height="200px")))),
                                          fluidRow(box(title="Matches won every season", width=12,solidHeader=T, status="info",plotOutput("wins_years_bar"))),
                                          br(),
                                          fluidRow(box(title="Team Statistics (Last 5 Seasons)", status="info", solidHeader=T,width=12,
                                                   tableOutput("last_five_table2"))),
                                          fluidRow(box(title="Team Performance Statistical Relations", status="info", solidHeader=T, width=12,height="570px",
                                                       column(width=3,selectInput("results_x", "Choose an X Variable to Plot: ", choices =  names(numeric_last_five), selected="Pace")),
                                                       column(width=3,selectInput("results_y", "Choose a Y Variable to PLot: ", choices =  names(numeric_last_five), selected="DRtg")),
                                                       plotOutput("team_wins"))),
                                          br()),
                                 tabPanel("Player Information", 
                                          br(),
                                          fluidRow(box(title="Individual Player Statistics (2020-2021)", solidHeader=T,status="warning",
                                                                       dataTableOutput("top12_player_stats"), width=12)),
                                          fluidRow(box(title="More Player Statistics (2020-2021)", solidHeader=T,status="warning",
                                                       dataTableOutput("more_player_stats_out"), width=12, collapsible = TRUE, collapsed=TRUE)),
                                          fluidRow(box(title="Average # of Points Scored Per Game By Each Player", solidHeader=T,status="warning",
                                                       plotOutput("player_points_bar"), width=12, collapsible = TRUE, collapsed=FALSE)),
                                          fluidRow(box(title="Player Metrics Correlation", solidHeader=T,status="warning", height="570px",
                                                       column(width=3,selectInput("x_variable", "Choose an X Variable to Plot:", choices = names(numeric_player_stats), selected="age")),
                                                       column(width=3,selectInput("y_variable", "Choose a Y Variable to Plot: ", choices =  names(numeric_player_stats),selected="three_point_percent")),
                                                       plotOutput('ind_stats'), width=12, collapsible = TRUE, collapsed=FALSE)
                                          )),
                                 tabPanel("Roster and Salary Information", 
                                          br(),
                                          fluidRow(box(title="Team's Current Roster", solidHeader=T,status="success",
                                                       tableOutput("roster_info"), width=8)),
                                          fluidRow(box(title="Team's Age Distribution", solidHeader=T,status="success",
                                                       plotOutput("age_graph"), width=12)),
                                          fluidRow(box(title="Team Salary Forecast", solidHeader=T,status="success",
                                                       tableOutput("salary_info"),plotOutput("year_salary"), width=12))
            )
          )
    ),
    tabItem(tabName = 'PlayerProfile',
            selectInput(inputId = "pid",
                        label = "Select a player",
                        "Names"),
            
            verbatimTextOutput("pn"),
            tableOutput("pi"),
            
            tabsetPanel(type = "tabs",
                        tabPanel("Single Player Profile",
                                 fluidRow(box(title="Individual Player Ratings", status="info",solidHeader=T,
                                          plotOutput("plot1"), width=12)),
                                 fluidRow(box(title="Players Attack Ranking", status="info", solidHeader=T, width=12,
                                              plotOutput("plot_att") %>% helper(icon="question", colour="green", type="inline",
                                                                                content = c("<p> <b>Graph 1</b>: Attacking Rank: based on Average Points </br> Height ( on y axis ) -> Average points earned by a player each game </br> Color Gradient ( black to red ) -> Redder color indicates higher number of total points. </br> This inturn indicates that the player has been playing for a long time </br> For players with a higher Total, the given Average rating statistic also becomes more reliable, as it has been recorded over a longer period of time </br> It would also mean that the player is more experienced </br> :: Higher the better! </br> :: Redder he better! </p>",
                                                                                            "<p> <b>Graph 2</b>: Attacking Rank: based on Average Assists </br> Height ( on y axis ) -> Average assists given by a player each game</br> Color Gradient ( black to blue ) -> Bluer color indicates higher number of total assists. </br> This inturn indicates that the player has been playing for a long time </br> For players with a higher Total, the given Average rating statistic also becomes more reliable, as it has been recorded over a longer period of time</br> It would also mean that the player is more experienced </br> Higher the better!</br> :: Bluer the better! </p>"))
                                 )),
                                 fluidRow(box(title="Players Defense Ranking", status="info", solidHeader=T, width=12,
                                              plotOutput("plot_def") %>% helper(icon="question", colour="green", type="inline",
                                                                                content = c("<p>Our metric for defensive performance is <b> defensive rebounds  </b> i.e., when a player obtains the possession of the ball after a missed shot by the offense.</br></br>Graph: Defensive Rank: based on Average Defensive Rebounds </br> X axis -> Average defensive rebounds given by a player each game</br> Color Gradient ( black to Green ) -> Green color indicates higher number of total defensive points</br></br>:: Further on the X Axis (horizontally), the better!</br>:: Greener the better! </p>"))))
                                 ),
                        tabPanel("Player Comparison",selectInput("pid2", "Select second player","Names"),verbatimTextOutput("pn2"),
                                 tableOutput("pi2"),
                                 fluidRow(box(title="Player Comparision", status="warning", solidHeader=T,width=12,
                                             plotOutput("compare")))
            )
            
            )       
    ),
    tabItem(tabName = 'Prediction',
           fluidRow(
             column(width=3,selectInput(inputId = "team1", label = "Select Your Home Team:", choices = team_id, selected="1610612738")),
             column(width=2),
             column(width=3,selectInput(inputId = "team2", label = "Select Your Away Team:", choices = team_id, selected="1610612751"))
           ),
           fluidRow(column(width=3,imageOutput("home1", height="280px")),
                    column(width=2,br(),br(),br(),br(),img(src="versus.png",width="100",height="100"), align="center"),
                    column(width=3,imageOutput("away1", height="280px"))),
           br(),
           fluidRow(
             column(width=4, box(title = "The Wining Team is:",solidHeader = T,span(uiOutput('table2'), style ='color:blue'), width="100")),
             column(width=4, box(title = 'Probability of winning',solidHeader = T,span(uiOutput('table1'), style ='color:red'), width="100"))
             )
    ),
    tabItem(tabName = 'SocialListening',
            fluidRow(column(width=7,box(title="Word Cloud of Twitter Reactions",width=12, solidHeader=T, status="danger",
                                        plotOutput('socialplot', width="100%", height="600px"))),
                     column( width = 5,
                             tags$script(src = "https://platform.twitter.com/widgets.js", charset = "utf-8"),
                             tags$link(href = "https://fonts.googleapis.com/css?family=Roboto+Mono", rel = "stylesheet"),
                             tags$style(HTML('*#sidebar {background-color: #fff;border: 0px;}
                             .rt-th {display: none;}
                             .rt-noData {display: none;}
                             .rt-pagination-nav {float: left;width: 100%;}')),
                             box(title="Twitter Feed for #NBA Tweets",status="danger",width=12, solidHeader=T,reactableOutput("embedded_user_tweets_table", width = "400px"))
                     )
            )
    ),
    tabItem(tabName = 'Clustering',
            fluidRow(box(title="Clusters of Players", width=12, status="info", solidHeader=T,
                         plotOutput("clusteranalysis", height="800px"))
            )
    )
  )  
)

ui <- dashboardPage (skin="black",
                     header=dashHeader,
                     sidebar = dashSidebar,
                     body= dashBody,
                     title = 'NBA Analytics Dashboard'
)

server <- function(input, output, session){
  
  ###### Observe Help Buttons####
  observe_helpers()
  ###############################
  ##### Twitter Timeline #####
  user_tweets <- reactive(
    tweets %>%
      select(status_url, created_at, favorite_count, retweet_count) %>%
      arrange(desc(created_at))
  )
  
  output$user_links <- renderUI(
    tagList(make_links(paste0("https://twitter.com/", input$user_name), "Twitter", "\U0001F4AC"))  # speech bubble
  )
  
  output$embedded_user_tweets_table <- renderReactable(
    user_tweets() %>%
      select(status_url) %>%
      make_embedded_tweets_table()
  )
  ############################
  #### Word Cloud ######  
  output$socialplot <- renderPlot({
    wordcloud <- wordcloud(tweets.text.corpus,min.freq = 2, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE,max.words=300)
  })
  ############################
  
  ###Team Profile######
  output$team_name <- renderText({
    team_name <- as.character(teams %>% filter(TEAM_ID==input$choose_team) %>% select(Team_Id))
  })
  output$teamimage <- renderImage({
    
    imagename <- as.character(team_list_img[as.character(teams %>% filter(TEAM_ID==input$choose_team) %>% select(Team_Id))])
    
    filename <- normalizePath(file.path('./www',
                                        paste(imagename, '.png', sep='')))
    list(src = filename,
         width=250,
         height=250)
  }, deleteFile = FALSE)
  
  output$home1 <- renderImage({
    
    imagename <- as.character(team_list_img[as.character(teams %>% filter(TEAM_ID==input$team1) %>% select(Team_Id))])
    
    filename <- normalizePath(file.path('./www',
                                        paste(imagename, '.png', sep='')))
    list(src = filename,
         width=250,
         height=250)
  }, deleteFile = FALSE)
  
  output$away1 <- renderImage({
    
    imagename <- as.character(team_list_img[as.character(teams %>% filter(TEAM_ID==input$team2) %>% select(Team_Id))])
    
    filename <- normalizePath(file.path('./www',
                                        paste(imagename, '.png', sep='')))
    list(src = filename,
         width=250,
         height=250)
  }, deleteFile = FALSE)
  
  #####################
  
  ####### Player Profile ########
  
  gen_data <- reactive({
    require(input$pid) 
    graph_data <- active_players_data %>% group_by(player_name) %>% summarise(avg_pts = mean(pts),total_pts = sum(pts),avg_ast = mean(ast), total_ast = sum(ast),avg_def = mean(dreb_pct),total_def = sum(dreb_pct), avg_rating = mean(net_rating),games_played = sum(gp),most_gp_ios = max(gp),avg_reb=mean(reb)) %>% filter(games_played > 100 | most_gp_ios > 50 ) 
    graph_data$selected = as.factor(ifelse(graph_data$player_name != input$pid,0,1))
    graph_data <-  graph_data %>% mutate(player_name = fct_reorder(player_name, avg_rating)) 
  })
  
  output$plot1 <- renderPlot(
    ggplot(gen_data(), aes(x = player_name, y = as.numeric(avg_rating), fill = selected)) +
      geom_bar(stat='identity') + theme_classic() + theme(axis.text.x=element_blank(), legend.position = "none") + ggtitle(paste("Overall Ranking - Based on Average Rating per Game -", input$pid)) +
      xlab("--All players--") + ylab("Average Ratings")  + theme(
        plot.title = element_text(color="black", size=20, face="bold"),
        axis.title.x = element_text(color="black", size=15, face="bold"),
        axis.title.y = element_text(color="black", size=15 , face="bold")
      )   + scale_fill_discrete(name = "", labels = c(paste("Not",input$pid),input$pid) )
    + geom_text(aes(label = player_name),data = gen_data() %>% filter(selected == 1),nudge_y = -9)
  )
  
  output$plot_att <- renderPlot({
    # graph 2 showing attacking rank among other players based on points - 
    p <- ggplot(data = gen_data(), aes(x = player_name, y = avg_pts)) + ggtitle("Attacking rank based on Points") +
      geom_point(aes(color = total_pts, shape = selected, size = selected)) + scale_colour_gradient(low = "black", high = "red") + xlab("--All players--") + ylab("Average Points")
    p <- p  + theme_classic() + theme(axis.text.x=element_blank())+ geom_text(aes(label = player_name),data = gen_data() %>% filter(selected == 1),nudge_y = 0.1) 
    p <- p + labs(color = "Total Points") + guides(shape=FALSE, size=FALSE)
    p <- p + theme(
      plot.title = element_text(color="black", size=15, face="bold"),
      axis.title.x = element_text(color="black", size=12, face="bold"),
      axis.title.y = element_text(color="black", size=12 , face="bold"))
   #graph 3 showing attacking rank among other players based on assists
    q <- ggplot(data = gen_data(), aes(x = player_name, y = avg_ast)) + ggtitle("Attacking rank based on Assists ") +
      geom_point(aes(color = total_ast, shape = selected, size = selected)) + scale_colour_gradient(low = "black", high = "blue") + xlab("--All players--") + ylab("Average Assists")
   
    q <- q + theme_classic() + theme(axis.text.x=element_blank()) + guides(shape=FALSE, size=FALSE) + geom_text(aes(label = player_name),data = gen_data() %>% filter(selected == 1),nudge_y = 0.1)
    q <- q + labs(color = "Total Assists") + theme(
      plot.title = element_text(color="black", size=15, face="bold"),
      axis.title.x = element_text(color="black", size=12, face="bold"),
      axis.title.y = element_text(color="black", size=12 , face="bold"))
    #displaying attacking stats graphs
    grid.arrange(p, q, nrow = 1, top=specific_player_name)
  } )
  
  output$plot_def <- renderPlot({
    graph_data <-  gen_data() %>% mutate(player_name = fct_reorder(player_name, avg_def))
    #plot
    gd <- ggplot(graph_data, aes(x=player_name, y=avg_def)) 
    gd = gd + geom_point(aes(color = total_def, shape = selected, size = selected)) 
    gd = gd + theme_classic() + theme(axis.text.y = element_blank()) + guides(shape=FALSE, size=FALSE) 
    gd = gd + coord_flip() 
    gd = gd + scale_colour_gradient(low = "black", high = "green") 
    gd = gd + geom_text(aes(label = player_name),data = gen_data() %>% filter(selected == 1),nudge_y = 0.04) 
    gd = gd + labs(title = "Defensive Ranking",color = "Total Defensive Reb.") +  xlab("--All Players--") + ylab("Average Defensive Rebounds")
    gd = gd + theme(
      plot.title = element_text(color="black", size=15, face="bold"),
      axis.title.x = element_text(color="black", size=12, face="bold"),
      axis.title.y = element_text(color="black", size=12 , face="bold"))
    gd
  })
  
  output$compare <- renderPlot({
    require(input$pid2)
    compare_graph <- gen_data() %>% filter(player_name == input$pid | player_name == input$pid2)
    g1 <- ggplot(compare_graph, aes(player_name, avg_pts,fill = player_name)) + theme(axis.title.x = element_blank(),axis.title.y = element_blank())
    g1<- g1 + geom_bar(stat="identity", width = 0.5) + 
      labs(title="Average Game Rating", x="Player Name", y="Average Points") + theme_classic() + theme(legend.position = "0") 
    g3 <- ggplot(compare_graph, aes(player_name, games_played ,fill = player_name)) + theme(axis.title.x = element_blank(),axis.title.y = element_blank())
    g3<- g3 + geom_bar(stat="identity", width = 0.5) + 
      labs(title="Games Played", x="Player Name", y="Games Played")+ theme_classic() + theme(legend.position = "0")
    
    g4 <- ggplot(compare_graph, aes(player_name, avg_reb,fill = player_name)) + theme(axis.title.x = element_blank(),axis.title.y = element_blank())
    g4<- g4 + geom_bar(stat="identity", width = 0.5) + 
      labs(title="Average Rebounds", x="Player Name", y="Average Rebounds")+ theme_classic() + theme(legend.position = "0")
    
    g5 <- ggplot(compare_graph, aes(player_name, avg_def,fill = player_name)) + theme(axis.title.x = element_blank(),axis.title.y = element_blank())
    g5<- g5 + geom_bar(stat="identity", width = 0.5) + theme_classic() +  theme(legend.position = "0") +
      labs(title="Average Defensive Rebounds", x="Player Name", y="Average Defensive Points") 
    
    g6  <- ggplot(compare_graph, aes(player_name, avg_ast,fill = player_name)) + theme(axis.title.x = element_blank(),axis.title.y = element_blank())
    g6 <- g6 + geom_bar(stat="identity", width = 0.5) + 
      labs(title="Average Assist", x="Player Name", y="Average Assists")+ theme_classic() +  theme(legend.position = "0")
    
    gpts  <- ggplot(compare_graph, aes(player_name, avg_pts,fill = player_name)) + theme(axis.title.x = element_blank(),axis.title.y = element_blank())
    gpts <- gpts + geom_bar(stat="identity", width = 0.5) + 
      labs(title="Average Points", x="Player Name", y="Average Points")+ theme_classic() + theme(legend.position = "0")
    
    #displaying comparative graphs
    grid.arrange(g3,g1, gpts,g4,g5,g6, nrow = 2, top= paste(input$pid,"v",input$pid2,"Stats - Last 5 seasons")) 
    
  })
  
  observe({
    updateSelectInput(session, "pid", choices = acn$player_name, selected="Stephen Curry")  })
  
  observe({
    updateSelectInput(session, "pid2", choices = acn$player_name, selected = "Giannis Antetokounmpo")  })
  
  #----------------------Text Output-------------------
  output$pn <- renderText({
    paste("Player Name :",input$pid)
    })
  
  output$pn2 <- renderText({
    paste("Player Name :",input$pid2)
    
  })
  
  p_info = reactive({
    pinf <- active_players_data %>% select(player_name,team_abbreviation,age,player_height,player_weight,college,country,draft_year,season,gp) %>% filter(player_name == input$pid) %>% arrange(desc(season))
    gp <- sum(as.numeric(pinf$gp))
    pinf <- pinf %>% select(team_abbreviation,age,player_height,player_weight,college,country,draft_year)
    pinf <- pinf %>% rename( "Team" =team_abbreviation,"Age" = age,"Height"=player_height,"Weight"=player_weight,"College"=college,"Country"=country,"Draft Year" =draft_year )
    pinf$gp <- gp
    pinf <- pinf %>% rename ("Total Games Played" = gp)
    pinf[1,]
  })
  output$pi = renderTable({(p_info())})
  
  output$pi2 = renderTable({(p_info2())})
  p_info2 = reactive({
    pinf <- active_players_data %>% select(player_name,team_abbreviation,age,player_height,player_weight,college,country,draft_year,season,gp) %>% filter(player_name == input$pid2)  %>% arrange(desc(season))
    gp <- sum(as.numeric(pinf$gp))
    pinf <- pinf %>% select(team_abbreviation,age,player_height,player_weight,college,country,draft_year)
    pinf <- pinf %>% rename( "Team" =team_abbreviation,"Age" = age,"Height"=player_height,"Weight"=player_weight,"College"=college,"Country"=country,"Draft Year" =draft_year )
    pinf$gp <- gp
    pinf <- pinf %>% rename ("Total Games Played" = gp)
    pinf[1,]
  })
  
  ###############################
  
  ####################### Team Profile ################################
  #Code to create the Team Information tab
  team_info_react = reactive({team_info = teams %>% 
    select(YEARFOUNDED, ARENA, ARENACAPACITY, OWNER, GENERALMANAGER, HEADCOACH) %>% 
    filter(teams$TEAM_ID == input$choose_team) %>% 
    rename('First Season' = YEARFOUNDED, 'Arena Name' = ARENA, 'Arena Capacaity' = ARENACAPACITY, 'Owner' = OWNER, 'General Manager' = GENERALMANAGER, 'Head Coach' = HEADCOACH)})
  output$new_teams = renderTable({team_info_react()})
  
  team_last_five_react1 = reactive({each_team_data1 = team_last_five %>% 
    select(Season, Wins, Losses, Win_Percent, Finish, Playoffs) %>% 
    filter(team_last_five$TEAM_ID == input$choose_team) %>%
    rename('Win Percentage' = Win_Percent, 'Place in Division' = Finish, 'Playoff Results' = Playoffs)
  })
  output$last_five_table1 = renderTable({team_last_five_react1()})
  
  team_last_five_react2 = reactive({each_team_data2 = team_last_five %>% 
    select(Season, SRS, Pace, Rel_Pace, ORtg, Rel_ORtg, DRtg, Rel_DRtg) %>% 
    filter(team_last_five$TEAM_ID == input$choose_team) %>%
    rename('Simple Rating System' = SRS,'Relative Pace' = Rel_Pace, 'Offensive Rating' = ORtg, 'Relative Offensive Rating' = Rel_ORtg, 'Defensive Rating' = DRtg, 'Relative Defensive Rating' = Rel_DRtg)
  })
  output$last_five_table2 = renderTable({team_last_five_react2()})
  
  #All Good to Go - Interactive Scatter Plot
  team_wins_react = reactive({team_last_five %>% 
      filter(team_last_five$TEAM_ID == input$choose_team)})
  output$team_wins = renderPlot({ggplot(team_wins_react(), aes_string(x = input$results_x, y = input$results_y)) +
      geom_point(size = 4) + 
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      theme(text = element_text(size = 16))})
  
  #All Good to Go - Bar Graph
  wins_years_react = reactive({team_last_five %>% select(Season, Wins, Losses) %>% filter(team_last_five$TEAM_ID == input$choose_team)})
  
  output$wins_years_bar = renderPlot({ggplot(wins_years_react(), aes(x = Season, y = Wins)) + 
      geom_bar(stat = "identity", fill = "lightblue") + 
      xlab("Season") + 
      ylab("Number of Wins") + 
      theme_classic() +
      ggtitle("Number of Wins (Last 5 Seasons)") + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      theme(text = element_text(size = 16)) +
      geom_text(aes(label = Wins), vjust = -.5, size = 6) + 
      ylim(0,70) 
  })
  
  
  
  #Code to create Player Information Tab
  player_stats_table_react = reactive({stats_table = player_stats1 %>% 
    select(player_name, gs, min, fg_percent, three_point_percent, two_point_percent, ft_percent, trb, ast, stl, blk, points) %>% 
    filter(player_stats1$TEAM_ID == input$choose_team) %>% rename("Player" = player_name, "GS" = gs, "MPG" = min, "FG%" = fg_percent, "3P%" = three_point_percent, "2P%" = two_point_percent, "FT%" = ft_percent, "RPG" = trb, "APG" = ast, "SPG" = stl, "BPG" = blk, "PPG" = points)
  })
  output$top12_player_stats = renderDataTable({player_stats_table_react()})
  
  more_player_stats_react = reactive({ player_stats1 %>% 
      select(player_name, age, gp, fgm,fga, three_point_made, two_point_made, efg, ftm, orb, drb, turnovers, fouls) %>% 
      filter(player_stats1$TEAM_ID == input$choose_team) %>% rename("Player" = player_name, "Age" = age, "GP" = gp, "FGM" = fgm, "FGA" = fga, "3PM" = three_point_made, "2PM" = "two_point_made", "EFG%" = efg, "FTM" = ftm, "ORPG" = orb, "DRPG" = drb, "TPG" = turnovers, "FPG" = fouls)
  })
  output$more_player_stats_out = renderDataTable({more_player_stats_react()})
  
  ind_stats_react = reactive({player_stats1 %>% filter(player_stats1$TEAM_ID == input$choose_team)})
  output$ind_stats = renderPlot({ggplot(ind_stats_react(), aes_string(x = input$x_variable, y = input$y_variable)) + 
      geom_line( size = 1.4) +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5)) + 
      theme(text = element_text(size = 16))
    
  })
  
  #All good to go - Bar Graph
  player_points_react = reactive({player_stats1 %>% select(player_name, points) %>% filter(player_stats1$TEAM_ID == input$choose_team)})
  output$player_points_bar = renderPlot({ggplot(player_points_react(), aes(x = reorder(player_name, -points), y = points)) + 
      geom_bar(stat = "identity", fill = "lightblue") + 
      xlab("Player") + 
      ylab("Points Per Game (PPG)") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5)) + 
      theme(text = element_text(size = 16)) +
      geom_text(aes(label = points), vjust = -.5, size = 6) + 
      ylim(0,35) 
  })
  
  
  #code to create roster and salary tab
  roster_react = reactive({roster %>% select(name, number, position, birth_date, exp, college) %>% 
      filter(roster$TEAM_ID == input$choose_team) %>%
      rename("Player" = name, "Jersey Number" = number, "Position" = position, "Date of Birth" = birth_date, "Number of Years in NBA" = exp, "College Attended" = college)
  })
  output$roster_info = renderTable({roster_react()})
  
  
  salary_react = reactive({salary %>% select(player_name, age, y2020_21, y2021_22, y2022_23, y2023_24, y2024_25, contract_type, guaranteed) %>%
      filter(salary$TEAM_ID == input$choose_team) %>%
      rename("Player" = player_name, "Age" = age, "2020-2021"= y2020_21,"2021-2022"= y2021_22,"2022-2023"= y2022_23, "2023-2024"= y2023_24,"2024-2025"= y2024_25, "Type of Contract" = contract_type, "Amount of Guaranteed Money" = guaranteed)
  })
  output$salary_info = renderTable({salary_react()})
  
  
  player_salary_react = reactive({salary %>% select(age, player_name) %>% filter(salary$TEAM_ID == input$choose_team & player_name != 'Team Totals')})
  output$age_graph = renderPlot({ggplot(player_salary_react(), aes(x = reorder(player_name, age), y = age)) + 
      geom_bar(stat = "identity", fill = "lightblue") + 
      xlab("Player") + 
      ylab("Player Age") +
      theme_classic() +
      ggtitle("Age by Player") +
      theme(plot.title = element_text(hjust = 0.5)) + 
      theme(text = element_text(size = 12)) +
      geom_text(aes(label = age), vjust = -.5, size = 6) +
      ylim(0,36)
  })
  
  
  year_salary_react = reactive({salary %>% select(player_name, y2020_21_new) %>% filter(salary$TEAM_ID == input$choose_team & player_name != 'Team Totals')})
  output$year_salary = renderPlot({ggplot(year_salary_react(), aes(x = reorder(player_name, -y2020_21_new), y = y2020_21_new)) + 
      geom_bar(stat = "identity", fill = "lightblue") +
      xlab("Player") +
      ylab("Salary") +
      theme_classic() +
      ggtitle("Player Salary in 2020-2021 Season") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(text = element_text(size = 11)) +
      theme(plot.title = element_text(size = 20)) +
      geom_text(aes(label = y2020_21_new), vjust = -.5, size = 4) 
  })
  
  #####################################################################
  
  ##################### Clustering Analysis ###########################
  s <- c("player_name", "player_height", "player_weight", "gp", "pts", "reb", 
         "ast", "net_rating", "oreb_pct", "dreb_pct", "usg_pct", "ts_pct", "ast_pct")
  active_df <- player_data %>% select(s) %>% group_by(player_name) %>% 
    summarise(player_height=mean(player_height), player_weight=mean(player_weight), 
              gp=round(mean(gp),0), pts=mean(pts), rb=mean(reb), ast=mean(ast), net_rating=mean(net_rating),
              oreb_pct=mean(oreb_pct), dreb_pct=mean(dreb_pct), usg_pct=mean(usg_pct), 
              ts_pct=mean(ts_pct), ast_pct=mean(ast_pct))
  
  library(BBmisc)
  library(factoextra)
  active_df <- normalize(active_df, method= "standardize") 
  res.cov <- cov(active_df[,2:ncol(active_df)])
  # Rounding it to decimal places
  round(res.cov,2)
  # Find eigenvectors
  eig<-eigen(res.cov)$vectors
  # Select 1st 2 eigenvectors
  eigenv2<-eig[,(1:2)]
  # Convert to matrix for PCA calculations
  statsMatrix<-data.matrix(active_df[,2:ncol(active_df)], rownames.force = NA)
  # Matrix multiplication
  PrincipalComponents2<-statsMatrix%*%eigenv2
  # Convert back to statsDF for ggplot
  statsDF<-data.frame(PrincipalComponents2)
  
  km.res <- kmeans(active_df[,2:ncol(active_df)],10, nstart = 25)
    res.dist <- get_dist(active_df[,2:ncol(active_df)], stand = TRUE, method = "euclidean")
    fviz_dist(res.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"),lab_size= 2)
    fviz_nbclust(active_df[,2:ncol(active_df)], kmeans, method = "wss")
    #fviz_nbclust(active_df[,2:ncol(active_df)], kmeans, method = "gap_stat")
    #fviz_nbclust(active_df[,2:ncol(active_df)], kmeans, method = "silhouette")
  
  # Visualize the Clusters
  #top_50 <- head(active_players
  output$clusteranalysis <- renderPlot({
    fviz_cluster(km.res, active_df[,2:ncol(active_df)], ellipse = TRUE, ellipse.alpha= 0.1,
                 palette = "jco",repel = TRUE, ggtheme = theme_minimal(), 
                 main= FALSE, xlab= FALSE, ylab = FALSE,
                 labelsize= 10,lwd=2) + geom_text_repel(label=active_df$player_name) + theme_classic() + 
      theme(axis.line = element_blank(),axis.ticks = element_blank(), axis.text = element_blank(),
            axis.title = element_blank())
  })
  
  #####################################################################

  
  ###################### Prediction Modelling #########################
  final = read.csv('./Data/final.csv')
  idd = c( 'PTS_home', 'FG_PCT_home','FT_PCT_home', 'FG3_PCT_home', 'AST_home', 'REB_home','PTS_away','FG_PCT_away', 'FT_PCT_away', 'FG3_PCT_away', 'AST_away', 'REB_away','W_PCT','home_loose_o', 'W_pct_away', 'away_win_o')
  preProcValues <- preProcess(final[,idd], method = c("range"))
  
  # Step 2) the predict() function actually does the transformation using the 
  # parameters identified in the previous step. Weird that it uses predict() to do 
  # this, but it does!
  final <- predict(preProcValues, final)
  idd3 = c( 'Home_wins', 'FG_PCT_home', 'FG3_PCT_home', 'REB_home','FG_PCT_away', 'FG3_PCT_away', 'REB_away','home_loose_o', 'away_win_o','W_pct_away')
  
  data = final[,idd3]
  data$Home_wins = as.factor(data$Home_wins)
  
  levels(data$Home_wins) <- make.names(levels(factor(data$Home_wins)))
  inTrain <- createDataPartition(y = data$Home_wins,   # outcome variable
                                  p = .70,   # % of training data you want
                                  list = F)
  #Best_model
  # create your partitions
  train <- data[inTrain,]  # training data set
  test <- data[-inTrain,]

  ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                       number=3,        # k number of times to do k-fold
                       classProbs = T,  # if you want probabilities
                       summaryFunction = twoClassSummary, # for classification
                       allowParallel=T,
                       savePredictions = T)
  
  myModel1 <- train(Home_wins ~ .,               # model specification
                    data = train,        # train set used to build model
                    method = "glm",      # type of model you want to build
                    trControl = ctrl,    # how you want to learn
                    family = "binomial", # specify the type of glm
                    metric = "ROC"       # performance measure
  )
  myModel1
  
  
  myGrid <-  expand.grid(size = c(10)     # number of units in the hidden layer.
                         , decay = c(.09))
  # train a feed-forward neural net on train set 
  myModel2 <- train(Home_wins ~ .,               # model specification
                    data = train,        # train set used to build model
                    method = "nnet",     # type of model you want to build
                    trControl = ctrl,    # how you want to learn
                    tuneGrid = myGrid,   # how many tuning parameter combos to try
                    maxit = 10,         # max # of iterations
                    metric = "ROC"       # performance measure
  )
  myModel2
 
  
  pred_away = read.csv('./Data/away_pred.csv')
  pred_home = read.csv('./Data/home_pred.csv')
  
#  team1 <- reactive({input$team1})
  
#  output$team2_away <- renderUI ({
#    selectInput("team2", "Select Your Away Team", choices = team_id[team_id!=team1()] )
#    
#  })
  
  output$table1 <- renderInfoBox({
    pred_home_shiny_1 <- filter(pred_home,HOME_TEAM_ID == input$team1) 
    pred_away_shiny_1 <- filter(pred_away,VISITOR_TEAM_ID == input$team2)
    
    pred_data_shiny <- cbind(pred_away_shiny_1,pred_home_shiny_1)
    pred_data_shiny <- pred_data_shiny[idd]
    norm_pred_shiny <- predict(preProcValues, pred_data_shiny)
    final_pred_shiny <- predict(myModel1, newdata=norm_pred_shiny, type='prob')
    result <- round(as.numeric(final_pred_shiny[[2]]), 2)
    if (result >= 0.5) {
      final_prob = result
      team_id1 = pred_home_shiny_1
    }
    else{
      final_prob = 1- result
      team_id1 = pred_away_shiny_1
    }
    infoBox('#Probability',final_prob,
            icon = icon('trophy'), color = 'olive', fill = T)
  })
  
  output$table2 <- renderInfoBox({
    pred_away_shiny_1 <- filter(pred_away,VISITOR_TEAM_ID == input$team2)
    
    pred_home_shiny_1 <- filter(pred_home,HOME_TEAM_ID == input$team1) 
    
    
    pred_data_shiny <- cbind(pred_away_shiny_1,pred_home_shiny_1)
    pred_data_shiny <- pred_data_shiny[idd]
    norm_pred_shiny <- predict(preProcValues, pred_data_shiny)
    final_pred_shiny <- predict(myModel1, newdata=norm_pred_shiny, type='prob')
    result <- round(as.numeric(final_pred_shiny[[2]]), 2)
    if (result >= 0.5) {
      final_prob = result
      team_id1 = input$team1
    }
    else{
      final_prob = 1- result
      team_id1 = input$team2
    }
    team_name <- teams %>% filter(TEAM_ID==team_id1) %>% select(Team_Id)
    
    infoBox('#Winner',team_name,
            icon = icon('basketball-ball'), color = 'olive', fill = T)
  })
  #####################################################################
}

shinyApp(ui=ui, server=server)
