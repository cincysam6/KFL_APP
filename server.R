library(shiny)
library(ffscrapr)
library(gt)
library(gtExtras)
library(sqldf)
library(tidyverse)
library(plotly)
library(DBI)
library(htmlwidgets)
library(shinycssloaders)
library(shinythemes)
library(shinydashboard)


#### Connect to our POSTGRESQL database to get out KFL app data 
kfl_db <-"kfl_ff_data" 
host_db <- "localhost"
db_port <- "5432"
db_user <- "postgres"  
db_password <- "Cincinnati1"
kfl_con <- dbConnect(RPostgres::Postgres(), dbname = kfl_db, host=host_db, port=db_port, user=db_user, password=db_password)   
kfl_h2h<-dbReadTable(kfl_con,"kfl_head_to_head_results")
kfl_franchises<-dbReadTable(kfl_con,"kfl_franchises_2007_present")
#write.csv(kfl_h2h,"/Users/kirssa01.ENTERPRISE/Documents/Sports Analytics/KFL_APP/data/kfl_h2h.csv")
#write.csv(kfl_franchises,"/Users/kirssa01.ENTERPRISE/Documents/Sports Analytics/KFL_APP/data/kfl_h2h.csv")
#dbDisconnect(kfl_con)

### May need to use this in order to get local data
#kfl_h2h<-read.csv("/Users/kirssa01.ENTERPRISE/Documents/Sports Analytics/KFL_APP/data/kfl_h2h.csv")
#kfl_franchises<-read.csv("/Users/kirssa01.ENTERPRISE/Documents/Sports Analytics/KFL_APP/data/kfl_franchises.csv")

### CLEAN THE TIES. This is manual and should be done in the database, but oh well ####
kfl_h2h<-kfl_h2h%>%unite(game_id,c("Years","week","franchise_id","opponent_id"),remove=FALSE)
tie_games<-kfl_h2h%>%filter(result=="T")
tie_games$win<-c(1,0,1,0,0,1,1,0,1,0,1,0,0,1,0,1,0,1,1,0,0,1,1,0,1,0,0,1,0,1,0,1,1,0,1,0,1,0,0,1,1,0,0,1)
tie_game_update<-sqldf("Select A.*,B.win as tie_win
      From kfl_h2h A
      Left outer Join tie_games B
      on A.game_id = B.game_id")

tie_game_update<-tie_game_update%>%mutate(wins = ifelse(is.na(tie_win),win,tie_win))
kfl_h2h$win<-tie_game_update$wins

### GENERATE PRE AND POST EXPANSION DATA ###
post_expansion_h2h<-kfl_h2h%>%filter(week<15 & Years>2012)%>%select(week,Years,franchise_id,franchise_name,franchise_score,opponent_id,opponent_score,opp_franchise_name,win)
pre_expansion_h2h<-kfl_h2h%>%filter(week<16 & Years<2013)%>%select(week,Years,franchise_id,franchise_name,franchise_score,opponent_id,opponent_score,opp_franchise_name,win)
h2h_data<-as.data.frame(rbind(post_expansion_h2h,pre_expansion_h2h))


######## FF SCRAPER FOR CURRENT SEASON DATA ######
week_num <-16
swid<-"{FB82F2B3-89EB-4E1A-9677-B1333054B883}"
ESPN_S2<-"AECkrDCmS9dG4sssE55%2BNQRasW7jwjJVFyEUw6mR2wLFylIkvGy89sVJxc8GMvORMKJaBksVZfGnYB4BXKiM80pkw352zpsk%2BEbMg2ILadghIeMp2iz1Etpke3FMM2pyVdNg23edB2DoYIHMoUT72KS3Ho9bKNLPkkJ761JMh4o4ikTDTeL1uBJYNiahAMg1uvT4mHvszAP1UoDHQGLUPuOLchpl8Os8BsibtvBOTn778ksyx6gU513HRQGrpge7iQ9G04xOeWwYAjXfwcxa%2FCQr3Gx9kECPmidN9%2F7PrHImFQ%3D%3D"

espn_conn_2021 <- espn_connect(
  season = 2021,
  league_id = 214888,
  espn_s2 = ESPN_S2,
  swid = swid
)

### NEED TO UPDATE A COUPLE LOGOS
kfl_franchises_2021<-ff_franchises(espn_conn_2021)
kfl_franchises_2021$logo[10]<-"https://image.shutterstock.com/image-vector/lightning-warrior-logo-template-design-260nw-1019248687.jpg"
kfl_franchises_2021$logo[3]<-"https://www.pngkit.com/png/detail/269-2691865_winners-logo-winners.png"

### Get KFL SCHEDULE 
kfl_schedule_2021<-ff_schedule(espn_conn_2021)
kfl_week_2021<-kfl_schedule_2021%>%filter(week<=week_num)%>%mutate(win=(ifelse(result=="W",1,0)))
kfl_week_2021$Years<-2021
kfl_week_2021<-kfl_week_2021%>%unite(game_id,c("Years","week","franchise_id","opponent_id"),remove=FALSE)
kfl_week_2021

kfl_week_2021<-sqldf("select A.game_id,A.Years,A.Week,A.franchise_id,B.franchise_name as franchise_name,B.user_name as franchise_owner,A.franchise_score,A.result,A.opponent_score,A.opponent_id,C.franchise_name as opp_franchise_name,C.user_name as opp_franchise_owner,A.win
      from kfl_week_2021 A
      left outer join kfl_franchises_2021 B
      on A.franchise_id = B.franchise_id
      left outer join kfl_franchises_2021 C
      on A.opponent_id = C.franchise_id")


kfl_week_2021[37,7]<-1
kfl_week_2021[40,7]<-0
kfl_week_2021[39,7]<-1
kfl_week_2021[44,7]<-0


data<-as.data.frame(rbind(kfl_h2h,kfl_week_2021))


## introduce division data and join it to franchise data
franchise_id<-1:12
division_name<-c("Ohio","Glum","Ohio","Indiana","Glum","Ohio","Ohio","Indiana","Indiana","Glum","Glum","Indiana")
kfl_division<-as.data.frame(cbind(franchise_id,division_name))

kfl_franchise_data<-sqldf("Select A.*,B.division_name 
      from  kfl_franchises_2021 A
      left outer join kfl_division B
      on a.franchise_id = b.franchise_id")


kfl_franchise_data$trophy_img<-c("JeffKTrophy.PNG","JoeRTrophy.PNG","JoeSTrophy.PNG","KurtKTrophy.PNG","ScottGTrophy.PNG","SamKTrophy.PNG","WillKTrophy.PNG","NickKTrophy.PNG","AlexKTrophy.PNG","ElliotGTrophy.PNG","MichaelGTrophy.PNG","MaxKTrophy.PNG")

## update will's franchise_name and opponent name for 2007-2009
data<-data%>%mutate(franchise_owner = ifelse(franchise_id == 7 & franchise_owner!="Will Kirschner","Will Kirschner",franchise_owner))
data<-data%>%mutate(opp_franchise_owner = ifelse(opponent_id == 7 & opp_franchise_owner!="Will Kirschner","Will Kirschner",opp_franchise_owner))



server <- function(input, output, session) {


output$Summary <- renderText({paste("Welcome to the KFL historical record app! This site will let you take a look at head to head records for any two franchises on the first tab. The second tab lets a franchise look at their history and all time records againt all other KFL teams. Enjoy! ")})


### need to update the data so Will's 2007-2010 seasons come through
h2h_vs_all<-eventReactive(input$franchise2,{
data%>%filter(franchise_owner==input$franchise2)%>%
    select(opponent_id,win)%>%group_by(opponent_id)%>%summarise(
      Wins = length(win[win==1]),
      Losses = length(win[win==0]
      ),
      outcomes = list(win), .groups = "drop")%>%mutate(Win_Pct = Wins/(Wins+Losses)) %>% 
    left_join(kfl_franchise_data, by = c("opponent_id" = "franchise_id")) %>% 
    select(logo,user_name,division_name,Wins,Losses,Win_Pct, Wins:outcomes)
})


h2h_records<-eventReactive(input$Run_Query,{
  data%>%filter(franchise_owner==input$franchise & opp_franchise_owner==input$opponent)%>%select(Years,week,franchise_owner,franchise_name,franchise_score,result,opponent_score,opp_franchise_name,opp_franchise_owner)%>%arrange(desc(Years))
    })



output$table.output <- render_gt({
  h2h_records()%>%gt()%>%tab_style(
    style = cell_text(color = "red"),
    locations = list(
      cells_body(
        columns = 6,
        rows = result == "L"
      )))%>%tab_style(
        style = cell_text(color = "blue"),
        locations = list(
          cells_body(
            columns = 6,
            rows = result == "W"
          )))%>%cols_label(Years ="Year",
                                    week = "Week",
                                    franchise_name="Team Name",
                                    franchise_owner="Team Owner",
                                    franchise_score="Score",
                                    result="Result",
                                    opponent_score="Opponent Score",
                                    opp_franchise_name ="Opponent Team",
                                    opp_franchise_owner="Opponent Owner")
  
  
})


yr_summary<-eventReactive(input$franchise2,{
  yr_sum<-data%>%filter(franchise_owner == input$franchise2)%>%group_by(Years)%>%summarise(wins = sum(win),
                                                                               games_played=n(),
                                                                               win_pct = round(wins/games_played,3),
                                                                               points_for = sum(franchise_score),
                                                                               points_against = sum(opponent_score),
                                                                               point_diff = points_for - points_against,
                                                                               avg_pf = round(mean(franchise_score),2),
                                                                               avg_pa = round(mean(opponent_score),2))%>%arrange(desc(Years))
  
nickname<-data%>%filter(franchise_owner==input$franchise2)%>%select(Years,franchise_name)%>%unique()%>%arrange(desc(Years))
sqldf("Select A.franchise_name,B.* from nickname A inner join yr_sum B on A.Years=B.Years")  

})
#switched out query button to be just reactive franchise 
#input$Run_Query2

#team_name_and_logo<-eventReactive(input$Run_Query2,{
 #kfl_franchise_data%>%filter(user_name==input$franchise)%>%select(logo,franchise_name)
#})

tm<-reactive({input$franchise2})

#tm_nickname<-eventReactive(input$franchise2,{
#  data%>%filter(franchise_owner==input$franchise2)%>%select(Years,franchise_name)%>%unique()%>%arrange(desc(Years))
#})


#output$team.nickname<-render_gt({
#tm_nickname()%>%gt()%>%cols_label(franchise_name="team name")
#})

#franchise_tbl<-eventReactive(input$franchise2,{
#kfl_franchise_data%>%filter(user_name==input$franchise2)%>%select(logo,franchise_name,franchise_abbrev,division_name)
#})


output$team.table<-render_gt({
  kfl_franchise_data%>%filter(user_name==tm())%>%select(logo,franchise_name,franchise_abbrev,division_name,trophy_img)%>%gt()%>%
    tab_style(
      locations = cells_body(columns=franchise_name),
      style = cell_text(
        weight="bold",
        size = "xx-large",
        align ="left"
      ))%>%
      tab_style(
        locations =cells_body(columns=franchise_abbrev),
         style = cell_text(
        weight="bold",
        size = "x-large",
        align ="right"
      ))%>%gt_img_rows(logo,height=150)%>%gt_img_rows(trophy_img,height=250)%>%cols_label(logo="",franchise_name="team name",franchise_abbrev="abbrev",division_name="Division",trophy_img="")%>%tab_options(row.striping.include_table_body =FALSE,
                                                                                                                     table.border.top.color = "white",
                                                                                                                     heading.border.bottom.color = "white",
                                                                                                                     row_group.border.bottom.color = "white",
                                                                                                                     row_group.border.top.color = "white",table.align="left")%>%tab_options(table.width = pct(90))
  ### NO LONGER USED FOR THE IMAGE. REPLACED BY GT IMG ROWS 
    #text_transform(
    #locations = cells_body(columns=logo),
    #fn = function(x) {
    #  web_image(
    #    url= logo,
    #    height = 150
    #  )})
  
})



output$table.output2<-render_gt({
                       yr_summary()%>%gt()%>%
                      fmt_percent(columns=win_pct,decimals=2)%>%tab_style(
                        style = cell_text(color = "red"),
                        locations = list(
                          cells_body(
                            columns = 8,
                            rows = point_diff < 0
                          )))%>%
    cols_label(wins ="Wins",
               games_played = "Games Played",
               win_pct="Win Pct",
               points_for="Points For",
               points_against="Points Against",
               point_diff="Point Differential",
               avg_pf="Avg Points For",
               avg_pa ="Avg Points Against")%>%tab_header(title="Yearly Summary of Franchise Records")
                                                          
})




output$table.output3<-render_gt({
  h2h_vs_all()%>%gt(groupname_col="division_name")%>%gt_img_rows(logo)%>%gt_plt_winloss(outcomes,max_wins = 40)%>%tab_options(table.align="left",table.width = pct(100))%>%cols_width(outcomes~px(250))%>%cols_move(columns = Win_Pct,after=Losses )%>%fmt_percent(columns=Win_Pct,decimals=2)%>%tab_header(title="All Time Head to Head Results")
})






}