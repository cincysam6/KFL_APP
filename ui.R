library(shiny)
library(sqldf)
library(tidyverse)
library(plotly)
library(DBI)
library(htmlwidgets)
library(shinycssloaders)
library(shinythemes)
library(shinydashboard)
library(gtExtras)
library(gt)

shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  titlePanel(title=div(img(src="KFL_analytics.png",height=100,width=80),"Kirschner Fantasy Football League")),
  navbarPage("KFL History",  
             tabPanel("Head to Head Records",
                      sidebarLayout(
                        sidebarPanel(selectInput("franchise","Select A Team",choices=kfl_franchise_data$user_name),
                                     selectInput("opponent","Select an Opponent",choices=kfl_franchise_data$user_name),
                                     actionButton("Run_Query","Get Results"),
                                     br(),
                                     br()
                                     ),
                        mainPanel(textOutput("Summary"),
                                  gt_output("table.output")
                        ))),
             tabPanel("Franchise History",
                      sidebarPanel(selectInput("franchise2","Select A Team",choices=kfl_franchise_data$user_name),
                                   br(),
                                   p(strong("Please give the app a few seconds to load after selecting a franchise"))),
                      mainPanel(
                        gt_output("team.table"),
                        br(),
                        br(),
                        gt_output("table.output2"),
                        br(),
                        br(),
                        gt_output("table.output3")
                      )
               
             ),
          tabPanel("About",
                    mainPanel(
                      p("This App was created as an interactive tool to explore the history of the Kirschner Fantasy Football League (KFL).
                        More great KFL analytics can be found at",a("KFL Analytics Blog.", 
          href = "https://kfl-analytics.netlify.app")),
          br(),
          p(" A big thanks to Tan Ho for the ffscrapr package that has made this page possible.")
                    ))
  )))
