# This code creates a shiny app which allows for advanced filtering of
# data. This data comes from the masterbigTEST.R file which creates
# a dataset called 'master'.

# This is a test change

#### Dependencies ####
# uncomment when publishing
#source('masterbigTEST(1).R')
#setwd("C:/Users/andrb/OneDrive/Documents/SportsAnalytics")
library(shiny)
library(tidyverse)
library(purrr)
library(readxl)

#### SET UP SYSTEM TO DEPLOY APP ####
# rsconnect::setAccountInfo(name='wfuanalytics',
#                           token='54006636346BF0F925ABFEBC7CF6B2C5',
#                           secret='pOMojl0oRky5Ae1g3zOdGW3Si/EjD8v388BiDWMS')
# 

#### IMPORT DATA ####
mastertotal <- read_csv("mastertotal.csv", col_types = cols(X1 = col_skip()))
# add home/away and win/loss
mastertotal$WinLoss = rep(NA,nrow(mastertotal))
mastertotal$HomeAway = rep("Home",nrow(mastertotal))
mastertotal = mastertotal[,c(1:7,38,37,8:36)]
#master <- read_csv("Master1Test.csv", col_types = cols(X1 = col_skip()))
master = mastertotal
All_Results <- read_excel("All-Time FB Results.xlsx", sheet = "All Gms")
All_Results = All_Results %>% select(Year,`W/L` ,Opponent2,`Opp Conf`,WFU,Opp,Margin)
colnames(All_Results) = c('calYear',"W/L","Opponent",'OppConf','WFU_Score','OPP_Score','Score_Margin')

def_indiv = read_csv("def_ind_reformatted.csv")[-1]
Wake_TeamStats <- read_excel("TeamStats.xlsx", sheet = "Wake")
Opp_TeamStats <- read_excel("TeamStats.xlsx", sheet = "Opp")

#### SET UP CHOICES ####
off_pos_choices = unique(master$PlayerPosition)
#off_pos_choices = sort(off_pos_choices[which(!is.na(off_pos_choices))])
off_pos_choices = sort(off_pos_choices)
def_pos_choices = unique(sort(def_indiv$PlayerPosition))
off_academic_yr_choices = unique(sort(master$PlayerYear))
def_academic_yr_choices = unique(sort(def_indiv$PlayerYear))
#off_academic_yr_choices = sort(off_academic_yr_choices[which(!is.na(off_academic_yr_choices))])
game_area_choices = c('All Areas','Rushing','Passing','Recieving','Punting','Kicking','Interceptions','All Purpose','Punt Return','Kick Return')





#### SET UP FOR UI ####
offenseFilterRowInput<-function (inputID1,inputID2,inputID3) 
{
  fluidRow(
    column(3, selectInput(inputID1, "Choose the Variable to Filter:",
                          colnames(master[,c(3,10:length(master))]))),# only the numeric columns without pre-existing filters
    column(3, selectInput(inputID2, 'Relationship', c("Less Than", "Equal To", "Greater Than"))),
    column(3, numericInput(inputID3,"Value",0))
  )
}
defenseFilterRowInput<-function (inputID1,inputID2,inputID3) 
{
  fluidRow(
    column(3, selectInput(inputID1, "Choose the Variable to Filter:",
                          colnames(def_indiv[,5:25]))),# only the numeric columns without pre-existing filters
    column(3, selectInput(inputID2, 'Relationship', c("Less Than", "Equal To", "Greater Than"))),
    column(3, numericInput(inputID3,"Value",0))
  )
}

addFilter = function(tableName,number_add,add,eq,rhs){
  rows = 1:nrow(tableName)
  for (i in 1:number_add){
    if(eq[i] == 'Greater Than'){
      rows = intersect(rows,which(tableName[,add[i]] > rhs[i]))
    }
    else if(eq[i] == 'Less Than'){
      rows = intersect(rows,which(tableName[,add[i]] < rhs[i]))
    }
    else{
      rows = intersect(rows,which(tableName[,add[i]] == rhs[i]))
    }
    if(length(rows) == 0){
      return(NULL)
    }
  }
  return(rows)
}



off_pos_box = list(tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput(inputId  = 'pos_off', 
                                   label    = "Select Positions:", 
                                   choices  = off_pos_choices,
                                   inline   = FALSE)))
def_pos_box = list(tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput(inputId  = 'pos_def', 
                                   label    = "Select Positions:", 
                                   choices  = def_pos_choices,
                                   inline   = FALSE)))

maximum_year = as.numeric(format(Sys.Date(), "%Y"))
minimum_year = as.numeric(min(c(master$calYear,def_indiv$calYear)))


#### UI ####
ui = fluidPage(
  titlePanel("Wake Forest Football Statistics"),
  ## Choose Dataset to use
  selectInput("v1", "Choose Offense or Defense:",
              c("Offense (Individual Game)" = "off_ind",
                "Offense (Team)" = "off_team",
                "Defense (Individual Season)" = "def_ind",
                "Defense (Team)" = "def_team")),
  
  ## IF offensive individual, choose OFFENSIVE position ##
  conditionalPanel(
    condition = "input.v1 == 'off_ind'",
    fluidRow(column(width = 5, off_pos_box)),
    checkboxInput('all_off','All/None',value = FALSE)),
  
  ## IF defensive individual, choose DEFENSIVE position ##
  conditionalPanel(
    condition = "input.v1 == 'def_ind'",
    fluidRow(column(width = 5, def_pos_box)),
    checkboxInput('all_def','All/None',value = FALSE)),
  
  ## IF Offensive individual, choose academic year ##
  conditionalPanel(
    condition = "input.v1 == 'off_ind'",
    checkboxGroupInput("v6", "Academic Year:",
                       choices = off_academic_yr_choices),
    checkboxInput('off_yr','All/None',value = FALSE)),
  
  ## IF Defensive individual, choose academic year ##
  conditionalPanel(
    condition = "input.v1 == 'def_ind'",
    checkboxGroupInput("Def_Ac_Year", "Academic Year:",
                       choices = def_academic_yr_choices),
    checkboxInput('def_yr','All/None',value = FALSE)),
  
  ## Are we wanting consecutive or no?
  #selectInput("v2", "Consecutive or stand-alone:",
  #            c("Stand-Alone" = "alone",
  #              "Consecutive" = "cons")),
  
  ## Are we wanting sums of columns or no?
  selectInput("sums",'Sum Columns for Each Player?',
              c("No" = 'n',"Yes" = 'y')),
  
  ## Choose between rushing, passing, recieving, etc...
  selectInput("v7", "Area of Game:",
                       choices = game_area_choices),
  
  ## Choose between home and away
  conditionalPanel(
    condition = "input.v1 == 'off_ind'",
    radioButtons('home_away','Home vs. Away',
                 c('Both','Home','Away')) 
  ),
  
  #headerPanel('_________________'),
  
  ## Choose the number of additional filters to add ##
  numericInput("num_add","Number of Additional Individual Game Filters (Max 7):",0,min = 0,max = 7),
  
  conditionalPanel(
    condition = "input.v1 == 'off_ind' & input.num_add > 0",
    offenseFilterRowInput("add1","eq_1","rhs_1"),
    conditionalPanel(
      condition = "input.v1 == 'off_ind' & input.num_add > 1",
      offenseFilterRowInput("add2","eq_2","rhs_2")
    ),
      conditionalPanel(
        condition = "input.v1 == 'off_ind' & input.num_add > 2",
        offenseFilterRowInput("add3","eq_3","rhs_3")
        ),
        conditionalPanel(
          condition = "input.v1 == 'off_ind' & input.num_add > 3",
          offenseFilterRowInput("add4","eq_4","rhs_4")
        ),
          conditionalPanel(
            condition = "input.v1 == 'off_ind' & input.num_add > 4",
            offenseFilterRowInput("add5","eq_5","rhs_5")
          ),
            conditionalPanel(
              condition = "input.v1 == 'off_ind' & input.num_add > 5",
              offenseFilterRowInput("add6","eq_6","rhs_6")
            ),
              conditionalPanel(
                condition = "input.v1 == 'off_ind' & input.num_add > 6",
                offenseFilterRowInput("add7","eq_7","rhs_7")
              )
    
  ),
  
  conditionalPanel(
    condition = "input.v1 == 'def_ind' & input.num_add > 0",
    defenseFilterRowInput("def_add1","def_eq_1","def_rhs_1"),
    conditionalPanel(
      condition = "input.v1 == 'def_ind' & input.num_add > 1",
      defenseFilterRowInput("def_add2","def_eq_2","def_rhs_2")
    ),
    conditionalPanel(
      condition = "input.v1 == 'def_ind' & input.num_add > 2",
      defenseFilterRowInput("def_add3","def_eq_3","def_rhs_3")
    ),
    conditionalPanel(
      condition = "input.v1 == 'def_ind' & input.num_add > 3",
      defenseFilterRowInput("def_add4","def_eq_4","def_rhs_4")
    ),
    conditionalPanel(
      condition = "input.v1 == 'def_ind' & input.num_add > 4",
      defenseFilterRowInput("def_add5","def_eq_5","def_rhs_5")
    ),
    conditionalPanel(
      condition = "input.v1 == 'def_ind' & input.num_add > 5",
      defenseFilterRowInput("def_add6","def_eq_6","def_rhs_6")
    ),
    conditionalPanel(
      condition = "input.v1 == 'def_ind' & input.num_add > 6",
      defenseFilterRowInput("def_add7","def_eq_7","def_rhs_7")
    )
    
  ),
  
  #headerPanel('_________________'),
  
  ## Choose year range ##
  sliderInput("v5", "Years of Analysis",
              min = minimum_year, 
              max = maximum_year, 
              value = c(minimum_year,maximum_year),
              sep = ''),
  dataTableOutput('test')
)
    



#### SERVER ####
server = function(input, output, session) {
  observe({
    ## IF DEFENSIVE INDIVIDUAL ##
    if(input$v1 == 'def_ind'){
      # select all academic years
      updateCheckboxGroupInput(
        session, 'Def_Ac_Year', choices = def_academic_yr_choices,
        selected = if(input$def_yr) def_academic_yr_choices)
      # select all positions
      updateCheckboxGroupInput(
        session, 'pos_def', choices = def_pos_choices,
        selected = if(input$all_def) def_pos_choices,
        inline = T)
    }
    
    ## IF OFFENSIVE INDIVIDUAL ##
    #### FIX BUG ####
    if(input$v1 == 'off_ind'){
      # select all positions
      updateCheckboxGroupInput(
        session, 'pos_off', choices = off_pos_choices,
        selected = if(input$all_off) off_pos_choices,
        inline = T)
      # select all academic years
      updateCheckboxGroupInput(
        session, 'v6', choices = off_academic_yr_choices,
        selected = if(input$off_yr) off_academic_yr_choices)
    }
  })
  output$test = renderDataTable({
    cal_yr_list = input$v5[1]:input$v5[2]
    if(input$v1 == 'def_ind'){
      filterRows = 1:nrow(def_indiv)
      ## Make "Additional" Filters by Making Changes to Master ##
      if(input$num_add == 1){
        adds = c(input$def_add1)
        eqs = c(input$def_eq_1)
        rhss = c(input$def_rhs_1)
        filterRows = addFilter(def_indiv,1,adds,eqs,rhss)
      }
      else if(input$num_add == 2){
        adds = c(input$def_add1,input$def_add2)
        eqs = c(input$def_eq_1,input$def_eq_2)
        rhss = c(input$def_rhs_1,input$def_rhs_2)
        filterRows = addFilter(def_indiv,2,adds,eqs,rhss)
      }
      else if(input$num_add == 3){
        adds = c(input$def_add1,input$def_add2,input$def_add3)
        eqs = c(input$def_eq_1,input$def_eq_2,input$def_eq_3)
        rhss = c(input$def_rhs_1,input$def_rhs_2,input$def_rhs_3)
        filterRows = addFilter(def_indiv,3,adds,eqs,rhss)
      }
      else if(input$num_add == 4){
        adds = c(input$def_add1,input$def_add2,input$def_add3,input$def_add4)
        eqs = c(input$def_eq_1,input$def_eq_2,input$def_eq_3,input$def_eq_4)
        rhss = c(input$def_rhs_1,input$def_rhs_2,input$def_rhs_3,input$def_rhs_4)
        filterRows = addFilter(def_indiv,4,adds,eqs,rhss)
      }
      else if(input$num_add == 5){
        adds = c(input$def_add1,input$def_add2,input$def_add3,input$def_add4,input$def_add5)
        eqs = c(input$def_eq_1,input$def_eq_2,input$def_eq_3,input$def_eq_4,input$def_eq_5)
        rhss = c(input$def_rhs_1,input$def_rhs_2,input$def_rhs_3,input$def_rhs_4,input$def_rhs_5)
        filterRows = addFilter(def_indiv,5,adds,eqs,rhss)
      }
      else if(input$num_add == 6){
        adds = c(input$def_add1,input$def_add2,input$def_add3,input$def_add4,input$def_add5,input$def_add6)
        eqs = c(input$def_eq_1,input$def_eq_2,input$def_eq_3,input$def_eq_4,input$def_eq_5,input$def_eq_6)
        rhss = c(input$def_rhs_1,input$def_rhs_2,input$def_rhs_3,input$def_rhs_4,input$def_rhs_5,input$def_rhs_6)
        filterRows = addFilter(def_indiv,6,adds,eqs,rhss)
      }
      else if(input$num_add == 7){
        adds = c(input$def_add1,input$def_add2,input$def_add3,input$def_add4,input$def_add5,input$def_add6,input$def_add7)
        eqs = c(input$def_eq_1,input$def_eq_2,input$def_eq_3,input$def_eq_4,input$def_eq_5,input$def_eq_6,input$def_eq_7)
        rhss = c(input$def_rhs_1,input$def_rhs_2,input$def_rhs_3,input$def_rhs_4,input$def_rhs_5,input$def_rhs_6,input$def_rhs_7)
        filterRows = addFilter(def_indiv,7,adds,eqs,rhss)
      }
      def_pos_list = vector()
      for(name in def_pos_choices){
        if(name %in% input$pos_def){
          def_pos_list = c(def_pos_list,name)
        }
      }
      
      def_year_list = vector()
      for(year in def_academic_yr_choices){
        if(year %in% input$Def_Ac_Year){
          def_year_list = c(def_year_list,year)
        }
      }
      
      ## Use this if statement to decide which rows and columns get printed in the table
      ## Use this if statement to decide which rows and columns get printed in the table
      if(length(def_pos_list) > 0 & length(def_year_list) > 0){
        rows = which(def_indiv$PlayerPosition %in% def_pos_list & 
                       def_indiv$PlayerYear %in% def_year_list &
                       def_indiv$calYear %in% cal_yr_list 
                       )  
        rows = intersect(rows,filterRows)
        # If we want sums
        if (input$sums == 'y'){
          a1 = def_indiv[rows,] %>%
            arrange(Name,PlayerPosition,calYear) %>%
            mutate(calYear = as.character(calYear)) %>%
            group_by(Name,PlayerPosition) %>%
            mutate(yearNumbers = paste0(calYear,collapse = ', '))
          
          a1 %>%
            group_by(Name,PlayerPosition,yearNumbers) %>%
            summarise_if(is.numeric, sum, na.rm = TRUE)
          
        } else {
          def_indiv[rows,]
        }
      }
    }
    else if(input$v1 == 'off_team'){
      
    }
    else if(input$v1 == 'def_team'){
      
    }
    ## IF OFFENSIVE INDIVIDUAL ##
    else if(input$v1 == 'off_ind'){
      filterRows = 1:nrow(master)
      ## Make "Additional" Filters by Making Changes to Master ##
      if(input$num_add == 1){
        adds = c(input$add1)
        eqs = c(input$eq_1)
        rhss = c(input$rhs_1)
        filterRows = addFilter(master,1,adds,eqs,rhss)
      }
      else if(input$num_add == 2){
        adds = c(input$add1,input$add2)
        eqs = c(input$eq_1,input$eq_2)
        rhss = c(input$rhs_1,input$rhs_2)
        filterRows = addFilter(master,2,adds,eqs,rhss)
      }
      else if(input$num_add == 3){
        adds = c(input$add1,input$add2,input$add3)
        eqs = c(input$eq_1,input$eq_2,input$eq_3)
        rhss = c(input$rhs_1,input$rhs_2,input$rhs_3)
        filterRows = addFilter(master,3,adds,eqs,rhss)
      }
      else if(input$num_add == 4){
        adds = c(input$add1,input$add2,input$add3,input$add4)
        eqs = c(input$eq_1,input$eq_2,input$eq_3,input$eq_4)
        rhss = c(input$rhs_1,input$rhs_2,input$rhs_3,input$rhs_4)
        filterRows = addFilter(master,4,adds,eqs,rhss)
      }
      else if(input$num_add == 5){
        adds = c(input$add1,input$add2,input$add3,input$add4,input$add5)
        eqs = c(input$eq_1,input$eq_2,input$eq_3,input$eq_4,input$eq_5)
        rhss = c(input$rhs_1,input$rhs_2,input$rhs_3,input$rhs_4,input$rhs_5)
        filterRows = addFilter(master,5,adds,eqs,rhss)
      }
      else if(input$num_add == 6){
        adds = c(input$add1,input$add2,input$add3,input$add4,input$add5,input$add6)
        eqs = c(input$eq_1,input$eq_2,input$eq_3,input$eq_4,input$eq_5,input$eq_6)
        rhss = c(input$rhs_1,input$rhs_2,input$rhs_3,input$rhs_4,input$rhs_5,input$rhs_6)
        filterRows = addFilter(master,6,adds,eqs,rhss)
      }
      else if(input$num_add == 7){
        adds = c(input$add1,input$add2,input$add3,input$add4,input$add5,input$add6,input$add7)
        eqs = c(input$eq_1,input$eq_2,input$eq_3,input$eq_4,input$eq_5,input$eq_6,input$eq_7)
        rhss = c(input$rhs_1,input$rhs_2,input$rhs_3,input$rhs_4,input$rhs_5,input$rhs_6,input$rhs_7)
        filterRows = addFilter(master,7,adds,eqs,rhss)
      }
      
      # Filter on home and away
      home_away_list = NA
      if(input$home_away == 'Home'){
        home_away_list = "Home"
      }
      else if(input$home_away == 'Away'){
        home_away_list = "Away"
      }
      else{
        home_away_list = c("Home","Away")
      }
      
      # Filter on area of game
      game_area_filter_rows = 1:9
      game_area_list = vector()
      if(input$v7 == 'Rushing'){
        game_area_filter_rows = c(game_area_filter_rows,10:12)
        game_area_list = 10:12
      } else if(input$v7 == 'Passing'){
        game_area_filter_rows = c(game_area_filter_rows,16:21)
        game_area_list = 16:21
      } else if(input$v7 == 'Recieving'){
        game_area_filter_rows = c(game_area_filter_rows,13:15)
        game_area_list = 13:15
      } else if(input$v7 == 'Punting'){
        game_area_filter_rows = c(game_area_filter_rows,22:26)
        game_area_list = 22:26
      } else if(input$v7 == 'All Purpose'){
        game_area_filter_rows = c(game_area_filter_rows,10:21,33:38)
        game_area_list = c(10:21,33:38)
      } else if(input$v7 == 'Kicking'){
        game_area_filter_rows = c(game_area_filter_rows,27:29)
        game_area_list = 27:29
      } else if(input$v7 == 'Interceptions'){
        game_area_filter_rows = c(game_area_filter_rows,30:32)
        game_area_list = 30:32
      } else if(input$v7 == 'Punt Return'){
        game_area_filter_rows = c(game_area_filter_rows,33:35)
        game_area_list = 33:35
      } else if(input$v7 == 'Kick Return'){
        game_area_filter_rows = c(game_area_filter_rows,36:38)
        game_area_list = 36:38
      } else if(input$v7 == 'All Areas'){
        game_area_filter_rows = c(game_area_filter_rows,10:38)
        game_area_list = 10:38
      }
      
      pos_list = vector()
      for(name in off_pos_choices){
        if(name %in% input$pos_off){
          pos_list = c(pos_list,name)
        }
      }
      
      year_list = vector()
      for(year in off_academic_yr_choices){
        if(year %in% input$v6){
          year_list = c(year_list,year)
        }
      }
      
      
      ## Use this if statement to decide which rows and columns get printed in the table
      if(length(pos_list) > 0 & length(year_list) > 0){
        rows = which(master$PlayerPosition %in% pos_list & 
                       master$PlayerYear %in% year_list &
                       master$calYear %in% cal_yr_list &
                       master$HomeAway %in% home_away_list &
                       rowSums(is.na(master[,game_area_list])) != length(master[,game_area_list])# exclude if entirely empty
                     )
        rows = intersect(rows,filterRows)
        # If we want sums
        if (input$sums == 'y'){
          a1 = master[rows,game_area_filter_rows] %>%
            arrange(Name,calYear,WeekNum) %>%
            mutate(WeekNum = as.character(WeekNum)) %>%
            group_by(Name,calYear) %>%
            mutate(gameNumbers = paste0(WeekNum,collapse = ', '))
          
          a1 %>%
            group_by(Name,calYear,gameNumbers) %>%
            summarise_if(is.numeric, sum, na.rm = TRUE)
          
        } else {
          master[rows,game_area_filter_rows]
        }
      }
    }
  },options = list(pageLength = 10))
}





#### RUN SHINYAPP ####
shinyApp(ui = ui, server = server)

