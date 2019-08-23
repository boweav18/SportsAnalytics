# This code creates a shiny app which allows for advanced filtering of
# data. This data comes from the masterbigTEST.R file which creates
# a dataset called 'master'.

# This is a test change

#### Dependencies ####
# uncomment when publishing
#setwd("C:/Users/andrb/OneDrive/Documents/SportsAnalytics")
library(shiny)
library(tidyverse)
library(purrr)
library(readxl)
library(googlesheets)

#### SET UP SYSTEM TO DEPLOY APP ####
 # rsconnect::setAccountInfo(name='wfuanalytics',
 #                           token='54006636346BF0F925ABFEBC7CF6B2C5',
 #                           secret='pOMojl0oRky5Ae1g3zOdGW3Si/EjD8v388BiDWMS')
# 

#### IMPORT DATA ####
master = gs_read(ss=gs_title('Offense_Individual'))[,-1] # read the sheet into a data frame (exclude first column of indeces)
def_indiv = gs_read(ss=gs_title('Defensive_Individual'))[,-1] # read the sheet into a data frame (exclude first column of indeces)
team = gs_read(ss=gs_title('Team'))[,-1] # read the sheet into a data frame (exclude first column of indeces)

#### Set up data values ####
maximum_year = as.numeric(format(Sys.Date(), "%Y"))

# If there are null values in calYear, impute as this year and give message
message = ""
idx_null_yr_off = which(is.na(master$calYear))
if(length(idx_null_yr_off) > 0){
  master$calYear[idx_null_yr_off] = maximum_year
  indexes = paste(idx_null_yr_off,collapse = ' ')
  message = paste(message,paste('There are missing values in the offensive data column "calYear" at index(s) ',indexes,' in the google sheet. ',
                  'The values in this column were imputed as ',maximum_year,'.',sep = ''))
}
idx_null_yr_def = which(is.na(def_indiv$calYear))
if(length(idx_null_yr_def) > 0){
  def_indiv$calYear[idx_null_yr_def] = maximum_year
  indexes = paste(idx_null_yr_def,collapse = ', ')
  message = paste(message,paste('There are missing values in the defensive individual data column "calYear" at index(s) ',indexes,' in the google sheet. ',
                  'The values in this column were imputed as ',maximum_year,'.',sep = ''))
}

minimum_year = as.numeric(min(c(master$calYear,def_indiv$calYear,team$Year)))



#### SET UP CHOICES ####
off_pos_choices = unique(master$PlayerPosition)
off_pos_choices = c(sort(off_pos_choices),'Unknown')
def_pos_choices = unique(sort(def_indiv$PlayerPosition))
off_academic_yr_choices = c(unique(sort(master$PlayerYear)),'Unknown')
def_academic_yr_choices = unique(sort(def_indiv$PlayerYear))
 game_area_choices = c('All Areas','Rushing','Passing','Recieving','Punting','Kicking','Interceptions','All Purpose','Punt Return','Kick Return')
team_choices = c('Rush Offense','Receiving Offense','Pass Offense','Kickoff Return',
                 'Punt Return','Total Offense','Defense','Place Kicking','Punting',
                 'Kickoffs','Other Measures')
info_choices = c('Scoring','Matchup Info','Coach Info','Game Details','Other')




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
teamFilterRowInput<-function (inputID1,inputID2,inputID3) 
{
  fluidRow(
    column(3, selectInput(inputID1, "Choose the Variable to Filter:",
                          colnames(team[,7:ncol(team)]))),# only the numeric columns without pre-existing filters
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

#### UI ####
ui = fluidPage(
  titlePanel("Wake Forest Football Statistics"),
  ## Choose Dataset to use
  selectInput("v1", "Choose Offense or Defense:",
              c("Offense (Individual Game)" = "off_ind",
                "Defense (Individual Season)" = "def_ind",
                "Team" = "team")),
  
  ## Choose between rushing, passing, recieving, etc...
  conditionalPanel(
    condition = "input.v1 == 'off_ind'",
    selectInput("v7", "Area of Game to View:",
                choices = game_area_choices)
  ),
  
  ## Choose area of game to view in table from team choices
  conditionalPanel(
    condition = "input.v1 == 'team'",
    htmlOutput('text')
  ),
  conditionalPanel(
    condition = "input.v1 == 'team'",
    fluidRow(
      column(3, checkboxGroupInput("wake_team_choices", "Wake:",
                                   choices = team_choices)),
      column(3, checkboxGroupInput("opp_team_choices", "Opponent:",
                                   choices = team_choices)),
      column(3, checkboxGroupInput("game_info",'Game Information:',
                                   choices = info_choices))
    )
  ),
  
  ## Choose between home and away
  conditionalPanel(
    condition = "input.v1 == 'off_ind'",
    radioButtons('home_away','Home vs. Away',
                 c('Both','Home','Away')) 
  ),
  
  ## Choose the number of additional filters to add ##
  numericInput("num_add","Number of Additional Individual Game Filters (Max 7):",0,min = 0,max = 7), 
  
  
  ## OFFENSIVE ADDITIONAL FILTERS ##
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
  
  ## DEFENSIVE ADDITIONAL FILTERS ##
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
  
  ## TEAM ADDITIONAL FILTERS ##
  conditionalPanel(
    condition = "input.v1 == 'team' & input.num_add > 0",
    teamFilterRowInput("team_add1","team_eq_1","team_rhs_1"),
    conditionalPanel(
      condition = "input.v1 == 'team' & input.num_add > 1",
      teamFilterRowInput("team_add2","team_eq_2","team_rhs_2")
    ),
    conditionalPanel(
      condition = "input.v1 == 'team' & input.num_add > 2",
      teamFilterRowInput("team_add3","team_eq_3","team_rhs_3")
    ),
    conditionalPanel(
      condition = "input.v1 == 'team' & input.num_add > 3",
      teamFilterRowInput("team_add4","team_eq_4","team_rhs_4")
    ),
    conditionalPanel(
      condition = "input.v1 == 'team' & input.num_add > 4",
      teamFilterRowInput("team_add5","team_eq_5","team_rhs_5")
    ),
    conditionalPanel(
      condition = "input.v1 == 'team' & input.num_add > 5",
      teamFilterRowInput("team_add6","team_eq_6","team_rhs_6")
    ),
    conditionalPanel(
      condition = "input.v1 == 'team' & input.num_add > 6",
      teamFilterRowInput("team_add7","team_eq_7","team_rhs_7")
    )
    
  ),
  
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
  
  
  ## Are we wanting sums of columns or no?
  conditionalPanel(
    condition = "input.v1 != 'team'",
    selectInput("sums",'Aggregate Over Games Within a Year?',
                c("No" = 'n',"Yes" = 'y'))
  ),
  
  ## Choose year range ##
  sliderInput("v5", "Years of Analysis",
              min = minimum_year, 
              max = maximum_year, 
              value = c(minimum_year,maximum_year),
              sep = ''),
  
  
  ## Consecutive years or games?
  conditionalPanel(
    condition = "input.v1 == 'team'", selectInput("cons1", "Choose No Aggregation or Game Aggregation:",
              c("None",
                "Game"))),
  
  ##Number of consecutive ____
  conditionalPanel(
    condition = "input.v1 == 'team'", selectInput("cons2", "Number of Games to Aggregate:", 
              c("NULL", "Max", "1", "2", "3", "4", "5", "6", "7", "8", "9"))),
  
  tagList(
    singleton(tags$head(tags$script(src='//cdn.datatables.net/fixedheader/2.1.2/js/dataTables.fixedHeader.min.js',type='text/javascript'))),
    singleton(tags$head(tags$link(href='//cdn.datatables.net/fixedheader/2.1.2/css/dataTables.fixedHeader.css',rel='stylesheet',type='text/css')))
  ), 
  
  dataTableOutput('table'),
  downloadButton('downloadData','Download')
)




#### SERVER ####
server = function(input, output, session) {
  if(nchar(message) > 0){
    observe({
    showModal(modalDialog(
      title = "Important Message",
      message
    ))
  })}
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
  output$text = renderText({
    '<b>Area of Game to View:</b>'  
  })
  
  datasetInput = reactive({
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
    
    
    
    ## IF TEAM ##
    else if(input$v1 == 'team'){
      cols = c(1:5,147,154,252:ncol(team))
      rows = 1:nrow(team)
      if('Rush Offense' %in% input$wake_team_choices){
        cols = c(cols,6:9)
      }
      if('Receiving Offense' %in% input$wake_team_choices){
        cols = c(cols,10:13)
      }
      if('Pass Offense' %in% input$wake_team_choices){
        cols = c(cols,14:19)
      }
      if('Kickoff Return' %in% input$wake_team_choices){
        cols = c(cols,20:23)
      }
      if('Punt Return' %in% input$wake_team_choices){
        cols = c(cols,24:27)
      }
      if('Total Offense' %in% input$wake_team_choices){
        cols = c(cols,28:33)
      }
      if('Defense' %in% input$wake_team_choices){
        cols = c(cols,34:47)
      }
      if('Place Kicking' %in% input$wake_team_choices){
        cols = c(cols,48:50,64:67)
      }
      if('Punting' %in% input$wake_team_choices){
        cols = c(cols,55:63)
      }
      if('Kickoffs' %in% input$wake_team_choices){
        cols = c(cols,68:72)
      }
      if('Other Measures' %in% input$wake_team_choices){
        cols = c(cols,73:75.51:54)
      }
      #-------
      if('Rush Offense' %in% input$opp_team_choices){
        cols = c(cols,76:79)
      }
      if('Receiving Offense' %in% input$opp_team_choices){
        cols = c(cols,80:83)
      }
      if('Pass Offense' %in% input$opp_team_choices){
        cols = c(cols,84:89)
      }
      if('Kickoff Return' %in% input$opp_team_choices){
        cols = c(cols,90:93)
      }
      if('Punt Return' %in% input$opp_team_choices){
        cols = c(cols,94:97)
      }
      if('Total Offense' %in% input$opp_team_choices){
        cols = c(cols,98:102)
      }
      if('Defense' %in% input$opp_team_choices){
        cols = c(cols,103:116)
      }
      if('Place Kicking' %in% input$opp_team_choices){
        cols = c(cols,117:119,133:136)
      }
      if('Punting' %in% input$opp_team_choices){
        cols = c(cols,124:132)
      }
      if('Kickoffs' %in% input$opp_team_choices){
        cols = c(cols,137:141)
      }
      if('Other Measures' %in% input$opp_team_choices){
        cols = c(cols,142:145,120:123)
      }
      #-------
      if('Scoring' %in% input$game_info){
        cols = c(cols,146:150)
      }
      if('Matchup Info' %in% input$game_info){
        cols = c(cols,153,155:164)
      }
      if('Coach Info' %in% input$game_info){
        cols = c(cols,167:169)
      }
      if('Game Details' %in% input$game_info){
        cols = c(cols,174:195,198:204,237)
      }
      if('Other' %in% input$game_info){
        cols = c(cols,151:152,165:166,169:173,196:197,205:236,238:ncol(team))
      }
      
      filterRows = 1:nrow(team)
      ## Make "Additional" Filters by Making Changes to Master ##
      if(input$num_add == 1){
        adds = c(input$team_add1)
        eqs = c(input$team_eq_1)
        rhss = c(input$team_rhs_1)
        filterRows = addFilter(team,1,adds,eqs,rhss)
      }
      else if(input$num_add == 2){
        adds = c(input$team_add1,input$team_add2)
        eqs = c(input$team_eq_1,input$team_eq_2)
        rhss = c(input$team_rhs_1,input$team_rhs_2)
        filterRows = addFilter(team,2,adds,eqs,rhss)
      }
      else if(input$num_add == 3){
        adds = c(input$team_add1,input$team_add2,input$team_add3)
        eqs = c(input$team_eq_1,input$team_eq_2,input$team_eq_3)
        rhss = c(input$team_rhs_1,input$team_rhs_2,input$team_rhs_3)
        filterRows = addFilter(team,3,adds,eqs,rhss)
      }
      else if(input$num_add == 4){
        adds = c(input$team_add1,input$team_add2,input$team_add3,input$team_add4)
        eqs = c(input$team_eq_1,input$team_eq_2,input$team_eq_3,input$team_eq_4)
        rhss = c(input$team_rhs_1,input$team_rhs_2,input$team_rhs_3,input$team_rhs_4)
        filterRows = addFilter(team,4,adds,eqs,rhss)
      }
      else if(input$num_add == 5){
        adds = c(input$team_add1,input$team_add2,input$team_add3,input$team_add4,input$team_add5)
        eqs = c(input$team_eq_1,input$team_eq_2,input$team_eq_3,input$team_eq_4,input$team_eq_5)
        rhss = c(input$team_rhs_1,input$team_rhs_2,input$team_rhs_3,input$team_rhs_4,input$team_rhs_5)
        filterRows = addFilter(team,5,adds,eqs,rhss)
      }
      else if(input$num_add == 6){
        adds = c(input$team_add1,input$team_add2,input$team_add3,input$team_add4,input$team_add5,input$team_add6)
        eqs = c(input$team_eq_1,input$team_eq_2,input$team_eq_3,input$team_eq_4,input$team_eq_5,input$team_eq_6)
        rhss = c(input$team_rhs_1,input$team_rhs_2,input$team_rhs_3,input$team_rhs_4,input$team_rhs_5,input$team_rhs_6)
        filterRows = addFilter(team,6,adds,eqs,rhss)
      }
      else if(input$num_add == 7){
        adds = c(input$team_add1,input$team_add2,input$team_add3,input$team_add4,input$team_add5,input$team_add6,input$team_add7)
        eqs = c(input$team_eq_1,input$team_eq_2,input$team_eq_3,input$team_eq_4,input$team_eq_5,input$team_eq_6,input$team_eq_7)
        rhss = c(input$team_rhs_1,input$team_rhs_2,input$team_rhs_3,input$team_rhs_4,input$team_rhs_5,input$team_rhs_6,input$team_rhs_7)
        filterRows = addFilter(team,7,adds,eqs,rhss)
      }

      # identify which rows to keep based on CALENDAR YEAR
      rows = intersect(rows,which(team$Year %in% cal_yr_list))
      rows = intersect(rows,filterRows)
      
      #Consecutive Years
      if(input$cons1 == 'Year'){
        split <- split(team$Year[rows], cumsum(c(1, diff(team$Year[rows]) != 1)))
        
        if(input$cons2 == 'Max'){
        maxlisty <- vector("double")
        for (i in 1:length(split)){
          conslengthsy <- vector("double")
          for (p in 1:length(split)){
            conslengthsy <- append(conslengthsy, length(split[[p]]))
            
          }
          if (length(split[[i]]) == max(conslengthsy)){
            maxlisty <- append(maxlisty, split[[i]])
          }
        }
        rows = intersect(rows,which(team$Year %in% maxlisty))
        }
        
        else if(input$cons2 == 1){
          specific1y <- vector("double")
          for (i in 1:length(split)){
            if (length(split[[i]]) == 1){
              specific1y <- append(specific1y, split[[i]])
            }
          }
          rows = intersect(rows,which(team$Year %in% specific1y))
        }
        
        else if(input$cons2 == 2){
          specific2y <- vector("double")
          for (i in 1:length(split)){
            if (length(split[[i]]) == 2){
              specific2y <- append(specific2y, split[[i]])
            }
          }
          rows = intersect(rows,which(team$Year %in% specific2y))
          
        }
        
        else if(input$cons2 == 3){
          specific3y <- vector("double")
          for (i in 1:length(split)){
            if (length(split[[i]]) == 3){
              specific3y <- append(specific3y, split[[i]])
            }
          }
          rows = intersect(rows,which(team$Year %in% specific3y))
          
        }
        
        else if(input$cons2 == 4){
          specific4y <- vector("double")
          for (i in 1:length(split)){
            if (length(split[[i]]) == 4){
              specific4y <- append(specific4y, split[[i]])
            }
          }
          rows = intersect(rows,which(team$Year %in% specific4y))
          
        }
        
        else if(input$cons2 == 5){
          specific5y <- vector("double")
          for (i in 1:length(split)){
            if (length(split[[i]]) == 5){
              specific5y <- append(specific5y, split[[i]])
            }
          }
          rows = intersect(rows,which(team$Year %in% specific5y))
          
        }
        
        else if(input$cons2 == 6){
          specific6y <- vector("double")
          for (i in 1:length(split)){
            if (length(split[[i]]) == 6){
              specific6y <- append(specific6y, split[[i]])
            }
          }
          rows = intersect(rows,which(team$Year %in% specific6y))
          
        }
        
        else if(input$cons2 == 7){
          specific7y <- vector("double")
          for (i in 1:length(split)){
            if (length(split[[i]]) == 7){
              specific7y <- append(specific7y, split[[i]])
            }
          }
          rows = intersect(rows,which(team$Year %in% specific7y))
          
        }
        
        else if(input$cons2 == 8){
          specific8y <- vector("double")
          for (i in 1:length(split)){
            if (length(split[[i]]) == 8){
              specific8y <- append(specific8y, split[[i]])
            }
          }
          rows = intersect(rows,which(team$Year %in% specific8y))
          
        }
        
        else if(input$cons2 == 9){
          specific9y <- vector("double")
          for (i in 1:length(split)){
            if (length(split[[i]]) == 9){
              specific9y <- append(specific9y, split[[i]])
            }
          }
          rows = intersect(rows,which(team$Year %in% specific9y))
          
        }
        
        else if(input$cons2 == 'NULL'){
          rows = rows
        }
        
      }
      
      #Consecutive Games
      if(input$cons1 == 'Game'){
        split <- split(team$GameNumber[rows], cumsum(c(1, diff(team$GameNumber[rows]) != 1)))
        
        if(input$cons2 == 'Max'){
        maxlistg <- vector("double")
        for (i in 1:length(split)){
          conslengthsg <- vector("double")
          for (p in 1:length(split)){
            conslengthsg <- append(conslengthsg, length(split[[p]]))
            
          }
          if (length(split[[i]]) == max(conslengthsg)){
            maxlistg <- append(maxlistg, split[[i]])
          }
        }
        rows = intersect(rows,which(team$GameNumber %in% maxlistg))
        }
        
        else if(input$cons2 == 1){
          specific1g <- vector("double")
          for (i in 1:length(split)){
            if (length(split[[i]]) == 1){
              specific1g <- append(specific1g, split[[i]])
            }
          }
          rows = intersect(rows,which(team$GameNumber %in% specific1g))
        }
        
        else if(input$cons2 == 2){
          specific2g <- vector("double")
          for (i in 1:length(split)){
            if (length(split[[i]]) == 2){
              specific2g <- append(specific2g, split[[i]])
            }
          }
          rows = intersect(rows,which(team$GameNumber %in% specific2g))
          
        }
        
        else if(input$cons2 == 3){
          specific3g <- vector("double")
          for (i in 1:length(split)){
            if (length(split[[i]]) == 3){
              specific3g <- append(specific3g, split[[i]])
            }
          }
          rows = intersect(rows,which(team$GameNumber %in% specific3g))
          
        }
        
        else if(input$cons2 == 4){
          specific4g <- vector("double")
          for (i in 1:length(split)){
            if (length(split[[i]]) == 4){
              specific4g <- append(specific4g, split[[i]])
            }
          }
          rows = intersect(rows,which(team$GameNumber %in% specific4g))
          
        }
        
        else if(input$cons2 == 5){
          specific5g <- vector("double")
          for (i in 1:length(split)){
            if (length(split[[i]]) == 5){
              specific5g <- append(specific5g, split[[i]])
            }
          }
          rows = intersect(rows,which(team$GameNumber %in% specific5g))
          
        }
        
        else if(input$cons2 == 6){
          specific6g <- vector("double")
          for (i in 1:length(split)){
            if (length(split[[i]]) == 6){
              specific6g <- append(specific6g, split[[i]])
            }
          }
          rows = intersect(rows,which(team$GameNumber %in% specific6g))
          
        }
        
        else if(input$cons2 == 7){
          specific7g <- vector("double")
          for (i in 1:length(split)){
            if (length(split[[i]]) == 7){
              specific7g <- append(specific7g, split[[i]])
            }
          }
          rows = intersect(rows,which(team$GameNumber %in% specific7g))
          
        }
        
        else if(input$cons2 == 8){
          specific8g <- vector("double")
          for (i in 1:length(split)){
            if (length(split[[i]]) == 8){
              specific8g <- append(specific8g, split[[i]])
            }
          }
          rows = intersect(rows,which(team$GameNumber %in% specific8g))
          
        }
        
        else if(input$cons2 == 9){
          specific9g <- vector("double")
          for (i in 1:length(split)){
            if (length(split[[i]]) == 9){
              specific9g <- append(specific9g, split[[i]])
            }
          }
          rows = intersect(rows,which(team$GameNumber %in% specific9g))
          
        }
        
        else if(input$cons2 == 'NULL'){
          rows = rows
        }
        
      }  
    
      team[rows,cols]
      
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
        home_away_list = "H"
      }
      else if(input$home_away == 'Away'){
        home_away_list = "A"
      }
      else{
        home_away_list = c("H","A")
      }
      
      # Filter on area of game
      gmae_area_filter_cols = 1:9
      game_area_list = vector()
      if(input$v7 == 'Rushing'){
        gmae_area_filter_cols = c(gmae_area_filter_cols,10:12)
        game_area_list = 10:12
      } else if(input$v7 == 'Passing'){
        gmae_area_filter_cols = c(gmae_area_filter_cols,16:21)
        game_area_list = 16:21
      } else if(input$v7 == 'Recieving'){
        gmae_area_filter_cols = c(gmae_area_filter_cols,13:15)
        game_area_list = 13:15
      } else if(input$v7 == 'Punting'){
        gmae_area_filter_cols = c(gmae_area_filter_cols,22:26)
        game_area_list = 22:26
      } else if(input$v7 == 'All Purpose'){
        gmae_area_filter_cols = c(gmae_area_filter_cols,10:21,33:ncol(master))
        game_area_list = c(37,10:21,33:ncol(master))
      } else if(input$v7 == 'Kicking'){
        gmae_area_filter_cols = c(gmae_area_filter_cols,27:29)
        game_area_list = 27:29
      } else if(input$v7 == 'Interceptions'){
        gmae_area_filter_cols = c(gmae_area_filter_cols,30:32)
        game_area_list = 30:32
      } else if(input$v7 == 'Punt Return'){
        gmae_area_filter_cols = c(gmae_area_filter_cols,33:35)
        game_area_list = 33:35
      } else if(input$v7 == 'Kick Return'){
        gmae_area_filter_cols = c(gmae_area_filter_cols,36:37)
        game_area_list = 36:37
      } else if(input$v7 == 'All Areas'){
        gmae_area_filter_cols = c(gmae_area_filter_cols,10:ncol(master))
        game_area_list = 10:ncol(master)
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
        if ("Unknown" %in% pos_list){
          rows = c(rows,which(is.na(master$PlayerPosition)))
        }
        if ("Unknown" %in% year_list){
          rows = c(rows,which(is.na(master$PlayerYear)))
        }
        rows = intersect(rows,filterRows)
        # If we want sums
        if (input$sums == 'y'){
          a1 = master[rows,gmae_area_filter_cols] %>%
            arrange(Name,calYear,WeekNum) %>%
            mutate(WeekNum = as.character(WeekNum)) %>%
            group_by(Name,calYear) %>%
            mutate(gameNumbers = paste0(WeekNum,collapse = ', '))
          
          a1 %>%
            group_by(Name,calYear,gameNumbers) %>%
            summarise_if(is.numeric, sum, na.rm = TRUE)
          
        } else {
          master[rows,gmae_area_filter_cols]
        }
      }
    }
  })
  output$table = renderDataTable({datasetInput()},options = list(pageLength = 25))
  output$downloadData = downloadHandler(filename = paste(input$v1,'.csv',sep=''),
                                        content = function(file){write.csv(datasetInput(),file,row.names = F)},
                                        contentType = 'text/csv')
  
}





#### RUN SHINYAPP ####
shinyApp(ui = ui, server = server)

