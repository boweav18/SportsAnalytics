library(tidyverse)
library(janitor)
library(readxl)
library(stringr)


#loading data
#setwd("~/WFU/Analytics Team/def")
#def1 <- read_excel("All-Time Defensive Stats.xlsx")
def1 <- read_csv("IndivDefensiveStats.csv")
#View(def1)

#testing
#unique(def1$Class)

#changing all RS FR values to RFR

for (i in 1:(nrow(def1))){
  if (def1$Class[i] %in% ("RS FR")){def1$Class[i]="RFR"}
}
#unique(def1$Class)

#found the na values for class
class_na <- def1 %>% filter(Class %in% c("", NA))
#View(class_na)

##ask steve ab sr-1 and sr-2 and
#unique(def1$Pos)



#formatting def
def1$Class = toupper(def1$Class)
def1$Pos = toupper(def1$Pos)
def1$`Name (last, first)` = toupper(def1$`Name (last, first)`)

def2_namesformatted <- def1
def2_namesformatted$`Name (last, first)` <- gsub(".+,", "", def1$`Name (last, first)`)
#View(def2_namesformatted)

#changing column names to match master1
names(def1)[4]<-"calYear"
names(def1)[1]<-"PlayerYear"
names(def1)[2]<-"PlayerPosition"
names(def1)[3]<-"Name"

names(def2_namesformatted)[4]<-"calYear"
names(def2_namesformatted)[1]<-"PlayerYear"
names(def2_namesformatted)[2]<-"PlayerPosition"
names(def2_namesformatted)[3]<-"Name"

colnames(def2_namesformatted) = c('PlayerYear','PlayerPosition','Name','calYear','Games','SoloTackles','AsstTackles','TotalTackles',
                                  'TacklesForLoss','TFL_Yds','Sacks','Sack_Yds','Int','Int_Yds','IntTD','PassBreakUps','QBHurries',
                                  'FumbleRecov','FumbleRecov_Yds','ForcedFumbles','BlockedKicks','Safety','TotalCheck','Diff',
                                  'PDF','Tkls/Gm','Tm Rank','PDF_1')

def2_namesformatted = def2_namesformatted %>%
  mutate(`Tkls/Gm` = TotalTackles/Games)
def2_namesformatted = def2_namesformatted[,c(1:22,26,23,24,25,27,28)]
def2_namesformatted[which(is.na(def2_namesformatted$PlayerYear)),1] = 'Unknown'
def2_namesformatted[which(is.na(def2_namesformatted$PlayerPosition)),2] = 'Unknown'

write.csv(def2_namesformatted,file = "def_ind_reformatted.csv")

