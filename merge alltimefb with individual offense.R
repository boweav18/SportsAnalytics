library(tidyverse)
library(readxl)

setwd("C:/Users/fiscs14/Desktop/WFU Football")

alltimefb = read_xls("All-Time FB Results.xls",sheet="All Gms") %>%
  filter(Year>=1944)

master = read.csv("mastertotal.csv",stringsAsFactors = FALSE)

alltimefb$Opponent2=toupper(alltimefb$Opponent2)

atopp = unique(alltimefb$Opponent2)
masteropp = unique(master$Opponent)
r1 = masteropp[which(!(masteropp%in% atopp))]

master$Opponent[master$Opponent=="EASTERN CAROLINA"]="EAST CAROLINA"
master$Opponent[master$Opponent=="ARIZONA STATE"]="ARIZONA STATE1"


m1 = master %>%
  select(calYear,Opponent,everything())
m1=m1[,-3]

a1 = alltimefb %>%
  select(Year,Opponent2,`W/L`,`H A`,Score)
names(a1)=c("calYear","Opponent","Win/Loss","Home/Away","Score")

m2 = merge(a1,m1,by=c("calYear","Opponent"),all=TRUE)
m2 = m2 %>%
  filter(calYear>=1950) %>%
  arrange(calYear,WeekNum)

write.csv(m2,"mastermerge.csv")
