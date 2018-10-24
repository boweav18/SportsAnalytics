library(tidyverse)
library(readxl)

setwd("C:/Users/fiscs14/Desktop/WFU Football")

alltimefb = read_xls("All-Time FB Results.xls", sheet= "All Gms") %>%
  filter(Year>=1944)

gxg = read_xls("Game by Game stats-Nick.xls", sheet = "Wake")

#gxg games seem to be off by a game
 gxg$`Gm #`=gxg$`Gm #`+1 
#location in alltime-fb of where the gxg games are
gxg_in_alltimefb = which(alltimefb$`Gm #` %in% gxg$`Gm #`)

gxg$Day = ""

for (i  in 1:nrow(gxg)){
  gxg$Day[i]=alltimefb$Day[(gxg_in_alltimefb[i])]
}

gxg=gxg[,-2]
setcolorder(gxg,c(1,76,2:75))

gxg1 <- gxg %>%
  select(Day, everything())



