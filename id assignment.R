library(tidyverse)
library(readxl)
library(janitor)

setwd("~/WFU/Analytics Team/ids")

all_players <- read_excel("All-Time FB Players-Nick.xlsx", sheet = 'All-Time Players')


#finding all of the unique names and pulling them out
names <- as.data.frame(as.matrix(unique(all_players$Name)))


#checking for duplicate names, there are many players that have duplicate names but those are for if they played multiple seasons with wake, ex. fr year and so year they played

dup <- get_dupes(all_players, Name)

#if uniques of dupe_count is are not within {2,3,4} and exceed 4 then that means a player throughout wake history has had the same first and last name and that has to be addressed
unique(dup$dupe_count)
#looks like there was not any greater than 4 so we are good


#assigning numbers to the names
names$id <- {1:nrow(names)}
names(names)[names(names) == 'V1'] <- 'Name'

#now going to try and merge with the all_players data set
player_id <- merge(all_players, names, by = "Name") %>%
  select(Name, No., Pos, Cl, Yr, id)
#noice



#now gonna merge with defensive list
player_id$Name <- toupper(player_id$Name)
def$Name <- toupper(def$Name)
View(player_id)
names1 <- names
names1$Name <- toupper(names1$Name)

def <- read.csv("def1.csv")
def$X <- NULL

def$id <- "x"

#uhhh building loop, ignore

#def_id <- merge(names1, def, by = "Name")
#def_id <- merge(player_id, def, by = "Name")

def[303, 3]
player_id[631,1]

def[303, 3] %in% player_id$Name
kk <- match(def[303,3], player_id$Name)
l <- player_id[kk, 6]

def[303,29] <- as.character(player_id[kk, 6])
assign(def[303,29], as.numeric(player_id[kk, 6]))
assign(def[303,29], as.character(l))
##################



for (i in 1:nrow(def)){
  k <- match(def[i,3], player_id$Name)
  if(def[i, 3] %in% player_id$Name){def[i, 29] <- as.character(player_id[k, 6]) }
}

testloop <- def %>% filter(id == 'x')






"DAWSON, GRANT" %in% player_id$Name
match("DAWSON, GRANT", player_id$Name)








#########################################################################################
##########################################################################
##########################################################

#RETRY \\\ diff method###
#def#

def <- read.csv("def1.csv")

#delete random extra column#
def$X <- NULL

#create dummy values for new column#
def$id <- "x"

#pull out unique names#
def_name <- as.data.frame(as.matrix(unique(def$Name)))

#assign ids to the unique names under a var called id#
def_name$id <- {1:nrow(def_name)}
#change the name of a column#
names(def_name)[names(def_name) == 'V1'] <- 'Name'

#match the unique ids and assign them into the defensive data set#
for (i in 1:nrow(def)){
  d <- match(def[i,3], def_name$Name)
  if(def[i, 3] %in% def_name$Name){def[i, 29] <- as.character(def_name[d, 2]) }
}

#####noiiiice got the def set ^^^#


#off#

###creating a unique set of offensive observations
#extract unique identifiers from master1 and conc into one column#
off <- master1 %>% select(Name, calYear, PlayerYear, PlayerPosition) %>%
  unite(uni, sep = "-")

#now pull out unique values from that conc new column of all the unique identifiers#
off_name <- as.data.frame(as.matrix(unique(off)))

#assign ids to all of the unique values we pulled out#
off_name$id <- {1:nrow(off_name)}

###
# off_name <- off_name %>% separate(uni, c("Name", "calYear", "PlayerYear", "PlayerPosition"), sep = "-")
# for (i in 1:nrow(def)){
#   o <- match(off[i,3], off_name$Name)
#   if(master1[i, 1] %in% off_name$Name & master1[i, 7] %in% off_name$calYear & master1[i, 6] %in% off_name$PlayerYear & master1[i, 5] %in% off_name$PlayerPosition){def[i, 29] <- as.character(def_name[o, 37]) }
# }
###

#now format the master1 set to match the uni column from the off_name set#
off2 <- master1 %>% unite(uni, c("Name", "calYear", "PlayerYear", "PlayerPosition"), sep = "-")

#create a dummy val for a new var called id#
off2$id <- "x"

#match the values from the united column in off2 with its corresponding values in the set that contains the ids and then assign the ids to the off2 set#
for (i in 1:nrow(off2)){
  o <- match(off2[i,1], off_name$uni)
  if(off2[i, 1] %in% off_name$uni){off2[i, 34] <- as.character(off_name[o, 2]) }
}

#now separating the united columns after assigning the ids#
off3 <- off2 %>% separate(uni, c("Name", "calYear", "PlayerYear", "PlayerPosition"), sep = "-")

#NOIIIIIICE


#okay now basic test
t_def <- def %>% filter(is.na(id))
View(t_def)
unique(def$id)

t_off <- off3 %>% filter(is.na(id))
View(t_off)
unique(off3$id)
