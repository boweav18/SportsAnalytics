library(tidyverse)
library(stringr)
library(readxl)
library(janitor)
library(rlist)

rm(list=ls())

setwd("C:/Users/fiscs14/Desktop/WFU Football")

###prep excel: RS Fr -> RFR; RFr -> RFR; RSFR -> RFR; Jr -> JR; So -> SO; Sr -> SR; Grad -> GRAD; Fr -> FR; RS -> RSO, Rs -> RSO, RSOO -> RSO, RSOR -> RSR;
##further prep: take out (INCLUDES BOWL STATS) row in 1999 under rushing, OPP in 2015, "Love-Lane" -> "Love Lane" (1999,2000)
## change Dawson 16 IntTd from 6 to 0
#move stone total one row down in 99 (KOR)
#move around all punting to before FG (10,04,01,87)


master = data.frame(matrix(ncol = 38,nrow=0))
names1= c("Name","Opponent","WeekNum","PlayType","PlayerPosition","PlayerYear","calYear","HomeAway","WinLoss")
names2= c("RushAtt","RushYds","RushTD","ReceiveRec","ReceiveYds","ReceiveTD","PassCmp","PassAtt","PassInt","PassYds","PassTD","PassEff","PuntNo","PuntYds","PuntAvg","PuntBLK","PuntLNG","FGM","FGA","FGLP","IntInt","IntYds","IntTD","PRRet","PRYds","PRLP","KORet","KOYds","KOLP")
names(master)=c(names1,names2)
possibleyear = c("FR","SO","JR","SR","RFR","RSO","RJR","RSR","GRAD")



t=1986
while(t <2018) {
  s1 <- read_excel("1986-2017edited.xlsx", sheet = as.character(t), col_names = FALSE)
  #identify number of games (assumption: first game of season will always be in same spot (3,1)
  numgames = 0
  for (i in 3:17) {
    if (is.na(s1[i,1]==TRUE)){break}
    else if(is.na(s1[i,1])==FALSE){numgames=numgames+1}
  }
  
  #rushing
  #identify numbser of players in this playertype
  playerlocation = grep(".-.",s1[1,])
  numplayer = length(playerlocation)
  #create spacing for columns, essentially a generalized spacing of number of games played each season
  space = as.integer(seq(from = 1, to = numplayer*numgames,by=numgames))
  space1=as.integer(seq(from=13, to=numplayer*numgames+numgames,by=numgames)) #some difficulty with spacing here, why
  
  #create dataset
  s2 = data.frame(matrix(ncol=38,nrow=numplayer*numgames))
  names(s2)=c(names1,names2)
  i=1
  while(i<=numplayer) {
    s2[space[i]:space1[i],1]=s1[1,playerlocation[i]]
    #next line needs to be more genearlized, grab opponents after year would be pattern
    s2[space[i]:space1[i],2]=s1[3:15,1]
    s2[space[i]:space1[i],3]=rep(seq(from=1,to=numgames,by=1),times=numgames)[space[i]:space1[i]]
    #will need to check if playtype is in same location every year
    s2[space[i]:space1[i],4]=s1[1,1]
    ##playtype left alone for now as will be a little more difficult s2[space[i]:space1[i],3]=
    #s2[space[i]:space1[i],5]=separate()
    ##same deal with player year (will need to decide what is player year/playertype in string, choose from a list or so)
    #will need to check if year is in same location every time
    s2[space[i]:space1[i],7]=s1[2,1]
    s2[space[i]:space1[i],10]=s1[3:15,playerlocation[i]]
    s2[space[i]:space1[i],11]=s1[3:15,playerlocation[i]+1]
    s2[space[i]:space1[i],12]=s1[3:15,playerlocation[i]+2]
    i=i+1
  }


  
  #add receiving
  
  
  
  recloc = min(which(str_detect(s1$X__1,"Receiving")==TRUE))
  playerlocation = grep(".-.",s1[recloc,])
  numplayer = length(playerlocation)
  
  space = as.integer(seq(from = 1, to = numplayer*numgames,by=numgames))
  space1=as.integer(seq(from=numgames, to=numplayer*numgames,by=numgames)) #some difficulty with spacing here, why
  
  s3 = data.frame(matrix( ncol=38,nrow=numplayer*numgames))
  names(s3)=c(names1,names2)
  
  i=1
  while(i<=numplayer) {
    s3[space[i]:space1[i],1]=s1[recloc,playerlocation[i]]
    #next line needs to be more genearlized, grab opponents after year would be pattern
    s3[space[i]:space1[i],2]=s1[3:(numgames+2),1]
    s3[space[i]:space1[i],3]=rep(seq(from=1,to=numgames,by=1),times=numgames)[space[i]:space1[i]]
    #will need to check if playtype is in same location every year
    s3[space[i]:space1[i],4]=s1[recloc,1]
    #player position and year
    
    
    ##same deal with player year (will need to decide what is player year/playertype in string, choose from a list or so)
    #will need to check if year is in same location every time
    s3[space[i]:space1[i],7]=s1[2,1]
    #att/rec yards
    s3[space[i]:space1[i],13]=s1[(recloc+2):(recloc+numgames+1),playerlocation[i]]
    s3[space[i]:space1[i],14]=s1[(recloc+2):(recloc+numgames+1),playerlocation[i]+1]
    s3[space[i]:space1[i],15]=s1[(recloc+2):(recloc+numgames+1),playerlocation[i]+2]
    i=i+1
  }
  
  #passing
  
  
  #finiding row in which receiving starts
  passloc = min(which(str_detect(s1$X__1,"Passing")==TRUE))
  playerlocation = grep(".-.",s1[passloc,])
  numplayer = length(playerlocation)
  
  space = as.integer(seq(from = 1, to = numplayer*numgames,by=numgames))
  space1=as.integer(seq(from=numgames, to=numplayer*numgames,by=numgames)) #some difficulty with spacing here, why
  
  s4 = data.frame(matrix( ncol=38,nrow=numplayer*numgames))
  names(s4)=c(names1,names2)
  
  
  
  i=1
  while(i<=numplayer) {
    s4[space[i]:space1[i],1]=s1[passloc,playerlocation[i]]
    #next line needs to be more genearlized, grab opponents after year would be pattern
    s4[space[i]:space1[i],2]=s1[3:(numgames+2),1]
    s4[space[i]:space1[i],3]=rep(seq(from=1,to=numgames,by=1),times=numgames)[space[i]:space1[i]]
    #will need to check if playtype is in same location every year
    s4[space[i]:space1[i],4]=s1[passloc,1]
    #player position and year
    s4[space[i]:space1[i],7]=s1[2,1]
    #att (rush)/rec(receiving)/no(punting) --- assumption that these always listed in same order
    s4[space[i]:space1[i],17]=s1[(passloc+2):(passloc+numgames+1),playerlocation[i]+1]
    #yds
    s4[space[i]:space1[i],19]=s1[(passloc+2):(passloc+numgames+1),playerlocation[i]+3]
    #td
    s4[space[i]:space1[i],20]=s1[(passloc+2):(passloc+numgames+1),playerlocation[i]+4]
    #cmp
    s4[space[i]:space1[i],16]=s1[(passloc+2):(passloc+numgames+1),playerlocation[i]]
    #int
    s4[space[i]:space1[i],18]=s1[(passloc+2):(passloc+numgames+1),playerlocation[i]+2]
    #eff
    s4[space[i]:space1[i],21]=s1[(passloc+2):(passloc+numgames+1),playerlocation[i]+5]
    
    
    i=i+1
  }
  
  #punting
  
  #finiding row in which receiving starts
  puntloc = min(which(str_detect(s1$X__1,"Punting")==TRUE))
  fgloc=min(which(str_detect(s1[puntloc,],"FG")==TRUE))
  playerlocationpunt = grep(".-.",s1[puntloc,1:fgloc])
  numplayerpunt = length(playerlocationpunt)
  
  
  spacepunt = as.integer(seq(from = 1, to = numplayerpunt*numgames,by=numgames))
  spacepunt1=as.integer(seq(from=numgames, to=numplayerpunt*numgames,by=numgames)) #some difficulty with spacing here, why
  
  
  
  s8 = data.frame(matrix(ncol=38,nrow=numplayerpunt*numgames))
  names(s8)=c(names1,names2)
  
  
  
  i=1
  while(i<=numplayerpunt) {
    s8[spacepunt[i]:spacepunt1[i],1]=s1[puntloc,playerlocationpunt[i]]
    #next line needs to be more genearlized, grab opponents after year would be pattern
    s8[spacepunt[i]:spacepunt1[i],2]=s1[3:(numgames+2),1]
    s8[spacepunt[i]:spacepunt1[i],3]=rep(seq(from=1,to=numgames,by=1),times=numgames)[spacepunt[i]:spacepunt1[i]]
    #will need to check if playtype is in same location every year
    s8[spacepunt[i]:spacepunt1[i],4]=s1[puntloc,1]
    #player position and year
    s8[spacepunt[i]:spacepunt1[i],7]=s1[2,1]
    #att/rec/no --- assumption 
    s8[spacepunt[i]:spacepunt1[i],22]=s1[(puntloc+2):(puntloc+numgames+1),playerlocationpunt[i]]
    #yds
    s8[spacepunt[i]:spacepunt1[i],23]=s1[(puntloc+2):(puntloc+numgames+1),playerlocationpunt[i]+1]
    #avg
    s8[spacepunt[i]:spacepunt1[i],24]=s1[(puntloc+2):(puntloc+numgames+1),playerlocationpunt[i]+2]
    #blk
    s8[spacepunt[i]:spacepunt1[i],25]=s1[(puntloc+2):(puntloc+numgames+1),playerlocationpunt[i]+3]
    #lng
    s8[spacepunt[i]:spacepunt1[i],26]=s1[(puntloc+2):(puntloc+numgames+1),playerlocationpunt[i]+4]
    i=i+1
  }
  
  #fg
  
  puntloc = min(which(str_detect(s1$X__1,"Punting")==TRUE))
  fgloc=min(which(str_detect(s1[puntloc,],"FG")==TRUE))
  playerlocationpuntfg=(grep(".-.",s1[puntloc,fgloc:ncol(s1)]))
  numplayerfg=length(playerlocationpuntfg)
  
  if (numplayerfg>0){
    
    spacefg = as.integer(seq(from = 1, to = numplayerfg*numgames,by=numgames))
    spacefg1=as.integer(seq(from=numgames, to=numplayerfg*numgames,by=numgames)) #some difficulty with spacing here, why
    
    
    
    s9 = data.frame(matrix( ncol=38,nrow=numplayerfg*numgames))
    names(s9)=c(names1,names2)
    
    
    i=1
    while(i<=numplayerfg) {
      s9[spacefg[i]:spacefg1[i],1]=s1[puntloc,(fgloc+playerlocationpuntfg[i])-1]
      #next line needs to be more genearlized, grab opponents after year would be pattern
      s9[spacefg[i]:spacefg1[i],2]=s1[3:(numgames+2),1]
      s9[spacefg[i]:spacefg1[i],3]=rep(seq(from=1,to=numgames,by=1),times=numgames)[spacefg[i]:spacefg1[i]]
      #will need to check if playtype is in same location every year
      s9[spacefg[i]:spacefg1[i],4]=s1[puntloc,1]
      #player position and year
      s9[spacefg[i]:spacefg1[i],7]=s1[2,1]
      #att/rec/no --- assumption 
      s9[spacefg[i]:spacefg1[i],28]=s1[(puntloc+2):(puntloc+numgames+1),(fgloc+playerlocationpuntfg[i])]
      #fgm
      s9[spacefg[i]:spacefg1[i],27]=s1[(puntloc+2):(puntloc+numgames+1),(fgloc+playerlocationpuntfg[i])-1]
      #length
      s9[spacefg[i]:spacefg1[i],29]=s1[(puntloc+2):(puntloc+numgames+1),(fgloc+playerlocationpuntfg[i])+1]
      i=i+1
    }
  }
  
  #interceptions
  
  #finiding row in which receiving starts
  intloc = min(which(str_detect(s1$X__1,"Interceptions")==TRUE))
  playerlocation = grep(".-.",s1[intloc,])
  numplayer = length(playerlocation)
  
  space = as.integer(seq(from = 1, to = numplayer*numgames,by=numgames))
  space1=as.integer(seq(from=numgames, to=numplayer*numgames,by=numgames)) #some difficulty with spacing here, why
  
  s5 = data.frame(matrix( ncol=38,nrow=numplayer*numgames))
  names(s5)=c(names1,names2)
  
  
  
  i=1
  while(i<=numplayer) {
    s5[space[i]:space1[i],1]=s1[intloc,playerlocation[i]]
    #next line needs to be more genearlized, grab opponents after year would be pattern
    s5[space[i]:space1[i],2]=s1[3:(numgames+2),1]
    s5[space[i]:space1[i],3]=rep(seq(from=1,to=numgames,by=1),times=numgames)[space[i]:space1[i]]
    #will need to check if playtype is in same location every year
    s5[space[i]:space1[i],4]=s1[intloc,1]
    #player position and year
    s5[space[i]:space1[i],7]=s1[2,1]
    #yds
    s5[space[i]:space1[i],31]=s1[(intloc+2):(intloc+numgames+1),playerlocation[i]+1]
    #td
    s5[space[i]:space1[i],32]=s1[(intloc+2):(intloc+numgames+1),playerlocation[i]+2]
    #int
    s5[space[i]:space1[i],30]=s1[(intloc+2):(intloc+numgames+1),playerlocation[i]]
    i=i+1
  }
  
  #punt returns
  
  #finiding row in which receiving starts
  puntrloc = min(which(str_detect(s1$X__1,"Punt Returns")==TRUE))
  playerlocation = grep(".-.",s1[puntrloc,])
  numplayer = length(playerlocation)
  
  space = as.integer(seq(from = 1, to = numplayer*numgames,by=numgames))
  space1=as.integer(seq(from=numgames, to=numplayer*numgames,by=numgames)) #some difficulty with spacing here, why
  
  s6 = data.frame(matrix( ncol=38,nrow=numplayer*numgames))
  names(s6)=c(names1,names2)
  
  
  
  i=1
  while(i<=numplayer) {
    s6[space[i]:space1[i],1]=s1[puntrloc,playerlocation[i]]
    #next line needs to be more genearlized, grab opponents after year would be pattern
    s6[space[i]:space1[i],2]=s1[3:(numgames+2),1]
    s6[space[i]:space1[i],3]=rep(seq(from=1,to=numgames,by=1),times=numgames)[space[i]:space1[i]]
    #will need to check if playtype is in same location every year
    s6[space[i]:space1[i],4]=s1[puntrloc,1]
    #player position and year
    s6[space[i]:space1[i],7]=s1[2,1]
    #att (rush)/rec(receiving)/no(punting)/ret(punt return) --- assumption that these always listed in same order
    s6[space[i]:space1[i],33]=s1[(puntrloc+2):(puntrloc+numgames+1),playerlocation[i]]
    #yds
    s6[space[i]:space1[i],34]=s1[(puntrloc+2):(puntrloc+numgames+1),playerlocation[i]+1]
    #lp
    s6[space[i]:space1[i],35]=s1[(puntrloc+2):(puntrloc+numgames+1),playerlocation[i]+2]
    
    i=i+1
  }
  
  #ko returns
  
  #finiding row in which receiving starts
  koloc = min(which(str_detect(s1$X__1,"KO Returns")==TRUE))
  playerlocation = grep(".-.",s1[koloc,])
  numplayer = length(playerlocation)
  
  space = as.integer(seq(from = 1, to = numplayer*numgames,by=numgames))
  space1=as.integer(seq(from=numgames, to=numplayer*numgames,by=numgames)) #some difficulty with spacing here, why
  
  s7 = data.frame(matrix( ncol=38,nrow=numplayer*numgames))
  names(s7)=c(names1,names2)
  
  
  
  i=1
  while(i<=numplayer) {
    s7[space[i]:space1[i],1]=s1[koloc,playerlocation[i]]
    #next line needs to be more genearlized, grab opponents after year would be pattern
    s7[space[i]:space1[i],2]=s1[3:(numgames+2),1]
    s7[space[i]:space1[i],3]=rep(seq(from=1,to=numgames,by=1),times=numgames)[space[i]:space1[i]]
    #will need to check if playtype is in same location every year
    s7[space[i]:space1[i],4]=s1[koloc,1]
    #player position and year
    s7[space[i]:space1[i],7]=s1[2,1]
    #att (rush)/rec(receiving)/no(punting) --- assumption that these always listed in same order
    s7[space[i]:space1[i],36]=s1[(koloc+2):(koloc+numgames+1),playerlocation[i]]
    #yds
    s7[space[i]:space1[i],37]=s1[(koloc+2):(koloc+numgames+1),playerlocation[i]+1]
    #td
    s7[space[i]:space1[i],38]=s1[(koloc+2):(koloc+numgames+1),playerlocation[i]+2]
    i=i+1
  }

  
  master=rbind(master,s2,s3,s4,s8,s9,s5,s6,s7)
  t=t+1
}





tabyl(is.na(master$Opponent))

#pretty much only doing this because spacing on rushing is slightly off, fixed it for later sections but couldn't find the flaw in the first one anymore
master=master%>%
  filter(is.na(master$Opponent)==FALSE)

#filtering through names to get to assign player position and year
for (i in 1:nrow(master)){
  hycount = 0 
  if (str_count(master[i,1],"-")==1) {hycount=1}
  else if (str_count(master[i,1],"-")==2) {hycount=2}
  #add hycount = 3
  else if (is.na(str_count(master[i,1],"-"))==TRUE) {hycount=0}
  
  if (hycount==1) {
    if(gsub( "(.*)-(.*)", "\\2",master[i,1]) %in% possibleyear){
      master$PlayerYear[i] = gsub( "(.*)-(.*)", "\\2",master[i,1])
    }
    else {master$PlayerPosition[i] = gsub( "(.*)-(.*)", "\\2",master[i,1])}
  }
  
  else if (hycount==2) {
    if (gsub( "(.*)-(.*)-(.*)", "\\2",master[i,1]) %in% possibleyear){
      master$PlayerYear[i]=gsub( "(.*)-(.*)-(.*)", "\\2",master[i,1])
      master$PlayerPosition[i]=gsub( "(.*)-(.*)-(.*)", "\\3",master[i,1])
    }
    else {
      master$PlayerPosition[i]=gsub( "(.*)-(.*)-(.*)", "\\2",master[i,1])
      master$PlayerYear[i]=gsub( "(.*)-(.*)-(.*)", "\\3",master[i,1])
    }
  }
  
}

#forgot that some columns were character
master[,10:38]=lapply((10:38),function(x) as.numeric(master[[x]]))

#fucking mccoy in here twice
master= master[-c(6787:6798),]




master$PassEff=master$PassCmp/master$PassAtt


master$Name = toupper(sub("\\-.*","",master$Name))
master$Opponent = toupper(master$Opponent)



#arranging the data in the way we need it to combine rows (using identifiers that will make same-game observations appear underneath each other in the output)
master1=master%>%
  arrange(calYear,Name,Opponent)




#have to run this as least 3 times, not sure why a loop won't do it
#actually, for loop won't do it because it breaks after the error (so would be after first iteration of the loop)
for (j in 1:(nrow(master1)-1)){
#only doing this when the identifiers match up
if(master1$Name[j]==master1$Name[(j+1)] & master1$calYear[j]==master1$calYear[(j+1)] & master1$Opponent[j]==master1$Opponent[(j+1)] ){
  #identifying where the relevant stats are in bottom line
  statloc= which(str_detect((is.na(master1[j+1,])),"FALSE"),arr.ind=FALSE)
  #deleting the first seven observations in statloc because they're irrelevant for the combination (they're name:calyear)
  statloc=statloc[-c(1:6)]
  #filling in the top row with the information from the bottom row
  if(length(statloc)>0){
  for(i in 1:length(statloc)){
    master1[j,statloc[i]]=master1[(j+1),statloc[i]]
  }
  }
  #combining playtypes to better reflect what a player did in a game
  master1$PlayType[j]=paste(master1$PlayType[j],master1$PlayType[j+1],sep = " / ")
  #deleting the row that we just got the info from as it's now superfluous
  master1=master1[-(j+1),]
}
}


#error message resulting because reducing number of observations in loop while running for initial row number 




##is this deleting entries? (e.g @SYR no longer present, should just be there as SYR)
for (i in 1:nrow(master1)){
  if (str_detect(master1$Opponent[i],"@")==TRUE){
    master1$Opponent[i]=gsub('@','',master1$Opponent[i])
  }
  else if (str_detect(master1$Opponent[i],"^AT")==TRUE){
    master1$Opponent[i]=gsub('^AT ','',master1$Opponent[i])
  }
  else if (str_detect(master1$Opponent[i],"^VS.")==TRUE){
    master1$Opponent[i]=gsub('^VS\\.','',master1$Opponent[i])
  }
}

#Also need to add an exception for where the top case doesn't have some of the stats (e.g. PlayerYear or PlayerPosition), but the bottom line does. 


#Changing names so that there aren't double entries:

for(i in 1:(nrow(master1))){
  if (master1$Opponent[i]=="UNC"){master1$Opponent[i]="NORTH CAROLINA"}
  if (master1$Opponent[i]=="BOSTON"){master1$Opponent[i]="BOSTON UNIVERSITY"}
  if (master1$Opponent[i]=="ASU"){master1$Opponent[i]="APPALACHIAN STATE"}
  if (master1$Opponent[i]=="CU"){master1$Opponent[i]="CLEMSON"}
  if (master1$Opponent[i]=="CLEM"){master1$Opponent[i]="CLEMSON"}
  if (master1$Opponent[i]=="DU"){master1$Opponent[i]="DUKE"}
  if (master1$Opponent[i]=="FS"){master1$Opponent[i]="FLORIDA STATE"}
  if (master1$Opponent[i]=="GT"){master1$Opponent[i]="GEORGIA TECH"}
  if (master1$Opponent[i]=="MD"){master1$Opponent[i]="MARYLAND"}
  if (master1$Opponent[i]=="NC"){master1$Opponent[i]="NORTH CAROLINA"}
  if (master1$Opponent[i]=="NV"){master1$Opponent[i]="NAVY"}
  if (master1$Opponent[i]=="ST"){master1$Opponent[i]="NC STATE"}
  if (master1$Opponent[i]=="VA"){master1$Opponent[i]="VIRGINIA"}
  if (master1$Opponent[i]=="VU"){master1$Opponent[i]="VANDERBILT"}
  if (master1$Opponent[i]=="BC"){master1$Opponent[i]="BOSTON COLLEGE"}
  if (master1$Opponent[i]=="FSU"){master1$Opponent[i]="FLORIDA STATE"}
  if (master1$Opponent[i]=="VANDY"){master1$Opponent[i]="VANDERBILT"}
  if (master1$Opponent[i]=="NC ST"){master1$Opponent[i]="NC STATE"}
  if (master1$Opponent[i]=="UVA"){master1$Opponent[i]="VIRGINIA"}
  if (master1$Opponent[i]=="NCST"){master1$Opponent[i]="NC STATE"}
  if (master1$Opponent[i]=="PC"){master1$Opponent[i]="PRESBYTERIAN"}
  if (master1$Opponent[i]=="GARD-WEBB"){master1$Opponent[i]="GARDNER-WEBB"}
  if (master1$Opponent[i]=="GARD-WEB"){master1$Opponent[i]="GARDNER-WEBB"}
  if (master1$Opponent[i]=="VT"){master1$Opponent[i]="VIRGINIA TECH"}
  if (master1$Opponent[i]=="VA TECH"){master1$Opponent[i]="VIRGINIA TECH"}
  if (master1$Opponent[i]=="SYR"){master1$Opponent[i]="SYRACUSE"}
  if (master1$Opponent[i]=="MISS ST"){master1$Opponent[i]="MISSISSIPPI STATE"}
  if (master1$Opponent[i] == "TAMU (BELK)") {master1$Opponent[i]="TEXAS A&M"}
  if (master1$Opponent[i] == "GA TECH") {master1$Opponent[i]="GEORGIA TECH"}
  if (master1$Opponent[i] == "LOUISV") {master1$Opponent[i]="LOUISVILLE"}
  if (master1$Opponent[i] == "UM") {master1$Opponent[i]="MIAMI (FL)"}
  if (master1$Opponent[i] == "MIAMI") {master1$Opponent[i]="MIAMI (FL)"}
  if (master1$Opponent[i] == "APP") {master1$Opponent[i]="TEXAS A&M"}
  if (master1$Opponent[i] == "APP") {master1$Opponent[i]="APPALACHIAN STATE"}
  if (master1$Opponent[i] == "APP STATE") {master1$Opponent[i]="APPALACHIAN STATE"}
  if (master1$Opponent[i] == "USU") {master1$Opponent[i]="UTAH STATE"}
  if (master1$Opponent[i] == "NC A&T") {master1$Opponent[i]="NORTH CAROLINA A&T"}
  if (master1$Opponent[i] == "OLE MISS") {master1$Opponent[i]="MISSISSIPPI"}
  if (master1$Opponent[i] == " NAVY") {master1$Opponent[i]="NAVY"}
  if (master1$Opponent[i] == "UTAH ST") {master1$Opponent[i]="UTAH STATE"}
  
}



master1 = master1 %>%
  arrange(calYear,PlayType,Name,WeekNum)

master1 = master1[,-c(8,9)]

#solving WeekNum NA's
for (i in 1:nrow(master1)) {
  if (is.na(master1$WeekNum[i])==TRUE){
    m1 = master1 %>%
      filter(Opponent==master1$Opponent[i] & calYear==master1$calYear[i]) 
    master1$WeekNum[i] = m1[min(which(!is.na(m1$WeekNum))),3]
    }
}
#solving PlayerPosition NA's

master1$calYear=as.numeric(master1$calYear)

for (i in 1:nrow(master1)){
if (is.na(master1$PlayerPosition[i])==TRUE){
 
  m2 = master1 %>%
    filter(Name == master1$Name[i] & calYear %in% c(calYear[i]-1,calYear[i]-2,calYear[i]-3,calYear[i]+1,calYear[i]+2,calYear[i]+3))
  
  master1$PlayerPosition[i] = m2[min(which(!is.na(m2$PlayerPosition))),5]
}
}
#warnings here are fine - no affect on computation (simply attempting to find the min of a table that doesn't exist -- happens when there are no records of a playerposition in the dataset at all)

write.csv(master1,file = "master1new.csv")
