library(tidyverse)
library(readxl)
library(janitor)

setwd("C:/Users/fiscs14/Desktop/WFU Football")

alltimefb = read_xls("All-Time FB Results.xls", sheet= "All Gms") %>%
  filter(Year>=1944)

gxg = read_xls("Game by Game stats-Nick.xls", sheet = "Wake")


gxg$`Opp 2`[gxg$`Opp 2`=="NC St"] = "NC State"
gxg$`Opp 2`[gxg$`Opp 2`=="UNC"] = "North Carolina"
gxg$`Opp 2`[gxg$`Opp 2`=="North Carolina (2)"] = "North Carolina"
gxg$`Opp 2`[gxg$`Opp 2`=="Baylor"] = "Baylor"


for(i in 1:(nrow(gxg))){
  if (gxg$`Opp 2`[i]=="UNC"){gxg$`Opp 2`[i]="NORTH CAROLINA"}
  if (gxg$`Opp 2`[i]=="BOSTON"){gxg$`Opp 2`[i]="BOSTON UNIVERSITY"}
  if (gxg$`Opp 2`[i]=="ASU"){gxg$`Opp 2`[i]="APPALACHIAN STATE"}
  if (gxg$`Opp 2`[i]=="CU"){gxg$`Opp 2`[i]="CLEMSON"}
  if (gxg$`Opp 2`[i]=="CLEM"){gxg$`Opp 2`[i]="CLEMSON"}
  if (gxg$`Opp 2`[i]=="DU"){gxg$`Opp 2`[i]="DUKE"}
  if (gxg$`Opp 2`[i]=="FS"){gxg$`Opp 2`[i]="FLORIDA STATE"}
  if (gxg$`Opp 2`[i]=="GT"){gxg$`Opp 2`[i]="GEORGIA TECH"}
  if (gxg$`Opp 2`[i]=="MD"){gxg$`Opp 2`[i]="MARYLAND"}
  if (gxg$`Opp 2`[i]=="NC"){gxg$`Opp 2`[i]="NORTH CAROLINA"}
  if (gxg$`Opp 2`[i]=="NV"){gxg$`Opp 2`[i]="NAVY"}
  if (gxg$`Opp 2`[i]=="ST"){gxg$`Opp 2`[i]="NC STATE"}
  if (gxg$`Opp 2`[i]=="VA"){gxg$`Opp 2`[i]="VIRGINIA"}
  if (gxg$`Opp 2`[i]=="VU"){gxg$`Opp 2`[i]="VANDERBILT"}
  if (gxg$`Opp 2`[i]=="BC"){gxg$`Opp 2`[i]="BOSTON COLLEGE"}
  if (gxg$`Opp 2`[i]=="FSU"){gxg$`Opp 2`[i]="FLORIDA STATE"}
  if (gxg$`Opp 2`[i]=="VANDY"){gxg$`Opp 2`[i]="VANDERBILT"}
  if (gxg$`Opp 2`[i]=="NC ST"){gxg$`Opp 2`[i]="NC STATE"}
  if (gxg$`Opp 2`[i]=="UVA"){gxg$`Opp 2`[i]="VIRGINIA"}
  if (gxg$`Opp 2`[i]=="NCST"){gxg$`Opp 2`[i]="NC STATE"}
  if (gxg$`Opp 2`[i]=="PC"){gxg$`Opp 2`[i]="PRESBYTERIAN"}
  if (gxg$`Opp 2`[i]=="GARD-WEBB"){gxg$`Opp 2`[i]="GARDNER-WEBB"}
  if (gxg$`Opp 2`[i]=="GARD-WEB"){gxg$`Opp 2`[i]="GARDNER-WEBB"}
  if (gxg$`Opp 2`[i]=="VT"){gxg$`Opp 2`[i]="VIRGINIA TECH"}
  if (gxg$`Opp 2`[i]=="VA TECH"){gxg$`Opp 2`[i]="VIRGINIA TECH"}
  if (gxg$`Opp 2`[i]=="SYR"){gxg$`Opp 2`[i]="SYRACUSE"}
  if (gxg$`Opp 2`[i]=="MISS ST"){gxg$`Opp 2`[i]="MISSISSIPPI STATE"}
  if (gxg$`Opp 2`[i] == "TAMU (BELK)") {gxg$`Opp 2`[i]="TEXAS A&M"}
  if (gxg$`Opp 2`[i] == "GA TECH") {gxg$`Opp 2`[i]="GEORGIA TECH"}
  if (gxg$`Opp 2`[i] == "LOUISV") {gxg$`Opp 2`[i]="LOUISVILLE"}
  if (gxg$`Opp 2`[i] == "UM") {gxg$`Opp 2`[i]="MIAMI (FL)"}
  if (gxg$`Opp 2`[i] == "MIAMI") {gxg$`Opp 2`[i]="MIAMI (FL)"}
  if (gxg$`Opp 2`[i] == "APP") {gxg$`Opp 2`[i]="TEXAS A&M"}
  if (gxg$`Opp 2`[i] == "APP") {gxg$`Opp 2`[i]="APPALACHIAN STATE"}
  if (gxg$`Opp 2`[i] == "APP STATE") {gxg$`Opp 2`[i]="APPALACHIAN STATE"}
  if (gxg$`Opp 2`[i] == "USU") {gxg$`Opp 2`[i]="UTAH STATE"}
  if (gxg$`Opp 2`[i] == "NC A&T") {gxg$`Opp 2`[i]="NORTH CAROLINA A&T"}
  if (gxg$`Opp 2`[i] == "OLE MISS") {gxg$`Opp 2`[i]="MISSISSIPPI"}
  if (gxg$`Opp 2`[i] == " NAVY") {gxg$`Opp 2`[i]="NAVY"}
  if (gxg$`Opp 2`[i] == "UTAH ST") {gxg$`Opp 2`[i]="UTAH STATE"}
  
}

#seeing which ones are still not right
atopp = unique(alltimefb$Opponent2)
gxgopp= unique(gxg$`Opp 2`)
r1 = gxgopp[which(!(gxgopp%in% atopp),useNames = FALSE)]

gxg$`Opp 2`[gxg$`Opp 2`=="Baylor2"] = "Baylor"
gxg$`Opp 2`[gxg$`Opp 2`=="Va Tech"] = "Virginia Tech"
gxg$`Opp 2`[gxg$`Opp 2`=="Florida St"] = "Florida State"
gxg$`Opp 2`[gxg$`Opp 2`=="ECU"] = "East Carolina"
gxg$`Opp 2`[gxg$`Opp 2`=="Memphis St"] = "Memphis State"
gxg$`Opp 2`[gxg$`Opp 2`=="Miami"] = "Miami (FL)"
gxg$`Opp 2`[gxg$`Opp 2`=="Penn St"] = "Penn State"
gxg$`Opp 2`[gxg$`Opp 2`=="App St"] = "Appalachian State"
gxg$`Opp 2`[gxg$`Opp 2`=="Kansas St"] = "Kansas State"
gxg$`Opp 2`[gxg$`Opp 2`=="Georiga"] = "Georgia"
gxg$`Opp 2`[gxg$`Opp 2`=="Citadel"] = "The Citadel"
gxg$`Opp 2`[gxg$`Opp 2`=="WCU"] = "Western Carolina"
gxg$`Opp 2`[gxg$`Opp 2`=="Ga Tech"] = "Georgia Tech"
gxg$`Opp 2`[gxg$`Opp 2`=="VPI"] = "Virginia Tech"
gxg$`Opp 2`[gxg$`Opp 2`=="App St."] = "Appalachian State"
gxg$`Opp 2`[gxg$`Opp 2`=="App. St."] = "Appalachian State"
gxg$`Opp 2`[gxg$`Opp 2`=="NC St."] = "NC State"
gxg$`Opp 2`[gxg$`Opp 2`=="Gerogia Tech"] = "Georgia Tech"
gxg$`Opp 2`[gxg$`Opp 2`=="W&M"] = "William & Mary"
gxg$`Opp 2`[gxg$`Opp 2`=="BU"] = "Boston University"
gxg$`Opp 2`[gxg$`Opp 2`=="Tenn"] = "Tennessee"
gxg$`Opp 2`[gxg$`Opp 2`=="Tenn."] = "Tennessee"
gxg$`Opp 2`[gxg$`Opp 2`=="Boston U"] = "Boston University"
gxg$`Opp 2`[gxg$`Opp 2`=="UR"] = "Richmond"
gxg$`Opp 2`[gxg$`Opp 2`=="Ill. St."] = "Illinois State"
gxg$`Opp 2`[gxg$`Opp 2`=="Georiga Tech."] = "Georgia Tech"
gxg$`Opp 2`[gxg$`Opp 2`=="Virgina"] = "Virginia"
gxg$`Opp 2`[gxg$`Opp 2`=="N'Western"] = "Northwestern"
gxg$`Opp 2`[gxg$`Opp 2`=="Florida St."] = "Florida State"
gxg$`Opp 2`[gxg$`Opp 2`=="Georiga. Tech"] = "Georgia Tech"
gxg$`Opp 2`[gxg$`Opp 2`=="Georgia. Tech"] = "Georgia Tech"
gxg$`Opp 2`[gxg$`Opp 2`=="Georgia Tech."] = "Georgia Tech"
gxg$`Opp 2`[gxg$`Opp 2`=="Ga Tech"] = "Georgia Tech"
gxg$`Opp 2`[gxg$`Opp 2`=="Arizona St"] = "Arizona State1"
gxg$`Opp 2`[gxg$`Opp 2`=="Miami, Fla"] = "Miami (FL)"
gxg$`Opp 2`[gxg$`Opp 2`=="USC"] = "South Carolina"


gxg$`Opp 2`=toupper(gxg$`Opp 2`)
alltimefb$Opponent2=toupper(alltimefb$Opponent2)

gxg$`Opp 2`[gxg$`Opp 2`=="GA TECH"] = "GEORGIA TECH"









gxg$Day=""


for (i in 1:nrow(gxg)){
  if(gxg$`Opp 2`[i]==alltimefb$Opponent2[i] & gxg$Year[i]==alltimefb$Year[i]){
    gxg$Day[i]=alltimefb$Day[i]
  }
  else if (length(which(alltimefb$Year==gxg$Year[i] & alltimefb$Opponent2==gxg$`Opp 2`[i]))>0){
    x=0
    x=which(alltimefb$Year==gxg$Year[i] & alltimefb$Opponent2==gxg$`Opp 2`[i])
    gxg$Day[i]=alltimefb$Day[x]
  }
}
gxg1 <- gxg %>%
  select(Day, everything())

#four instances for which this doesn't work, so manually enter
which(gxg1$Day=="")
gxg1$Day[c(32,201,210,656)]=c("Jan. 1","Nov. 20","Nov. 12","Jan. 2")


#now do the above for opp

gxg_opp = read_xls("Game by Game stats-Nick.xls",sheet="Opp")
gxg_opp$Day = ""


#get to work after 3rd observation

for(i in 1:(nrow(gxg_opp))){
  if (gxg_opp$`Opp 2`[i]=="UNC"){gxg_opp$`Opp 2`[i]="NORTH CAROLINA"}
  if (gxg_opp$`Opp 2`[i]=="BOSTON"){gxg_opp$`Opp 2`[i]="BOSTON UNIVERSITY"}
  if (gxg_opp$`Opp 2`[i]=="ASU"){gxg_opp$`Opp 2`[i]="APPALACHIAN STATE"}
  if (gxg_opp$`Opp 2`[i]=="CU"){gxg_opp$`Opp 2`[i]="CLEMSON"}
  if (gxg_opp$`Opp 2`[i]=="CLEM"){gxg_opp$`Opp 2`[i]="CLEMSON"}
  if (gxg_opp$`Opp 2`[i]=="DU"){gxg_opp$`Opp 2`[i]="DUKE"}
  if (gxg_opp$`Opp 2`[i]=="FS"){gxg_opp$`Opp 2`[i]="FLORIDA STATE"}
  if (gxg_opp$`Opp 2`[i]=="GT"){gxg_opp$`Opp 2`[i]="GEORGIA TECH"}
  if (gxg_opp$`Opp 2`[i]=="MD"){gxg_opp$`Opp 2`[i]="MARYLAND"}
  if (gxg_opp$`Opp 2`[i]=="NC"){gxg_opp$`Opp 2`[i]="NORTH CAROLINA"}
  if (gxg_opp$`Opp 2`[i]=="NV"){gxg_opp$`Opp 2`[i]="NAVY"}
  if (gxg_opp$`Opp 2`[i]=="ST"){gxg_opp$`Opp 2`[i]="NC STATE"}
  if (gxg_opp$`Opp 2`[i]=="VA"){gxg_opp$`Opp 2`[i]="VIRGINIA"}
  if (gxg_opp$`Opp 2`[i]=="VU"){gxg_opp$`Opp 2`[i]="VANDERBILT"}
  if (gxg_opp$`Opp 2`[i]=="BC"){gxg_opp$`Opp 2`[i]="BOSTON COLLEGE"}
  if (gxg_opp$`Opp 2`[i]=="FSU"){gxg_opp$`Opp 2`[i]="FLORIDA STATE"}
  if (gxg_opp$`Opp 2`[i]=="VANDY"){gxg_opp$`Opp 2`[i]="VANDERBILT"}
  if (gxg_opp$`Opp 2`[i]=="NC ST"){gxg_opp$`Opp 2`[i]="NC STATE"}
  if (gxg_opp$`Opp 2`[i]=="UVA"){gxg_opp$`Opp 2`[i]="VIRGINIA"}
  if (gxg_opp$`Opp 2`[i]=="NCST"){gxg_opp$`Opp 2`[i]="NC STATE"}
  if (gxg_opp$`Opp 2`[i]=="PC"){gxg_opp$`Opp 2`[i]="PRESBYTERIAN"}
  if (gxg_opp$`Opp 2`[i]=="GARD-WEBB"){gxg_opp$`Opp 2`[i]="GARDNER-WEBB"}
  if (gxg_opp$`Opp 2`[i]=="GARD-WEB"){gxg_opp$`Opp 2`[i]="GARDNER-WEBB"}
  if (gxg_opp$`Opp 2`[i]=="VT"){gxg_opp$`Opp 2`[i]="VIRGINIA TECH"}
  if (gxg_opp$`Opp 2`[i]=="VA TECH"){gxg_opp$`Opp 2`[i]="VIRGINIA TECH"}
  if (gxg_opp$`Opp 2`[i]=="SYR"){gxg_opp$`Opp 2`[i]="SYRACUSE"}
  if (gxg_opp$`Opp 2`[i]=="MISS ST"){gxg_opp$`Opp 2`[i]="MISSISSIPPI STATE"}
  if (gxg_opp$`Opp 2`[i] == "TAMU (BELK)") {gxg_opp$`Opp 2`[i]="TEXAS A&M"}
  if (gxg_opp$`Opp 2`[i] == "GA TECH") {gxg_opp$`Opp 2`[i]="GEORGIA TECH"}
  if (gxg_opp$`Opp 2`[i] == "LOUISV") {gxg_opp$`Opp 2`[i]="LOUISVILLE"}
  if (gxg_opp$`Opp 2`[i] == "UM") {gxg_opp$`Opp 2`[i]="MIAMI (FL)"}
  if (gxg_opp$`Opp 2`[i] == "MIAMI") {gxg_opp$`Opp 2`[i]="MIAMI (FL)"}
  if (gxg_opp$`Opp 2`[i] == "APP") {gxg_opp$`Opp 2`[i]="TEXAS A&M"}
  if (gxg_opp$`Opp 2`[i] == "APP") {gxg_opp$`Opp 2`[i]="APPALACHIAN STATE"}
  if (gxg_opp$`Opp 2`[i] == "APP STATE") {gxg_opp$`Opp 2`[i]="APPALACHIAN STATE"}
  if (gxg_opp$`Opp 2`[i] == "USU") {gxg_opp$`Opp 2`[i]="UTAH STATE"}
  if (gxg_opp$`Opp 2`[i] == "NC A&T") {gxg_opp$`Opp 2`[i]="NORTH CAROLINA A&T"}
  if (gxg_opp$`Opp 2`[i] == "OLE MISS") {gxg_opp$`Opp 2`[i]="MISSISSIPPI"}
  if (gxg_opp$`Opp 2`[i] == " NAVY") {gxg_opp$`Opp 2`[i]="NAVY"}
  if (gxg_opp$`Opp 2`[i] == "UTAH ST") {gxg_opp$`Opp 2`[i]="UTAH STATE"}
  
}

#seeing which ones are still not right
atopp = unique(alltimefb$Opponent2)
gxg_oppopp= unique(gxg_opp$`Opp 2`)
r1 = gxg_oppopp[which(!(gxg_oppopp%in% atopp),useNames = FALSE)]

gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Baylor2"] = "Baylor"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Va Tech"] = "Virginia Tech"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Florida St"] = "Florida State"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="ECU"] = "East Carolina"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Memphis St"] = "Memphis State"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Miami"] = "Miami (FL)"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Penn St"] = "Penn State"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="App St"] = "Appalachian State"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Kansas St"] = "Kansas State"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Georiga"] = "Georgia"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Citadel"] = "The Citadel"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="WCU"] = "Western Carolina"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Ga Tech"] = "Georgia Tech"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="VPI"] = "Virginia Tech"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="App St."] = "Appalachian State"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="App. St."] = "Appalachian State"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="NC St."] = "NC State"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Gerogia Tech"] = "Georgia Tech"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="W&M"] = "William & Mary"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="BU"] = "Boston University"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Tenn"] = "Tennessee"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Tenn."] = "Tennessee"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Boston U"] = "Boston University"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="UR"] = "Richmond"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Ill. St."] = "Illinois State"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Georiga Tech."] = "Georgia Tech"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Virgina"] = "Virginia"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="N'Western"] = "Northwestern"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Florida St."] = "Florida State"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Georiga. Tech"] = "Georgia Tech"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Georgia. Tech"] = "Georgia Tech"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Georgia Tech."] = "Georgia Tech"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Ga Tech"] = "Georgia Tech"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Arizona St"] = "Arizona State1"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Miami, Fla"] = "Miami (FL)"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="USC"] = "South Carolina"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Penn St"] = "Penn State"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Kansas St"] = "Kansas State"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Citadel "] = "The Citadel"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="NC ST "] = "NC State"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="App State "] = "Appalachian State"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Appalachian"] = "Appalachian State"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="Miss State"] = "Mississippi State"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="\\G"] = "Georgia Tech"

gxg_opp$`Opp 2`=toupper(gxg_opp$`Opp 2`)

gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="NC ST"] = "NC STATE"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="GA TECH"] = "GEORGIA TECH"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="APP STATE"] = "APPALACHIAN STATE"
gxg_opp$`Opp 2`[gxg_opp$`Opp 2`=="OLE MISS"] = "MISSISSIPPI"






for (i in 1:nrow(gxg_opp)){
  if(gxg_opp$`Opp 2`[i]==alltimefb$Opponent2[i] & gxg_opp$Yr[i]==alltimefb$Year[i]){
    gxg_opp$Day[i]=alltimefb$Day[i]
  }
  else if (length(which(alltimefb$Year==gxg_opp$Yr[i] & alltimefb$Opponent2==gxg_opp$`Opp 2`[i]))>0){
    x=0
    x=which(alltimefb$Year==gxg_opp$Yr[i] & alltimefb$Opponent2==gxg_opp$`Opp 2`[i])
    gxg_opp$Day[i]=alltimefb$Day[x]
  }
}

which(gxg_opp$Day=="")
gxg_opp$Day[c(191,200)]=c("Nov. 20","Nov. 19")

gxg1_opp <- gxg_opp %>%
  select(Day, everything())

#create comparable, relevant column names

gxg1=gxg1[,-c(3,4)]
gxg1_opp=gxg1_opp[,-c(3,4)]


names(gxg1)=c("Day","Year","Opponent","GameNumber","Wake_Result","Wake_Rushes","Wake_Rush_Yds","Wake_Rush_TD","Wake_Rush_Lg","Wake_Receptions","Wake_Receptions_Yds",
              "Wake_Receptions_TD","Wake_Receptions_Lg","Wake_Pass_Cmp","Wake_Pass_Att","Wake_Pass_Int","Wake_Pass_Yds","Wake_Pass_TD","Wake_Pass_Lg",
              "Wake_KOR","Wake_KOR_Yds","Wake_KOR_TD","Wake_KOR_Lg","Wake_PR","Wake_PR_Yds","Wake_PR_TD","Wake_PR_LG","Wake_Total_Offense","Wake_Total_Of_Plays" ,"Wake_1st_Downs",
              "Wake_1DRsh","Wake_1DPs","Wake_1DPen","Wake_Solo","Wake_Ast","Wake_Total_Tackles", "Wake_TFL","Wake_TFL_Yds","Wake_Sacks","Wake_Sacks_Yds","Wake_FF","Wake_FR",
              "Wake_Fumble_Yds","Wake_Int","Wake_Int_Yds","Wake_QBH","Wake_Brk","Wake_Blk_Kick","Wake_PAT_M","Wake_PAT_Att","Wake_Run","Wake_Rcv","Wake_safety","Wake_Pts",
              "Wake_Punts","Wake_Punts_Yds","Wake_Punts_Avg","Wake_Punts_Lg","Wake_Blkd","Wake_TB","Wake_FB","Wake_50+","Wake_I20","Wake1_FGA","Wake1_FGM",
              "Wake1_Lg","Wake1_Blkd","Wake_Ko","Wake_Ko_Yds","Wake_Ko_Avg","Wake1_TB","Wake_OB","Wake_2gm_TO","Wake_Rush_2gms","Wake_Rush_3gms"
              )
names(gxg1_opp)=c("Day","Year","Opponent","Opp_Rushes","Opp_Rushes_Yds","Opp_Rushes_TD","Opp_Rushes_Lg","Opp_Receptions","Opp_Receptions_Yds","Opp_Receptions_TD",
                  "Opp_Receptions_Lg","Opp_Pass_Cmp","Opp_Pass_Att","Opp_Pass_Int","Opp_Pass_Yds","Opp_Pass_TD","Opp_Pass_Lg","Opp_KOR","Opp_KOR_Yds","Opp_KOR_TD",
                  "Opp_KOR_LG","Opp","Opp","Opp_PR_TD","Opp_PR_LG","Opp_Total_Offense","Opp_1st_Downs","Opp_1D-Ru","Opp_1D-Pa","Opp_1D-Pn",
                  "Opp_Solo","Opp_Ast","Opp_Total_Tackles", "Opp_TFL","Opp_TFL_Yds","Opp_Sacks","Opp_Sacks_Yds","Opp_FF","Opp_FR",
                  "Opp_Fumble_Yds","Opp_Int","Opp_Int_Yds","Opp_QBH","Opp_Brk","Opp_Blk_Kick",
                  "Opp_PAT_Att","Opp_PAT_M",
                  "Opp_Run","Opp_Rcv","Opp_Safety","Opp_Pts","Opp_Punts","Opp_Punts_Yds","Opp_Punts_Avg","Opp_Punts_Lg","Opp_Blkd","Opp_TB","Opp_FC","Opp_50+",
                  "Opp_I20","Opp1_FGA","Opp1_FGM","Opp1_Lg","Opp1_Blk","Opp_Ko","Opp_Yds_10","Opp1_Avg","Opp1_TB","Opp_OB","Opp_YPC","Opp_YPA","Opp_Tot_Off_Plays","Opp_YPP"
                  )


alltimefb=alltimefb[,-c(3,4)]
x=paste("alltimefb", colnames(alltimefb), sep = "_")
x[c(1:3)]=c("Year","Day","Opponent")
names(alltimefb) = x

alltimefb1 <- alltimefb %>%
  select(Day, Year,Opponent, everything())

bigdata1 = merge(gxg1,gxg1_opp,by=c("Day","Year","Opponent"))
bd2 = merge(bigdata1,alltimefb1,by=c("Day","Year","Opponent")) 
bd2 = bd2 %>%
  arrange(GameNumber)

write.csv(bd2,file = "Team.csv")
