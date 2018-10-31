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

#now do the above for opp
gxg_opp = read_xls("Game by Game stats-Nick.xls",sheet="Opp")
gxg_opp$Day = ""
gxg_opp$`Opp 2`=toupper(gxg_opp$`Opp 2`)

#get to work after 3rd observation

for (i in 1:nrow(gxg_opp)){
  if(gxg_opp$`Opp 2`[i]==gxg1$`Opp 2`[i] & gxg_opp$Yr[i]==gxg1$Year[i]){
    gxg_opp$Day[i]=gxg1$Day[i]
  }
  else if (length(which(gxg1$Year==gxg_opp$Yr[i] & gxg1$`Opp 2`==gxg_opp$`Opp 2`[i]))>0) {
    x=0
    x=which(gxg1$Year==gxg_opp$Yr[i] & gxg1$`Opp 2`==gxg_opp$`Opp 2`[i])
    gxg_opp$Day[i]==gxg1$Day[x]
    
    }
}


