#Anthony Un - Cornell University
#Last updated July 21, 2024
#Dataset Creation and Visualization for:
#How do Recipient Country Characteristics Impact the Efficacy of Health Aid: Evidence From the COVID-19 Pandemic

#install.packages("tidyverse")
#install.packages("read.xl")

library(read.xl)
library(tidyverse)

#Calculating vaccine donation data using ISO Codes####################################
donationdata = read.csv("Vaccine Donation Data.csv")
countriesOfInterest <- c("AFG","ALB","DZA","AGO","ARG","ARM","AZE","BGD","BLR","BLZ","BEN","BTN","BOL",'BIH','BWA','BRA','BRN','BFA','BDI','CPV','KHM','CMR','CAF','TCD','COL','COM','COG','COK','CRI','CIV','CZE','PRK','COD','DJI','DMA','DOM','ECU','EGY','SLV','GNQ','SWZ','ETH','FJI','GAB','GMB','GEO','GHA','GTM','GIN','GNB','GUY','HTI','HND','IND','IDN','IRN','IRQ','JAM','JOR','KAZ','KEN','KIR','KGZ','LAO','LVA','LSO','LBR','LBY','MDG','MWI','MYS','MDV','MLI','MRT','MUS','MEX','MNG','MNE','MAR','MOZ','MMR','NAM','NRU','NPL','NIC','NER','NGA','MKD','OMN','PAK','PAN','PNG','PRY','PER','PHL','KOR','MDA','RWA','KNA','LCA','VCT','WSM','STP','SEN','SRB','SYC','SLE','SVK','SLB','SOM','ZAF','SSD','LKA','PSE','SDN','SYR','TJK','THA','TLS','TGO','TON','TUN','TUV','UGA','UKR','TZA','URY','UZB','VUT','VEN','VNM','YEM','ZMB','ZWE')
donatedChina <- c()
donatedTotal <-c()
percentChinaDonated <- c()
for (x in countriesOfInterest){
  
  countrydonatedChinaNC = (gsub(',','',donationdata$Donation.Amount[donationdata$Receiving.Country.Three.Letter.Code == x & donationdata$Donating.Entity == "China"]))
  countrydonatedChinaNC = append(countrydonatedChinaNC, gsub(',','',donationdata$Donation.Amount[donationdata$Receiving.Country.Three.Letter.Code == x & donationdata$Donating.Entity == "Sinovac"]))
  countrydonatedChinaNC = append(countrydonatedChinaNC, gsub(',','',donationdata$Donation.Amount[donationdata$Receiving.Country.Three.Letter.Code == x & donationdata$Donating.Entity == "Sinopharm"]))
  countrychinaSum = sum(as.numeric(countrydonatedChinaNC))       
  countryDonateListNC = gsub(',','',donationdata$Donation.Amount[donationdata$Receiving.Country.Three.Letter.Code == x])
  countryDonateSum = sum(as.numeric(countryDonateListNC))
  countryNonChinaSum = (countryDonateSum - countrychinaSum)
  
  donatedChina = append(donatedChina, countrychinaSum)
  donatedTotal = append(donatedTotal, countryDonateSum)
  print (donatedChina)
}

percentChinaDonated = (donatedChina/donatedTotal)
print(donatedChina)
print(percentChinaDonated)

donationDataExtracted <- data.frame(countriesOfInterest, donatedChina, donatedTotal, percentChinaDonated, stringsAsFactors = FALSE)
write.csv(donationDataExtracted, "pathway here", row.names=FALSE)

#######Extracting rurality data###################
ruralityData = read.csv("ruralityData.csv")
countriesOfInterest <- c("AFG","ALB","DZA","AGO","ARG","ARM","AZE","BGD","BLR","BLZ","BEN","BTN","BOL",'BIH','BWA','BRA','BRN','BFA','BDI','CPV','KHM','CMR','CAF','TCD','COL','COM','COG','CRI','CIV','CZE','PRK','COD','DJI','DMA','DOM','ECU','EGY','SLV','GNQ','SWZ','ETH','FJI','GAB','GMB','GEO','GHA','GTM','GIN','GNB','GUY','HTI','HND','IDN','IRN','IRQ','JAM','JOR','KAZ','KEN','KIR','KGZ','LAO','LVA','LSO','LBR','LBY','MDG','MWI','MYS','MDV','MLI','MRT','MUS','MEX','MNG','MNE','MAR','MOZ','MMR','NAM','NRU','NPL','NIC','NER','NGA','MKD','OMN','PAK','PAN','PNG','PRY','PER','PHL','KOR','MDA','RWA','KNA','LCA','VCT','WSM','STP','SEN','SRB','SYC','SLE','SVK','SLB','SOM','ZAF','SSD','LKA','PSE','SDN','SYR','TJK','THA','TLS','TGO','TON','TUN','TUV','UGA','UKR','TZA','URY','UZB','VUT','VEN','VNM','YEM','ZMB','ZWE')
rurality2019 <- c()
for (x in countriesOfInterest){
  countryRural = ruralityData$X2019[ruralityData$Country.Code == x]
  rurality2019 = append(rurality2019, countryRural)
}

ruralityDataExtracted <- data.frame(countriesOfInterest, rurality2019, stringsAsFactors = FALSE)
write.csv(ruralityDataExtracted, "pathway here", row.names=FALSE)

#####Creating VaxScore from Unicef Data###############################
BCG = read_excel('Unicef_Vaccination_Data.xlsx', sheet = "BCG")
DTP1 = read_excel('Unicef_Vaccination_Data.xlsx', sheet = "DTP1")
DTP3 = read_excel('Unicef_Vaccination_Data.xlsx', sheet = "DTP3")
HEPB3 = read_excel('Unicef_Vaccination_Data.xlsx', sheet = "HEPB3")
HEPBB = read_excel('Unicef_Vaccination_Data.xlsx', sheet = "HEPBB")
HIB3 = read_excel('Unicef_Vaccination_Data.xlsx', sheet = "HIB3")
IPV1 = read_excel('Unicef_Vaccination_Data.xlsx', sheet = "IPV1")
MCV1 = read_excel('Unicef_Vaccination_Data.xlsx', sheet = "MCV1")
MCV2 = read_excel('Unicef_Vaccination_Data.xlsx', sheet = "MCV2")

bcgList = c()
dtp1List = c()
dtp3List = c()
hepb3List = c()
hepbbList = c()
hib3List = c()
ipv1List = c()
mcv1List = c()
mcv2List = c()

for (x in countriesOfInterest){
  bcgtemp = BCG$'2019'[BCG$iso3 == x]
  dtp1temp = DTP1$'2019'[DTP1$iso3 == x]
  dtp3temp = DTP3$'2019'[DTP3$iso3 == x]
  hepb3temp = HEPB3$'2019'[HEPB3$iso3 == x]
  hib3temp = HIB3$'2019'[HIB3$iso3 == x]
  ipv1temp = IPV1$'2019'[IPV1$iso3 == x]
  mcv1temp = MCV1$'2019'[MCV1$iso3 == x]
  
  bcgList = append(bcgList, bcgtemp)
  dtp1List = append(dtp1List, dtp1temp)
  dtp3List = append(dtp3List, dtp3temp)
  hepb3List = append(hepb3List, hepb3temp)
  hib3List = append(hib3List, hib3temp)
  ipv1List = append(ipv1List, ipv1temp)
  mcv1List = append(mcv1List, mcv1temp)
}

Vaxdataextracted = data.frame(countriesOfInterest, bcgList, dtp1List, dtp3List, hepb3List, hib3List, ipv1List, mcv1List, bcgDist, dtp1Dist, dtp3Dist, hepb3Dist, hib3Dist, ipv1Dist, mcv1Dist)
write.csv(Vaxdataextracted, "pathway here")

bcgMean = mean(Vaxdataextracted$bcgList, na.rm = TRUE)
dtp1Mean = mean(Vaxdataextracted$dtp1List, na.rm = TRUE)
dtp3Mean = mean(Vaxdataextracted$dtp3List, na.rm = TRUE)
hepb3Mean = mean(Vaxdataextracted$hepb3List, na.rm = TRUE)
hib3Mean = mean(Vaxdataextracted$hib3List, na.rm = TRUE)
ipv1Mean = mean(Vaxdataextracted$ipv1List, na.rm = TRUE)
mcv1Mean = mean(Vaxdataextracted$mcv1List, na.rm = TRUE)

bcgDist = Vaxdataextracted$bcgList - bcgMean
dtp1Dist = Vaxdataextracted$dtp1List - dtp1Mean
dtp3Dist = Vaxdataextracted$dtp3List - dtp3Mean
hepb3Dist = Vaxdataextracted$hepb3List - hepb3Mean
hib3Dist = Vaxdataextracted$hib3List - hib3Mean
ipv1Dist = Vaxdataextracted$ipv1List - ipv1Mean
mcv1Dist = Vaxdataextracted$mcv1List - mcv1Mean

for (x in countriesOfInterest){
  vaxScore = mean(Vaxdataextracted$bcgDist[Vaxdataextracted$countriesOfInterest == x], na.rm = TRUE)
}

######Extracting gdp data################
gdp = read_xls("gdpdata.xls")
gdplist <- c()
for(ii in unique(mds2$Iso)){
  countrygdp = gdp$'2019'[gdp$`Country Code` == ii]
  gdplist = append(gdplist, countrygdp)
}
print(gdplist)

write.csv(gdplist,"pathway here")

##########extracting gdp per capita data################
gdppc = read_xls("gdppcdata.xls")
gdppclist <- c()
for(ii in unique(mds2$Iso)){
  countrygdppc = gdppc$'2019'[gdppc$`Country Code` == ii]
  gdppclist = append(gdppclist, countrygdppc)
}
print(gdppclist)

write.csv(gdppclist,"pathway here")

vaxleg = read.csv("vaxlegdata.csv")
View(vaxleg)

#Gathering oxford vaccination policy variables and aid variable to calculate into log aid. 

vaxprio12 <- c()
vaxavail12 <- c()
vaxfin12 <- c()
vaxman12 <- c()

for(x in countriesofInterestShorter){
  tempdate = mds2$`Date of 12 Month Data (dd-mm-yy)`[mds2$Iso == x]
  print(x)
  print(vaxleg$vaxprio[vaxleg$CountryCode==x & vaxleg$Datevalue == tempdate])
  vaxprio12 = append(vaxprio12, vaxleg$vaxprio[vaxleg$CountryCode==x & vaxleg$Datevalue == tempdate])
}

for(x in countriesofInterestShorter){
  tempdate = mds2$`Date of 12 Month Data (dd-mm-yy)`[mds2$Iso == x]
  print(x)
  print(vaxleg$vaxavail[vaxleg$CountryCode==x & vaxleg$Datevalue == tempdate])
  vaxavail12 = append(vaxavail12, vaxleg$vaxavail[vaxleg$CountryCode==x & vaxleg$Datevalue == tempdate])
}

for(x in countriesofInterestShorter){
  tempdate = mds2$`Date of 12 Month Data (dd-mm-yy)`[mds2$Iso == x]
  print(x)
  print(vaxleg$vaxfin[vaxleg$CountryCode==x & vaxleg$Datevalue == tempdate])
  vaxfin12 = append(vaxfin12, vaxleg$vaxfin[vaxleg$CountryCode==x & vaxleg$Datevalue == tempdate])
}

for(x in countriesofInterestShorter){
  tempdate = mds2$`Date of 12 Month Data (dd-mm-yy)`[mds2$Iso == x]
  print(x)
  print(vaxleg$vaxman[vaxleg$CountryCode==x & vaxleg$Datevalue == tempdate])
  vaxman12 = append(vaxman12, vaxleg$vaxman[vaxleg$CountryCode==x & vaxleg$Datevalue == tempdate])
}

vaxgov = cbind(countriesofInterestShorter, vaxprio12, vaxavail12, vaxfin12, vaxman12)
vaxgovdf = as.data.frame(vaxgov)
write.csv(vaxgovdf, "vaxgovextracted.csv")

aid = read.csv("oecd.csv")
totalaid20 <- c()
for(x in countriesofInterestShort){
  totalaid20 = append(totalaid20, aid$X2020[aid$X == x])
}

######################Repeating for 9 month data#################################

vaxprio9 <- c()
vaxavail9 <- c()
vaxfin9 <- c()
vaxman9 <- c()

for(x in countriesofInterestShorter){
  tempdate = mds2$`Date of 9 Month Data (dd-mm-yy)`[mds2$Iso == x]
  print(x)
  print(vaxleg$vaxprio[vaxleg$CountryCode==x & vaxleg$Datevalue == tempdate])
  vaxprio9 = append(vaxprio9, vaxleg$vaxprio[vaxleg$CountryCode==x & vaxleg$Datevalue == tempdate])
}

for(x in countriesofInterestShorter){
  tempdate = mds2$`Date of 9 Month Data (dd-mm-yy)`[mds2$Iso == x]
  print(x)
  print(vaxleg$vaxavail[vaxleg$CountryCode==x & vaxleg$Datevalue == tempdate])
  vaxavail9 = append(vaxavail9, vaxleg$vaxavail[vaxleg$CountryCode==x & vaxleg$Datevalue == tempdate])
}

for(x in countriesofInterestShorter){
  tempdate = mds2$`Date of 9 Month Data (dd-mm-yy)`[mds2$Iso == x]
  print(x)
  print(vaxleg$vaxfin[vaxleg$CountryCode==x & vaxleg$Datevalue == tempdate])
  vaxfin9 = append(vaxfin9, vaxleg$vaxfin[vaxleg$CountryCode==x & vaxleg$Datevalue == tempdate])
}

for(x in countriesofInterestShorter){
  tempdate = mds2$`Date of 9 Month Data (dd-mm-yy)`[mds2$Iso == x]
  print(x)
  print(vaxleg$vaxman[vaxleg$CountryCode==x & vaxleg$Datevalue == tempdate])
  vaxman9 = append(vaxman9, vaxleg$vaxman[vaxleg$CountryCode==x & vaxleg$Datevalue == tempdate])
}

vaxgov9 = cbind(countriesofInterestShorter, vaxprio9, vaxavail9, vaxfin9, vaxman9)
vaxgovdf9 = as.data.frame(vaxgov9)
write.csv(vaxgovdf9, "vaxgovextracted9.csv")
############################Repeating for 6 months data ######################################################################
vaxprio6 <- c()
vaxavail6 <- c()
vaxfin6 <- c()
vaxman6 <- c()

for(x in countriesofInterestShorter){
  tempdate = mds2$`Date of 6 Month Data (dd-mm-yy)`[mds2$Iso == x]
  print(x)
  print(vaxleg$vaxprio[vaxleg$CountryCode==x & vaxleg$Datevalue == tempdate])
  vaxprio6 = append(vaxprio6, vaxleg$vaxprio[vaxleg$CountryCode==x & vaxleg$Datevalue == tempdate])
}

for(x in countriesofInterestShorter){
  tempdate = mds2$`Date of 6 Month Data (dd-mm-yy)`[mds2$Iso == x]
  print(x)
  print(vaxleg$vaxavail[vaxleg$CountryCode==x & vaxleg$Datevalue == tempdate])
  vaxavail6 = append(vaxavail6, vaxleg$vaxavail[vaxleg$CountryCode==x & vaxleg$Datevalue == tempdate])
}

for(x in countriesofInterestShorter){
  tempdate = mds2$`Date of 6 Month Data (dd-mm-yy)`[mds2$Iso == x]
  print(x)
  print(vaxleg$vaxfin[vaxleg$CountryCode==x & vaxleg$Datevalue == tempdate])
  vaxfin6 = append(vaxfin6, vaxleg$vaxfin[vaxleg$CountryCode==x & vaxleg$Datevalue == tempdate])
}

for(x in countriesofInterestShorter){
  tempdate = mds2$`Date of 6 Month Data (dd-mm-yy)`[mds2$Iso == x]
  print(x)
  print(vaxleg$vaxman[vaxleg$CountryCode==x & vaxleg$Datevalue == tempdate])
  vaxman6 = append(vaxman6, vaxleg$vaxman[vaxleg$CountryCode==x & vaxleg$Datevalue == tempdate])
}

vaxgov6 = cbind(countriesofInterestShorter, vaxprio6, vaxavail6, vaxfin6, vaxman6)
vaxgovdf6 = as.data.frame(vaxgov6)
write.csv(vaxgovdf6, "vaxgovextracted6.csv")