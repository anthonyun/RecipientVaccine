#Anthony Un - Cornell University
#Last updated March 30, 2024
#Dataset Creation for:
#How do Recipient Country Characteristics Impact the Efficacy of Health Aid: Evidence From the COVID-19 Pandemic
#install.packages("tidyverse")
#install.packages("read.xl")

library(read.xl)
library(tidyverse)

#calculating donation data using ISO Codes
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

#rurality data
ruralityData = read.csv("ruralityData.csv")
countriesOfInterest <- c("AFG","ALB","DZA","AGO","ARG","ARM","AZE","BGD","BLR","BLZ","BEN","BTN","BOL",'BIH','BWA','BRA','BRN','BFA','BDI','CPV','KHM','CMR','CAF','TCD','COL','COM','COG','CRI','CIV','CZE','PRK','COD','DJI','DMA','DOM','ECU','EGY','SLV','GNQ','SWZ','ETH','FJI','GAB','GMB','GEO','GHA','GTM','GIN','GNB','GUY','HTI','HND','IDN','IRN','IRQ','JAM','JOR','KAZ','KEN','KIR','KGZ','LAO','LVA','LSO','LBR','LBY','MDG','MWI','MYS','MDV','MLI','MRT','MUS','MEX','MNG','MNE','MAR','MOZ','MMR','NAM','NRU','NPL','NIC','NER','NGA','MKD','OMN','PAK','PAN','PNG','PRY','PER','PHL','KOR','MDA','RWA','KNA','LCA','VCT','WSM','STP','SEN','SRB','SYC','SLE','SVK','SLB','SOM','ZAF','SSD','LKA','PSE','SDN','SYR','TJK','THA','TLS','TGO','TON','TUN','TUV','UGA','UKR','TZA','URY','UZB','VUT','VEN','VNM','YEM','ZMB','ZWE')
rurality2019 <- c()
for (x in countriesOfInterest){
  countryRural = ruralityData$X2019[ruralityData$Country.Code == x]
  rurality2019 = append(rurality2019, countryRural)
}

ruralityDataExtracted <- data.frame(countriesOfInterest, rurality2019, stringsAsFactors = FALSE)
write.csv(ruralityDataExtracted, "pathway here", row.names=FALSE)

#Extracting COW Codes used in other extractions
countriesOfInterestLong <- c('Afghanistan','Albania','Algeria','Angola','Argentina','Armenia','Azerbaijan','Bangladesh','Belarus','Belize','Benin','Bhutan','Bolivia','Bosnia and Herzegovina','Botswana','Brazil','Brunei Darussalam','Burkina Faso','Burundi','Cabo Verde','Cambodia','Cameroon','Central African Republic','Chad','Colombia','Comoros','Congo','Costa Rica',"Cote d'Ivoire",'Czechia',"Democratic People's Republic of Korea",'Democratic Republic of the Congo','Djibouti','Dominica','Dominican Republic','Ecuador','Egypt','El Salvador','Equatorial Guinea','Eswatini','Ethiopia','Fiji','Gabon','Gambia','Georgia','Ghana','Guatemala','Guinea','Guinea-Bissau','Guyana','Haiti','Honduras','Indonesia','Iran','Iraq','Jamaica','Jordan','Kazakhstan','Kenya','Kiribati','Kyrgyzstan','Lao PDR','Latvia','Lesotho','Liberia','Libya','Madagascar','Malawi','Malaysia','Maldives','Mali','Mauritania','Mauritius','Mexico','Mongolia','Montenegro','Morocco','Mozambique','Myanmar','Namibia','Nauru','Nepal','Nicaragua','Niger','Nigeria','North Macedonia','Oman','Pakistan','Panama','Papua New Guinea','Paraguay','Peru','Philippines','South Korea','Moldova','Rwanda','Saint Kitts and Nevis','Saint Lucia','Saint Vincent and the Grenadines','Samoa','Sao Tome and Principe','Senegal','Serbia','Seychelles','Sierra Leone','Slovakia','Solomon Islands','Somalia','South Africa','South Sudan','Sri Lanka','Sudan','Syria','Tajikistan','Thailand','Timor-Leste','Togo','Tonga','Tunisia','Tuvalu','Uganda','Ukraine','Tanzania','Uruguay','Uzbekistan','Vanuatu','Venezuela','Vietnam','Yemen','Zambia','Zimbabwe')
COWCodes = read.csv("COW-country-codes.csv")
COWList <- c()
for (x in countriesOfInterestLong){
  countryCOW = COWCodes$CCode[COWCodes$StateNme == x]
  COWList = append(COWList, countryCOW)
}

COWextracted <- data.frame(COWList, stringsAsFactors = FALSE)
write.csv(COWextracted, "pathway here", row.names=FALSE)

#Extracting China vote agreement data
correlatesOfWarList <- c(700,339,615,540,160,371,373,771,370,80,434,760,145,346,571,140,835,439,516,402,811,471,482,483,100,581,484,94,437,316,731,490,522,54,42,130,651,92,411,572,530,950,481,420,372,452,90,438,404,110,41,91,850,630,645,51,663,705,501,946,703,812,367,570,450,620,580,553,820,781,432,435,590,70,712,341,600,541,775,565,970,790,93,436,475,343,698,770,95,910,150,135,840,732,359,517,60,56,57,990,403,433,345,591,451,317,940,520,560,626,780,625,652,702,800,860,461,955,616,947,500,369,510,165,704,935,101,816,679,551,552)
dfAgree = read.csv("dfAgree.csv")
chinaPDList <- c()
chinaVAList <- c()
for (x in correlatesOfWarList){
  chinaPointDistance = dfAgree$IdealPointDistance[dfAgree$ccode1 == 710 & dfAgree$ccode2 == x & dfAgree$year == 2019]
  chinaVoteAgreement = dfAgree$agree[dfAgree$ccode1 == 710 & dfAgree$ccode2 == x & dfAgree$year == 2019]
  print(chinaPointDistance)
  print(chinaVoteAgreement)
  chinaPDList = append(chinaPDList, chinaPointDistance)
  chinaVAList = append(chinaVAList, chinaVoteAgreement)
}

COWextracted2 <- data.frame(chinaPDList, chinaVAList, stringsAsFactors = False)
write.csv(COWextracted2, "pathway here", row.names=FALSE)

#Creating VaxScore from Unicef Data
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
write.csv(Vaxdataextracted, "C:\\Users\\Anthony Un\\Desktop\\Datasets for Vaccine Project\\Datasets\\Vaxdataextracted.csv")

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


#histogram creation for vscore
hist(mds2$vscore, breaks = 10)

#Extracting gdp data
gdp = read_xls("gdpdata.xls")
gdplist <- c()
for(ii in unique(mds2$Iso)){
  countrygdp = gdp$'2019'[gdp$`Country Code` == ii]
  gdplist = append(gdplist, countrygdp)
}
print(gdplist)

write.csv(gdplist,"pathway here")

#extracting gdp per capita data
gdppc = read_xls("gdppcdata.xls")
gdppclist <- c()
for(ii in unique(mds2$Iso)){
  countrygdppc = gdppc$'2019'[gdppc$`Country Code` == ii]
  gdppclist = append(gdppclist, countrygdppc)
}
print(gdppclist)

write.csv(gdppclist,"pathway here")