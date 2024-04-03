#Anthony Un - Cornell University
#Last updated March 30, 2024
#Model Creation for:
#How do Recipient Country Characteristics Impact the Efficacy of Health Aid: Evidence From the COVID-19 Pandemic

#necessary libraries and calling
#install.packages("readxl")
#install.packages("nlme")
#install.packages("car")
#install.packages("jtools")
#install.packages("huxtable")
#install.packages("officer")
#install.packages("flextable")

library(readxl)
library(nlme)
library(car)
library(jtools)
library(huxtable)
library(officer)
library(flextable)
mds2 = read_xlsx("Copy of Master Data Spreadsheet2.xlsx")

#creating models from master spreadsheet variables - all variables (Model 3)
sixmo_model = lm(sixmoup ~ vdembin + vscore + percChinese + ruralperc + unagree + unideal, data = mds2)
ninemo_model = lm(ninemoup ~ vdembin + vscore + percChinese + ruralperc + unagree + unideal, data = mds2)
twelvemo_model = lm(twelvemoup ~ vdembin + vscore + percChinese + ruralperc + unagree + unideal, data = mds2)
fifteenmo_model = lm(fiftmoup ~ vdembin + vscore + percChinese + ruralperc + unagree + unideal, data = mds2)

#checking vif values, summarizing and creating plots
vif_values <- vif(twelvemo_model)
vif_values

summary(sixmo_model)
summary(ninemo_model)
summary(twelvemo_model)
summary(fifteenmo_model)
plot(sixmo_model)
plot(ninemo_model)
plot(twelvemo_model)
plot(fifteenmo_model)


#Creating model 1
summary(lm(twelvemoup ~ percChinese+vscore, data=mds2))

#Creating model 2
summary(lm(twelvemoup ~ ruralperc+vscore+gdppc+dvaxpc+vdembin+percChinese+gdp,data=mds2))        
summary(lm(ninemoup ~ ruralperc+vscore+gdppc+dvaxpc+vdembin+percChinese+gdp, data=mds2)) 
summary(lm(sixmoup ~ ruralperc+vscore+gdppc+dvaxpc+vdembin+percChinese+gdp, data=mds2)) 

#checking model by region. Not enough observations for OCE.
for(i in unique(mds2$cregion)){
  print(i)
  print(summary(lm(twelvemoup ~ ruralperc+vscore+gdp+dvaxpc+vdembin+percChinese, data=mds2[mds2$cregion == i,])))
}

#testing gdp, gdppc and vscore variables only
summary(lm(twelvemoup ~ gdp +vscore, data=mds2))
summary(lm(ninemoup ~ gdp +vscore, data=mds2))
summary(lm(sixmoup ~ gdp +vscore, data=mds2))
vif(lm(twelvemoup ~ gdp +vscore, data=mds2))

summary(lm(twelvemoup ~ gdp +vscore+gdppc, data=mds2))
summary(lm(ninemoup ~ gdp +vscore+gdppc, data=mds2))
summary(lm(sixmoup ~ gdp +vscore+gdppc, data=mds2))
summ(lm(sixmoup ~ gdp +vscore+gdppc, data=mds2), vifs = TRUE)

#exporting regression tables
export_summs(lm(sixmoup ~ gdp +vscore+gdppc+ruralperc,data=mds2),lm(ninemoup ~ gdp +vscore+gdppc+ruralperc, data=mds2),lm(twelvemoup ~ gdp +vscore+gdppc+ruralperc, data=mds2), scale = TRUE, vifs = TRUE,model.names = c("Six Month Uptake","Nine Month Uptake","Twelve Month Uptake"), to.file="docx", file.name="table1.docx")
export_summs(lm(twelvemoup ~ vscore+percChinese, data=mds2),lm(twelvemoup ~ vscore+percChinese+gdp+gdppc+ruralperc+vdembin+dvaxpc, data=mds2),lm(twelvemoup ~ vscore+percChinese+gdp+gdppc+ruralperc+vdembin+dvaxpc+unagree, data=mds2), scale = TRUE, vifs = TRUE,model.names = c("Model 1","Model 2","Model 3"), to.file="docx", file.name="table2.docx")