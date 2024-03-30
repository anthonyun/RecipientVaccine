#Anthony Un - Cornell University
#Last updated March 30, 2024
#Percent Vaccinated Dataset Creation for:
#How do Recipient Country Characteristics Impact the Efficacy of Health Aid: Evidence From the COVID-19 Pandemic
#Thank you to Matthew Kotz and John W. Woods for helping implement this code.

import pandas as pd

owidraw = pd.read_csv("owid-covid-data-copy.csv")
masterdata = pd.read_csv("Master Data Spreadsheet.csv")
isocodelist = masterdata["Iso codes"]

owid = owidraw[owidraw["iso_code"].isin(isocodelist)]

country_date_list = dict()

for isocode in isocodelist:
    country_frame = owid[owid["iso_code"] == isocode]
    rows_with_vaccinated = country_frame[country_frame["people_vaccinated_per_hundred"] > 0]
    date = rows_with_vaccinated["datevalue"].min()
    country_date_list[isocode] = date
country_date_df = pd.DataFrame.from_dict(country_date_list, orient = 'index', columns=['first_vaccinated_date'])


owidvax = owid[owid["people_vaccinated_per_hundred"].notnull()]


country_date_df['ideal6'] = country_date_df['first_vaccinated_date']+180
country_date_df['ideal9'] = country_date_df['first_vaccinated_date']+270
country_date_df['ideal12'] = country_date_df['first_vaccinated_date']+360
country_date_df['ideal15'] = country_date_df['first_vaccinated_date']+450

sixmonthlist = dict()
ninemonthlist = dict()
twelvemonthlist = dict()
fifteenmonthlist = dict()
findneighborscount = 0

#finding the closest date to the ideal dates and returning it
def find_neighbors(value, df, colname):
    global findneighborscount 
    findneighborscount = findneighborscount + 1
    exactmatch = df[df[colname] == value][colname]
    if not exactmatch.empty:
        print('exact match was not empty')
        return exactmatch.tolist()
    else:
        lowerneighbor_ind = df[df[colname] < value][colname].max()
        print('lower neighbor index is ')
        print(lowerneighbor_ind)
        upperneighbor_ind = df[df[colname] > value][colname].min()
        upperneighborvalue = df[df['datevalue'] == upperneighbor_ind][colname]
        lowerneighborvalue = df[df['datevalue'] == lowerneighbor_ind][colname]
        print('upper neighbor value is')
        print(upperneighborvalue.tolist())
        upperneighborcompare = abs(upperneighborvalue.tolist() - value)
        lowerneighborcompare = abs(lowerneighborvalue.tolist() - value)
        print('upper neighbor compare is')
        print(upperneighborcompare)
        print('lower neighbor compare is')
        print(lowerneighborcompare)
        if (upperneighborcompare > lowerneighborcompare):
            return lowerneighborvalue.tolist()
        else:
            return upperneighborvalue.tolist()
        
for isocode in country_date_df.index:
    sixmonthclosest = find_neighbors(country_date_df.loc[isocode]['ideal6'], owidvax[owidvax['iso_code'] == isocode],'datevalue')
    sixmonthlist[isocode] = sixmonthclosest
    ninemonthclosest = find_neighbors(country_date_df.loc[isocode]['ideal9'],owidvax[owidvax['iso_code'] == isocode],'datevalue')
    ninemonthlist[isocode] = ninemonthclosest
    twelvemonthclosest = find_neighbors(country_date_df.loc[isocode]['ideal12'],owidvax[owidvax['iso_code'] == isocode],'datevalue')
    twelvemonthlist[isocode] = twelvemonthclosest
    fifteenmonthclosest = find_neighbors(country_date_df.loc[isocode]['ideal15'],owidvax[owidvax['iso_code'] == isocode],'datevalue')
    fifteenmonthlist[isocode] = fifteenmonthclosest

sixmo_df = pd.DataFrame.from_dict(sixmonthlist, orient = 'index', columns=['sixmonthclosestdate'])
ninemo_df = pd.DataFrame.from_dict(ninemonthlist, orient = 'index', columns=['ninemonthclosestdate'])
twelvemo_df = pd.DataFrame.from_dict(twelvemonthlist, orient = 'index', columns=['twelvemonthclosestdate'])
fifteenmo_df = pd.DataFrame.from_dict(fifteenmonthlist, orient = 'index', columns=['fifteenmonthclosestdate'])

country_date_df = country_date_df.merge(sixmo_df, left_index=True, right_index=True)
country_date_df = country_date_df.merge(ninemo_df, left_index=True, right_index=True)
country_date_df = country_date_df.merge(twelvemo_df, left_index=True, right_index=True)
country_date_df = country_date_df.merge(fifteenmo_df, left_index=True, right_index=True)
print(country_date_df['fifteenmonthclosestdate'].to_string())

sixmonthvaxlist= dict()
ninemonthvaxlist= dict()
twelvemonthvaxlist= dict()
fifteenmonthvaxlist= dict()

for isocode in country_date_df.index:
        owidvaxcountry = owidvax[owidvax['iso_code']==isocode]
        closestdatebyisosix = country_date_df[country_date_df.index == isocode]['sixmonthclosestdate'].values[0]
        closestdatebyisonine = country_date_df[country_date_df.index == isocode]['ninemonthclosestdate'].values[0]
        closestdatebyisotwelve = country_date_df[country_date_df.index == isocode]['twelvemonthclosestdate'].values[0]
        print(isocode)
        print(closestdatebyisotwelve)
        closestdatebyisofifteen = country_date_df[country_date_df.index == isocode]['fifteenmonthclosestdate'].values[0]


        sixmonthvax = owidvaxcountry[owidvaxcountry['datevalue']==closestdatebyisosix]['people_vaccinated_per_hundred'].values[0]
        sixmonthvaxlist[isocode] = sixmonthvax
        ninemonthvax = owidvaxcountry[owidvaxcountry['datevalue']==closestdatebyisonine]['people_vaccinated_per_hundred'].values[0]
        ninemonthvaxlist[isocode] = ninemonthvax
        twelvemonthvax = owidvaxcountry[owidvaxcountry['datevalue']==closestdatebyisotwelve]['people_vaccinated_per_hundred'].values[0]
        twelvemonthvaxlist[isocode] = twelvemonthvax
        fifteenmonthvax = owidvaxcountry[owidvaxcountry['datevalue']==closestdatebyisofifteen]['people_vaccinated_per_hundred'].values[0]
        fifteenmonthvaxlist[isocode] = fifteenmonthvax

sixmovax_df = pd.DataFrame.from_dict(sixmonthvaxlist, orient = 'index', columns=['sixmonthvax'])
ninemovax_df = pd.DataFrame.from_dict(ninemonthvaxlist, orient = 'index', columns=['ninemonthvax'])
twelvemovax_df = pd.DataFrame.from_dict(twelvemonthvaxlist, orient = 'index', columns=['twelvemonthvax'])
fifteenmovax_df = pd.DataFrame.from_dict(fifteenmonthvaxlist, orient = 'index', columns=['fifteenmonthvax'])

country_date_df = country_date_df.merge(sixmovax_df, left_index=True, right_index=True)
country_date_df = country_date_df.merge(ninemovax_df, left_index=True, right_index=True)
country_date_df = country_date_df.merge(twelvemovax_df, left_index=True, right_index=True)
country_date_df = country_date_df.merge(fifteenmovax_df, left_index=True, right_index=True)
print(country_date_df)

country_date_df = pd.merge(country_date_df, ninemo_df, left_index=True, right_index=True)
country_date_df = pd.merge(country_date_df, twelvemo_df, left_index=True, right_index=True)
country_date_df = pd.merge(country_date_df, fifteenmo_df, left_index=True, right_index=True)
print(country_date_df.head())

country_date_df.to_csv('owiddataextracted.csv')