# -*- coding: utf-8 -*-

# Revision on 2023-05-15
## Added alpha value chosen for analysis in summary output file name
## Added datetime to summary output file name
## Inside the loop, there was j==1, then break which was causing issue, now commented

# Revision on 2023-05-13
## Added alpha value chosen for analysis in output file name

# Revision on 2022-09-02
## Retrieve the p-value from Pettit Test

# Revision on 2022-09-01
## Perform a error check in date format
## Convert to date time object and extract the year
## Converting the date to year
## Pettit test function modified to determine statitically sigfnifcant change point

# Revision on 2022-08-25
## Pettit Test
## Change point detection

# Revision on 2021-10-13
## Add Hamed and Rao Modified MK test
## Rename mk_test to original_mk_test
## included two columns with result from Original and HR_modified
## Final Trend based on ACF lag 1 result

# Revision on 2021-09-15
## 1 Trying to add test for variance
## 2 Trying to add auto correlation value (lag 1)
## 3 Change the title of trend name
## 4 Add USGS Count and Calc Count
## 5 nan printed due # in "CAPE FEAR R AT LOCK #1 NR KELLY, NC"
## 6 changed delimiter="\t" to sep="\t" while reading stations file
## STATUS - 1 (levene test - 2 divide and 3 divide), 2 (both lag 1 and lag 2), 3 and 4 complete

# Revision on 202008810
## Change the condition to decide whether a station is S or NS
## Add state code
## Add lat, long to result
## Add test pass condition
## Added ANA

## To find the time taken for the script
import time
from time import sleep

from datetime import datetime
startTime = datetime.now()
print("Analysis started at :", startTime)

import sys, os
sys.path.append('/home/mygeohub/joseph57/Packages')


## Import the required Modules/Packages for obtaining the data from portal
import urllib.parse
import urllib.request

#import numpy as np
#import matplotlib.pyplot as plt
#%matplotlib inline
import pandas as pd
from statsmodels.tsa.stattools import adfuller
from statsmodels.tsa.stattools import kpss
from arch.unitroot import PhillipsPerron

import pymannkendall as mk


from scipy.stats import shapiro
from scipy.stats import normaltest
from scipy.stats import anderson

import pyhomogeneity as hg

import statsmodels.api as spi #acf calculation

from scipy.stats import levene


import shutil

import sys

if not sys.warnoptions:
    import warnings
    warnings.simplefilter("ignore")
    
import statistics #for calculating variance    
    


############################################################
## Define significance level
alpha=.05
alpha_levene=.05

## Path for Input and Output
FolderName1="C:/temp/JibinJoseph/FFA_Analysis/FFA_EntireUS/z_Input10a"
FolderName2="C:/temp/JibinJoseph/FFA_Analysis/FFA_EntireUS/z_Result10e"

#FolderName1="N:/FFA_Analysis/z_Input10a"
#FolderName2="N:/FFA_Analysis/z_Result10a"


desire_year_length=30

############################################################
## For executing the script only for a state for debugging or other purposes
"""
state_list_dict={
                 'ar':'Arkansas',#4
}

"""
state_list_dict={'al':'Alabama',#1                 
             'az':'Arizona',#3
             'ar':'Arkansas',#4
             'ca':'California',#5
             'co':'Colorado',#6
             'ct':'Connecticut',#7
             'de':'Delaware',#8
             'fl':'Florida',#9
             'ga':'Georgia',#10
             'id':'Idaho',#12
             'il':'Illinois',#13
             'in':'Indiana',#14
             'ia':'Iowa',#15
             'ks':'Kansas',#16
             'ky':'Kentucky',#17
             'la':'Louisiana',#18
             'me':'Maine',#19
             'md':'Maryland',#20
             'ma':'Massachusetts',#21
             'mi':'Michigan',#22
             'mn':'Minnesota',#23
             'ms':'Mississippi',#24
             'mo':'Missouri',#25
             'mt':'Montana',#26
             'ne':'Nebraska',#27
             'nv':'Nevada',#28
             'nh':'New Hampshire',#29
             'nj':'New Jersey',#30
             'nm':'New Mexico',#31
             'ny':'New York',#32
             'nc':'North Carolina',#33
             'nd':'North Dakota',#34
             'oh':'Ohio',#35
             'ok':'Oklahoma',#36
             'or':'Oregon',#37
             'pa':'Pennsylvania',#38
             'ri':'Rhode Island',#39
             'sc':'South Carolina',#40
             'sd':'South Dakota',#41
             'tn':'Tennessee',#42
             'tx':'Texas',#43
             'ut':'Utah',#44
             'vt':'Vermont',#45
             'va':'Virginia',#46
             'wa':'Washington',#47
             'wv':'West Virginia',#48
             'wi':'Wisconsin',#49
             'wy':'Wyoming',#50
             'ak':'Alaska',#2
             'hi':'Hawaii',#11
             'pr':'Puerto Rico',#51
             }

############################################################
#"""
"""
## Excluded states as we are dealing with Conterminous US
state_list_dict={'ak':'Alaska','hi':'Hawaii',}
"""
############################################################



## Define a function for obtaining the peak flow data from USGS Surface Data Portal


def GetAnnualPeakFlowData_f(station_number,FolderName):
    """
    Input: Station Number, Folder Name
    Output: Peak Flow Values, Station Name
    """
    ## Building URLs
    var1 = {'site_no': station_number}
    part1 = 'https://nwis.waterdata.usgs.gov/nwis/peak?'
    part2 = '&agency_cd=USGS&format=rdb'
    link = (part1 + urllib.parse.urlencode(var1) + part2)
    #print("The USGS Link is: \t")
    #print (link)

    ## Opening the link & retrieving data
    response = urllib.request.urlopen(link)
    html = response.read()

    ## Assigning the location & Storing the Original Data

    #DataStore=FolderName + station_number + ".txt"
    with open(FolderName+'/Data_' + station_number + '_raw'  + '.txt', 'wb') as f1:
        f1.write(html)
    f1.close

    ## Converts html from bytes class to str class
    html = html.decode()
    ## Splits the string by \n and converts list
    html2 = html.split('\r\n')

    #print("HTML2: ",html2)
    #print(len(html2))

    ## To get the station name
    line_no=0
    for line_no in range(len(html2)):
        ## Check if first six (use 0:7) characters is "#  USGS",
        if html2[line_no][0:7]=="#  USGS":
            usgs_station_name=html2[line_no][3:]
            break
        line_no+=1

    ## Define an empty string
    reqd_data = 'Year,Discharge'+'\n'
    #print(type(reqd_data))
    reqd_flow_list=[]
    reqd_flow_list.append(["Year","Discharge"])

    #print(type(html2))
    kk=1 ## for avoiding the last newline
    for line in html2[74:]:
        ## Splits each line to col by tab separator
        cols = line.split('\t')
        if len(cols) == 1:
            continue
        if cols[0]!='USGS': ## To remove "20d, 14n" line
            continue
        ## Joins only date and peakflow
        ## cols[2] corresponds to Date of peak streamflow (format YYYY-MM-DD)
        ## cols[4] corresponds to Annual peak streamflow value in cfs
        #print("Line: ", line)
        #print("KK: ",kk)
        if cols[4]!='':
            #print(cols[4])
            newline = ','.join([cols[2],cols[4]])
            #print("Calc",len(html2)-(74+1))
            if(kk!=(len(html2)-(74+1))):
                reqd_data += newline + '\n'
                reqd_flow_list.append((cols[4]))
            else:
                reqd_data += newline
                reqd_flow_list.append((cols[4]))
            kk=kk+1
        else:
            #print("missing:",station_number,cols[2])
            kk=kk+1
            continue


    #print(reqd_data)
    ## Converts reqd_data from str class to bytes class
    reqd_data = reqd_data.encode()
    ## Saves the date and peakflow into a new file
    with open(FolderName+'/Data_' + station_number + '_reqd'  + '.txt', 'wb') as f2:
        f2.write(reqd_data)
    f2.close
    #print ('\n')
    #print("Processed Data is stored in Results Folder for station: ",station_number)
    #print(reqd_data)

    ## Time between accessing USGS data
    #ime.sleep(0)

    ## Returns the peak flow data as list for calculation of return period
    return ()


############################################################


##Definition of stationarity tests namely: ADF,KPSS,PP
##Definition of Trend test: MK
##Definition of Normality tests: SW,Normality,AD


def ADF_test_f(timeseries):
    #from statsmodels.tsa.stattools import adfuller
    #print ('Results of ADF Test:','\n')
    #print('Null Hypothesis: Data has a unit root and is non-stationary.')
    #print('Alternative Hypothesis: Data does not have a unit root and is stationary.\n')
    #X = timeseries.values
    #adftest = adfuller(timeseries)
    adftest = adfuller(x=timeseries,maxlag=1,regression='c')
    #print('ADF Statistic: %f' % result[0])
    #print('p-value: %f' % result[1])
    #print('Critical Values:')
    #for key, value in result[4].items():
        #print('%s: %.3f' % (key, value))
    #print('\n')
    if adftest[1]<alpha:
        return("S",adftest[1])
    else:
        return("NS",adftest[1])


def KPSS_test_f(timeseries):
    #from statsmodels.tsa.stattools import kpss
    #print ('Results of KPSS Test:','\n')
    #print('Null Hypothesis: Data is level or trend stationary')
    #kpsstest = kpss(timeseries, regression='c')
    kpsstest = kpss(timeseries, regression='c',nlags=1)
    #kpss_output = pd.Series(kpsstest[0:3], index=['Test Statistic','p-value','Lags Used'])
    #for key,value in kpsstest[3].items():
        #kpss_output['Critical Value (%s)'%key] = value
    #print(kpsstest[1])
    #print(kpss_output,'\n')
    if kpsstest[1]<alpha:
        return("NS",kpsstest[1])
    else:
        return("S",kpsstest[1])

def PP_test_f(timeseries):
    #from arch.unitroot import PhillipsPerron
    #print ('Results of PP Test:','\n')
    #print('Null Hypothesis: Data has a unit root and is non-stationary.')
    #print('Alternative Hypothesis: Data does not have a unit root and is stationary.\n')

    #pptest=PhillipsPerron(timeseries,trend='c',test_type='tau')
    pptest=PhillipsPerron(timeseries,lags=1,trend='c',test_type='tau')
    # lags (int, optional) – The number of lags to use in the Newey-West estimator
    # of the long-run covariance. If omitted or None, the lag length is set
    # automatically to 12 * (nobs/100) ** (1/4)

    # trend is trend component "n" - No trend components, "c" - Include a constant (default),
    # "ct" - include a constant and linear time trend

    #test_type ({"tau", "rho"}) – The test to use when computing the test statistic.
    # "tau" is based on the t-stat and "rho" uses a test based on nobs times the re-centered regression coefficient

    #print('ADF Statistic: %f' % result[0])
    #print('p-value: %f' % result[1])
    #print('Critical Values:')
    #for key, value in result[4].items():
        #print('%s: %.3f' % (key, value))
    #print('\n')
    if pptest.pvalue<alpha:
        return("S",pptest.pvalue)
    else:
        return("NS",pptest.pvalue)

## Change point test
def Pettitt_test_f(timeseries):
    h, cp, p, U, mu = hg.pettitt_test(timeseries,alpha)   
    if p < alpha:
        return("ChangePoint",cp,p)
    else:
        return("No_ChangePoint",cp,p)

## Trend Test with Original and Hamed Rao Modified
def MK_test_f(timeseries):
    #import pymannkendall as mk
    #print ('Results of KPSS Test:','\n')
    #print('It is a nonparametric trend test which assumes no distribution of the data')
    #print('Null Hypothesis:  Data has no trend (i.e. independent and randomly ordered)')
    #print('Alternative Hypothesis:  Data has a trend.')
    #print('trend: tells the trend (increasing, decreasing or no trend')
    #print('h: True (if trend is present) or False (if the trend is absence)\n')
    original_mktest = mk.original_test(timeseries,alpha = alpha)
    hr_modified_mktest = mk.hamed_rao_modification_test(timeseries,alpha=alpha, lag=1)
    #result3 = mk.yue_wang_modification_test(timeseries)
    #print(result1[2])
    #print(result1,'\n')
    #print(result2)
    #print(result3)
    return(original_mktest[0],hr_modified_mktest[0],
           original_mktest[2],hr_modified_mktest[2],
               original_mktest[3],hr_modified_mktest[3],
               original_mktest[7],hr_modified_mktest[7])

##############

## NORMALITY TEST

# Shapiro-Wilk Test
def SW_test_f(timeseries):

    stat, p = shapiro(timeseries)
    # The Shapiro-Wilk test tests the null hypothesis that the data was drawn from a normal distribution.
    #print('Statistics=%.3f, p=%.3f' % (stat, p))
    # interpret

    if p > alpha:
        #print('Sample looks Gaussian (fail to reject H0)')
        return("Normal")
    else:
        #print('Sample does not look Gaussian (reject H0)')
        return("Non-normal")

## Normality test
def norm_test_f(timeseries):
    stat, p = normaltest((timeseries))
    #print('Statistics=%.3f, p=%.3f' % (stat, p))
    # interpret

    if p > alpha:
    	#print('Sample looks Gaussian (fail to reject H0)')
        return("Normal")
    else:
    	#print('Sample does not look Gaussian (reject H0)')
        return("Non-normal")

## Anderson-Darling Test
def AD_test_f(timeseries):
    test_dist='norm'
    result = anderson(timeseries, dist=test_dist)
    #print('Statistic: %.3f' % result.statistic)
    """
    p = 0
    for i in range(len(result.critical_values)):
    	sl, cv = result.significance_level[i], result.critical_values[i]
    	if result.statistic < result.critical_values[i]:
    		print('%.3f: %.3f, data looks "%s" (fail to reject H0)' % (sl, cv,test_dist))
    	else:
    		print('%.3f: %.3f, data does not look "%s" (reject H0)' % (sl, cv, test_dist))
    """
    #sl, cv = result.significance_level[2], result.critical_values[2]
    if result.statistic < result.critical_values[2]:
        return("Normal")
    else:
        return("Non-normal")
    
## USGS has some months and days as 00
## The below function replaces it by 01 for the further processing
def date_error_check(date_str):
    if date_str[4:8]=='-00-':
        #print("Yes: ", date_str)
        date_str_new=''.join([date_str[0:4],'-01-',date_str[8::]])
        #print(date_str_new)
    else:
        date_str_new=date_str
        
              
    if date_str[7::]=='-00':
        #print("Yes: ", date_str_new)
        date_str_new=''.join([date_str_new[0:8],'01'])
        #print('Again::::::::::::::',date_str_new)
    return(date_str_new)    


##########################################

print("Analysis started!")

## Create two folders with state code in Inputs Folder
FolderName_StateList=FolderName1+"/A_StateTextFile"
Folder_PeakFlowData=FolderName1+"/B_PeakFlowData"

## Create two folders with state code in Output Folder
Folder_StationarityResults=FolderName2+"/StationarityResults"

## Make folder to save the results
if os.path.exists(FolderName1) == False:
    os.mkdir(FolderName1)
if os.path.exists(FolderName2) == False:
    os.mkdir(FolderName2)

###########################################
"""
## Download the list of stations in each state

# Make folder to save the results
if os.path.exists(FolderName_StateList) == True:
    shutil.rmtree(FolderName_StateList)
    os.mkdir(FolderName_StateList)
else:
    os.mkdir(FolderName_StateList)


for key in state_list_dict:
    print("Station List Downloading for state: ",state_list_dict[key],"-",key.upper())
    
    ## Building URLs
    part1 = 'https://nwis.waterdata.usgs.gov/nwis/peak?state_cd='
    part2 = '&group_key=NONE&format=sitefile_output&sitefile_output_format=rdb&column_name=agency_cd&column_name=site_no&column_name=station_nm&column_name=site_tp_cd&column_name=lat_va&column_name=long_va&column_name=dec_lat_va&column_name=dec_long_va&column_name=coord_meth_cd&column_name=coord_acy_cd&column_name=coord_datum_cd&column_name=dec_coord_datum_cd&column_name=district_cd&column_name=state_cd&column_name=county_cd&column_name=country_cd&column_name=land_net_ds&column_name=map_nm&column_name=map_scale_fc&column_name=alt_va&column_name=alt_meth_cd&column_name=alt_acy_va&column_name=alt_datum_cd&column_name=huc_cd&column_name=basin_cd&column_name=topo_cd&column_name=data_types_cd&column_name=instruments_cd&column_name=construction_dt&column_name=inventory_dt&column_name=drain_area_va&column_name=contrib_drain_area_va&column_name=tz_cd&column_name=local_time_fg&column_name=reliability_cd&column_name=gw_file_cd&column_name=nat_aqfr_cd&column_name=aqfr_cd&column_name=aqfr_type_cd&column_name=well_depth_va&column_name=hole_depth_va&column_name=depth_src_cd&column_name=project_no&column_name=rt_bol&column_name=peak_begin_date&column_name=peak_end_date&column_name=peak_count_nu&column_name=qw_begin_date&column_name=qw_end_date&column_name=qw_count_nu&column_name=gw_begin_date&column_name=gw_end_date&column_name=gw_count_nu&column_name=sv_begin_date&column_name=sv_end_date&column_name=sv_count_nu&set_logscale_y=1&date_format=YYYY-MM-DD&rdb_compression=file&hn2_compression=file&list_of_search_criteria=state_cd'
    
    link = (part1 + key + part2)
    #print (link)

    ## Opening the link & retrieving data
    response = urllib.request.urlopen(link)
    html = response.read()

    ## Converts reqd_data from str class to bytes class
    reqd_data = html

    ## Saves the list of stations as per state into a new file
    with open(FolderName_StateList+'/Data_' + key.upper() + '_raw'  + '.txt', 'wb') as f2:
        f2.write(html)
    f2.close
"""
##########################################################

"""
## Downloading peak flow for stations in a state for all states

# Make folder to save the results
if os.path.exists(Folder_PeakFlowData) == True:
    shutil.rmtree(Folder_PeakFlowData)
    os.mkdir(Folder_PeakFlowData)
else:
    os.mkdir(Folder_PeakFlowData)

for key in state_list_dict:
    #print(key.upper())
    state_code=key.upper()
    #Used State Codes: "FL","GA","ND","MI",WI",SD","IA",NE","KS","OK","TX","AL","LA","MS","AR","TN","MO","KY","OH","IL","IN"

    #stations=['03275000','03303000']
    #stations=pd.read_csv('./Input2/stations_'+state_code+'.csv',converters={'site_no': lambda x: str(x)})

    #colNames = ['agency_cd', 'site_no', 'station_nm', 'dec_lat_va', 'dec_long_va','coord_acy_cd','dec_coord_datum_cd','state_cd','county_cd',
    #            'huc_cd','basin_cd','drain_area_va','peak_begin_date','peak_end_date']

    stations = pd.read_csv(FolderName_StateList+'/Data_' + key.upper() + '_raw'  + '.txt', header=0, 
                         delimiter="\t",comment='#',na_values=['Eqp'],converters={'site_no': lambda x: str(x)})
    stations.drop([0],inplace=True)
    #print(stations)
    j=0

    FolderName_PeakFlowData_Statewise=Folder_PeakFlowData+"/"+state_code+"/"
    
    if os.path.exists(FolderName_PeakFlowData_Statewise) == False:
        os.mkdir(FolderName_PeakFlowData_Statewise)

    for i in stations['site_no']:
        #print(i)
        GetAnnualPeakFlowData_f(i,FolderName_PeakFlowData_Statewise)
        #sleep(0.01)
        
    print("The streamflow data downloaded completely for all stations in state: ",state_list_dict[key],"-",key.upper())
"""
############################################################

## 

if os.path.exists(Folder_StationarityResults) == True:
    shutil.rmtree(Folder_StationarityResults)
    os.mkdir(Folder_StationarityResults)
else:
    os.mkdir(Folder_StationarityResults)

for key in state_list_dict:   #{'in':'Indiana'}:#
    #print(key.upper())
    state_code=key.upper()
    
    stations = pd.read_csv(FolderName_StateList+'/Data_' + state_code + '_raw'  + '.txt', header=0,delimiter="\t",comment='#',na_values=['Eqp'],converters={'site_no': lambda x: str(x)},lineterminator='\n')
    
    stations.drop(index=[0],inplace=True)
    stations.reset_index(inplace = True, drop = True)
 
    j=0
    #print("Current j is:", j)

    sl_num=1

    FolderName_PeakFlowData_Statewise=Folder_PeakFlowData+"/"+state_code

    #result=[]
    #result.append(["SlNo","USGS_Station","Station_Name","ADF_Result","KPSS_Result","PP_Result"])
    len_rows=len(stations)
    #print(f'No of rows in dataframe is {len_rows}')
        #iii=0
    #for iii in range(len_rows):
        #sid=stations['site_no'][iii]
        #sname=stations['station_nm'][iii]
        #print(f'iii:{iii:03d}/SID:{sid}/SName:{sname}')
    result_data = ('SlNo\tUSGS_Station\tStation_Name\tState_Name\tStation_Lat\tStation_Lon\tDrain_Area\tCountyCode\tState_Code\tHUC_Code\tBasin_Code\tPeakBeginDate\tPeakEndDate\tUSGS_Count\tCalc_Count\tACF_Lag1\tACF_Lag2\tOri_Trend\tOri_Trend_pvalue\tOri_Trend_z\tOri_Trend_slope\tHR_Mod_Trend\tHR_Mod_Trend_pvalue\tOri_Trend_z\tOri_Trend_slope\tTrend_Result\tTrend_pvalue\tTrend_z\tTrend_slope\tCP_p-value\tCP_decision\tChange_Point\tChange_Point_corr\tChange_Point_year\tADF_pvalue\tKPSS_pvalue\tPP_pvalue\tADF_Result\tKPSS_Result\tPP_Result\tStationarity_Scenario\tStationarity_MAR\tVar_2Divide\tVar_2Divide_First\tVar_2Divide_Second\tVar_2Divide_Comparison\tVar_3Divide\tVar_CPDivide\tVar_CPDivide_First\tVar_CPDivide_Second\tVar_CPDivide_Comparison\tSW_Result\tNorm_Result\tAD_Result\tNormality_Scenario\tNormality_Decision\tStationarity_AIN\tRemark'+'\n')
    #print(len(stations['site_no']))
    for j in range(len_rows):
        #print(j)
        station_id=stations['site_no'][j]
        print(station_id)
        #print("Test Result for : USGS",str(station_id),'\n',stations['station_name'][j])
        station_data_ts = pd.read_csv(FolderName_PeakFlowData_Statewise+'/Data_'+str(station_id)+'_reqd.txt') #Data_03337000_reqd.txt
        discharge_data=station_data_ts['Discharge']
        
        

        if (len(discharge_data)>=desire_year_length):
            
            station_name=stations['station_nm'][j]
            station_lat=stations['dec_lat_va'][j]
            station_lon=stations['dec_long_va'][j]
            station_drain_area=stations['drain_area_va'][j]
            station_county_cd=stations['county_cd'][j]
            station_state_cd=stations['state_cd'][j]
            station_huc_cd=stations['huc_cd'][j]
            station_basin_cd=stations['basin_cd'][j]
            station_pkbg_date=stations['peak_begin_date'][j]
            station_pkend_date=stations['peak_end_date'][j]
            station_pkcount=stations['peak_count_nu'][j]
            #print(i,": ", stations['station_nm'][j])
            #print(len(discharge_data))
            #df = pd.DataFrame(station_data_ts)
            #print(df.index)
            #datetime_series = pd.to_datetime(df['Year'])
            adf_result='ANA'
            kpss_result='ANA'
            pp_result='ANA'
            stationarity_scenario='ANA'
            stationarity_decision='ANA'
    
            original_mk_result='ANA'
            hr_modified_mk_result='ANA'
            mk_result='ANA'
    
            sw_result='ANA'
            norm_result='ANA'
            ad_result='ANA'
            normality_scenario='ANA'
            normality_decision='ANA'
    
            remark="NIL"
            
            cp_decision, cp_value,cp_p_value = Pettitt_test_f(discharge_data)
            cp_result=station_data_ts['Year'][cp_value-1]
            
            ## Perform a error check in date format
            ## Convert to date time object and extract the year
            cp_result1=date_error_check(cp_result)
            date_time_obj=datetime.strptime(cp_result1,'%Y-%m-%d')
            cp_year=date_time_obj.year
            #if j==1:
            #    break
            #ts = pd.Series(station_data_ts['Discharge'])
            ## Normalization by discharge area
            #ts_norm=pd.Series(station_data_ts['Discharge']/stations['drain_area_va'][j])
            #print("DrainArea",stations['drain_area_va'][j])
            #ts.plot(figsize=(15,5))
            #plt.xlabel("Year")
            #plt.ylabel("Peakflow (cfs)")
            #plt.title("Time Series Plot \n"+station_name)
            #plt.show()
            #plt.close()
            #print(i)
            ## Stationarity tests
            adf_result=ADF_test_f(discharge_data)
            kpss_result=KPSS_test_f(discharge_data)
            pp_result=PP_test_f(discharge_data)

            ## Normality tests
            sw_result=SW_test_f(discharge_data)
            norm_result=norm_test_f(discharge_data)
            ad_result=AD_test_f(discharge_data)
            
            ## Auto Correlation Function
            autocorr_fn,conf_int = spi.tsa.acf(discharge_data,nlags = 5,alpha=alpha,fft = False)
            lag1_acf="Check"
            i=1
            if (autocorr_fn[i]>conf_int[i][0]-autocorr_fn[i] and autocorr_fn[i]<conf_int[i][1]-autocorr_fn[i]):
                lag1_acf='insignificant'
            else:
                lag1_acf='significant'
            
            lag2_acf="Check"
            i=2
            if (autocorr_fn[i]>conf_int[i][0]-autocorr_fn[i] and autocorr_fn[i]<conf_int[i][1]-autocorr_fn[i]):
                lag2_acf='insignificant'
            else:
                lag2_acf='significant'
                
            ## Trend test
            ## Trend test
            original_mk_result, hr_modified_mk_result,original_mk_pvalue, hr_modified_mk_pvalue, original_mk_z, hr_modified_mk_z, original_mk_slope, hr_modified_mk_slope=MK_test_f(discharge_data)
            
            if lag1_acf=='insignificant':
                mk_pvalue=original_mk_pvalue
                mk_result=original_mk_result
                mk_z=original_mk_z
                mk_slope=original_mk_slope
            elif lag1_acf=='significant':
                mk_pvalue=hr_modified_mk_pvalue
                mk_result=hr_modified_mk_result
                mk_z=hr_modified_mk_z
                mk_slope=hr_modified_mk_slope
            else:
                mk_pvalue='check error'
                mk_result='check error'
                mk_z='check error'
                mk_slope='check error'
           

            #print("******* ******* *******"'\n')
            #result.append[j+1,i,station_name]
            #print(adf_result)
            #print(kpss_result)
                
            ## Variance by dividing the sample to two and three parts
            
            two_divide=int(len(discharge_data)/2)
            three_divide=int(len(discharge_data)/3)
            #print((len(discharge_data)))
            #print(two_divide,three_divide)
            
            #print(discharge_data[:two_divide])
            #print(discharge_data[two_divide:])
            
            stat_two_divide, p_two_divide = levene(discharge_data[:two_divide], discharge_data[two_divide:])
            #print(p_two_divide)
            
            if p_two_divide>=alpha_levene:
                variance_2divide='equal'
            else:
                variance_2divide='not equal'
                
            #print(variance_2divide)
            
            var_two_divide_comparison="ANA"
            
            if statistics.variance(discharge_data[two_divide:])>=statistics.variance(discharge_data[:two_divide]):
                var_two_divide_comparison="second higher than first division"
            else:
                var_two_divide_comparison="second lower than first division"
    
            ## Three divide
            
            #print(discharge_data[:three_divide])
            #print(discharge_data[three_divide:2*three_divide])
            #print(discharge_data[2*three_divide:])
            
            stat_three_divide, p_three_divide = levene(discharge_data[:three_divide],
                                                     discharge_data[three_divide:2*three_divide],
                                                     discharge_data[2*three_divide:])
            #print(p_three_divide)
            
            if p_three_divide>=alpha_levene:
                variance_3divide='equal'
            else:
                variance_3divide='not equal'
                
            #print(variance_3divide)
            
            ## Divide based on change_point
            
            #print(discharge_data[:cp_value-1])
            #print(discharge_data[cp_value-1:])
            stat_cp_divide, p_cp_divide = levene(discharge_data[:cp_value-1], discharge_data[cp_value-1:])
            #print(p_two_divide)
            
            if p_cp_divide>=alpha_levene:
                variance_cpdivide='equal'
            else:
                variance_cpdivide='not equal'
                
            #print(variance_cpdivide)
            
            var_cp_divide_comparison="ANA"
            
            if len(discharge_data[:cp_value-1])<2:
                var_cp_divide_comparison="either division do not have atleast 2 data points"
                value_first_division="not possible"
                value_second_division=statistics.variance(discharge_data[cp_value-1:])
            elif len(discharge_data[cp_value-1:])<2:
                value_second_division="not possible"
                value_first_division=statistics.variance(discharge_data[:cp_value-1])
            
            elif statistics.variance(discharge_data[:cp_value-1])>=statistics.variance(discharge_data[cp_value-1:]):
                var_cp_divide_comparison="second higher than or equal to first division"
                value_first_division=statistics.variance(discharge_data[:cp_value-1])
                value_second_division=statistics.variance(discharge_data[cp_value-1:])
            elif statistics.variance(discharge_data[:cp_value-1])<statistics.variance(discharge_data[cp_value-1:]):
                var_cp_divide_comparison="second lower than first division"
                value_first_division=statistics.variance(discharge_data[:cp_value-1])
                value_second_division=statistics.variance(discharge_data[cp_value-1:])
            else:
                var_cp_divide_comparison="some problem"
                value_first_division="some problem"
                value_second_division="some problem"
                
            print("Comparison: ",var_cp_divide_comparison)    

            # For creating different scenarios (as there are three different tests, 2^3 scenarios)
            # of normality
            if(sw_result=="Normal" and norm_result=="Normal" and ad_result=="Normal"):
                normality_scenario="N_Scenario1"
                normality_decision="Normal"

            elif((sw_result=="Non-normal" and norm_result=="Normal" and ad_result=="Normal" )):
                normality_scenario="N_Scenario2"
                normality_decision="Normal"

            elif((sw_result=="Normal" and norm_result=="Non-normal" and ad_result=="Normal" )):
                normality_scenario="N_Scenario3"
                normality_decision="Normal"

            elif((sw_result=="Non-normal" and norm_result=="Non-normal" and ad_result=="Normal")):
                normality_scenario="N_Scenario4"
                normality_decision="Non-normal"

            elif((sw_result=="Normal" and norm_result=="Normal" and ad_result=="Non-normal")):
                normality_scenario="N_Scenario5"
                normality_decision="Normal"


            elif((sw_result=="Non-normal" and norm_result=="Normal" and ad_result=="Non-normal")):
                normality_scenario="N_Scenario6"
                normality_decision="Non-normal"

            elif((sw_result=="Normal" and norm_result=="Non-normal" and ad_result=="Non-normal")):
                normality_scenario="N_Scenario7"
                normality_decision="Non-normal"

            elif((sw_result=="Non-normal" and norm_result=="Non-normal" and ad_result=="Non-normal")):
                normality_scenario="N_Scenario8"
                normality_decision="Non-normal"

            else:
                normality_scenario="check"
                normality_decision="check"

            # For creating different scenarios (as there are three different tests, 2^3 scenarios)
            # of stationarity
            if(adf_result[0]=="S" and kpss_result[0]=="S" and pp_result[0]=="S"):
                stationarity_scenario="Scenario1"
                stationarity_decision="S"

            elif((adf_result[0]=="NS" and kpss_result[0]=="S" and pp_result[0]=="S" )):
                stationarity_scenario="Scenario2"
                stationarity_decision="S"

            elif((adf_result[0]=="S" and kpss_result[0]=="NS" and pp_result[0]=="S" )):
                stationarity_scenario="Scenario3"
                stationarity_decision="S"

            elif((adf_result[0]=="NS" and kpss_result[0]=="NS" and pp_result[0]=="S")):
                stationarity_scenario="Scenario4"
                stationarity_decision="NS"

            elif((adf_result[0]=="S" and kpss_result[0]=="S" and pp_result[0]=="NS")):
                stationarity_scenario="Scenario5"
                stationarity_decision="NS"


            elif((adf_result[0]=="NS" and kpss_result[0]=="S" and pp_result[0]=="NS")):
                stationarity_scenario="Scenario6"
                stationarity_decision="NS"

            elif((adf_result[0]=="S" and kpss_result[0]=="NS" and pp_result[0]=="NS")):
                stationarity_scenario="Scenario7"
                stationarity_decision="NS"

            elif((adf_result[0]=="NS" and kpss_result[0]=="NS" and pp_result[0]=="NS")):
                stationarity_scenario="Scenario8"
                stationarity_decision="NS"

            else:
                stationarity_scenario="check"
                stationarity_decision="check"

            ## Checking Scenario 4 and 5 of Stationarity with whether data is normal

            if stationarity_scenario=="Scenario4":
                if normality_decision=="Normal":
                    combined_result="NS"
                    remark="change1"
                elif normality_decision=="Non-normal":
                    combined_result="S"
                    remark="change2"
                else:
                    combined_result="check"
                    remark="check"
            elif stationarity_scenario=="Scenario5":
                if normality_decision=="Normal":
                    combined_result="S"
                    remark="change3"
                elif normality_decision=="Non-normal":
                    combined_result="NS"
                    remark="change4"
                else:
                    combined_result="check"
                    remark="check"
            else:
                combined_result=stationarity_decision

            newline2 = '\t'.join([str(sl_num),str(station_id),station_name,state_code,str(station_lat),str(station_lon),str(station_drain_area),
                                  str(station_county_cd),str(station_state_cd),str(station_huc_cd),str(station_basin_cd),
                                  str(station_pkbg_date),str(station_pkend_date),str(station_pkcount),
                                  str(len(discharge_data)),str(lag1_acf),str(lag2_acf),
                                  original_mk_result, str(original_mk_pvalue), str(original_mk_z), str(original_mk_slope), 
                                  hr_modified_mk_result, str(hr_modified_mk_pvalue), str(hr_modified_mk_z), str(hr_modified_mk_slope),
                                  mk_result, str(mk_pvalue), str(mk_z), str(mk_slope),
                                  str(cp_p_value),cp_decision,str(cp_result),str(cp_result1),str(cp_year),
                                  str(adf_result[1]),str(kpss_result[1]),str(pp_result[1]),
                                  adf_result[0],kpss_result[0],pp_result[0],
                                  stationarity_scenario,stationarity_decision,
                                  variance_2divide, str(statistics.variance(discharge_data[:two_divide])),str(statistics.variance(discharge_data[two_divide:])),var_two_divide_comparison,
                                  variance_3divide,
                                  variance_cpdivide,str(value_first_division),str(value_second_division),var_two_divide_comparison,
                                sw_result,norm_result,ad_result,normality_scenario,normality_decision,
                                combined_result,remark])
            #print(newline2)
            if (j+1<len(stations['site_no'])):
                result_data += newline2 + '\n'
            else:
                result_data += newline2
            #print("greater than 50"," - ",i,": ", stations['station_nm'][j],station_name," - ",len(discharge_data))
            sl_num=sl_num+1      
        else:
            #print(f"Discharge Data less than {desire_year_length} years for USGS Station: {station_id}")
            #print(f""XXXXXXXXXless than 50",i," - ",": ", stations['station_nm'][j],station_name," - ",len(discharge_data))
            #Added line below
            continue
        #print("*----*----*----")
        #print(f'sl_num:{sl_num-1:03d}/j:{j+1:03d}/SID:{station_id}/SName:{station_name}')
        j=j+1
        #print("j is:" ,j)
        #if station_id=="02105769":
            #break
            

    ## Converts reqd_data from str class to bytes class
    result_data2 = result_data.encode()
    with open(Folder_StationarityResults+'/A_'+state_code+'_Result_Analysis' + '.txt', 'wb') as f2:
        f2.write(result_data2)
    f2.close
print("Trend and Stationarity Results completed \n")

del sl_num,j


###########################################################################
## Part 3
## Creates a summary file containing the states with total stations,
## analyzed stations and count in each category based on ADF, PP and KPSS results

sl_no=1

Filename_State_Summary=FolderName2 + '/A_Summary_US_Statewise_alpha_'+ str(alpha) + "_"+datetime.now().strftime("%Y%m%d-%H%M%S") + '.txt'

## Remove the file is exists as we are writing to only this file
## But, in earlier case, we have to remove entire folder otherwise, glob function will combine new and old files.
if os.path.exists(Filename_State_Summary):
    os.remove(Filename_State_Summary)

state_result_data_header = ('SlNo\tState_Name\tState_ShortCode\tTotalStations_Downloaded\tPeakCountUSGS\tStations_Analysed\tStationary_ThreePass\tStationary_TwoPass\tStationary_OnePass\tStationary_NoPass'+'\n')
with open(Filename_State_Summary, 'w') as f2:
    f2.write(state_result_data_header)
f2.close

for key in state_list_dict:   #{'in':'Indiana'}:#    
    
    #print(key)
    
    state_code=key.upper()
    print("\n",state_code)
           
    ## For getting the count of total stations for a state
    stations = pd.read_csv(FolderName_StateList+'/Data_' + state_code + '_raw'  + '.txt', header=0, 
                         delimiter="\t",comment='#',na_values=['Eqp'],converters={'site_no': lambda x: str(x)})
    
    stations.drop(index=[0],inplace=True)
    stations.reset_index(inplace = True, drop = True)
    
     #print((len(stations['site_no'])))
    
    ## Use the previously added serial number as index column by using [0]
    stations_testresults=pd.read_csv(Folder_StationarityResults+'/A_'+state_code+'_Result_Analysis' + '.txt',header=0,sep='\t',index_col=0,converters={'USGS_Station': lambda x: str(x)})
    
    ## Returns a new object with all original columns in addition to new ones. 
    ## Existing columns that are re-assigned will be overwritten.
    stations_testresults = stations_testresults.assign(Stationary=0)
    
    count_ThreePass=0
    count_TwoPass=0
    count_TwoPass_Con=0
    count_OnePass=0
    count_NoPass=0
    
    #print(stations_testresults.index)
    
    for ind in stations_testresults.index:
        ## Based on three tests, decide
        ## Three Pass if all tests yield stationarity
        ## Two Pass if any two tests yield stationarity
        ## One Pass if any one test yield stationarity
        ## No Pass if any all tests yield non-stationarity
        
        ## You can append '',' ','\t' instead of default '\n'
        #print(ind,end='\n')
                
        if(stations_testresults['ADF_Result'][ind]=="S" and
           stations_testresults['KPSS_Result'][ind]=="S" and
           stations_testresults['PP_Result'][ind]=="S"):

            stations_testresults.loc[stations_testresults.index[ind-1],'Stationary']="S-3, NS-0"
            count_ThreePass+=1

        elif((stations_testresults['ADF_Result'][ind]=="S" and stations_testresults['PP_Result'][ind]=="S" and stations_testresults['KPSS_Result'][ind]=="NS") or
             (stations_testresults['KPSS_Result'][ind]=="S" and stations_testresults['PP_Result'][ind]=="S" and stations_testresults['ADF_Result'][ind]=="NS")):

            stations_testresults.loc[stations_testresults.index[ind-1],'Stationary']="S-2, NS-1"
            count_TwoPass+=1

        elif((stations_testresults['ADF_Result'][ind]=="S" and stations_testresults['KPSS_Result'][ind]=="S" and
              stations_testresults['PP_Result'][ind]=="NS") ):

            stations_testresults.loc[stations_testresults.index[ind-1],'Stationary']="S-2_Con, NS-1_Con"
            count_TwoPass_Con+=1


        elif(stations_testresults['ADF_Result'][ind]=="S" or
             stations_testresults['KPSS_Result'][ind]=="S" or
             stations_testresults['PP_Result'][ind]=="S"):

            stations_testresults.loc[stations_testresults.index[ind-1],'Stationary']="S-1, NS-2"
            count_OnePass+=1

        elif(stations_testresults['ADF_Result'][ind]=="NS" and
             stations_testresults['KPSS_Result'][ind]=="NS" and
             stations_testresults['PP_Result'][ind]=="NS"):
            stations_testresults.loc[stations_testresults.index[ind-1],'Stationary']="S-0, NS-3"
            count_NoPass+=1

    #print(stations_testresults)
    
    #print(stations_testresults['Stationary'].value_counts().sort_index())
    
    # print("ThreePass: ", count_ThreePass,end='\\')
    # print("TwoPass: ", count_TwoPass,end='\\')
    # print("TwoPass_Con: ", count_TwoPass_Con,end='\\')
    # print("OnePass: ", count_OnePass,end='\\')
    # print("NoPass: ", count_NoPass,end='\\')
      
    #print("Total Stations analysed: ",count_ThreePass+count_TwoPass+count_TwoPass_Con+count_OnePass+count_NoPass)
    
    ## Convert peak_count_nu column from text to int
    stations['peak_count_nu'] = pd.to_numeric(stations['peak_count_nu'])        
    peak_count=sum(stations['peak_count_nu']>=desire_year_length)
    
    state_newline2 = '\t'.join([str(sl_no),state_list_dict[key],key,str(len(stations['site_no'])),str(peak_count),
                                str(count_ThreePass+count_TwoPass+count_OnePass+count_NoPass),
                                str(count_ThreePass),str(count_TwoPass),str(count_OnePass),
                                str(count_NoPass)])
    state_result_data_newline = state_newline2 + '\n'

    with open(Filename_State_Summary, 'a') as f2:
        f2.write(state_result_data_newline)
    f2.close
    #print("Result Saved: ", state_list_dict[key],"-",key.upper())
    sl_no=sl_no+1

del sl_no

############################################################

## For combining the text files containing the stationarity results into one file so that
## it can be directly used in ArcGIS for generating the maps

import glob

#read_files = glob.glob("*.txt")
read_files = glob.glob(Folder_StationarityResults+"/*.txt")

result_data='\n'
result_data2 = result_data.encode()

file_no=1

with open(FolderName2+"/B_Results_Trend_Stationarity_alpha_"+str(alpha)+"_"+datetime.now().strftime("%Y%m%d-%H%M%S")+".txt", "wb") as outfile:

    for f in read_files:
        #print(f'Adding results of state:{file_no:2d}')
        with open(f, "rb") as infile:

            if file_no!=1:
                # Skip the column names
                infile.readline()

                outfile.write(infile.read())
                outfile.write(result_data2)

            else:
                outfile.write(infile.read())
                outfile.write(result_data2)
        file_no=file_no+1

print("Combining files completed \n")
############################################################
## Final Message
print("Analysis Completed \n")

## To display the total time taken for running the script
print("Total time taken: ",datetime.now() - startTime)



    
