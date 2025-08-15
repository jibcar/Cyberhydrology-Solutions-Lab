## Modifications

## 2024-01-14
## Copied R code from trend data prep of same nature

##############################
## Import Packages ###########
##############################
library(stringr)
library("data.table")
library(tidyverse)
library(sf)
library(viridis)
library(ggplot2)
library(ggsn)
library(dplyr)
library(stringr)

##############################
##############################

## Clear all variables
rm(list=ls())

## Clear Console
cat("\014")


variable_selected<-"MK_Combi_Trend"#"MK_Combi_Trend"#"Var_2Divide"#"KPSS_Result"

start_year='1950'
end_year='2014'

if(variable_selected=="MK_Combi_Trend"){
  variable_selected2="Trend_Result"
}else{
  variable_selected2=variable_selected
}

## Change it as required
folder_name_absolute<-"C:\\Users\\joseph57\\Box\\01_SharedBox_Jibin\\JournalDataRepo\\"

setwd(paste0(folder_name_absolute,"\\Code_R"))

## Read the shapefile as sf object``
huc2_sf<-st_read(paste0(folder_name_absolute,"/Data/Shapefiles/HUC2_modified/HUC2_modified5.shp"))
conus_sf<-st_read(paste0(folder_name_absolute,"/Data/Shapefiles/cb_2018_us_conus_5m/cb_2018_us_conus_5m.shp"))

## Plots Output Folder
folder_out<-paste0(folder_name_absolute,"\\Plots_IntermediateResults\\Plots_USGS_PF5\\")

## Historical Results

df_list<-vector("list")

## Important
## Change "18500101-20051231" in file name hist 001
## to "19200101-20051231"

sim_name<-c(rep('BHISTcmip6',40),rep('BHISTsmbb',50))
sim_num<-c(rep('1231',10),rep('1251',10),rep('1281',10),rep('1301',10),
           c('1011','1031','1051','1071','1091','1111','1131','1151','1171','1191'),
           rep('1231',10),rep('1251',10),rep('1281',10),rep('1301',10))
glade_num<-c(rep(c('001','002','003','004','005','006','007','008','009','010'),5),rep(c('011','012','013','014','015','016','017','018','019','020'),4))


for (ii in 1:90){
  #print(i)
  data<-read.csv(paste0(folder_name_absolute,"\\Data\\Data_CESM2_QRUNOFF_Hist\\A_Result_b.e21.",sim_name[ii],".f09_g17.LE2-",sim_num[ii],".",glade_num[ii],".clm2.h5.QRUNOFF.18500101-20141231.nc_max.txt"),
                 sep="\t")[ ,c('Num_Lat','Num_Lon','Latitude','Longitude',variable_selected2)]
  ## DO NOT INCLUDE SERIAL NUMBER as it is generated from MULTIPROCESSING
  
  #var_name<-paste0("data_",str_pad(ii, 3, pad = "0"))
  col_names<-c( 'Num_Lat','Num_Lon','Latitude','Longitude',paste0(variable_selected,'_',sim_num[ii],'_',glade_num[ii]))
  
  names(data)<-col_names
  
  #assign(var_name,data)
  df_list[[length(df_list)+1]]<-data
  rm(data)
  
  }

## Option1

my_merge_func <- function(df1, df2){                                # Create own merging function
  merge(df1, df2, 
        by=c( 'Num_Lat','Num_Lon','Latitude','Longitude'),all=TRUE)
}
merged_df_1<-Reduce(my_merge_func, df_list)  

## Option2
merged_df_2 <- Reduce(
  function(x, y) merge(x, y, by=c('Num_Lat','Num_Lon','Latitude','Longitude'),all=TRUE),
                       df_list)

rm(my_merge_func,ii,col_names,df_list)


#hist_result <- cbind(merged_df_1[1:4], stack(merged_df_1[5:44]))
hist_result<-merged_df_1
rm(merged_df_1,merged_df_2)


hist_result$Longitude<-hist_result$Longitude-360

## Add lat and lon separately
hist_result$Longitude2<-hist_result$Longitude
hist_result$Latitude2<-hist_result$Latitude

## Convert dataframe to sf object
hist_result_sf<-sf::st_as_sf(hist_result, 
                              coords = c("Longitude2", "Latitude2"), 
                              crs = st_crs(huc2_sf))
rm(hist_result)

colnames(hist_result_sf)
min(hist_result_sf$Longitude)
max(hist_result_sf$Longitude)
min(hist_result_sf$Latitude)
max(hist_result_sf$Latitude)


hist_result_sf<-st_intersection(hist_result_sf,huc2_sf)
sf::st_bbox(hist_result_sf)

## There will be warning as grid point outside HUC2 boundary are there
rm(huc2_sf,conus_sf)


#abc<-hist_result_sf%>%
#  filter(HUC2=="01")

result_colnames<-c()
for (ii in 1:90){
  result_colnames=append(result_colnames,paste0(variable_selected,'_',sim_num[ii],'_',glade_num[ii]))
}


data_mod<-hist_result_sf%>%
  st_drop_geometry()%>%
  select(HUC2,all_of(result_colnames))

rm(hist_result_sf)

colnames(data_mod)

cesm1_hist_df <- cbind(data_mod[1], stack(data_mod[2:91]))
#rm(data_mod)

#abc2<-cesm1_hist_df%>%
#  filter(HUC2=="01")

## 1440 = 40 ensembles with 2 (no trend and significant) for 18 HUC2 regions
## 40 x 2 x 18 = 1440
## 40 x 3 x 18 = 2160 (earlier when there was no trend, increasing, decreasing)

if(variable_selected=="MK_Combi_Trend"){
  
## Replace a value across the entire DataFrame:
## df[df == "Old Value"] <- "New Value"
cesm1_hist_df[cesm1_hist_df == "increasing"] <- "significant"
cesm1_hist_df[cesm1_hist_df == "decreasing"] <- "significant"

df_cesm1<-as.data.frame(table(cesm1_hist_df))
rm(cesm1_hist_df)

#abc3<-df_cesm1%>%
#  filter(HUC2=="01")

df2_cesm1<-df_cesm1%>%
  dplyr::rename(!!variable_selected :=values,
                Ensemble=ind)%>%
  dplyr::arrange(HUC2,Ensemble)

#abc4<-df2_cesm1%>%
#  filter(HUC2=="01")

df3_cesm1<-df2_cesm1%>%
  dplyr::group_by(Ensemble,HUC2)%>%
  dplyr::mutate(Percent=round(Freq/sum(Freq)*100,digits=2))%>%
  dplyr::arrange(HUC2,Ensemble)

#abc5<-df3_cesm1%>%
#  filter(HUC2=="01")

df3_n<-df3_cesm1%>%
  filter(Ensemble=='Ensemble001')%>%
  arrange(HUC2)

df4_cesm1<-data.table::setDT(df2_cesm1)[, Percent := round(prop.table(Freq)*100,digits=2), 
                                        key=.(Ensemble,HUC2)]

#abc6<-df4_cesm1%>%
#  filter(HUC2=="01")

df4_n<-df4_cesm1%>%
  filter(Ensemble=='Ensemble001')%>%
  arrange(HUC2)

##############################
## Comparing two dataframes ##
##############################
library(arsenal)
summary(comparedf(df3_n, df4_n))


data_cesm1_df<-df3_cesm1%>%
  ungroup()  ## Without this Ensemble column was getting added. Reason: For consistency sake the grouping variables should be always present when defined earlier and thus are added when select(value) is executed. ungroup should resolve it:
  #%>%dplyr::select(HUC2,all_of(result_colnames),Percent)

rm(df_cesm1,df2_cesm1,df3_cesm1,df3_n,
   df4_cesm1,df4_n)

data_cesm1_df['Type']="CESM2 Historical"
write.csv(data_cesm1_df,paste0("data_CESM2_2modified_",variable_selected,"_",start_year,"_",end_year,"_hist_df.csv"),row.names=FALSE)
#rm(usgs_huc2_acf_sf,df5_usgs,df6_usgs,df7_usgs,df8_usgs)

###################################
###################################
###################################

#rm(username,folder_out,data_cesm1_df)
rm(data_cesm1_df)
}


###################################
## Original Result without 
## changing decreasing and increasing
## to significant and non-significant
#################################

cesm1_hist_df <- cbind(data_mod[1], stack(data_mod[2:91]))
#rm(data_mod)

#abc2<-cesm1_futu_df%>%
#  filter(HUC2=="01")

## 1440 = 40 ensembles with 2 (no trend and significant) for 18 HUC2 regions
## 40 x 2 x 18 = 1440
## 40 x 3 x 18 = 2160 (earlier when there was no trend, increasing, decreasing)


df_cesm1<-as.data.frame(table(cesm1_hist_df))
rm(cesm1_hist_df)

df2_cesm1<-df_cesm1%>%
  rename(!!variable_selected := values,
         Ensemble=ind)%>%
  arrange(HUC2,Ensemble)

df3_cesm1<-df2_cesm1%>%
  dplyr::group_by(Ensemble,HUC2)%>%
  dplyr::mutate(Percent=round(Freq/sum(Freq)*100,digits=2))%>% 
  dplyr::arrange(HUC2,Ensemble)

df3_n<-df3_cesm1%>%
  filter(Ensemble=='Ensemble001')%>%
  arrange(HUC2)
df4_cesm1<-data.table::setDT(df2_cesm1)[, Percent := round(prop.table(Freq)*100,digits=2), 
                                        key=.(Ensemble,HUC2)]%>% 
  dplyr::arrange(HUC2,Ensemble)

df4_n<-df4_cesm1%>%
  filter(Ensemble=='Ensemble001')%>%
  arrange(HUC2)

##############################
## Comparing two dataframes ##
##############################
library(arsenal)
summary(comparedf(df3_n, df4_n))


data_cesm1_df<-df3_cesm1%>%
  ungroup() ## Without this Ensemble column was getting added. Reason: For consistency sake the grouping variables should be always present when defined earlier and thus are added when select(value) is executed. ungroup should resolve it:
#%>%dplyr::select(HUC2,all_of(variable_selected),Percent)

rm(df_cesm1,df2_cesm1,df3_cesm1,df3_n,
   df4_cesm1,df4_n)

data_cesm1_df['Type']="CESM2 Historical"

if(variable_selected=="MK_Combi_Trend")
{

write.csv(data_cesm1_df,paste0("data_CESM2_1original_",variable_selected,"_",start_year,"_",end_year,"_hist_df.csv"),row.names=FALSE)
#rm(usgs_huc2_acf_sf,df5_usgs,df6_usgs,df7_usgs,df8_usgs)
}else{
  write.csv(data_cesm1_df,paste0("data_CESM2_1original_",variable_selected,"_",start_year,"_",end_year,"_hist_df.csv"),row.names=FALSE)
}

###################################
###################################
###################################

rm(username,folder_out,data_cesm1_df)
###################################
rm(data_mod,result_colnames,variable_selected)

