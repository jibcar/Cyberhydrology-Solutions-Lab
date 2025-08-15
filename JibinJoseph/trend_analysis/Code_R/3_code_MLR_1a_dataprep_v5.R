## Modifications

## 2024-08-16
## added year start and end to plot output and data output


## 2024-03-08
## Multiply ws_mean_precip by 365
## 2024-01-08



## Added CONUS level results to drivers of peak flow.


## 2023-06-27
## Comparison between USGS Results and USGS Shapefile

#July 27 - ACF_lag1 and Var_2Divide will also be stored into cart input data

library(sf)
library(ggplot2)
library(ggsn)
library(dplyr)
library(tmap)
## https://www.statology.org/summary-table-in-r/
#install.packages("psych")
library(psych)
library(MLCM)
library(units)

## for natural jenks function

library(stringr)
library(stringi)
library(ggplot2)
library(viridis)
library(data.table)
library(BAMMtools) # fast calculation of jenks natural breaks


########################
########################

## Clear all variables
rm(list=ls())

## Clear Console
cat("\014")

year_dam_start="all"
year_dam_end="all"

year_usgs_start="all"
year_usgs_end="all"

## Change it as required
folder_name_absolute<-"C:\\Users\\joseph57\\Box\\01_SharedBox_Jibin\\JournalDataRepo\\"

setwd(paste0(folder_name_absolute,"\\Code_R"))

if (year_usgs_start=="all" | year_usgs_end=="all"){
  
usgs_flow_file_name<-paste0("./USGS_Results/B_Results_Trend_Stationarity_",
       year_usgs_start,
       "_",
       year_usgs_end,
       "_alpha_0.05.txt")
}else if(year_usgs_start!="all" | year_usgs_end!="all"){
  usgs_flow_file_name<-paste0("./USGS_Results/B_Results_Trend_Stationarity_",
                              year_usgs_start,
                              "_",
                              year_usgs_end,
                              "_missing_5percent_alpha_0.05.txt")
  
}



##  Direction to Output Folder and also save the path the R code
folder_plot_out<-paste0(folder_name_absolute,"\\Plots_IntermediateResults\\plots_multiple_regression_v1_",
                        year_usgs_start,"_",
                        year_usgs_end,
                        "\\")
dir.create(folder_plot_out, showWarnings = TRUE)


folder_data_out<-paste0(folder_name_absolute,
                        "\\Data_Intermediate\\data_multiple_regression_v1_",
                        year_usgs_start,"_",
                        year_usgs_end,
                        "\\")
dir.create(folder_data_out, showWarnings = TRUE)

fileConn<-file(paste0(folder_data_out,"00_code_full_path.txt"))
writeLines(rstudioapi::getSourceEditorContext()$path, 
           fileConn)
close(fileConn)
rm(fileConn)

usgs_watersheds<-st_read(paste0(folder_name_absolute,"\\Data_Intermediate\\MLR_Data\\watersheds8_3890.gpkg"))

summary(usgs_watersheds$wsavg_mean_annual_total)

st_crs(usgs_watersheds)

usgs_watersheds$DA_sqkm<-units::set_units(st_area(usgs_watersheds), km^2)
usgs_watersheds$DA_sqmi <- units::set_units(st_area(usgs_watersheds), mi^2)

usgs_watersheds_df<-usgs_watersheds%>%sf::st_drop_geometry()

usgs_watersheds_df%>%
  filter(USGS_St=="01076500")


class(usgs_watersheds_df)


nid_maxstorage_cat<-read.csv(paste0(folder_name_absolute,"\\Results\\results_nid_dams\\",
                                    "data_max_storage_catchment_conus_",
                                    year_dam_start,
                                    "_",
                                    year_dam_end,
                                    ".csv"),
                              colClasses = c("USGS_St" = "character"))



usgs_watersheds_df$DA_sqkm<-units::drop_units(usgs_watersheds_df$DA_sqkm)

usgs_watersheds_df<-merge(usgs_watersheds_df,nid_maxstorage_cat,
                          by="USGS_St",
                          all.x=TRUE)


library(units)

sta_700 <- usgs_watersheds_df %>%
  filter(DA_sqmi > set_units(650, "mi^2") & DA_sqmi < set_units(750, "mi^2"))


## Replace NA with 0 in column 'MaxStorage' as some watersheds may not have any dam
usgs_watersheds_df$MaxStorage[is.na(usgs_watersheds_df$MaxStorage)] <- 0

usgs_watersheds_df$norm_max_stor<-as.numeric(usgs_watersheds_df$MaxStorage*1233.48)/((usgs_watersheds_df$wsavg_mean_annual_total/1000)*(usgs_watersheds_df$DA_sqkm * 1000000))

summary(usgs_watersheds_df$norm_max_stor*100)




quantile(usgs_watersheds_df$norm_max_stor*100)
quantile(usgs_watersheds_df$norm_max_stor*100,na.rm = T,probs = c(0.85,0.95))

rm(nid_maxstorage_cat)
#rm(nid_maxstorage2_cat,nid_maxstorage3_cat,nid_maxstorage4_cat)
#rm(noaa_meanprecip_cat)
#rm(usgs_sites_df)

#nid_normstor_cat$norm_max_stor_old<-nid_normstor_cat$norm_max_stor
#nid_normstor_cat$norm_max_stor<-units::drop_units(nid_normstor_cat$norm_max_stor)

#write.csv(nid_normstor_cat,paste0(folder_data_out,"nid_normstor_cat.csv"),row.names=FALSE)


## Read the shapefile as sf object``
huc2_sf<-st_read(paste0(folder_name_absolute,"/Data/Shapefiles/HUC2_modified/HUC2_modified5.shp"))
huc2_sf_code_name<-huc2_sf%>%
  st_drop_geometry()%>%
  select(HUC2,NAME)

rownames(huc2_sf_code_name) <- huc2_sf_code_name$HUC2

usgs_sites_sf<-usgs_watersheds

usgs_sites_sf<-usgs_sites_sf%>%
  select(USGS_St)
#usgs_sites1_sf<-st_transform(usgs_sites_sf,st_crs(huc2_sf))
#rm(usgs_sites_sf)

st_crs(huc2_sf)==st_crs(usgs_watersheds)

#huc2_mod_sf<-st_transform(huc2_sf,st_crs(usgs_watersheds))

#huc2_mod_sf <- st_transform(huc2_sf, crs = 4326)

# sf_use_s2(FALSE)
# usgs_sites1_sf<-st_make_valid(usgs_sites1_sf)
# #st_write(usgs_sites2_sf,paste0(folder_data_out,"usgs_gages_sites_valid.shp"))
# sf_use_s2(TRUE)
## Check whether valid geometry is fine
#st_is_valid(usgs_sites2_sf,reason=T)

#usgs_sites2_sf<-usgs_sites1_sf
usgs_sites2_sf<-usgs_sites_sf

#rm(usgs_sites1_sf)
rm(usgs_sites_sf)

######################################
## Land Use data######################
######################################



lu_data_raw<-st_read(paste0(folder_name_absolute,"/Data_Intermediate/LULC_data/nlcd_2019_usgs_sites_3890.shp"))

colnames(lu_data_raw)

lu_data<-lu_data_raw%>%
  dplyr::select(USGS_St,LU2019_For,LU2019_Agr,LU2019_Urb)#,CLASS)

colnames(lu_data_raw)

lu_data<-rename(lu_data, 
                USGS_SiteNo = USGS_St
                #,USGS_SiteName=Sttn_Nm
                #,State_Name=Stat_Nm
                )
rm(lu_data_raw)

# regression_input_data<-merge(lu_data,trial4_max_,
#                              by="USGS_SiteNo",
#                              all.x=TRUE)
# 
# regression_input_data2<-merge(regression_input_data,trial4_mean_,
#                               by="USGS_SiteNo",
#                               all.y=TRUE)
# rm(trial4_max_,trial4_mean_)
# #rm(lu_data)   
# rm(regression_input_data)
# 
# nid_normstor_cat<-nid_normstor_cat%>%
#   rename(USGS_SiteNo=USGS_St)
# 
# regression_input_data3<-merge(regression_input_data2,nid_normstor_cat,
#                               by="USGS_SiteNo",
#                               all.y=TRUE)

regression_input_data3<-merge(usgs_watersheds_df,lu_data%>%st_drop_geometry(),
                              by.x="USGS_St",
                              by.y="USGS_SiteNo",
                              all.x=TRUE)

#rm(regression_input_data2,nid_normstor_cat)


####################################
## Read NEW 
## trend-stationary results####
####################################

alpha_value="0.05"
years_count=60


usgs_results_30<-read.csv(usgs_flow_file_name,
                          
                          sep='\t',
                          colClasses = c("USGS_Station" = "character"))
colnames(usgs_results_30)


## Filter for conterminous US

usgs_results_cont<- usgs_results_30 %>% 
  filter(!(State_Name %in% c("AK","HI","PR")))

#usgs_results_cont<- usgs_results_30 %>% 
#filter(State_Name %in% c("AL","AZ","AR","CA","CO","CT","DE","FL","GA","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"))

if (year_usgs_start=="all" | year_usgs_end=="all"){
  usgs_results<-filter(usgs_results_cont,Calc_Count>=years_count)
}else{
  usgs_results<-usgs_results_cont
}

rm(usgs_results_cont,usgs_results_30)

usgs_results2<-usgs_results%>%
  select(USGS_Station,Station_Lat,Station_Lon,MK_Combi_Trend,MK_Combi_slope,StdDev,State_Name)

# write.csv(usgs_results2%>%select(USGS_Station,MK_Combi_slope,StdDev)%>%arrange(USGS_Station),
#           paste0(folder_data_out,"all2_all2.csv"),
#           row.names = F)

# missing_shapefiles<-usgs_results2%>%
#   filter(!(USGS_Station%in%usgs_sites_df$USGS_St))%>%
#   arrange(desc("USGS_station"))

###############################################

## For checking the downloaded shapefile ######

###############################################

# usgs_shapefile<-sf::st_read(folder_name_absolute,"\\Data\\Shapefiles\\landuse_analysis\\FFA_usgs_polygons.shp")
# 
# ## Remove First Row from Data Frame in R
# ## As 03335500 is a repetition
# usgs_shapefile <- usgs_shapefile %>% 
#   dplyr::slice(-1)
# 
# ## Calculate the area
# #usgs_shapefile$calc_area <- st_area(usgs_shapefile)
# usgs_shapefile$calc_area_sqm <- st_area(usgs_shapefile)
# usgs_shapefile$calc_area_sqkm <- units::set_units(st_area(usgs_shapefile), km^2)
# usgs_shapefile$calc_area_sqmi <- units::set_units(st_area(usgs_shapefile), mi^2)
# 
# usgs_shapefile_df<-usgs_shapefile%>%
#   st_drop_geometry()
# 
# ## For df1 values not in df2
# a<-usgs_results %>% 
#   filter(!usgs_results$USGS_Station %in% usgs_shapefile_df$identifier)
# 
# usgs_results_comparison<-merge(usgs_results,usgs_shapefile_df,
#                               by.x="USGS_Station",
#                               by.y="identifier",
#                               all=TRUE)
# 
# usgs_results_comparison2<-usgs_results_comparison%>%
#   select(USGS_Station,Station_Name,Drain_Area,calc_area_sqmi)
# 
# ## Compare two columns and print non matching values in each dataframe
# #df1 %>% 
# #  filter(!df1$ID1 %in% df2$ID2) #For df1 values not in df2
# #df2 %>% 
# #  filter(!df2$ID2 %in% df1$ID1) #For df2 values not in df1
# 
# b<-usgs_results_comparison2 %>% 
#   filter(!usgs_results_comparison2$USGS_Station %in% regression_input_data3$USGS_SiteNo) #For df1 values not in df2
# c<-usgs_results2 %>% 
#   filter(!usgs_results2$USGS_Station 
#          %in% regression_input_data3$USGS_SiteNo)
# c1<-c %>% 
#   filter(c$USGS_Station %in% usgs_shapefile_df$identifier)
# c2<-usgs_shapefile %>%
#   filter(usgs_shapefile$identifier %in% c1$USGS_Station)
# 
# ## Write the missing USGS sites
# #sf::st_write(c2%>%select(identifier),paste0(folder_data_out,"missing_usgs_sites2.shp"))
# 
# 
# c<-regression_input_data3 %>% 
#   filter(regression_input_data3$USGS_SiteNo %in% a$USGS_Station) #For df1 values in df2
# d<-a %>% 
#   filter(a$USGS_Station %in% regression_input_data3$USGS_SiteNo) #For df2 values in df1
# 
# 
# rm(usgs_results)

usgs_results2$Norm_Trend_slope<-usgs_results2$MK_Combi_slope/usgs_results2$StdDev

#rm(b,c,c1,c2,d)

## Merging Trend Slope

usgs_results2<-usgs_results2%>%
  rename(USGS_SiteNo=USGS_Station)

regression_input_data4<-merge(regression_input_data3%>%filter(USGS_St%in%usgs_results2$USGS_SiteNo),usgs_results2,
                              by.x="USGS_St",
                              by.y="USGS_SiteNo",
                              all.x=TRUE,
                              all.y=FALSE)
rm(regression_input_data3)
rm(huc2_sf,usgs_results2)#,usgs_sites2_sf)

colnames(regression_input_data4)
class(regression_input_data4)


############################################################

## To get the HUC2 code ####################################

############################################################

## Copy lat and lon to new column, otherwise both lost while converting to sf object
regression_input_data4$Station_Lon2<-regression_input_data4$Station_Lon
regression_input_data4$Station_Lat2<-regression_input_data4$Station_Lat

class(regression_input_data4)

regression_input_data4<-regression_input_data4%>%
  st_drop_geometry()

colnames(regression_input_data4)
class(regression_input_data4)

#############################################
## Data Analysis#############################
#############################################
## Read the shapefile as sf object``
huc2_sf<-st_read(paste0(folder_name_absolute,"/Data/Shapefiles/HUC2_modified/HUC2_modified5.shp"))
st_crs(huc2_sf)

## Remove rows with
regression_input_data4<-regression_input_data4%>%
  filter(!is.na(Station_Lat2))

## Convert dataframe to sf object
regression_input_data4_sf<-sf::st_as_sf(regression_input_data4
                              ,coords = c("Station_Lon2", "Station_Lat2") 
                              ,crs = st_crs(huc2_sf)
                              )
st_crs(regression_input_data4_sf)
st_crs(huc2_sf)

sf_use_s2(FALSE)
system.time(regression_input_data4a_sf<-st_intersection(regression_input_data4_sf,huc2_sf%>%select(HUC2,NAME)))


st_write(regression_input_data4a_sf,
         paste0(folder_data_out,"regression_input_data4a_USGS_",
                year_usgs_start,"_",year_usgs_end,
                "_dam_",
                year_dam_start,"_",year_dam_end,
                ".gpkg"),
         driver="gpkg",
         #use either append=TRUE to append to layer or append=FALSE to overwrite layer
         append=FALSE)

regression_input_data5<-regression_input_data4a_sf%>%
  st_drop_geometry()%>%
  select(USGS_St, Station_Lat, Station_Lon,DA_sqkm,
         HUC2, NAME, Station_Lat, Station_Lon,
         MK_Combi_Trend,MK_Combi_slope,
         Norm_Trend_slope,
         LU2019_Urb, LU2019_Agr, LU2019_For,
         percent_max_increase,percent_max_decrease,
         percent_mean_increase,percent_mean_decrease,
         Weigh_Norm_Max_TS,Weigh_Norm_Mean_TS, ## Added
         MaxStorage,norm_max_stor
         )

# write.csv(regression_input_data5,
#           paste0(folder_data_out,,'regression_input_data5_USGS_"),
#                  year_usgs_start,"_",year_usgs_end,
#                  "_dam_",
#                  year_dam_start,"_",year_dam_end,
#                  ".csv"),
#           row.names = FALSE)

write.csv(regression_input_data5,
          paste0(folder_data_out,"regression_input_data5_USGS_new2_",#regression_input_data5_USGS_new_",
                 year_usgs_start,"_",year_usgs_end,
                 "_dam_",
                 year_dam_start,"_",year_dam_end,
                 ".csv"),
          row.names = FALSE)
#sf::st_write(regression_input_data5,paste0(folder_data_out,"regression_input_data5.shp"))
#rm(regression_input_data4)

sort(unique(regression_input_data5$HUC2))
length(sort(unique(regression_input_data5$HUC2)))


