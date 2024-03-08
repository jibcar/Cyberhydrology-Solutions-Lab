############################################

## Input
## 1. USGS Watersheds with percent_max_decrease, percent_mean_decrease, percent_mean_increase, and percent)max_increase
## 2. Max Storagr for each watershed, non overlapping watersheds assigned zero in the code
## 3. Land Data for each watersheds based on 2019
## 4. Results from USGS data analysis containing Trend slope, trend result, and Std Dev
## 5. HIC2 data



library(sf)
library(ggplot2)
library(ggsn)
library(dplyr)
library(tmap)
library(psych)
library(MLCM)
library(units)

library(stringr)
library(stringi)
library(ggplot2)
library(viridis)
library(data.table)
library(BAMMtools)

########################
########################

## Clear all variables
rm(list=ls())

## Clear Console
cat("\014")


username="joseph57"

year_dam_start="all"
year_dam_end="all"

year_usgs_start="1950"
year_usgs_end="2014"

if (year_usgs_start=="all" | year_usgs_end=="all"){
  
usgs_flow_file_name<-paste0("C:/Users/",
       username,
       "/Box/01_SharedBox_Jibin/Data_Intermediate/FFA_Analysis/FFA_EntireUS/z_Result10e/B_Results_Trend_Stationarity_",
       year_usgs_start,
       "_",
       year_usgs_end,
       "_alpha_0.05.txt")
}else if(year_usgs_start!="all" | year_usgs_end!="all"){
  usgs_flow_file_name<-paste0("C:/Users/",
                              username,
                              "/Box/01_SharedBox_Jibin/Data_Intermediate/FFA_Analysis/FFA_EntireUS/z_Result10e/B_Results_Trend_Stationarity_",
                              year_usgs_start,
                              "_",
                              year_usgs_end,
                              "_missing_5percent_alpha_0.05.txt")
  
}

setwd(paste0("C:\\Users\\",
             username,
             "\\Dropbox\\01_SharedFolder_Jibin\\Code_R\\code_multiple_regression"))

##  Direction to Output Folder and also save the path the R code
folder_out<-paste0("C:\\Users\\",
                   username,
                   "\\Box\\01_SharedBox_Jibin\\Results\\results_multiple_regression_v3\\")
fileConn<-file(paste0(folder_out,"00_code_full_path.txt"))
writeLines(rstudioapi::getSourceEditorContext()$path, 
           fileConn)
close(fileConn)
rm(fileConn)

usgs_watersheds<-st_read(paste0("C:\\Users\\",
                         username,
                         "\\Dropbox\\01_SharedFolder_Jibin\\Code_Python\\prec_noaa_analysis\\estimate_p_missing\\watersheds7_3890.gpkg"))

st_crs(usgs_watersheds)

usgs_watersheds$DA_sqkm<-units::set_units(st_area(usgs_watersheds), km^2)
usgs_watersheds$DA_sqmi <- units::set_units(st_area(usgs_watersheds), mi^2)

usgs_watersheds_df<-usgs_watersheds%>%sf::st_drop_geometry()

usgs_watersheds_df%>%
  filter(USGS_St=="01076500")


class(usgs_watersheds_df)

nid_maxstorage_cat<-read.csv(paste0("C:\\Users\\",
                                     username,
                                    "\\Box\\01_SharedBox_Jibin\\Results\\results_nid_dams\\",
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


## Replace NA with 0 in column 'MaxStorage' as some watersheds may not have any dam
usgs_watersheds_df$MaxStorage[is.na(usgs_watersheds_df$MaxStorage)] <- 0

usgs_watersheds_df$norm_max_stor<-as.numeric(usgs_watersheds_df$MaxStorage*1233.48)/((usgs_watersheds_df$ws_mean_precip/1000)*(usgs_watersheds_df$DA_sqkm * 1000000))



rm(nid_maxstorage_cat)


usgs_sites_sf<-usgs_watersheds

usgs_sites_sf<-usgs_sites_sf%>%
  select(USGS_St)

st_crs(huc2_sf)==st_crs(usgs_watersheds)

#usgs_sites2_sf<-usgs_sites1_sf
usgs_sites2_sf<-usgs_sites_sf

#rm(usgs_sites1_sf)
rm(usgs_sites_sf)


######################################
## Land Use data######################
######################################


#lu_data_raw<-st_read(paste0("C:/Users/",
#                            username,
#                            "/Box/01_SharedBox_Jibin/Data/Shapefiles/landuse_analysis/usgs_gages_sites.shp"))
lu_data_raw<-st_read(paste0("C:/Users/",
                            username,
                            "/Box/01_SharedBox_Jibin/Data_Intermediate/LULC_data/nlcd_2019_usgs_sites_3890.shp"))

colnames(lu_data_raw)

lu_data<-lu_data_raw%>%
  dplyr::select(USGS_St,LU2019_For,LU2019_Agr,LU2019_Urb)#,CLASS)

colnames(lu_data_raw)

lu_data<-rename(lu_data, 
                USGS_SiteNo = USGS_St
                
                )
rm(lu_data_raw)



regression_input_data3<-merge(usgs_watersheds_df,lu_data%>%st_drop_geometry(),
                              by.x="USGS_St",
                              by.y="USGS_SiteNo",
                              all.x=TRUE)



####################################
## Read
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

usgs_results<-filter(usgs_results_cont,Calc_Count>=years_count)
rm(usgs_results_cont,usgs_results_30)

usgs_results2<-usgs_results%>%
  select(USGS_Station,Station_Lat,Station_Lon,MK_Combi_Trend,MK_Combi_slope,StdDev)

usgs_results2$Norm_Trend_slope<-usgs_results2$MK_Combi_slope/usgs_results2$StdDev

usgs_results2<-usgs_results2%>%
  rename(USGS_SiteNo=USGS_Station)

regression_input_data4<-merge(regression_input_data3%>%filter(USGS_St%in%usgs_results2$USGS_SiteNo),usgs_results2,
                              by.x="USGS_St",
                              by.y="USGS_SiteNo",
                              all.x=TRUE,
                              all.y=FALSE)
rm(regression_input_data3)
rm(huc2_sf,usgs_results2)

colnames(regression_input_data4)
class(regression_input_data4)


############################################
## To get the HUC2 code ####################
############################################

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
huc2_sf<-st_read(paste0("C:/Users/",
                        username,
                        "/Box/01_SharedBox_Jibin/Data/Shapefiles/HUC2_modified/HUC2_modified5.shp"))
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
         paste0(folder_out,"regression_input_data4a_USGS_",
                year_usgs_start,"_",year_usgs_end,
                "_dam_",
                year_dam_start,"_",year_dam_end,
                ".gpkg"),
         driver="gpkg")

regression_input_data5<-regression_input_data4a_sf%>%
  st_drop_geometry()%>%
  select(USGS_St, Station_Lat, Station_Lon,
         HUC2, NAME, Station_Lat, Station_Lon,
         MK_Combi_Trend,MK_Combi_slope,
         Norm_Trend_slope,
         LU2019_Urb, LU2019_Agr, LU2019_For,
         percent_max_increase,percent_max_decrease,
         percent_mean_increase,percent_mean_decrease,
         MaxStorage,norm_max_stor
         )

write.csv(regression_input_data5,
          paste0("./data/regression_input_data5_USGS_new_",
                 year_usgs_start,"_",year_usgs_end,
                 "_dam_",
                 year_dam_start,"_",year_dam_end,
                 ".csv"),
          row.names = FALSE)

sort(unique(regression_input_data5$HUC2))
length(sort(unique(regression_input_data5$HUC2)))


