## 2024-01-16
## Added a loop to print plots in different category

## 2024-01-04
## Add NCA regions shapefile
## Reordered the plotting: CONUS first, NCA second and HUC2 third

## 2023-02-09
## Grey circle intensities in plots has been increased

## Modified on July 26, 2022
## variable names incorrect in ACF1 and ACF2 plot
## Folder Out changed to Box Folder

library(sf)
library(ggplot2)
library(ggsn)
library(ggrepel)
library(dplyr)
library(tmap)
## https://www.statology.org/summary-table-in-r/
#install.packages("psych")
library(psych)

## Clear all variables
rm(list=ls())

## Clear Console
cat("\014")


username='joseph57'
alpha_value="0.05"

setwd(paste0("C:\\Users\\",username,"\\Dropbox\\01_SharedFolder_Jibin\\Code_R\\code_usgs_analysis"))

## Basic Parameters

## Plots Output Folder
folder_out<-paste0("C:\\Users\\",
                   username,
                   "\\Box\\01_SharedBox_Jibin\\Plots_IntermediateResults\\plots_usgs_analysis_v8\\")
fileConn<-file(paste0(folder_out,"00_code_full_path.txt"))
writeLines(rstudioapi::getSourceEditorContext()$path, 
           fileConn)
close(fileConn)
rm(fileConn)

years_count=60

## Read the shapefile as sf object``
huc2_sf<-st_read(paste0("C:/Users/",username,"/Box/01_SharedBox_Jibin/Data/Shapefiles/HUC2_modified/HUC2_modified5.shp"))
huc2_sf_map<-st_read(paste0("C:/Users/",username,"/Box/01_SharedBox_Jibin/Data/Shapefiles/HUC2_modified/HUC2_modified6.shp"))

conus_sf<-st_read(paste0("C:/Users/",username,"/Box/01_SharedBox_Jibin/Data/Shapefiles/cb_2018_us_conus_5m/cb_2018_us_conus_5m.shp"))
conus_sf_map<-st_read(paste0("C:/Users/",username,"/Box/01_SharedBox_Jibin/Data/Shapefiles/states_conterminous/states_conterminous.shp"))

#regions_sf_map<-st_read(paste0("C:/Users/",username,"/Box/01_SharedBox_Jibin/Data/Shapefiles/NCA_Regions_Cont/NCA_Regions_Cont1.shp"))
## Transform the coordiante system 
#st_crs(regions_sf_map)
#regions_sf_map<-st_transform(regions_sf_map, st_crs(huc2_sf_map))
#st_crs(regions_sf_map)
#st_write(regions_sf_map, paste0("C:/Users/",username,"/Box/01_SharedBox_Jibin/Data/Shapefiles/NCA_Regions_Cont/NCA_Regions_Cont1_NAD83.shp"))

regions_sf_map<-st_read(paste0("C:/Users/",username,"/Box/01_SharedBox_Jibin/Data/Shapefiles/NCA_Regions_Cont/NCA_Regions_Cont2_NAD83.shp"))

## Display colnames of huc2_sf
huc2_df <-huc2_sf%>%
  st_drop_geometry()
colnames(huc2_df)


## Read Metadata: CRS
st_crs(huc2_sf)
st_crs(huc2_sf_map)
st_crs(conus_sf)
st_crs(conus_sf_map)
st_crs(regions_sf_map)

## Read Metadata: geometry type
st_geometry_type(huc2_sf)


## Change the projection to WGS 84
#huc2_sf<-st_set_crs(huc2_sf,4326)
#huc2_sf<-st_transform(huc2_sf, "+proj=longlat +datum=WGS84")

##Check the projection
st_crs(huc2_sf)

## Read Metadata: extent
st_bbox(huc2_sf)

#ggplot() + geom_sf(data = huc2_sf) + ggtitle("HUC2 - Map") + coord_sf()


#ggplot() + geom_sf(data = huc2_sf_map, aes(fill=NAME)) + ggtitle("HUC2 - Map") + coord_sf()


## Display colnames of huc2_sf
huc2_sf%>%
  st_drop_geometry()%>%
  colnames()
####################################
## Read trend-stationary results####
####################################
#usgs_results<-read.csv("C:\\Users\\joseph57\\Dropbox\\01_SharedFolder_Jibin\\CE699_ResearchPhDThesis\\Conferences\\2022_EWRI\\AbstractSubmission\\InitialResults\\result_20201201_combinedresult_edited20210510_1.csv")
#usgs_results_30<-read.csv("C:\\temp\\JibinJoseph\\FFA_Analysis\\FFA_EntireUS\\z_Result10b\\results.txt",sep='\t')
usgs_results_30<-read.csv(paste0("C:/Users/",username,"/Box/01_SharedBox_Jibin/Data_Intermediate/FFA_Analysis/FFA_EntireUS/z_Result10e/B_Results_Trend_Stationarity_alpha_",
                                 alpha_value,
                                 "_20230515-234759.txt"),
                          sep='\t')
colnames(usgs_results_30)


## Filter for conterminous US
usgs_results_cont<- usgs_results_30 %>% 
  filter(!(State_Name %in% c("AK","HI","PR")))

#usgs_results_cont<- usgs_results_30 %>% 
  #filter(State_Name %in% c("AL","AZ","AR","CA","CO","CT","DE","FL","GA","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"))

usgs_results<-filter(usgs_results_cont,Calc_Count>=years_count)

head(usgs_results)
colnames(usgs_results)

rm(usgs_results_30,usgs_results_cont)

hcdn_gages_df<-read.csv(paste0("C:/Users/",username,"/Box/01_SharedBox_Jibin/Data_Intermediate/hcdn_gages_df.csv"))

x<-usgs_results
y<-hcdn_gages_df

usgs_results_30_merged_result<-merge(x, y, 
                                     by.x = "USGS_Station", by.y = "STAID", all.x = TRUE, all.y = FALSE)

write.csv(usgs_results_30_merged_result,
          paste0("C:/Users/",username,"/Box/01_SharedBox_Jibin/Data_Intermediate/merged_result_usgs_analysis.csv"), 
          row.names = FALSE)

colnames(usgs_results_30_merged_result)

rm(hcdn_gages_df,x,y)

colnames(usgs_results_30_merged_result)

## Copy lat and lon to new column, otherwise both lost while converting to sf object
usgs_results_30_merged_result$Station_Lon2<-usgs_results_30_merged_result$Station_Lon
usgs_results_30_merged_result$Station_Lat2<-usgs_results_30_merged_result$Station_Lat

#############################################
## Data Analysis#############################
#############################################
## Convert dataframe to sf object
usgs_results_sf<-sf::st_as_sf(usgs_results_30_merged_result, 
                              coords = c("Station_Lon2", "Station_Lat2"), 
                              crs = st_crs(huc2_sf))
st_crs(huc2_sf)
st_crs(usgs_results_sf)

rm(usgs_results_30_merged_result,usgs_results)

colnames(usgs_results_sf)

## A quick plot using qtm
tmap::qtm(usgs_results_sf)

### Not required any more###

#st_is_valid(huc2_sf)
#st_is_valid(huc2_sf,reason=T)

## Check for empty geometries
#any(is.na(st_dimension(huc2_sf)))
## Check for corrupt geometries
#any(is.na(st_is_valid(huc2_sf)))
## Check for invalid geometries
#any(na.omit(st_is_valid(huc2_sf)) == FALSE)
## in case of invalid geometries, query the reason for invalidity by 
#st_is_valid(huc2_sf,reason=TRUE)

## Making geometries valid
#huc2_sf<-st_make_valid(huc2_sf)
## Check whether valid geometry is fine
#st_is_valid(huc2_sf,reason=T)

## Intersection with huc 2 level
st_crs(usgs_results_sf)
st_crs(huc2_sf)
usgs_huc2_acf_sf_full<-st_intersection(usgs_results_sf,huc2_sf)

rm(usgs_results_sf)


############################################
## Extracting some statistics ##############
## for all USGS stations ###################
############################################


## Summary function returns the minimum value, maximum value, median, mean, the first quartile and the third quartile.
summary(usgs_huc2_acf_sf_full$Drain_Area)


IQR(usgs_huc2_acf_sf_full$Drain_Area,na.rm = TRUE)

## range for 95% of all values actually represents the middle 95% values.
quantile(usgs_huc2_acf_sf_full$Drain_Area,probs=c(0.025,0.50,0.975),na.rm = TRUE)

############################################
## Extracting some statistics ##############
## for all HCDN stations ###################
############################################


## Summary function returns the minimum value, maximum value, median, mean, the first quartile and the third quartile.

data_summary<-usgs_huc2_acf_sf_full%>%dplyr::filter(CLASS=='Ref')
summary(data_summary$Drain_Area)


IQR(data_summary$Drain_Area,na.rm = TRUE)

## range for 95% of all values actually represents the middle 95% values.
quantile(data_summary$Drain_Area,probs=c(0.025,0.50,0.975),na.rm = TRUE)

rm(data_summary)

############################################
## Plotting USGS Stations ##################
## with states and huc2 ####################
############################################
#round(table(usgs_results$Var_2Divide)/nrow(usgs_results)*100,1)


## Creating new column
usgs_huc2_acf_sf_full$site_classification<-paste0(usgs_huc2_acf_sf_full$HCDN_2009,", ",
                                             usgs_huc2_acf_sf_full$CLASS)

table(usgs_huc2_acf_sf_full$site_classification)

## Change the character value based on the value
usgs_huc2_acf_sf_full$site_classification[usgs_huc2_acf_sf_full$site_classification=="NA, NA"] <- "Other USGS Sites"
usgs_huc2_acf_sf_full$site_classification[usgs_huc2_acf_sf_full$site_classification=="NA, Non-ref"] <- "GAGESII Non-Ref"
usgs_huc2_acf_sf_full$site_classification[usgs_huc2_acf_sf_full$site_classification=="NA, Ref"] <- "GAGESII Ref"
usgs_huc2_acf_sf_full$site_classification[usgs_huc2_acf_sf_full$site_classification=="yes, Ref"] <- "HCDN & GAGESII Ref"
#usgs_huc2_acf_sf_full$site_classification[usgs_huc2_acf_sf_full$site_classification=="yes, Non-ref"] <- "Check"
#usgs_huc2_acf_sf_full$site_classification[usgs_huc2_acf_sf_full$site_classification=="yes, NA"] <- "Check"


table(usgs_huc2_acf_sf_full$site_classification)


#rm(usgs_huc2_acf_sf_full)

## Specify the factor levels in the order you want for legend
usgs_huc2_acf_sf_full$site_classification<-factor(usgs_huc2_acf_sf_full$site_classification, 
                                             levels=c("HCDN & GAGESII Ref", 
                                                      "GAGESII Ref",
                                                      "GAGESII Non-Ref",
                                                      "Other USGS Sites"))

usgs_huc2_states_0summary_plot <- ggplot()+
  geom_point(data=usgs_huc2_acf_sf_full,
             aes(Station_Lon,
                 y=Station_Lat,
                 color=site_classification,
                 shape=site_classification))+
  
  theme_minimal() +
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  geom_sf(data = regions_sf_map, colour="grey60", fill="transparent", linetype = "solid", linewidth=0.5) +
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(15,16,3,5)
                     ,labels = paste(levels(usgs_huc2_acf_sf_full$site_classification)
                                     ,' ('
                                     ,table(usgs_huc2_acf_sf_full$site_classification)
                                     #,'-'
                                     #,round(table(usgs_huc2_acf_sf_full$site_classification)/nrow(usgs_huc2_acf_sf_full)*100,1)
                                     #,"%"
                                     ,")"
                                     , sep = "")
  )+
  scale_color_manual(values=c('saddlebrown','sandybrown','deepskyblue','dodgerblue3')
                     ,labels = paste(levels(usgs_huc2_acf_sf_full$site_classification)
                                     ,' ('
                                     ,table(usgs_huc2_acf_sf_full$site_classification)
                                     #,'-'
                                     #,round(table(usgs_huc2_acf_sf_full$site_classification)/nrow(usgs_huc2_acf_sf_full)*100,1)
                                     #,"%"
                                     ,")"
                                     , sep = ""))+
  labs(y= "Latitude", x = "Longitude")+
  ggtitle(paste0("Plot of ",nrow(usgs_huc2_acf_sf_full%>%st_drop_geometry()), " USGS sites for alpha = ", alpha_value))+ # for the main title+

  labs(shape='Site Type (\u2265 60 years)') +
  labs(color='Site Type (\u2265 60 years)') +
  coord_sf()#xlim = c(-130, -60),
#ylim = c(23, 53),expand=FALSE)
#usgs_huc2_states_0summary_plot

## Change legend title
#usgs_huc2_states_0summary_plot <- usgs_huc2_states_0summary_plot+labs(shape="Trend")

# Right -> inside the plot area
usgs_huc2_states_0summary_plot <-usgs_huc2_states_0summary_plot + theme(
  legend.position = c(.995, .35),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)

## add a north symbol and a scale bar with segments of 200 km
## Load ggspatial and ggsn package
usgs_huc2_states_0summary_plot <- usgs_huc2_states_0summary_plot+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(2, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = ""
    ))+
  ggsn::scalebar(conus_sf_map, dist = 1000, dist_unit = "km", 
                 st.size = 3,location = "bottomleft",
                 transform = TRUE, model = "WGS84")

#usgs_huc2_states_0summary_plot
ggsave(
  paste0(folder_out,"usgs_huc2_states_0summary_plot.png"),
  plot = usgs_huc2_states_0summary_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(usgs_huc2_states_0summary_plot)

## Main Loop
site_type_list<-c("All Sites", 
                  "HCDN Only", 
                  "GAGES-II Reference Only", 
                  "GAGES-II Non-reference Only", 
                  "GAGES-II Non-reference + Other USGS")

for (site_type in site_type_list){
  if (site_type=="All Sites"){
    usgs_huc2_acf_sf<-usgs_huc2_acf_sf_full
  }else if(site_type=="HCDN Only"){
    usgs_huc2_acf_sf<-usgs_huc2_acf_sf_full%>%
      filter(HCDN_2009=="yes")
  }else if(site_type=="GAGES-II Reference Only"){
    usgs_huc2_acf_sf<-usgs_huc2_acf_sf_full%>%
      filter(CLASS=="Ref")
    
  }else if(site_type=="GAGES-II Non-reference Only"){
    usgs_huc2_acf_sf<-usgs_huc2_acf_sf_full%>%
      filter(CLASS=="Non-ref")
    
  }else if(site_type=="GAGES-II Non-reference + Other USGS"){
    usgs_huc2_acf_sf<-usgs_huc2_acf_sf_full%>%
      filter(CLASS%in%c(NA,"Non-ref"))
  }else{
    print("Check\nCheck\nCheck")
  }


############################################
## Plotting USGS Stations ##################
## Normally Distributed ####################
############################################
usgs_results_cont_60_normal<-filter(usgs_huc2_acf_sf,Normality_Decision=="Normal")

usgs_huc2_states_1normal_sitetype_plot <- ggplot()+
  geom_point(data=usgs_results_cont_60_normal,aes(Station_Lon,y=Station_Lat,
                                       color=site_classification,shape=site_classification))+
  theme_minimal() +
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  geom_sf(data = regions_sf_map, colour="grey60", fill="transparent", linetype = "solid", linewidth=0.5) +
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(15,16,3,5)
                     ,labels = paste(levels(usgs_results_cont_60_normal$site_classification)
                                     ,' ('
                                     ,table(usgs_results_cont_60_normal$site_classification)
                                     #,'-'
                                     #,round(table(usgs_huc2_acf_sf$site_classification)/nrow(usgs_huc2_acf_sf)*100,1)
                                     #,"%"
                                     ,")"
                                     , sep = "")
  )+
  scale_color_manual(values=c('saddlebrown','sandybrown','deepskyblue','dodgerblue3')
                     ,labels = paste(levels(usgs_results_cont_60_normal$site_classification)
                                     ,' ('
                                     ,table(usgs_results_cont_60_normal$site_classification)
                                     #,'-'
                                     #,round(table(usgs_huc2_acf_sf$site_classification)/nrow(usgs_huc2_acf_sf)*100,1)
                                     #,"%"
                                     ,")"
                                     , sep = "")
  )+
  labs(color='Site Type (Normality)') +
  labs(shape='Site Type (Normality)') +
  #ggtitle(paste(nrow(usgs_huc2_acf_sf),"USGS stations with atleast",years_count,"years with CONUS and HUC2 boundaries")) +
  labs(y= "Latitude", x = "Longitude")+
  ggtitle(paste0("Plot of ",nrow(usgs_results_cont_60_normal%>%st_drop_geometry())," (",site_type, ") USGS sites for alpha = ", alpha_value))+ # for the main title+
  
  coord_sf()#xlim = c(-130, -60),
#ylim = c(23, 53),expand=FALSE)
#usgs_huc2_states_1normal_sitetype_plot

## Change legend title
#usgs_huc2_states_1normal_sitetype_plot <- usgs_huc2_states_1normal_sitetype_plot+labs(shape="Trend")

# Right -> inside the plot area
usgs_huc2_states_1normal_sitetype_plot <-usgs_huc2_states_1normal_sitetype_plot + theme(
  legend.position = c(.995, .35),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)

## add a north symbol and a scale bar with segments of 200 km
## Load ggspatial and ggsn package
usgs_huc2_states_1normal_sitetype_plot <- usgs_huc2_states_1normal_sitetype_plot+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(2, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = ""
    ))+
  ggsn::scalebar(conus_sf_map, dist = 1000, dist_unit = "km", 
                 st.size = 3,location = "bottomleft",
                 transform = TRUE, model = "WGS84")

#usgs_huc2_states_1normal_sitetype_plot
ggsave(
  paste0(folder_out,"usgs_huc2_states_1normal_sitetype_plot.png"),
  plot = usgs_huc2_states_1normal_sitetype_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(usgs_huc2_states_1normal_sitetype_plot,usgs_results_cont_60_normal)

############################################
## Plotting USGS Stations ##################
## Lag1 site type####################
############################################
usgs_results_cont_60_lag1<-filter(usgs_huc2_acf_sf,ACF_Lag1=="significant")

usgs_huc2_states_2lag1_sitetype_plot <- ggplot()+
  geom_point(data=usgs_results_cont_60_lag1,aes(Station_Lon,y=Station_Lat,
                                                  color=site_classification,shape=site_classification))+
  theme_minimal() +
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  geom_sf(data = regions_sf_map, colour="grey60", fill="transparent", linetype = "solid", linewidth=0.5) +
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(15,16,3,5)
                     ,labels = paste(levels(usgs_results_cont_60_lag1$site_classification)
                                                          ,' ('
                                                          ,table(usgs_results_cont_60_lag1$site_classification)
                                                          #,'-'
                                                          #,round(table(usgs_huc2_acf_sf$site_classification)/nrow(usgs_huc2_acf_sf)*100,1)
                                                          #,"%"
                                                          ,")"
                                                          , sep = "")
  )+
  scale_color_manual(values=c('saddlebrown','sandybrown','deepskyblue','dodgerblue3')
                     ,labels = paste(levels(usgs_results_cont_60_lag1$site_classification)
                                     ,' ('
                                     ,table(usgs_results_cont_60_lag1$site_classification)
                                     #,'-'
                                     #,round(table(usgs_huc2_acf_sf$site_classification)/nrow(usgs_huc2_acf_sf)*100,1)
                                     #,"%"
                                     ,")"
                                     , sep = "")
  )+
  labs(color='Site Type (ACF Lag-1)') +
  labs(shape='Site Type (ACF Lag-1)') +
  #ggtitle(paste(nrow(usgs_huc2_acf_sf),"USGS stations with atleast",years_count,"years with CONUS and HUC2 boundaries")) +
  labs(y= "Latitude", x = "Longitude")+
  ggtitle(paste0("Plot of ",nrow(usgs_results_cont_60_lag1%>%st_drop_geometry())," (",site_type, ") USGS  sites for alpha = ", alpha_value))+ # for the main title+
  coord_sf()#xlim = c(-130, -60),
#ylim = c(23, 53),expand=FALSE)
#usgs_huc2_states_2lag1_sitetype_plot

## Change legend title
#usgs_huc2_states_2lag1_sitetype_plot <- usgs_huc2_states_2lag1_sitetype_plot+labs(shape="Trend")

# Right -> inside the plot area
usgs_huc2_states_2lag1_sitetype_plot <-usgs_huc2_states_2lag1_sitetype_plot + theme(
  legend.position = c(.995, .35),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)

## add a north symbol and a scale bar with segments of 200 km
## Load ggsn package
usgs_huc2_states_2lag1_sitetype_plot <- usgs_huc2_states_2lag1_sitetype_plot+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(2, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = ""
    ))+
  ggsn::scalebar(conus_sf_map, dist = 1000, dist_unit = "km", 
                 st.size = 3,location = "bottomleft",
                 transform = TRUE, model = "WGS84")

#usgs_huc2_states_2lag1_sitetype_plot
ggsave(
  paste0(folder_out,site_type,"_usgs_huc2_states_2lag1_sitetype_plot.png"),
  plot = usgs_huc2_states_2lag1_sitetype_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(usgs_huc2_states_2lag1_sitetype_plot,usgs_results_cont_60_lag1)

####################################
## Plotting ACF Lag1 #######
####################################
## Specify the factor levels in the order you want for legend
usgs_huc2_acf_sf$ACF_Lag1<-factor(usgs_huc2_acf_sf$ACF_Lag1, levels=c("insignificant", "significant"))


usgs_huc2_states_2lag1_plot <- ggplot()+
  geom_point(data=usgs_huc2_acf_sf,aes(Station_Lon,y=Station_Lat,
                                       shape=ACF_Lag1,
                                       color=ACF_Lag1,
                                       fill=ACF_Lag1),size=1)+
  
  theme_minimal()+
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  geom_sf(data = regions_sf_map, colour="grey60", fill="transparent", linetype = "solid", linewidth=0.5) +
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(1, 22),
                     labels = paste(levels(usgs_huc2_acf_sf$ACF_Lag1),' (',
                                    table(usgs_huc2_acf_sf$ACF_Lag1),'-',
                                    round(table(usgs_huc2_acf_sf$ACF_Lag1)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_color_manual(values=c('grey','dodgerblue4'),
                     labels = paste(levels(usgs_huc2_acf_sf$ACF_Lag1),' (',
                                    table(usgs_huc2_acf_sf$ACF_Lag1),'-',
                                    round(table(usgs_huc2_acf_sf$ACF_Lag1)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_fill_manual(values=c('grey','dodgerblue4'),
                    labels = paste(levels(usgs_huc2_acf_sf$ACF_Lag1),' (',
                                   table(usgs_huc2_acf_sf$ACF_Lag1),'-',
                                   round(table(usgs_huc2_acf_sf$ACF_Lag1)/nrow(usgs_huc2_acf_sf)*100,1)
                                   ,'%)', sep = ""))+
  labs(shape='ACF Lag-1') +
  labs(color='ACF Lag-1') +
  labs(fill='ACF Lag-1') +
  #ggtitle(paste("Result of ACF Lag-1 for",nrow(usgs_huc2_acf_sf),"USGS stations with",years_count,"and more years")) +
  labs(y= "Latitude", x = "Longitude")+
  ggtitle(paste0("Plot of ",nrow(usgs_huc2_acf_sf%>%st_drop_geometry())," (",site_type, ") USGS  sites for alpha = ", alpha_value))+ # for the main title+
  
  coord_sf()#xlim = c(-130, -60),
#ylim = c(23, 53),expand=FALSE)
#usgs_huc2_states_2lag1_plot

## Change legend title
#usgs_huc2_states_2lag1_plot <- usgs_huc2_states_2lag1_plot+labs(shape="Trend")

# Right -> inside the plot area
usgs_huc2_states_2lag1_plot <-usgs_huc2_states_2lag1_plot + theme(
  legend.position = c(.97, .35),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)

## add a north symbol and a scale bar with segments of 200 km
## Load ggsn package
usgs_huc2_states_2lag1_plot <- usgs_huc2_states_2lag1_plot+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(2, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = ""
    ))+
  ggsn::scalebar(conus_sf_map, dist = 1000, dist_unit = "km", 
                 st.size = 3,location = "bottomleft",
                 transform = TRUE, model = "WGS84")

#usgs_huc2_states_2lag1_plot
ggsave(
  paste0(folder_out,site_type,"_usgs_huc2_states_2lag1_plot.png"),
  plot = usgs_huc2_states_2lag1_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(usgs_huc2_states_2lag1_plot)


####################################
## Plotting ACF Lag2 #######
####################################
usgs_huc2_acf_sf$ACF_Lag2<-factor(usgs_huc2_acf_sf$ACF_Lag2, levels=c("insignificant", "significant"))

usgs_huc2_states_2lag2_plot <- ggplot()+
  theme_minimal()+
  geom_point(data=usgs_huc2_acf_sf,aes(Station_Lon,y=Station_Lat,
                                       shape=ACF_Lag2,
                                       color=ACF_Lag2,
                                       fill=ACF_Lag2))+
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  geom_sf(data = regions_sf_map, colour="grey60", fill="transparent", linetype = "solid", linewidth=0.5) +
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(1, 21),
                     labels = paste(levels(usgs_huc2_acf_sf$ACF_Lag2),' (',
                                    table(usgs_huc2_acf_sf$ACF_Lag2),'-',
                                    round(table(usgs_huc2_acf_sf$ACF_Lag2)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  
  scale_color_manual(values=c('grey','dodgerblue4'),
                     labels = paste(levels(usgs_huc2_acf_sf$ACF_Lag2),' (',
                                    table(usgs_huc2_acf_sf$ACF_Lag2),'-',
                                    round(table(usgs_huc2_acf_sf$ACF_Lag2)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_fill_manual(values = c('grey','dodgerblue4'),
                    labels = paste(levels(usgs_huc2_acf_sf$ACF_Lag2),' (',
                                   table(usgs_huc2_acf_sf$ACF_Lag2),'-',
                                   round(table(usgs_huc2_acf_sf$ACF_Lag2)/nrow(usgs_huc2_acf_sf)*100,1)
                                   ,'%)', sep = ""))+
  labs(shape='ACF Lag-2') +
  labs(color='ACF Lag-2') +
  labs(fill='ACF Lag-2') +
  
  #ggtitle(paste("Result of ACF Lag-2 for",nrow(usgs_huc2_acf_sf),"USGS stations with",years_count,"and more years")) +
  labs(y= "Latitude", x = "Longitude")+
  ggtitle(paste0("Plot of ",nrow(usgs_huc2_acf_sf%>%st_drop_geometry())," (",site_type, ") USGS  sites for alpha = ", alpha_value))+ # for the main title+
  
  coord_sf()
#usgs_huc2_states_2lag2_plot

## Change legend title
#usgs_huc2_states_2lag2_plot <- usgs_huc2_states_2lag2_plot+labs(shape="Trend")

# Right -> inside the plot area
usgs_huc2_states_2lag2_plot <-usgs_huc2_states_2lag2_plot + theme(
  legend.position = c(.97, .35),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)

## add a north symbol and a scale bar with segments of 200 km
## Load ggsn package
usgs_huc2_states_2lag2_plot <- usgs_huc2_states_2lag2_plot+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(2, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = ""
    ))+
  ggsn::scalebar(conus_sf_map, dist = 1000, dist_unit = "km", 
                 st.size = 3,location = "bottomleft",
                 transform = TRUE, model = "WGS84")
#usgs_huc2_states_2lag2_plot
ggsave(
  paste0(folder_out,site_type,"_usgs_huc2_states_2lag2_plot.png"),
  plot = usgs_huc2_states_2lag2_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(usgs_huc2_states_2lag2_plot)


####################################
## Plotting results of Trend #######
####################################

usgs_results_cont_60_trend_no<-filter(usgs_huc2_acf_sf,Trend_Result=="no trend")

usgs_huc2_states_3ctrend_no_plot <- ggplot()+
  geom_point(data=usgs_results_cont_60_trend_no,aes(Station_Lon,y=Station_Lat,
                                                color=site_classification,
                                                shape=site_classification))+
  theme_minimal() +
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  geom_sf(data = regions_sf_map, colour="grey60", fill="transparent", linetype = "solid", linewidth=0.5) +
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(5,7,10,13),labels = paste(levels(usgs_results_cont_60_trend_no$site_classification)
                                                          ,' ('
                                                          ,table(usgs_results_cont_60_trend_no$site_classification)
                                                          ,'-'
                                                          ,round(table(usgs_results_cont_60_trend_no$site_classification)/nrow(usgs_results_cont_60_trend_no)*100,1)
                                                          ,"%"
                                                          ,")"
                                                          , sep = ""))+
  scale_color_manual(values=c('saddlebrown','sandybrown','deepskyblue','dodgerblue3')
                     ,labels = paste(levels(usgs_results_cont_60_trend_no$site_classification)
                                     ,' ('
                                     ,table(usgs_results_cont_60_trend_no$site_classification)
                                     ,'-'
                                     ,round(table(usgs_results_cont_60_trend_no$site_classification)/nrow(usgs_results_cont_60_trend_no)*100,1)
                                     ,"%"
                                     ,")"
                                     , sep = "")
  )+
  labs(color='Site Type (No Trend)') +
  labs(shape='Site Type (No Trend)') +
  #ggtitle(paste(nrow(usgs_huc2_acf_sf),"USGS stations with atleast",years_count,"years with CONUS and HUC2 boundaries")) +
  labs(y= "Latitude", x = "Longitude")+
  ggtitle(paste0("Plot of ",nrow(usgs_results_cont_60_trend_no%>%st_drop_geometry())," (",site_type, ") USGS  sites for alpha = ", alpha_value))+ # for the main title+
  
  coord_sf()#xlim = c(-130, -60),
#ylim = c(23, 53),expand=FALSE)
#usgs_huc2_states_3ctrend_no_plot

## Change legend title
#usgs_huc2_states_3ctrend_no_plot <- usgs_huc2_states_3ctrend_no_plot+labs(shape="Trend")

# Right -> inside the plot area
usgs_huc2_states_3ctrend_no_plot <-usgs_huc2_states_3ctrend_no_plot + theme(
  legend.position = c(.995, .35),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6),
  legend.title = element_text(size = 8), 
  legend.text = element_text(size = 8)
)

## add a north symbol and a scale bar with segments of 200 km
## Load ggsn package
usgs_huc2_states_3ctrend_no_plot <- usgs_huc2_states_3ctrend_no_plot+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(2, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = ""
    ))+
  ggsn::scalebar(conus_sf_map, dist = 1000, dist_unit = "km", 
                 st.size = 3,location = "bottomleft",
                 transform = TRUE, model = "WGS84")

#usgs_huc2_states_3ctrend_no_plot
ggsave(
  paste0(folder_out,site_type,"_usgs_huc2_states_3ctrend_no_plot.png"),
  plot = usgs_huc2_states_3ctrend_no_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(usgs_huc2_states_3ctrend_no_plot,usgs_results_cont_60_trend_no)


#######
#Increasing
##


usgs_results_cont_60_trend_inc<-filter(usgs_huc2_acf_sf,Trend_Result=="increasing")

usgs_huc2_states_3ctrend_inc_plot <- ggplot()+
  geom_point(data=usgs_results_cont_60_trend_inc,aes(Station_Lon,y=Station_Lat,
                                                    color=site_classification,
                                                    shape=site_classification))+
  theme_minimal() +
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  geom_sf(data = regions_sf_map, colour="grey60", fill="transparent", linetype = "solid", linewidth=0.5) +
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(5,7,10,13)
                     ,labels = paste(levels(usgs_results_cont_60_trend_inc$site_classification)
                                     ,' ('
                                     ,table(usgs_results_cont_60_trend_inc$site_classification)
                                     ,'-'
                                     ,round(table(usgs_results_cont_60_trend_inc$site_classification)/nrow(usgs_results_cont_60_trend_inc)*100,1)
                                     ,"%"
                                     ,")"
                                     , sep = "")
  )+
  scale_color_manual(values=c('saddlebrown','sandybrown','deepskyblue','dodgerblue3')
                     ,labels = paste(levels(usgs_results_cont_60_trend_inc$site_classification)
                                     ,' ('
                                     ,table(usgs_results_cont_60_trend_inc$site_classification)
                                     ,'-'
                                     ,round(table(usgs_results_cont_60_trend_inc$site_classification)/nrow(usgs_results_cont_60_trend_inc)*100,1)
                                     ,"%"
                                     ,")"
                                     , sep = "")
  )+
  labs(color='Site Type (Increasing Trend)') +
  labs(shape='Site Type (Increasing Trend)') +
  #ggtitle(paste(nrow(usgs_huc2_acf_sf),"USGS stations with atleast",years_count,"years with CONUS and HUC2 boundaries")) +
  labs(y= "Latitude", x = "Longitude")+
  ggtitle(paste0("Plot of ",nrow(usgs_results_cont_60_trend_inc%>%st_drop_geometry())," (",site_type, ") USGS  sites for alpha = ", alpha_value))+ # for the main title+
  
  coord_sf()#xlim = c(-130, -60),
#ylim = c(23, 53),expand=FALSE)
#usgs_huc2_states_3ctrend_inc_plot

## Change legend title
#usgs_huc2_states_3ctrend_inc_plot <- usgs_huc2_states_3ctrend_inc_plot+labs(shape="Trend")

# Right -> inside the plot area
usgs_huc2_states_3ctrend_inc_plot <-usgs_huc2_states_3ctrend_inc_plot + theme(
  legend.position = c(.995, .35),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6),
  legend.title = element_text(size = 8), 
  legend.text = element_text(size = 8)
)

## add a north symbol and a scale bar with segments of 200 km
## Load ggsn package
usgs_huc2_states_3ctrend_inc_plot <- usgs_huc2_states_3ctrend_inc_plot+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(2, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = ""
    ))+
  ggsn::scalebar(conus_sf_map, dist = 1000, dist_unit = "km", 
                 st.size = 3,location = "bottomleft",
                 transform = TRUE, model = "WGS84")

#usgs_huc2_states_3ctrend_inc_plot
ggsave(
  paste0(folder_out,site_type,"_usgs_huc2_states_3ctrend_inc_plot.png"),
  plot = usgs_huc2_states_3ctrend_inc_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(usgs_huc2_states_3ctrend_inc_plot,usgs_results_cont_60_trend_inc)


#######
#Decreasing
##


usgs_results_cont_60_trend_dec<-filter(usgs_huc2_acf_sf,Trend_Result=="decreasing")

usgs_huc2_states_3ctrend_dec_plot <- ggplot()+
  geom_point(data=usgs_results_cont_60_trend_dec,aes(Station_Lon,y=Station_Lat,
                                                     color=site_classification,
                                                     shape=site_classification))+
  theme_minimal() +
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  geom_sf(data = regions_sf_map, colour="grey60", fill="transparent", linetype = "solid", linewidth=0.5) +
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(5,7,10,13),labels = paste(levels(usgs_results_cont_60_trend_dec$site_classification)
                                                          ,' ('
                                                          ,table(usgs_results_cont_60_trend_dec$site_classification)
                                                          ,'-'
                                                          ,round(table(usgs_results_cont_60_trend_dec$site_classification)/nrow(usgs_results_cont_60_trend_dec)*100,1)
                                                          ,"%"
                                                          ,")"
                                                          , sep = "")
  )+
  scale_color_manual(values=c('saddlebrown','sandybrown','deepskyblue','dodgerblue3')
                     ,labels = paste(levels(usgs_results_cont_60_trend_dec$site_classification)
                                     ,' ('
                                     ,table(usgs_results_cont_60_trend_dec$site_classification)
                                     ,'-'
                                     ,round(table(usgs_results_cont_60_trend_dec$site_classification)/nrow(usgs_results_cont_60_trend_dec)*100,1)
                                     ,"%"
                                     ,")"
                                     , sep = "")
  )+
  labs(color='Site Type (Decreasing Trend)') +
  labs(shape='Site Type (Decreasing Trend)') +
  #ggtitle(paste(nrow(usgs_huc2_acf_sf),"USGS stations with atleast",years_count,"years with CONUS and HUC2 boundaries")) +
  labs(y= "Latitude", x = "Longitude")+
  ggtitle(paste0("Plot of ",nrow(usgs_results_cont_60_trend_dec%>%st_drop_geometry())," (",site_type, ") USGS  sites for alpha = ", alpha_value))+ # for the main title+
  
  coord_sf()#xlim = c(-130, -60),
#ylim = c(23, 53),expand=FALSE)
#usgs_huc2_states_3ctrend_dec_plot

## Change legend title
#usgs_huc2_states_3ctrend_dec_plot <- usgs_huc2_states_3ctrend_dec_plot+labs(shape="Trend")

# Right -> inside the plot area
usgs_huc2_states_3ctrend_dec_plot <-usgs_huc2_states_3ctrend_dec_plot + theme(
  legend.position = c(.995, .35),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6),
  legend.title = element_text(size = 8), 
  legend.text = element_text(size = 8)
)

## add a north symbol and a scale bar with segments of 200 km
## Load ggsn package
usgs_huc2_states_3ctrend_dec_plot <- usgs_huc2_states_3ctrend_dec_plot+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(2, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = ""
    ))+
  ggsn::scalebar(conus_sf_map, dist = 1000, dist_unit = "km", 
                 st.size = 3,location = "bottomleft",
                 transform = TRUE, model = "WGS84")

#usgs_huc2_states_3ctrend_dec_plot
ggsave(
  paste0(folder_out,site_type,"_usgs_huc2_states_3ctrend_dec_plot.png"),
  plot = usgs_huc2_states_3ctrend_dec_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(usgs_huc2_states_3ctrend_dec_plot,usgs_results_cont_60_trend_dec)

####################################
## Plotting results of Combined Original and Modified Version Trend #######
####################################

## Specify the factor levels in the order you want for legend
usgs_huc2_acf_sf$Trend_Result<-factor(usgs_huc2_acf_sf$Trend_Result, levels=c("increasing","no trend", "decreasing"))

#usgs_huc2_acf_sf<-usgs_huc2_acf_sf%>%
#  filter(HCDN_2009=='yes')

usgs_huc2_states_3atrend_plot <- ggplot()+
  geom_point(data=usgs_huc2_acf_sf,aes(Station_Lon,y=Station_Lat,fill=Trend_Result,shape=Trend_Result,color=Trend_Result))+
  theme_minimal() +
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  geom_sf(data = regions_sf_map, colour="grey60", fill="transparent", linetype = "solid", linewidth=0.5) +
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(24,1,25),
                     labels = paste(levels(usgs_huc2_acf_sf$Trend_Result),' (',
                                    table(usgs_huc2_acf_sf$Trend_Result),'-',
                                    round(table(usgs_huc2_acf_sf$Trend_Result)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_fill_manual(values=c('dodgerblue4','grey40','saddlebrown'),
                     labels = paste(levels(usgs_huc2_acf_sf$Trend_Result),' (',
                                    table(usgs_huc2_acf_sf$Trend_Result),'-',
                                    round(table(usgs_huc2_acf_sf$Trend_Result)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_color_manual(values=c('dodgerblue4','grey40','saddlebrown'),
                    labels = paste(levels(usgs_huc2_acf_sf$Trend_Result),' (',
                                   table(usgs_huc2_acf_sf$Trend_Result),'-',
                                   round(table(usgs_huc2_acf_sf$Trend_Result)/nrow(usgs_huc2_acf_sf)*100,1)
                                   ,'%)', sep = ""))+
  #scale_size_manual(values=c(2,3,4))+
  #scale_fill_manual(values=c(2,3,4))+
  #ggtitle(paste("Result of Trend Test for",nrow(usgs_huc2_acf_sf),"USGS stations with",years_count,"and more years")) +
  labs(y= "Latitude", x = "Longitude")+
  ggtitle(paste0("Plot of ",nrow(usgs_huc2_acf_sf%>%st_drop_geometry())," (",site_type, ") USGS  sites for alpha = ", alpha_value))+ # for the main title+
  
  labs(fill='Trend Result') +
  labs(shape='Trend Result') +
  labs(color='Trend Result') +
  coord_sf()
#usgs_huc2_states_3atrend_plot

## Change legend title
#usgs_huc2_states_3atrend_plot <- usgs_huc2_states_3atrend_plot+labs(shape="Trend")

# Right -> inside the plot area
usgs_huc2_states_3atrend_plot <-usgs_huc2_states_3atrend_plot + theme(
  legend.position = c(.97, .35),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)

## add a north symbol and a scale bar with segments of 200 km
## Load ggsn package
usgs_huc2_states_3atrend_plot <- usgs_huc2_states_3atrend_plot+
  #north(conus_sf_map, symbol=16, location = "bottomleft",scale=0.13) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(2, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = ""
    ))+
  ggsn::scalebar(conus_sf_map, dist = 1000, dist_unit = "km", 
           st.size = 3,location = "bottomleft",
           transform = TRUE, model = "WGS84")
#usgs_huc2_states_3atrend_plot
ggsave(
  paste0(folder_out,site_type,"_usgs_huc2_states_3atrend_plot.png"),
  plot = usgs_huc2_states_3atrend_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(usgs_huc2_states_3atrend_plot)

####################################
## Plotting results of Original Version Trend #######
####################################

## Specify the factor levels in the order you want for legend
usgs_huc2_acf_sf$Ori_Trend<-factor(usgs_huc2_acf_sf$Ori_Trend, levels=c("no trend","increasing", "decreasing"))

usgs_huc2_states_3btrend_ori_plot <- ggplot()+
  geom_point(data=usgs_huc2_acf_sf,aes(Station_Lon,y=Station_Lat,
                                       shape=Ori_Trend,
                                       color=Ori_Trend,
                                       fill=Ori_Trend))+
  theme_minimal() +
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  geom_sf(data = regions_sf_map, colour="grey60", fill="transparent", linetype = "solid", linewidth=0.5) +
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(1, 24, 25),
                     labels = paste(levels(usgs_huc2_acf_sf$Ori_Trend),' (',
                                    table(usgs_huc2_acf_sf$Ori_Trend),'-',
                                    round(table(usgs_huc2_acf_sf$Ori_Trend)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_color_manual(values=c('grey', 'blue', 'red'),
                     labels = paste(levels(usgs_huc2_acf_sf$Ori_Trend),' (',
                                    table(usgs_huc2_acf_sf$Ori_Trend),'-',
                                    round(table(usgs_huc2_acf_sf$Ori_Trend)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_fill_manual(values=c('grey', 'blue', 'red'),
                     labels = paste(levels(usgs_huc2_acf_sf$Ori_Trend),' (',
                                    table(usgs_huc2_acf_sf$Ori_Trend),'-',
                                    round(table(usgs_huc2_acf_sf$Ori_Trend)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  #ggtitle(paste("Result of Trend Test for",nrow(usgs_huc2_acf_sf),"USGS stations with",years_count,"and more years")) +
  labs(y= "Latitude", x = "Longitude")+
  ggtitle(paste0("Plot of ",nrow(usgs_huc2_acf_sf%>%st_drop_geometry())," (",site_type, ") USGS  sites for alpha = ", alpha_value))+ # for the main title+
  
  labs(shape='Original MK') +
  labs(color='Original MK') +
  labs(fill='Original MK') +
  coord_sf()
#usgs_huc2_states_3btrend_ori_plot

## Change legend title
#usgs_huc2_states_3btrend_ori_plot <- usgs_huc2_states_3btrend_ori_plot+labs(shape="Trend")

# Right -> inside the plot area
usgs_huc2_states_3btrend_ori_plot <-usgs_huc2_states_3btrend_ori_plot + theme(
  legend.position = c(.97, .35),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)

## add a north symbol and a scale bar with segments of 200 km
## Load ggsn package
usgs_huc2_states_3btrend_ori_plot <- usgs_huc2_states_3btrend_ori_plot+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(2, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = ""
    ))+
  ggsn::scalebar(conus_sf_map, dist = 1000, dist_unit = "km", 
                 st.size = 3,location = "bottomleft",
                 transform = TRUE, model = "WGS84")
#usgs_huc2_states_3btrend_ori_plot
ggsave(
  paste0(folder_out,site_type,"_usgs_huc2_states_3btrend_ori_plot.png"),
  plot = usgs_huc2_states_3btrend_ori_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(usgs_huc2_states_3btrend_ori_plot)

####################################
## Plotting results of HR Modified Version Trend #######
####################################

## Specify the factor levels in the order you want for legend
usgs_huc2_acf_sf$HR_Mod_Trend<-factor(usgs_huc2_acf_sf$HR_Mod_Trend, levels=c("no trend","increasing", "decreasing"))

usgs_huc2_states_3btrend_hr_plot <- ggplot()+
  geom_point(data=usgs_huc2_acf_sf,aes(Station_Lon,y=Station_Lat,
                                       shape=HR_Mod_Trend,
                                       color=HR_Mod_Trend,
                                       fill=HR_Mod_Trend,
                                       ))+
  theme_minimal() +
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  geom_sf(data = regions_sf_map, colour="grey60", fill="transparent", linetype = "solid", linewidth=0.5) +
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(1, 24, 25),
                     labels = paste(levels(usgs_huc2_acf_sf$HR_Mod_Trend),' (',
                                    table(usgs_huc2_acf_sf$HR_Mod_Trend),'-',
                                    round(table(usgs_huc2_acf_sf$HR_Mod_Trend)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_color_manual(values=c('grey', 'blue', 'red'),
                     labels = paste(levels(usgs_huc2_acf_sf$HR_Mod_Trend),' (',
                                    table(usgs_huc2_acf_sf$HR_Mod_Trend),'-',
                                    round(table(usgs_huc2_acf_sf$HR_Mod_Trend)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_fill_manual(values=c('grey', 'blue', 'red'),
                     labels = paste(levels(usgs_huc2_acf_sf$HR_Mod_Trend),' (',
                                    table(usgs_huc2_acf_sf$HR_Mod_Trend),'-',
                                    round(table(usgs_huc2_acf_sf$HR_Mod_Trend)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  #ggtitle(paste("Result of Trend Test for",nrow(usgs_huc2_acf_sf),"USGS stations with",years_count,"and more years")) +
  labs(y= "Latitude", x = "Longitude")+
  ggtitle(paste0("Plot of ",nrow(usgs_huc2_acf_sf%>%st_drop_geometry())," (",site_type, ") USGS  sites for alpha = ", alpha_value))+ # for the main title+
  
  labs(shape='Modified MK') +
  labs(color='Modified MK') +
  labs(fill='Modified MK') 
  coord_sf()
#usgs_huc2_states_3btrend_hr_plot

## Change legend title
#usgs_huc2_states_3btrend_hr_plot <- usgs_huc2_states_3btrend_hr_plot+labs(shape="Trend")

# Right -> inside the plot area
usgs_huc2_states_3btrend_hr_plot <-usgs_huc2_states_3btrend_hr_plot + theme(
  legend.position = c(.97, .35),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)

## add a north symbol and a scale bar with segments of 200 km
## Load ggsn package
usgs_huc2_states_3btrend_hr_plot <- usgs_huc2_states_3btrend_hr_plot+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(2, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = ""
    ))+
  ggsn::scalebar(conus_sf_map, dist = 1000, dist_unit = "km", 
                 st.size = 3,location = "bottomleft",
                 transform = TRUE, model = "WGS84")
#usgs_huc2_states_3btrend_hr_plot
ggsave(
  paste0(folder_out,site_type,"_usgs_huc2_states_3btrend_hr_plot.png"),
  plot = usgs_huc2_states_3btrend_hr_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(usgs_huc2_states_3btrend_hr_plot)


############################################
## Plotting USGS Stations ##################
## Significant Variance Difference##########
############################################


usgs_results_cont_60_var2<-filter(usgs_huc2_acf_sf,Var_2Divide=="not equal")

usgs_huc2_states_4var2_sitetype_plot <- ggplot()+
  geom_point(data=usgs_results_cont_60_var2,aes(Station_Lon,y=Station_Lat, 
                                                shape=site_classification,
                                                color=site_classification,
                                                fill=site_classification))+
  theme_minimal() +
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  geom_sf(data = regions_sf_map, colour="grey60", fill="transparent", linetype = "solid", linewidth=0.5) +
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(15,16,3,5)
                     ,labels = paste(levels(usgs_results_cont_60_var2$site_classification)
                                     ,' ('
                                     ,table(usgs_results_cont_60_var2$site_classification)
                                     #,'-'
                                     #,round(table(usgs_huc2_acf_sf$site_classification)/nrow(usgs_huc2_acf_sf)*100,1)
                                     #,"%"
                                     ,")"
                                     , sep = "")
                     )+
  scale_color_manual(values=c('saddlebrown','sandybrown','deepskyblue','dodgerblue3')
                     ,labels = paste(levels(usgs_results_cont_60_var2$site_classification)
                                     ,' ('
                                     ,table(usgs_results_cont_60_var2$site_classification)
                                     #,'-'
                                     #,round(table(usgs_huc2_acf_sf$site_classification)/nrow(usgs_huc2_acf_sf)*100,1)
                                     #,"%"
                                     ,")"
                                     , sep = "")
                     )+
  scale_fill_manual(values = c(15,16,3,5)
                    ,labels = paste(levels(usgs_results_cont_60_var2$site_classification)
                                    ,' ('
                                    ,table(usgs_results_cont_60_var2$site_classification)
                                    #,'-'
                                    #,round(table(usgs_huc2_acf_sf$site_classification)/nrow(usgs_huc2_acf_sf)*100,1)
                                    #,"%"
                                    ,")"
                                    , sep = "")
                    )+
  labs(shape='Site Type (Variance)') +
  labs(color='Site Type (Variance)') +
  labs(fill='Site Type (Variance)') +
  #ggtitle(paste(nrow(usgs_huc2_acf_sf),"USGS stations with atleast",years_count,"years with CONUS and HUC2 boundaries")) +
  labs(y= "Latitude", x = "Longitude")+
  ggtitle(paste0("Plot of ",nrow(usgs_results_cont_60_var2%>%st_drop_geometry())," (",site_type, ") USGS  sites for alpha = ", alpha_value))+ # for the main title+
  
  coord_sf()#xlim = c(-130, -60),
#ylim = c(23, 53),expand=FALSE)
#usgs_huc2_states_4var2_sitetype_plot

## Change legend title
#usgs_huc2_states_4var2_sitetype_plot <- usgs_huc2_states_4var2_sitetype_plot+labs(shape="Trend")

# Right -> inside the plot area
usgs_huc2_states_4var2_sitetype_plot <-usgs_huc2_states_4var2_sitetype_plot + theme(
  legend.position = c(.995, .35),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)

## add a north symbol and a scale bar with segments of 200 km
## Load ggsn package
usgs_huc2_states_4var2_sitetype_plot <- usgs_huc2_states_4var2_sitetype_plot+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(2, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = ""
    ))+
  ggsn::scalebar(conus_sf_map, dist = 1000, dist_unit = "km", 
                 st.size = 3,location = "bottomleft",
                 transform = TRUE, model = "WGS84")

#usgs_huc2_states_4var2_sitetype_plot
ggsave(
  paste0(folder_out,site_type,"_usgs_huc2_states_4var2_sitetype_plot.png"),
  plot = usgs_huc2_states_4var2_sitetype_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(usgs_huc2_states_4var2_sitetype_plot,usgs_results_cont_60_var2)


############################################
## Plotting Levene Test Result##############
## Dividing dataset into 2 equal parts######
############################################
#round(table(usgs_results$Var_2Divide)/nrow(usgs_results)*100,1)

usgs_huc2_acf_sf$Var_2Divide<-factor(usgs_huc2_acf_sf$Var_2Divide, levels=c("equal","not equal"))

usgs_huc2_states_4var2_plot <- ggplot()+
  theme_minimal() +
  geom_point(data=usgs_huc2_acf_sf,aes(Station_Lon,y=Station_Lat,
                                       shape=Var_2Divide,
                                       color=Var_2Divide,
                                       fill=Var_2Divide))+
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  geom_sf(data = regions_sf_map, colour="grey60", fill="transparent", linetype = "solid", linewidth=0.5) +
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(0, 21)
                     ,labels = paste(levels(usgs_huc2_acf_sf$Var_2Divide),' (',
                                     table(usgs_huc2_acf_sf$Var_2Divide),'-',
                                     round(table(usgs_huc2_acf_sf$Var_2Divide)/nrow(usgs_huc2_acf_sf)*100,1)
                                     ,'%)', sep = ""))+
  scale_color_manual(values=c('grey40','dodgerblue4')
                     ,labels = paste(levels(usgs_huc2_acf_sf$Var_2Divide),' (',
                                     table(usgs_huc2_acf_sf$Var_2Divide),'-',
                                     round(table(usgs_huc2_acf_sf$Var_2Divide)/nrow(usgs_huc2_acf_sf)*100,1)
                                     ,'%)', sep = ""))+
  scale_fill_manual(values=c('grey40','dodgerblue4')
                     ,labels = paste(levels(usgs_huc2_acf_sf$Var_2Divide),' (',
                                     table(usgs_huc2_acf_sf$Var_2Divide),'-',
                                     round(table(usgs_huc2_acf_sf$Var_2Divide)/nrow(usgs_huc2_acf_sf)*100,1)
                                     ,'%)', sep = ""))+
  
  #ggtitle(paste("Result of Levene Variance Test for",nrow(usgs_huc2_acf_sf),"USGS stations with",years_count,"and more years")) +
  labs(y= "Latitude", x = "Longitude")+
  ggtitle(paste0("Plot of ",nrow(usgs_huc2_acf_sf%>%st_drop_geometry())," (",site_type, ") USGS  sites for alpha = ", alpha_value))+ # for the main title+
  
  labs(shape='Variance Test') +
  labs(color='Variance Test') +
  labs(fill='Variance Test') +
  coord_sf()#xlim = c(-130, -60),
#ylim = c(23, 53),expand=FALSE)
#usgs_huc2_states_4var2_plot

## Change legend title
#usgs_huc2_states_4var2_plot <- usgs_huc2_states_4var2_plot+labs(shape="Trend")

# Right -> inside the plot area
usgs_huc2_states_4var2_plot <-usgs_huc2_states_4var2_plot + theme(
  legend.position = c(.97, .35),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)

## add a north symbol and a scale bar with segments of 200 km
## Load ggsn package
usgs_huc2_states_4var2_plot <- usgs_huc2_states_4var2_plot+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(2, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = ""
    ))+
  ggsn::scalebar(conus_sf_map, dist = 1000, dist_unit = "km", 
                 st.size = 3,location = "bottomleft",
                 transform = TRUE, model = "WGS84")

#usgs_huc2_states_4var2_plot
ggsave(
  paste0(folder_out,site_type,"_usgs_huc2_states_4var2_plot.png"),
  plot = usgs_huc2_states_4var2_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(usgs_huc2_states_4var2_plot)

############################################
## Plotting Levene Test Result##############
## Dividing dataset into 3 equal parts######
############################################
usgs_huc2_states_4var3_plot <- ggplot()+
  theme_minimal() +
  geom_point(data=usgs_huc2_acf_sf,aes(Station_Lon,y=Station_Lat,
                                       shape=Var_3Divide,
                                       color=Var_3Divide,
                                       fill=Var_3Divide))+
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  geom_sf(data = regions_sf_map, colour="grey60", fill="transparent", linetype = "solid", linewidth=0.5) +
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(0, 21),
                     labels = paste(levels(usgs_huc2_acf_sf$Var_3Divide),' (',
                                    table(usgs_huc2_acf_sf$Var_3Divide),'-',
                                    round(table(usgs_huc2_acf_sf$Var_3Divide)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_color_manual(values=c('grey','dodgerblue4'),
                     labels = paste(levels(usgs_huc2_acf_sf$Var_3Divide),' (',
                                    table(usgs_huc2_acf_sf$Var_3Divide),'-',
                                    round(table(usgs_huc2_acf_sf$Var_3Divide)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_fill_manual(values=c('grey','dodgerblue4'),
                     labels = paste(levels(usgs_huc2_acf_sf$Var_3Divide),' (',
                                    table(usgs_huc2_acf_sf$Var_3Divide),'-',
                                    round(table(usgs_huc2_acf_sf$Var_3Divide)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  #ggtitle(paste("Result of Levene Variance Test (divide dataset into 3 parts) for",nrow(usgs_huc2_acf_sf),"USGS stations with",years_count,"and more years")) +
  labs(y= "Latitude", x = "Longitude")+
  ggtitle(paste0("Plot of ",nrow(usgs_huc2_acf_sf%>%st_drop_geometry())," (",site_type, ") USGS  sites for alpha = ", alpha_value))+ # for the main title+
  
  labs(shape='Variance Test\n(3 Divide)') +
  labs(color='Variance Test\n(3 Divide)') +
  labs(fill='Variance Test\n(3 Divide)') +
  coord_sf()#xlim = c(-130, -60),
#ylim = c(23, 53),expand=FALSE)
#usgs_huc2_states_4var3_plot

## Change legend title
#usgs_huc2_states_4var3_plot <- usgs_huc2_states_4var3_plot+labs(shape="Trend")

# Right -> inside the plot area
usgs_huc2_states_4var3_plot <-usgs_huc2_states_4var3_plot + theme(
  legend.position = c(.97, .35),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)

## add a north symbol and a scale bar with segments of 200 km
## Load ggsn package
usgs_huc2_states_4var3_plot <- usgs_huc2_states_4var3_plot+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(2, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = ""
    ))+
  ggsn::scalebar(conus_sf_map, dist = 1000, dist_unit = "km", 
                 st.size = 3,location = "bottomleft",
                 transform = TRUE, model = "WGS84")

#usgs_huc2_states_4var3_plot
ggsave(
  paste0(folder_out,site_type,"_usgs_huc2_states_4var3_plot.png"),
  plot = usgs_huc2_states_4var3_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(usgs_huc2_states_4var3_plot)


############################################
## Plotting USGS Stations ##################
## Significant Variance Difference##########
############################################
usgs_results_cont_60_varcp<-filter(usgs_huc2_acf_sf,Var_CPDivide=="not equal")

usgs_huc2_states_4varcp_sitetype_plot <- ggplot()+
  geom_point(data=usgs_results_cont_60_varcp,aes(Station_Lon,y=Station_Lat, 
                                                shape=site_classification,
                                                color=site_classification,
                                                fill=site_classification))+
  theme_minimal() +
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  geom_sf(data = regions_sf_map, colour="grey60", fill="transparent", linetype = "solid", linewidth=0.5) +
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(15,16,3,5)
                     ,labels = paste(levels(usgs_results_cont_60_varcp$site_classification)
                                     ,' ('
                                     ,table(usgs_results_cont_60_varcp$site_classification)
                                     #,'-'
                                     #,round(table(usgs_huc2_acf_sf$site_classification)/nrow(usgs_huc2_acf_sf)*100,1)
                                     #,"%"
                                     ,")"
                                     , sep = "")
  )+
  scale_color_manual(values=c('saddlebrown','sandybrown','deepskyblue','dodgerblue3')
                     ,labels = paste(levels(usgs_results_cont_60_varcp$site_classification)
                                     ,' ('
                                     ,table(usgs_results_cont_60_varcp$site_classification)
                                     #,'-'
                                     #,round(table(usgs_huc2_acf_sf$site_classification)/nrow(usgs_huc2_acf_sf)*100,1)
                                     #,"%"
                                     ,")"
                                     , sep = "")
  )+
  scale_fill_manual(values = c(15,16,3,5)
                    ,labels = paste(levels(usgs_results_cont_60_varcp$site_classification)
                                    ,' ('
                                    ,table(usgs_results_cont_60_varcp$site_classification)
                                    #,'-'
                                    #,round(table(usgs_huc2_acf_sf$site_classification)/nrow(usgs_huc2_acf_sf)*100,1)
                                    #,"%"
                                    ,")"
                                    , sep = "")
  )+
  labs(shape='Site Type (Variance)') +
  labs(color='Site Type (Variance)') +
  labs(fill='Site Type (Variance)') +
  #ggtitle(paste(nrow(usgs_huc2_acf_sf),"USGS stations with atleast",years_count,"years with CONUS and HUC2 boundaries")) +
  labs(y= "Latitude", x = "Longitude")+
  ggtitle(paste0("Plot of ",nrow(usgs_results_cont_60_varcp%>%st_drop_geometry())," (",site_type, ") USGS  sites for alpha = ", alpha_value))+ # for the main title+
  
  coord_sf()#xlim = c(-130, -60),
#ylim = c(23, 53),expand=FALSE)
#usgs_huc2_states_4varcp_sitetype_plot

## Change legend title
#usgs_huc2_states_4varcp_sitetype_plot <- usgs_huc2_states_4varcp_sitetype_plot+labs(shape="Trend")

# Right -> inside the plot area
usgs_huc2_states_4varcp_sitetype_plot <-usgs_huc2_states_4varcp_sitetype_plot + theme(
  legend.position = c(.995, .35),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)

## add a north symbol and a scale bar with segments of 200 km
## Load ggsn package
usgs_huc2_states_4varcp_sitetype_plot <- usgs_huc2_states_4varcp_sitetype_plot+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(2, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = ""
    ))+
  ggsn::scalebar(conus_sf_map, dist = 1000, dist_unit = "km", 
                 st.size = 3,location = "bottomleft",
                 transform = TRUE, model = "WGS84")

#usgs_huc2_states_4varcp_sitetype_plot
ggsave(
  paste0(folder_out,site_type,"_usgs_huc2_states_4varcp_sitetype_plot.png"),
  plot = usgs_huc2_states_4varcp_sitetype_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(usgs_huc2_states_4varcp_sitetype_plot,usgs_results_cont_60_varcp)


############################################
## Plotting Levene Test Result##############
## Dividing dataset into 2 equal parts######
############################################
#round(table(usgs_results$Var_CPDivide)/nrow(usgs_results)*100,1)

usgs_huc2_acf_sf$Var_CPDivide<-factor(usgs_huc2_acf_sf$Var_CPDivide, levels=c("equal","not equal"))

usgs_huc2_states_4varcp_plot <- ggplot()+
  theme_minimal() +
  geom_point(data=usgs_huc2_acf_sf,aes(Station_Lon,y=Station_Lat,
                                       shape=Var_CPDivide,
                                       color=Var_CPDivide,
                                       fill=Var_CPDivide))+
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  geom_sf(data = regions_sf_map, colour="grey60", fill="transparent", linetype = "solid", linewidth=0.5) +
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(0, 21)
                     ,labels = paste(levels(usgs_huc2_acf_sf$Var_CPDivide),' (',
                                     table(usgs_huc2_acf_sf$Var_CPDivide),'-',
                                     round(table(usgs_huc2_acf_sf$Var_CPDivide)/nrow(usgs_huc2_acf_sf)*100,1)
                                     ,'%)', sep = ""))+
  scale_color_manual(values=c('grey','dodgerblue4')
                     ,labels = paste(levels(usgs_huc2_acf_sf$Var_CPDivide),' (',
                                     table(usgs_huc2_acf_sf$Var_CPDivide),'-',
                                     round(table(usgs_huc2_acf_sf$Var_CPDivide)/nrow(usgs_huc2_acf_sf)*100,1)
                                     ,'%)', sep = ""))+
  scale_fill_manual(values=c('grey','dodgerblue4')
                    ,labels = paste(levels(usgs_huc2_acf_sf$Var_CPDivide),' (',
                                    table(usgs_huc2_acf_sf$Var_CPDivide),'-',
                                    round(table(usgs_huc2_acf_sf$Var_CPDivide)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  
  #ggtitle(paste("Result of Levene Variance Test for",nrow(usgs_huc2_acf_sf),"USGS stations with",years_count,"and more years")) +
  labs(y= "Latitude", x = "Longitude")+
  ggtitle(paste0("Plot of ",nrow(usgs_huc2_acf_sf%>%st_drop_geometry())," (",site_type, ") USGS  sites for alpha = ", alpha_value))+ # for the main title+
  
  labs(shape='Variance Test') +
  labs(color='Variance Test') +
  labs(fill='Variance Test') +
  coord_sf()#xlim = c(-130, -60),
#ylim = c(23, 53),expand=FALSE)
#usgs_huc2_states_4varcp_plot

## Change legend title
#usgs_huc2_states_4varcp_plot <- usgs_huc2_states_4varcp_plot+labs(shape="Trend")

# Right -> inside the plot area
usgs_huc2_states_4varcp_plot <-usgs_huc2_states_4varcp_plot + theme(
  legend.position = c(.97, .35),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)

## add a north symbol and a scale bar with segments of 200 km
## Load ggsn package
usgs_huc2_states_4varcp_plot <- usgs_huc2_states_4varcp_plot+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(2, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = ""
    ))+
  ggsn::scalebar(conus_sf_map, dist = 1000, dist_unit = "km", 
                 st.size = 3,location = "bottomleft",
                 transform = TRUE, model = "WGS84")

#usgs_huc2_states_4varcp_plot
ggsave(
  paste0(folder_out,site_type,"_usgs_huc2_states_4varcp_plot.png"),
  plot = usgs_huc2_states_4varcp_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(usgs_huc2_states_4varcp_plot)


############################################
## Plotting Levene Test Result##############
## Dividing dataset into 2 equal parts######
############################################
#round(table(usgs_results$Var_CPDivide)/nrow(usgs_results)*100,1)

usgs_huc2_acf_sf$Var_CPDivide<-factor(usgs_huc2_acf_sf$Var_CPDivide, levels=c("equal","not equal"))

usgs_huc2_acf_sf<-mutate(usgs_huc2_acf_sf,Var_Combined=ifelse(CP_decision=="ChangePoint",Var_CPDivide,"check"))

usgs_huc2_states_4varcp_plot <- ggplot()+
  theme_minimal() +
  geom_point(data=usgs_huc2_acf_sf,aes(Station_Lon,y=Station_Lat,
                                       shape=Var_CPDivide,
                                       color=Var_CPDivide,
                                       fill=Var_CPDivide))+
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  geom_sf(data = regions_sf_map, colour="grey60", fill="transparent", linetype = "solid", linewidth=0.5) +
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(0, 21)
                     ,labels = paste(levels(usgs_huc2_acf_sf$Var_CPDivide),' (',
                                     table(usgs_huc2_acf_sf$Var_CPDivide),'-',
                                     round(table(usgs_huc2_acf_sf$Var_CPDivide)/nrow(usgs_huc2_acf_sf)*100,1)
                                     ,'%)', sep = ""))+
  scale_color_manual(values=c('grey','dodgerblue4')
                     ,labels = paste(levels(usgs_huc2_acf_sf$Var_CPDivide),' (',
                                     table(usgs_huc2_acf_sf$Var_CPDivide),'-',
                                     round(table(usgs_huc2_acf_sf$Var_CPDivide)/nrow(usgs_huc2_acf_sf)*100,1)
                                     ,'%)', sep = ""))+
  scale_fill_manual(values=c('grey','dodgerblue4')
                    ,labels = paste(levels(usgs_huc2_acf_sf$Var_CPDivide),' (',
                                    table(usgs_huc2_acf_sf$Var_CPDivide),'-',
                                    round(table(usgs_huc2_acf_sf$Var_CPDivide)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  
  #ggtitle(paste("Result of Levene Variance Test for",nrow(usgs_huc2_acf_sf),"USGS stations with",years_count,"and more years")) +
  labs(y= "Latitude", x = "Longitude")+
  ggtitle(paste0("Plot of ",nrow(usgs_huc2_acf_sf%>%st_drop_geometry())," (",site_type, ") USGS  sites for alpha = ", alpha_value))+ # for the main title+
  
  labs(shape='Variance Test') +
  labs(color='Variance Test') +
  labs(fill='Variance Test') +
  coord_sf()#xlim = c(-130, -60),
#ylim = c(23, 53),expand=FALSE)
#usgs_huc2_states_4varcp_plot

## Change legend title
#usgs_huc2_states_4varcp_plot <- usgs_huc2_states_4varcp_plot+labs(shape="Trend")

# Right -> inside the plot area
usgs_huc2_states_4varcp_plot <-usgs_huc2_states_4varcp_plot + theme(
  legend.position = c(.97, .35),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)

## add a north symbol and a scale bar with segments of 200 km
## Load ggsn package
usgs_huc2_states_4varcp_plot <- usgs_huc2_states_4varcp_plot+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(2, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = ""
    ))+
  ggsn::scalebar(conus_sf_map, dist = 1000, dist_unit = "km", 
                 st.size = 3,location = "bottomleft",
                 transform = TRUE, model = "WGS84")

#usgs_huc2_states_4varcp_plot
ggsave(
  paste0(folder_out,site_type,"_usgs_huc2_states_4varcp_plot.png"),
  plot = usgs_huc2_states_4varcp_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(usgs_huc2_states_4varcp_plot)

#######################################
### Plotting of Stationarity Results###
#######################################


## Specify the factor levels in the order you want for legend
usgs_huc2_acf_sf$Stationarity_MAR<-factor(usgs_huc2_acf_sf$Stationarity_MAR, levels=c("S","NS"))

usgs_huc2_states_5bstat_mar_plot <- ggplot() + 
  theme_minimal() +
  geom_point(data=usgs_huc2_acf_sf, aes(x=Station_Lon,y=Station_Lat,
                                        shape=Stationarity_MAR,
                                        color=Stationarity_MAR,
                                        fill=Stationarity_MAR
                                        ))+
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  geom_sf(data = regions_sf_map, colour="grey60", fill="transparent", linetype = "solid", linewidth=0.5) +
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(0, 21),
                     labels = paste(levels(usgs_huc2_acf_sf$Stationarity_MAR),' (',
                                    table(usgs_huc2_acf_sf$Stationarity_MAR),'-',
                                    round(table(usgs_huc2_acf_sf$Stationarity_MAR)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_color_manual(values=c('grey','dodgerblue4'),
                     labels = paste(levels(usgs_huc2_acf_sf$Stationarity_MAR),' (',
                                    table(usgs_huc2_acf_sf$Stationarity_MAR),'-',
                                    round(table(usgs_huc2_acf_sf$Stationarity_MAR)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_fill_manual(values=c('grey','dodgerblue4'),
                     labels = paste(levels(usgs_huc2_acf_sf$Stationarity_MAR),' (',
                                    table(usgs_huc2_acf_sf$Stationarity_MAR),'-',
                                    round(table(usgs_huc2_acf_sf$Stationarity_MAR)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  #ggtitle(paste("Stationarity Results (Majority Rule Approach) for",nrow(usgs_huc2_acf_sf),"USGS stations with",years_count,"and more years")) + 
  labs(y= "Latitude", x = "Longitude")+
  ggtitle(paste0("Plot of ",nrow(usgs_huc2_acf_sf%>%st_drop_geometry())," (",site_type, ") USGS  sites for alpha = ", alpha_value))+ # for the main title+
  
  ## Change legend title
  labs(shape='Stationarity (MAR)') +
  labs(color='Stationarity (MAR)') +
  labs(fill='Stationarity (MAR)') +
 
  coord_sf()

# Right -> inside the plot area
usgs_huc2_states_5bstat_mar_plot <-usgs_huc2_states_5bstat_mar_plot + theme(
  legend.position = c(.97, .35),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)
## add a north symbol and a scale bar with segments of 200 km
## Load ggsn package
usgs_huc2_states_5bstat_mar_plot <- usgs_huc2_states_5bstat_mar_plot+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(2, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = ""
    ))+
  ggsn::scalebar(conus_sf_map, dist = 1000, dist_unit = "km", 
                 st.size = 3,location = "bottomleft",
                 transform = TRUE, model = "WGS84")
#usgs_huc2_states_5bstat_mar_plot
ggsave(
  paste0(folder_out,site_type,"_usgs_huc2_states_5bstat_mar_plot.png"),
  plot = usgs_huc2_states_5bstat_mar_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(usgs_huc2_states_5bstat_mar_plot)
#-------------
## Specify the factor levels in the order you want for legend
usgs_huc2_acf_sf$Stationarity_AIN<-factor(usgs_huc2_acf_sf$Stationarity_AIN, levels=c("S","NS"))

usgs_huc2_states_5cstat_ain_plot <- ggplot() + 
  theme_minimal() +
  geom_point(data=usgs_huc2_acf_sf, aes(x=Station_Lon,y=Station_Lat,
                                        shape=Stationarity_AIN,
                                        color=Stationarity_AIN,
                                        fill=Stationarity_AIN))+
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  geom_sf(data = regions_sf_map, colour="grey60", fill="transparent", linetype = "solid", linewidth=0.5) +
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(0, 21),
                     labels = paste(levels(usgs_huc2_acf_sf$Stationarity_AIN),' (',
                                    table(usgs_huc2_acf_sf$Stationarity_AIN),'-',
                                    round(table(usgs_huc2_acf_sf$Stationarity_AIN)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_color_manual(values=c('grey','dodgerblue4'),
                     labels = paste(levels(usgs_huc2_acf_sf$Stationarity_AIN),' (',
                                    table(usgs_huc2_acf_sf$Stationarity_AIN),'-',
                                    round(table(usgs_huc2_acf_sf$Stationarity_AIN)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_fill_manual(values=c('grey','dodgerblue4'),
                     labels = paste(levels(usgs_huc2_acf_sf$Stationarity_AIN),' (',
                                    table(usgs_huc2_acf_sf$Stationarity_AIN),'-',
                                    round(table(usgs_huc2_acf_sf$Stationarity_AIN)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  #ggtitle(paste("Stationarity Results (Majority Rule Approach) for",nrow(usgs_huc2_acf_sf),"USGS stations with",years_count,"and more years")) + 
  labs(y= "Latitude", x = "Longitude")+
  ggtitle(paste0("Plot of ",nrow(usgs_huc2_acf_sf%>%st_drop_geometry())," (",site_type, ") USGS  sites for alpha = ", alpha_value))+ # for the main title+
  
  ## Change legend title
  labs(shape='Stationarity (AIN)') +
  labs(color='Stationarity (AIN)') +
  labs(fill='Stationarity (AIN)') +
  coord_sf()

# Right -> inside the plot area
usgs_huc2_states_5cstat_ain_plot <-usgs_huc2_states_5cstat_ain_plot + theme(
  legend.position = c(.97, .35),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)

## add a north symbol and a scale bar with segments of 1000 km
## Load ggsn package
usgs_huc2_states_5cstat_ain_plot <- usgs_huc2_states_5cstat_ain_plot+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(2, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = ""
    ))+
  ggsn::scalebar(conus_sf_map, dist = 1000, dist_unit = "km", 
                 st.size = 3,location = "bottomleft",
                 transform = TRUE, model = "WGS84")
#usgs_huc2_states_5cstat_ain_plot
ggsave(
  paste0(folder_out,site_type,"_usgs_huc2_states_5cstat_ain_plot.png"),
  plot = usgs_huc2_states_5cstat_ain_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)

rm(usgs_huc2_states_5cstat_ain_plot)

#################################
###Plot of ADF Test##############
#################################

## Specify the factor levels in the order you want for legend
usgs_huc2_acf_sf$ADF_Result<-factor(usgs_huc2_acf_sf$ADF_Result, levels=c("S","NS"))

usgs_huc2_states_5astat_adf_plot <- ggplot() + 
  theme_minimal() +
  geom_point(data=usgs_huc2_acf_sf, aes(x=Station_Lon,y=Station_Lat,
                                        shape=ADF_Result,
                                        color=ADF_Result,
                                        fill=ADF_Result))+
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  geom_sf(data = regions_sf_map, colour="grey60", fill="transparent", linetype = "solid", linewidth=0.5) +
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(0, 21),
                     labels = paste(levels(usgs_huc2_acf_sf$ADF_Result),' (',
                                    table(usgs_huc2_acf_sf$ADF_Result),'-',
                                    round(table(usgs_huc2_acf_sf$ADF_Result)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_color_manual(values=c('grey','dodgerblue4'),
                     labels = paste(levels(usgs_huc2_acf_sf$ADF_Result),' (',
                                    table(usgs_huc2_acf_sf$ADF_Result),'-',
                                    round(table(usgs_huc2_acf_sf$ADF_Result)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_fill_manual(values=c('grey','dodgerblue4'),
                     labels = paste(levels(usgs_huc2_acf_sf$ADF_Result),' (',
                                    table(usgs_huc2_acf_sf$ADF_Result),'-',
                                    round(table(usgs_huc2_acf_sf$ADF_Result)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  #ggtitle(paste("Stationarity Results (Majority Rule Approach) for",nrow(usgs_huc2_acf_sf),"USGS stations with",years_count,"and more years")) + 
  labs(y= "Latitude", x = "Longitude")+
  ggtitle(paste0("Plot of ",nrow(usgs_huc2_acf_sf%>%st_drop_geometry())," (",site_type, ") USGS  sites for alpha = ", alpha_value))+ # for the main title+
  
  ## Change legend title
  labs(shape='ADF Test') +
  labs(color='ADF Test') +
  labs(fill='ADF Test') +
  coord_sf()
#usgs_huc2_states_5astat_adf_plot
# Right -> inside the plot area
usgs_huc2_states_5astat_adf_plot <-usgs_huc2_states_5astat_adf_plot + theme(
  legend.position = c(.97, .35),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)
## add a north symbol and a scale bar with segments of 200 km
## Load ggsn package
usgs_huc2_states_5astat_adf_plot <- usgs_huc2_states_5astat_adf_plot+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(2, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = ""
    ))+
  ggsn::scalebar(conus_sf_map, dist = 1000, dist_unit = "km", 
                 st.size = 3,location = "bottomleft",
                 transform = TRUE, model = "WGS84")
#usgs_huc2_states_5astat_adf_plot
ggsave(
  paste0(folder_out,site_type,"_usgs_huc2_states_5astat_adf_plot.png"),
  plot = usgs_huc2_states_5astat_adf_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(usgs_huc2_states_5astat_adf_plot)

#################################
###Plot of KPSS Test##############
#################################

## Specify the factor levels in the order you want for legend
usgs_huc2_acf_sf$KPSS_Result<-factor(usgs_huc2_acf_sf$KPSS_Result, levels=c("S","NS"))

usgs_huc2_states_5astat_kpss_plot <- ggplot() + 
  theme_minimal() +
  geom_point(data=usgs_huc2_acf_sf, aes(x=Station_Lon,y=Station_Lat,
                                        shape=KPSS_Result,
                                        color=KPSS_Result,
                                        fill=KPSS_Result))+
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  geom_sf(data = regions_sf_map, colour="grey60", fill="transparent", linetype = "solid", linewidth=0.5) +
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(0, 21),
                     labels = paste(levels(usgs_huc2_acf_sf$KPSS_Result),' (',
                                    table(usgs_huc2_acf_sf$KPSS_Result),'-',
                                    round(table(usgs_huc2_acf_sf$KPSS_Result)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_color_manual(values=c('grey40','dodgerblue4'),
                     labels = paste(levels(usgs_huc2_acf_sf$KPSS_Result),' (',
                                    table(usgs_huc2_acf_sf$KPSS_Result),'-',
                                    round(table(usgs_huc2_acf_sf$KPSS_Result)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_fill_manual(values=c('grey40','dodgerblue4'),
                     labels = paste(levels(usgs_huc2_acf_sf$KPSS_Result),' (',
                                    table(usgs_huc2_acf_sf$KPSS_Result),'-',
                                    round(table(usgs_huc2_acf_sf$KPSS_Result)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  #ggtitle(paste("Stationarity Results (Majority Rule Approach) for",nrow(usgs_huc2_acf_sf),"USGS stations with",years_count,"and more years")) + 
  labs(y= "Latitude", x = "Longitude")+
  ggtitle(paste0("Plot of ",nrow(usgs_huc2_acf_sf%>%st_drop_geometry())," (",site_type, ") USGS  sites for alpha = ", alpha_value))+ # for the main title+
  
  ## Change legend title
  labs(shape='KPSS Test') +
  labs(color='KPSS Test') +
  labs(fill='KPSS Test') +
  
  coord_sf()
#usgs_huc2_states_5astat_kpss_plot
# Right -> inside the plot area
usgs_huc2_states_5astat_kpss_plot <-usgs_huc2_states_5astat_kpss_plot + theme(
  legend.position = c(.97, .35),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)
## add a north symbol and a scale bar with segments of 200 km
## Load ggsn package
usgs_huc2_states_5astat_kpss_plot <- usgs_huc2_states_5astat_kpss_plot+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(2, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = ""
    ))+
  ggsn::scalebar(conus_sf_map, dist = 1000, dist_unit = "km", 
                 st.size = 3,location = "bottomleft",
                 transform = TRUE, model = "WGS84")
#usgs_huc2_states_5astat_kpss_plot
ggsave(
  paste0(folder_out,site_type,"_usgs_huc2_states_5astat_kpss_plot.png"),
  plot = usgs_huc2_states_5astat_kpss_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(usgs_huc2_states_5astat_kpss_plot)

#################################
###Plot of PP Test##############
#################################

## Specify the factor levels in the order you want for legend
usgs_huc2_acf_sf$PP_Result<-factor(usgs_huc2_acf_sf$PP_Result, levels=c("S","NS"))

usgs_huc2_states_5astat_pp_plot <- ggplot() + 
  theme_minimal() +
  geom_point(data=usgs_huc2_acf_sf, aes(x=Station_Lon,y=Station_Lat,
                                        shape=PP_Result,
                                        color=PP_Result,
                                        fill=PP_Result))+
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  geom_sf(data = regions_sf_map, colour="grey60", fill="transparent", linetype = "solid", linewidth=0.5) +
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(0, 21),
                     labels = paste(levels(usgs_huc2_acf_sf$PP_Result),' (',
                                    table(usgs_huc2_acf_sf$PP_Result),'-',
                                    round(table(usgs_huc2_acf_sf$PP_Result)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_color_manual(values=c('grey','dodgerblue4'),
                     labels = paste(levels(usgs_huc2_acf_sf$PP_Result),' (',
                                    table(usgs_huc2_acf_sf$PP_Result),'-',
                                    round(table(usgs_huc2_acf_sf$PP_Result)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_fill_manual(values=c('grey','dodgerblue4'),
                     labels = paste(levels(usgs_huc2_acf_sf$PP_Result),' (',
                                    table(usgs_huc2_acf_sf$PP_Result),'-',
                                    round(table(usgs_huc2_acf_sf$PP_Result)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  #ggtitle(paste("Stationarity Results (Majority Rule Approach) for",nrow(usgs_huc2_acf_sf),"USGS stations with",years_count,"and more years")) + 
  labs(y= "Latitude", x = "Longitude")+
  ggtitle(paste0("Plot of ",nrow(usgs_huc2_acf_sf%>%st_drop_geometry())," (",site_type, ") USGS  sites for alpha = ", alpha_value))+ # for the main title+
  
  ## Change legend title
  labs(shape='PP Test') +
  labs(color='PP Test') +
  labs(fill='PP Test') +
  coord_sf()
#usgs_huc2_states_5astat_kpss_plot
# Right -> inside the plot area
usgs_huc2_states_5astat_pp_plot <-usgs_huc2_states_5astat_pp_plot + theme(
  legend.position = c(.97, .35),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)
## add a north symbol and a scale bar with segments of 200 km
## Load ggsn package
usgs_huc2_states_5astat_pp_plot <- usgs_huc2_states_5astat_pp_plot+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(2, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = ""
    ))+
  ggsn::scalebar(conus_sf_map, dist = 1000, dist_unit = "km", 
                 st.size = 3,location = "bottomleft",
                 transform = TRUE, model = "WGS84")
#usgs_huc2_states_5astat_pp_plot
ggsave(
  paste0(folder_out,site_type,"_usgs_huc2_states_5astat_pp_plot.png"),
  plot = usgs_huc2_states_5astat_pp_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(usgs_huc2_states_5astat_pp_plot)

write.csv(usgs_huc2_acf_sf%>%st_drop_geometry(),paste0(folder_out,site_type,"_usgs_huc2_acf_sf.csv"), row.names = FALSE)

}

usgs_results<-usgs_huc2_acf_sf_full
#rm(usgs_huc2_acf_sf)
rm(years_count)
rm(conus_sf,conus_sf_map)
rm(huc2_df,huc2_sf,huc2_sf_map)
#rm(folder_out,username)

 #main loop ends
#ggarrange(futu_1trend_plot1, futu_1trend_plot2, futu_1trend_plot3, ncol = 1, nrow = 1)


first <-
  data.frame(
    "1" = c('0.44','0.554','0.67','0.64'),
    "2" = c('0.124','0.22','0.82','0.994'),
    "3" = c('0.82','1.22','0.73','1.23')
  )

second <-
  data.frame(
    "1" = runif(4),
    "2" = runif(4),
    "3" = runif(4),
    "d" = runif(4),
    "e" = runif(4)
  )

second[setdiff(names(second), names(first))]

colnames(usgs_results)
usgs_results_filtered<-usgs_results%>%
  select(USGS_Station,Calc_Count,ACF_Lag1,Trend_Result,Var_2Divide,KPSS_Result)
rm(usgs_results)

a<-usgs_results_filtered%>%
  filter(ACF_Lag1=='significant')%>%
  filter(Trend_Result%in%c('increasing','decreasing'))%>%
  filter(Var_2Divide=='not equal')%>%
  filter(KPSS_Result=='S')

b<-usgs_results_filtered%>%
  filter(ACF_Lag1=='insignificant')%>%
  filter(Trend_Result%in%c('no trend'))%>%
  filter(Var_2Divide=='equal')%>%
  filter(KPSS_Result=='NS')

rm(a,b)
rm(first,second)
rm(usgs_results_filtered)


data_cp<-usgs_huc2_acf_sf%>%
  filter(CP_decision=="ChangePoint")

cate_plot<-ggplot(data_cp, 
       aes(x = HUC2, 
           y = Change_Point_year)) +
  geom_violin(fill = "cornflowerblue") +
  geom_boxplot(width = .2, 
               fill = "orange",
               outlier.color = "orange",
               outlier.size = 2) +
  ylab("Year of Change Point")+
  xlab("HUC02 Regions")+
  #ylim(1900,2020)+
  scale_y_continuous(breaks=seq(1900,2020,10))+
  theme(axis.text.x = element_text(#face="bold", 
                                   #color="#993333",
                                   #size=14, 
                                   angle=0))
#+   stat_bin(aes(y=..count.., label=..count..), geom="text")
  
  

  #labs(title = "Change Point Year by HUC02 Region")

ggsave(
  paste0(folder_out,site_type,"_cate_plot.png"),
  plot = cate_plot,
  width = 300,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(cate_plot, data_cp)

##Increasing
data_increase<-usgs_huc2_acf_sf%>%
  filter(Trend_Result=="increasing",
         CP_decision=="ChangePoint")

cate_plot<-ggplot(data_increase, 
                  aes(x = HUC2, 
                      y = Change_Point_year)) +
  #geom_violin(fill = "cornflowerblue") +
  geom_boxplot(width = .2, 
               fill = "orange",
               outlier.color = "orange",
               outlier.size = 2) +
  ylab("Year of Change Point")+
  xlab("HUC02 Regions")+
  #ylim(1900,2020)+
  scale_y_continuous(breaks=seq(1900,2020,10))+
  theme(axis.text.x = element_text(#face="bold", 
    #color="#993333",
    #size=14, 
    angle=45))
#+  stat_bin(aes(y=..count.., label=..count..), geom="text")



#labs(title = "Change Point Year by HUC02 Region")

ggsave(
  paste0(folder_out,site_type,"_cate_plot_increase.png"),
  plot = cate_plot,
  width = 300,
  height = 225,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(cate_plot,data_increase)


##Decreasing
data_decrease<-usgs_huc2_acf_sf

data_decrease$HUC2<-as.factor(data_decrease$HUC2)
data_decrease<-usgs_huc2_acf_sf%>%
  st_drop_geometry()%>%
  filter(Trend_Result=="decreasing",
         CP_decision=="ChangePoint")%>%
  select(Change_Point_year,HUC2)

  
#data_decrease[nrow(data_decrease)+1,] <- c('1850','09')

#data_decrease %>% add_row(Change_Point_year = 1880, HUC2 = 09)
data_decrease$HUC2<-as.factor(data_decrease$HUC2)

cate_plot<-ggplot(data_decrease, 
                  aes(x = HUC2, 
                      y = Change_Point_year)) +
  geom_violin(fill = "cornflowerblue") +
  geom_boxplot(width = .2, 
               fill = "orange",
               outlier.color = "orange",
               outlier.size = 2) +
  ylab("Year of Change Point")+
  xlab("HUC02 Regions")+
  #ylim(1900,2020)+
  scale_y_continuous(breaks=seq(1900,2020,10))+
  theme(axis.text.x = element_text(#face="bold", 
    #color="#993333",
    #size=14, 
    angle=45))
#+  stat_bin(aes(y=..count.., label=..count..), geom="text")



#labs(title = "Change Point Year by HUC02 Region")

ggsave(
  paste0(folder_out,site_type,"_cate_plot_decrease.png"),
  plot = cate_plot,
  width = 300,
  height = 225,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(cate_plot,data_decrease)


#### Check
a<-usgs_huc2_acf_sf%>%
  filter(!HUC2%in%c('01','02','03','04',
                     '05',
                     '06','07','08','09','10',
                    '11','12','13','14','15','16','17','18'))
#st_write(a,paste0(folder_out,site_type,"_check.shp"))

