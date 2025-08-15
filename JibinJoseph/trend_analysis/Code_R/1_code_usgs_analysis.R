## 2024-08-15
## Removed regions_map_sf

## 2024-08-09
## As the legend in varying magnitude plot in for loop was moved outside, north arrow overlapped
## on Michigan area. So pad_x and pad_y was modified


## 2024-08-08
## Added in for loop for creating the figures:
## legend.position = "right",
## legend.box.margin = margin(0, 20, 0, 0)
## legend.title = element_text(size = 14),  # Title size
## legend.text = element_text(size = 12),   # Label size
## legend.key.size = unit(1.5, "lines")     # Key box size

## 2024-08-09
## Added "dir.create(folder_out, showWarnings = TRUE)"
## Changed MK_Combi_slope to Trend_slope
## BE CAREFUL ### THINK WHETHER YOU NEED THIS WHEN YOU USE 1981 to 2021
## usgs_results<-filter(usgs_results_cont,Calc_Count>=years_count)

## 2024-07-03
## Modified the code to grab 0 in USGS code for two instances of reading csv file
## Save the reference stations into a different folder

## 2024-01-19
## Add textbox in summary plot
## Removed NCA boundaries from all figures

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


library(colorspace)

## Clear all variables
rm(list=ls())

## Clear Console
cat("\014")

alpha_value="0.05"

year_usgs_start='all' #(Options: "all", "1981", "1950")
year_usgs_end='all'   #(Options: "all", "2021", "2014")

## Change it as required
folder_name_absolute<-"C:\\Users\\joseph57\\Box\\01_SharedBox_Jibin\\JournalDataRepo\\"

setwd(paste0(folder_name_absolute,"\\Code_R"))

## Basic Parameters

## Plots Output Folder
folder_out<-paste0(folder_name_absolute,"Plots_IntermediateResults\\plots_usgs_analysis_v9c_",
                   year_usgs_start,
                   "_",
                   year_usgs_end,
                   "\\")
dir.create(folder_out, showWarnings = TRUE)


fileConn<-file(paste0(folder_out,"00_code_full_path.txt"))
writeLines(rstudioapi::getSourceEditorContext()$path, 
           fileConn)
close(fileConn)
rm(fileConn)

years_count=60

## Read the shapefile as sf object``
huc2_sf<-st_read(paste0(folder_name_absolute,"/Data/Shapefiles/HUC2_modified/HUC2_modified5.shp"))
huc2_sf_map<-st_read(paste0(folder_name_absolute,"/Data/Shapefiles/HUC2_modified/HUC2_modified6.shp"))

conus_sf<-st_read(paste0(folder_name_absolute,"/Data/Shapefiles/cb_2018_us_conus_5m/cb_2018_us_conus_5m.shp"))
conus_sf_map<-st_read(paste0(folder_name_absolute,"/Data/Shapefiles/states_conterminous/states_conterminous.shp"))


## Display colnames of huc2_sf
huc2_df <-huc2_sf%>%
  st_drop_geometry()
colnames(huc2_df)


## Read Metadata: CRS
st_crs(huc2_sf)
st_crs(huc2_sf_map)
st_crs(conus_sf)
st_crs(conus_sf_map)


## Read Metadata: geometry type
st_geometry_type(huc2_sf)


## Change the projection to WGS 84
#huc2_sf<-st_set_crs(huc2_sf,4326)
#huc2_sf<-st_transform(huc2_sf, "+proj=longlat +datum=WGS84")

##Check the projection
st_crs(huc2_sf)

## Read Metadata: extent
st_bbox(huc2_sf)


## Display colnames of huc2_sf
huc2_sf%>%
  st_drop_geometry()%>%
  colnames()
####################################
## Read trend-stationary results####
####################################

if (year_usgs_start=="all" | year_usgs_end=="all"){
                            usgs_results_30<-read.csv(paste0("./USGS_Results/",
                                                             "B_Results_Trend_Stationarity_",
                                                             year_usgs_start,"_",year_usgs_end,
                                                             #"_missing_5percent",
                                                             "_alpha_0.05.txt"),
                                                      sep='\t',
                                                      colClasses = c("USGS_Station" = "character"))
                            }else{
                                  usgs_results_30<-read.csv(paste0("./USGS_Results/",
                                                                   "B_Results_Trend_Stationarity_",
                                                                   year_usgs_start,"_",year_usgs_end,
                                                                   "_missing_5percent",
                                                                   "_alpha_0.05.txt"),
                                                            sep='\t',
                                                            colClasses = c("USGS_Station" = "character"))
                                  }                            
  

colnames(usgs_results_30)


## Filter for conterminous US
usgs_results_cont<- usgs_results_30 %>% 
  filter(!(State_Name %in% c("AK","HI","PR")))

#usgs_results_cont<- usgs_results_30 %>% 
  #filter(State_Name %in% c("AL","AZ","AR","CA","CO","CT","DE","FL","GA","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"))

### BE CAREFUL ### THINK WHETHER YOU NEED THIS WHEN YOU USE 1981 to 2021
if (year_usgs_start=="all" | year_usgs_end=="all"){
  usgs_results<-filter(usgs_results_cont,Calc_Count>=years_count)
  legend_title<-'Site Type (\u2265 60 years)'
}else{
  usgs_results<-usgs_results_cont
  legend_title<-paste0('Site Type\n(',year_usgs_start,' to ', year_usgs_end,')')
  }
#



#station_vector<-usgs_results$USGS_Station
#new_df <- usgs_results_old %>% filter(!USGS_Station %in% station_vector)
#write.csv(new_df,paste0(folder_out,"_leftout_sites.csv"))

head(usgs_results)
colnames(usgs_results)

rm(usgs_results_30,usgs_results_cont)

hcdn_gages_df<-read.csv(paste0(folder_name_absolute,"/Data_Intermediate/hcdn_gages_df.csv"),
                        colClasses = c("STAID" = "character"))

x<-usgs_results
y<-hcdn_gages_df

usgs_results_30_merged_result<-merge(x, y, 
                                     by.x = "USGS_Station", by.y = "STAID", all.x = TRUE, all.y = FALSE)

write.csv(usgs_results_30_merged_result,
          paste0(folder_name_absolute,"/Data_Intermediate/merged_result_usgs_analysis.csv"), 
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


data_ref_select<-usgs_huc2_acf_sf_full%>%
  dplyr::filter(CLASS=='Ref')

write.csv(data_ref_select%>%
            st_drop_geometry()%>%
            dplyr::select(USGS_Station,Station_Name),
          paste0(folder_name_absolute,"\\Plots_IntermediateResults\\plots_landuse_analysis\\"
                 ,"ref_stations.csv"),
          row.names = FALSE)

############################################
## Table 1 ##################
## Statistics ####################
############################################

tot<-nrow(usgs_huc2_acf_sf_full)

#######################################################
###Table 4 for Trend Analysis ##########################
#######################################################
df<-usgs_huc2_acf_sf_full %>% st_drop_geometry()
a<- df %>% count(MK_Combi_Trend, sort = TRUE)
a$MK_Combi_Trend<-factor(a$MK_Combi_Trend, levels=c("no trend","increasing", "decreasing"))
a$percent<-round(a$n/nrow(df)*100,1)
print(nrow(df))
print("All USGS sites")
print(a)
print((a$n/tot*100))

df1<-df %>% 
  filter(HCDN_2009 == "yes")

a<-df %>% 
  filter(HCDN_2009 == "yes")%>%
  count(MK_Combi_Trend, sort = TRUE)
a$percent<-round(a$n/nrow(df1)*100,1)
print("HCDN Sites")
print(nrow(df1))
print(a)

df1<-df %>% 
  filter(CLASS == "Ref")
a<-df %>% 
  filter(CLASS == "Ref")%>%
  count(MK_Combi_Trend, sort = TRUE)
a$percent<-round(a$n/nrow(df1)*100,1)
print("GAGES II Reference")
print(nrow(df1))
print(a)

## GAGES II non-reference 
df1<-df %>% 
  filter(CLASS == "Non-ref")
a<-df %>% 
  filter(CLASS == "Non-ref")%>%
  count(MK_Combi_Trend, sort = TRUE)
a$percent<-round(a$n/nrow(df1)*100,1)
print("GAGES II non-reference")
print(nrow(df1))
print(a)

## Other USGS Sites
df1<-df %>% 
  filter(!HCDN_2009 %in% c("yes") & !CLASS %in% c("Ref","Non-ref"))
a<-df %>% 
  filter(!HCDN_2009 %in% c("yes") & !CLASS %in% c("Ref","Non-ref"))%>%
  count(MK_Combi_Trend, sort = TRUE)
a$percent<-round(a$n/nrow(df1)*100,1)
print("Other USGS Sites")
print(nrow(df1))
print(a)

rm(a,df1)

#######################################################
###Table for Variance ##########################
#######################################################
a<-df %>% count(Var_2Divide, sort = TRUE)

a$percent<-round(a$n/nrow(df)*100,1)
print("All USGS sites")
print(a)


df1<-df %>% 
  filter(HCDN_2009 == "yes")
a<-df %>% 
  filter(HCDN_2009 == "yes")%>%
  count(Var_2Divide, sort = TRUE)
a$percent<-round(a$n/nrow(df1)*100,1)
print("HCDN Sites")
print(a)

df1<-df %>% 
  filter(CLASS == "Ref")
a<-df %>% 
  filter(CLASS == "Ref")%>%
  count(Var_2Divide, sort = TRUE)
a$percent<-round(a$n/nrow(df1)*100,1)
print("GAGES II Reference")
print(a)


df1<-df %>% 
  filter(CLASS == "Non-ref")
a<-df %>% 
  filter(CLASS == "Non-ref")%>%
  count(Var_2Divide, sort = TRUE)
a$percent<-round(a$n/nrow(df1)*100,1)
print("GAGES II non-reference")
print(a)

## Other USGS Sites
df1<-df %>% 
  filter(!HCDN_2009 %in% c("yes") & !CLASS %in% c("Ref","Non-ref"))
a<-df %>% 
  filter(!HCDN_2009 %in% c("yes") & !CLASS %in% c("Ref","Non-ref"))%>%
  count(Var_2Divide, sort = TRUE)
a$percent<-round(a$n/nrow(df1)*100,1)
print("Other USGS Sites")
print(a)

rm(a,df1)

#######################################################
###Table  for KPSS Test ###############################
#######################################################
a<-df %>% count(KPSS_Result, sort = TRUE)

a$percent<-round(a$n/nrow(df)*100,1)
print("All USGS sites")
print(a)


df1<-df %>% 
  filter(HCDN_2009 == "yes")
a<-df %>% 
  filter(HCDN_2009 == "yes")%>%
  count(Var_2Divide, sort = TRUE)
a$percent<-round(a$n/nrow(df1)*100,1)
print("HCDN Sites")
print(a)

df1<-df %>% 
  filter(CLASS == "Ref")
a<-df %>% 
  filter(CLASS == "Ref")%>%
  count(Var_2Divide, sort = TRUE)
a$percent<-round(a$n/nrow(df1)*100,1)
print("GAGES II Reference")
print(a)


df1<-df %>% 
  filter(CLASS == "Non-ref")
a<-df %>% 
  filter(CLASS == "Non-ref")%>%
  count(Var_2Divide, sort = TRUE)
a$percent<-round(a$n/nrow(df1)*100,1)
print("GAGES II non-reference")
print(a)

## Other USGS Sites
df1<-df %>% 
  filter(!HCDN_2009 %in% c("yes") & !CLASS %in% c("Ref","Non-ref"))
a<-df %>% 
  filter(!HCDN_2009 %in% c("yes") & !CLASS %in% c("Ref","Non-ref"))%>%
  count(Var_2Divide, sort = TRUE)
a$percent<-round(a$n/nrow(df1)*100,1)
print("Other USGS Sites")
print(a)

rm(a,df1)



##########################
##########################

library(tidyr)

# Assume your data frame is called df
# It has columns: trend, p_value, slope

# Step 1: Create a new column for significance
df <- df %>%
  mutate(MK_significance = ifelse(MK_Combi_pvalue < 0.05, "MK_Significant", "MK_Insignificant"))

# Step 2: Summarize count, min, and max slope per combination
slope_summary <- df %>%
  group_by(MK_Combi_Trend, MK_significance) %>%
  summarise(
    count = n(),
    min_slope = min(MK_Combi_slope, na.rm = TRUE),
    max_slope = max(MK_Combi_slope, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Step 3: Ensure all 6 combinations are shown
  complete(
    MK_Combi_Trend = c("no trend", "increasing", "decreasing"),
    MK_significance = c("MK_Significant", "MK_Insignificant"),
    fill = list(count = 0, min_slope = NA, max_slope = NA)
  ) %>%
  arrange(MK_Combi_Trend, MK_significance)


# Step 4: View the summary table
print(slope_summary)

##########################
##########################


############################################
## Plotting USGS Stations ##################
## with states and huc2 ####################
############################################
#round(table(usgs_results$Var_2Divide)/nrow(usgs_results)*100,1)

nrow(usgs_huc2_acf_sf_full)
## Remove rows with NA values of Norm_Trend_slope
usgs_huc2_acf_sf_full<-usgs_huc2_acf_sf_full[!is.na(usgs_huc2_acf_sf_full$StdDev),]
nrow(usgs_huc2_acf_sf_full)


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
  
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  geom_sf_label(data=huc2_sf_map,
                aes(label = HUC2))+
  
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
  ggtitle(paste0("Plot of ",
                 nrow(usgs_huc2_acf_sf_full%>%st_drop_geometry()), 
                 " USGS sites for alpha = ", 
                 alpha_value,
                 " (", 
                 year_usgs_start,
                 " to ",
                 year_usgs_end,
                 ")"
                 ))+ # for the main title+

  labs(shape=legend_title) +
  labs(color=legend_title) +
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

## Plotting different sizes of triangles and circles
colnames(usgs_huc2_acf_sf_full)
table(usgs_huc2_acf_sf_full$Trend_slope)



## Normalize trend slope by standard deviation
usgs_huc2_acf_sf_full$Norm_Trend_slope<-usgs_huc2_acf_sf_full$MK_Combi_slope/
  usgs_huc2_acf_sf_full$StdDev

summary(usgs_huc2_acf_sf_full$Norm_Trend_slope)
## Remove rows with NA values of Norm_Trend_slope
usgs_huc2_acf_sf_full<-usgs_huc2_acf_sf_full[!is.na(usgs_huc2_acf_sf_full$Norm_Trend_slope),]
summary(usgs_huc2_acf_sf_full$Norm_Trend_slope)

## Detailed explanation is below
## 1. USGS peak flow time series data is utilized to obtain trend slopes using the Mann-Kendall test. The trend slope indicates the rate of change in the peak flow over time.
## 2. Next, the trend slope is normalized using the standard deviation of the time series. This standardization step helps to compare trends across different watersheds.
## 3. It is then multiplied by 60 years to scale the normalized trend slope to a 60-year period that provides a more practical understanding of the trend over a long period
## 4. Finally, it is converted to percentages in a more interpretable way, often making it easier to communicate the results.

data_trend_slope_na<-usgs_huc2_acf_sf_full%>%
  dplyr::filter(is.na(Norm_Trend_slope))

usgs_huc2_acf_sf_full$Norm_Trend_slope_6000<-usgs_huc2_acf_sf_full$Norm_Trend_slope*60*100

summary(usgs_huc2_acf_sf_full$Norm_Trend_slope_6000)

colnames(usgs_huc2_acf_sf_full)


abc<-usgs_huc2_acf_sf_full%>%
   select(Norm_Trend_slope_6000,MK_Combi_Trend,MK_Combi_h,
          MK_Combi_pvalue,MK_Combi_slope,MK_Combi_score)

#abc<-usgs_huc2_acf_sf_full%>%
#  select(Norm_Trend_slope_6000,Trend_Result,Trend_z,
#         Trend_pvalue,Trend_slope)


abc1<-abc%>%
  filter(MK_Combi_pvalue>=0.05)
unique(abc1$MK_Combi_Trend)
abc2<-abc%>%
  filter(MK_Combi_pvalue<0.05)
unique(abc2$MK_Combi_Trend)

abc3<-abc%>%
  filter(Norm_Trend_slope_6000==0)

usgs_huc2_acf_sf_full2<-usgs_huc2_acf_sf_full %>%
  mutate(
    Trend_Category = case_when(
      MK_Combi_Trend == "increasing" & Norm_Trend_slope_6000 > 100 ~ ">100",
      MK_Combi_Trend == "increasing" & Norm_Trend_slope_6000 > 60 & Norm_Trend_slope_6000 <=100 ~ "60 to 100",
      MK_Combi_Trend == "increasing" & Norm_Trend_slope_6000 > 20 & Norm_Trend_slope_6000 <=60 ~ "20 to 60",
      MK_Combi_Trend == "increasing" & Norm_Trend_slope_6000 >= 0 & Norm_Trend_slope_6000 <=20 ~ "0 to 20",
      MK_Combi_Trend == "no trend" & Norm_Trend_slope_6000 > 0 ~ ">0",
      MK_Combi_Trend == "no trend" & Norm_Trend_slope_6000 == 0 ~ "=0",
      MK_Combi_Trend == "no trend" & Norm_Trend_slope_6000 < 0 ~ "<0",
      MK_Combi_Trend == "decreasing" & Norm_Trend_slope_6000 < (-100) ~ "< -100",
      MK_Combi_Trend == "decreasing" & Norm_Trend_slope_6000 < (-60) & Norm_Trend_slope_6000 >= (-100) ~ "-100 to -60",
      MK_Combi_Trend == "decreasing" & Norm_Trend_slope_6000 < (-20) & Norm_Trend_slope_6000 >= (-60)  ~ "-60 to -20",
      MK_Combi_Trend == "decreasing" & Norm_Trend_slope_6000 <= 0 & Norm_Trend_slope_6000 >= (-20)  ~ "-20 to 0",
      TRUE ~ "Other"
    )
  )

table(usgs_huc2_acf_sf_full2$Trend_Category)

abc4<-usgs_huc2_acf_sf_full2%>%
  select(Trend_Category, Norm_Trend_slope_6000,MK_Combi_Trend,MK_Combi_h,
         MK_Combi_pvalue,MK_Combi_slope,MK_Combi_score)

unique(abc4$Trend_Category)

factor(usgs_huc2_acf_sf_full2$Trend_Category)


unique(usgs_huc2_acf_sf_full2$Trend_Category)
## Specify the factor levels in the order you want for legend
usgs_huc2_acf_sf_full2$Trend_Category<-factor(usgs_huc2_acf_sf_full2$Trend_Category, 
                                              levels=c(">100",
                                                       "60 to 100",
                                                       "20 to 60",
                                                       "0 to 20",
                                                       ">0",
                                                       "=0",
                                                       "<0",
                                                       "-20 to 0",
                                                       "-60 to -20",
                                                       "-100 to -60",
                                                       "< -100"))
table(usgs_huc2_acf_sf_full2$Trend_Category)
#factor(usgs_huc2_acf_sf_full2$Trend_Category)

library(monochromeR)
generate_palette("#8888C8", modification = "go_darker", 
                 n_colours = 6, view_palette = TRUE)

## Plot
usgs_huc2_states_varyingtrendmag_plot <- ggplot()+
  geom_point(data=usgs_huc2_acf_sf_full2,
             aes(Station_Lon,
                 y=Station_Lat,
                 shape=Trend_Category,
                 color=Trend_Category,
                 fill=Trend_Category
             ))+
  
  theme_minimal() +
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(24,24,24,24,
                                1,1,1,
                                25,25,25,25)
                     ,labels = paste(levels(usgs_huc2_acf_sf_full2$Trend_Category)
                                     ,' ('
                                     ,table(usgs_huc2_acf_sf_full2$Trend_Category)
                                     #,'-'
                                     #,round(table(usgs_huc2_acf_sf_full$site_classification)/nrow(usgs_huc2_acf_sf_full)*100,1)
                                     #,"%"
                                     ,")"
                                     , sep = "")
  )+
  #scale_color_discrete_diverging(palette = "Blue-Red",n=11)+
  #scale_fill_discrete_diverging(palette = "Blue-Red",n=11)+
  scale_color_manual(values=c(#'#3300CC','#3300FF','#6600ff','#9900ff',
    "#00008B","#000077","#0000FF","#8888C8",
    "#8888C8","grey","#FF0000",
    #'#ff0000','#cc0000','#990000',"#660000"
    "#FF0000","#BB0000","#770000","#330000")
    ,labels = paste(levels(usgs_huc2_acf_sf_full2$Trend_Category)
                    ,' ('
                    ,table(usgs_huc2_acf_sf_full2$Trend_Category)
                    #,'-'
                    #,round(table(usgs_huc2_acf_sf_full$site_classification)/nrow(usgs_huc2_acf_sf_full)*100,1)
                    #,"%"
                    ,")"
                    , sep = ""))+
  
  scale_fill_manual(values=c(#'#3300CC','#3300FF','#6600ff','#9900ff',
    "#00008B","#000077","#0000FF","#8888C8",
    "#8888C8","grey","#FF0000",
    #'#ff0000','#cc0000','#990000',"#660000"
    "#FF0000","#BB0000","#770000","#330000")
    ,labels = paste(levels(usgs_huc2_acf_sf_full2$Trend_Category)
                    ,' ('
                    ,table(usgs_huc2_acf_sf_full2$Trend_Category)
                    #,'-'
                    #,round(table(usgs_huc2_acf_sf_full$site_classification)/nrow(usgs_huc2_acf_sf_full)*100,1)
                    #,"%"
                    ,")"
                    , sep = ""))+
  labs(y= "Latitude", x = "Longitude")+
  ggtitle(paste0("Plot of ",
                 nrow(usgs_huc2_acf_sf_full2%>%st_drop_geometry()), 
                 " USGS sites for alpha = ", 
                 alpha_value,
                 " (", 
                 year_usgs_start,
                 " to ",
                 year_usgs_end,
                 ")"
  ))+ # for the main title+
  
  labs(shape='Normalized \nTrend Slope (%)') +
  labs(color='Normalized \nTrend Slope (%)') +
  labs(fill='Normalized \nTrend Slope (%)') +
  coord_sf()#xlim = c(-130, -60),
#ylim = c(23, 53),expand=FALSE)
#usgs_huc2_states_varyingtrendmag_plot

## Change legend title
#usgs_huc2_states_varyingtrendmag_plot <- usgs_huc2_states_varyingtrendmag_plot+labs(shape="Trend")

# Right -> inside the plot area
usgs_huc2_states_varyingtrendmag_plot <-usgs_huc2_states_varyingtrendmag_plot + theme(
  legend.position = c(.999, .65),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)

## add a north symbol and a scale bar with segments of 200 km
## Load ggspatial and ggsn package
usgs_huc2_states_varyingtrendmag_plot <- usgs_huc2_states_varyingtrendmag_plot+
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

#usgs_huc2_states_varyingtrendmag_plot
ggsave(
  paste0(folder_out,"usgs_huc2_states_0_varyingtrendmag_plot.png"),
  plot = usgs_huc2_states_varyingtrendmag_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(usgs_huc2_states_varyingtrendmag_plot)



## Main Loop
site_type_list<-c("All Sites", 
                  "HCDN Only", 
                  "GAGES-II Reference Only", 
                  "GAGES-II Non-reference Only", 
                  "GAGES-II Non-reference + Other USGS")

for (site_type in site_type_list){
  #site_type<-"All Sites"
  if (site_type=="All Sites"){
    usgs_huc2_acf_sf<-usgs_huc2_acf_sf_full2
  }else if(site_type=="HCDN Only"){
    usgs_huc2_acf_sf<-usgs_huc2_acf_sf_full2%>%
      filter(HCDN_2009=="yes")
  }else if(site_type=="GAGES-II Reference Only"){
    usgs_huc2_acf_sf<-usgs_huc2_acf_sf_full2%>%
      filter(CLASS=="Ref")
    
  }else if(site_type=="GAGES-II Non-reference Only"){
    usgs_huc2_acf_sf<-usgs_huc2_acf_sf_full2%>%
      filter(CLASS=="Non-ref")
    
  }else if(site_type=="GAGES-II Non-reference + Other USGS"){
    usgs_huc2_acf_sf<-usgs_huc2_acf_sf_full2%>%
      filter(CLASS%in%c(NA,"Non-ref"))
  }else{
    print("Check\nCheck\nCheck")
  }
#site_type<-"GAGES-II Reference Only"#"All Sites"
  
  write.csv(usgs_huc2_acf_sf,
            paste0(folder_out,site_type,"_usgs_huc2_states_data.csv"))
  
  ############################################
  ## Plotting USGS Stations ##################
  ## Trend Magnitude ####################
  ############################################
  
  
  
  
  ## Plot
  usgs_huc2_states_varyingtrendmag_plot <- ggplot()+
    geom_point(data=usgs_huc2_acf_sf,
               aes(Station_Lon,
                   y=Station_Lat,
                   color=Trend_Category,
                   shape=Trend_Category,
                   fill=Trend_Category))+
    
    theme_minimal() +
    
    geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
    
    geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
    
    scale_shape_manual(values = c(24,24,24,24,
                                  1,1,1,
                                  25,25,25,25)
                       ,labels = paste(levels(usgs_huc2_acf_sf$Trend_Category)
                                       ,' ('
                                       ,table(usgs_huc2_acf_sf$Trend_Category)
                                       #,'-'
                                       #,round(table(usgs_huc2_acf_sf_full$site_classification)/nrow(usgs_huc2_acf_sf_full)*100,1)
                                       #,"%"
                                       ,")"
                                       , sep = "")
    )+
    scale_color_manual(values=c(#'#3300CC','#3300FF','#6600ff','#9900ff',
      "#00008B","#000077","#0000FF","#8888C8",
      "#8888C8","grey","#FF0000",
      #'#ff0000','#cc0000','#990000',"#660000"
      "#FF0000","#BB0000","#770000","#330000")
      ,labels = paste(levels(usgs_huc2_acf_sf$Trend_Category)
                      ,' ('
                      ,table(usgs_huc2_acf_sf$Trend_Category)
                      #,'-'
                      #,round(table(usgs_huc2_acf_sf_full$site_classification)/nrow(usgs_huc2_acf_sf_full)*100,1)
                      #,"%"
                      ,")"
                      , sep = ""))+
    scale_fill_manual(values=c(#'#3300CC','#3300FF','#6600ff','#9900ff',
      "#00008B","#000077","#0000FF","#8888C8",
      "#8888C8","grey","#FF0000",
      #'#ff0000','#cc0000','#990000',"#660000"
      "#FF0000","#BB0000","#770000","#330000")
      ,labels = paste(levels(usgs_huc2_acf_sf$Trend_Category)
                      ,' ('
                      ,table(usgs_huc2_acf_sf$Trend_Category)
                      #,'-'
                      #,round(table(usgs_huc2_acf_sf_full$site_classification)/nrow(usgs_huc2_acf_sf_full)*100,1)
                      #,"%"
                      ,")"
                      , sep = ""))+
    labs(y= "Latitude", x = "Longitude")+
    ggtitle(paste0("Plot of ",
                   nrow(usgs_huc2_acf_sf%>%st_drop_geometry()),
                   " (",site_type, 
                   ") USGS sites for alpha = ", 
                   alpha_value,
                   " (", 
                   year_usgs_start,
                   " to ",
                   year_usgs_end,
                   ")"
                   ))+ # for the main title+
    
    labs(shape='Normalized \nTrend Slope (%)') +
    labs(color='Normalized \nTrend Slope (%)') +
    labs(fill='Normalized \nTrend Slope (%)') +
    coord_sf()#xlim = c(-130, -60),
  #ylim = c(23, 53),expand=FALSE)
  #usgs_huc2_states_varyingtrendmag_plot
  
  ## Change legend title
  #usgs_huc2_states_varyingtrendmag_plot <- usgs_huc2_states_varyingtrendmag_plot+labs(shape="Trend")
  
  # Right -> inside the plot area
  usgs_huc2_states_varyingtrendmag_plot <-usgs_huc2_states_varyingtrendmag_plot + theme(
    #legend.position = c(.999, .65),
    legend.position = "right",
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.box.margin = margin(0, 20, 0, 0) ,
    legend.title = element_text(size = 14),  # Title size
    legend.text = element_text(size = 12),   # Label size
    legend.key.size = unit(1.5, "lines")     # Key box size
  )
  
  ## add a north symbol and a scale bar with segments of 200 km
  ## Load ggspatial and ggsn package
  usgs_huc2_states_varyingtrendmag_plot <- usgs_huc2_states_varyingtrendmag_plot+
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true",
      pad_x = unit(1.2, "in"), pad_y = unit(0.2, "in"),
      style = ggspatial::north_arrow_nautical(
        fill = c("grey40", "white"),
        line_col = "grey20",
        text_family = ""
      ))+
    ggsn::scalebar(conus_sf_map, dist = 1000, dist_unit = "km", 
                   st.size = 3,location = "bottomleft",
                   transform = TRUE, model = "WGS84")
  
  #usgs_huc2_states_varyingtrendmag_plot
  ggsave(
    paste0(folder_out,site_type,"_usgs_huc2_states_0_varyingtrendmag_plot.png"),
    plot = usgs_huc2_states_varyingtrendmag_plot,
    width = 245,
    height = 150,
    units = c("mm"),
    dpi = 300,
    limitsize = TRUE
  )
  rm(usgs_huc2_states_varyingtrendmag_plot)

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
  ggtitle(paste0("Plot of ",
                 nrow(usgs_results_cont_60_normal%>%st_drop_geometry()),
                 " (",site_type, 
                 ") USGS sites for alpha = ", 
                 alpha_value,
                 " (", 
                 year_usgs_start,
                 " to ",
                 year_usgs_end,
                 ")"
                 ))+ # for the main title+
  
  
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
  paste0(folder_out,site_type,"_usgs_huc2_states_1normal_sitetype_plot.png"),
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
  paste0(folder_out,site_type,"_usgs_huc2_states_2acflag1_sitetype_plot.png"),
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
  paste0(folder_out,site_type,"_usgs_huc2_states_2acflag1_plot.png"),
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
  paste0(folder_out,site_type,"_usgs_huc2_states_2acflag2_plot.png"),
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

usgs_results_cont_60_trend_no<-filter(usgs_huc2_acf_sf,MK_Combi_Trend=="no trend")

usgs_huc2_states_3ctrend_no_plot <- ggplot()+
  geom_point(data=usgs_results_cont_60_trend_no,aes(Station_Lon,y=Station_Lat,
                                                color=site_classification,
                                                shape=site_classification))+
  theme_minimal() +
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  
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


usgs_results_cont_60_trend_inc<-filter(usgs_huc2_acf_sf,MK_Combi_Trend=="increasing")

usgs_huc2_states_3ctrend_inc_plot <- ggplot()+
  geom_point(data=usgs_results_cont_60_trend_inc,aes(Station_Lon,y=Station_Lat,
                                                    color=site_classification,
                                                    shape=site_classification))+
  theme_minimal() +
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  
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


usgs_results_cont_60_trend_dec<-filter(usgs_huc2_acf_sf,MK_Combi_Trend=="decreasing")

usgs_huc2_states_3ctrend_dec_plot <- ggplot()+
  geom_point(data=usgs_results_cont_60_trend_dec,aes(Station_Lon,y=Station_Lat,
                                                     color=site_classification,
                                                     shape=site_classification))+
  theme_minimal() +
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  
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
usgs_huc2_acf_sf$MK_Combi_Trend<-factor(usgs_huc2_acf_sf$MK_Combi_Trend, levels=c("increasing","no trend", "decreasing"))

#usgs_huc2_acf_sf<-usgs_huc2_acf_sf%>%
#  filter(HCDN_2009=='yes')

usgs_huc2_states_3atrend_plot <- ggplot()+
  geom_point(data=usgs_huc2_acf_sf,aes(Station_Lon,y=Station_Lat,fill=MK_Combi_Trend,shape=MK_Combi_Trend,color=MK_Combi_Trend))+
  theme_minimal() +
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(24,1,25),
                     labels = paste(levels(usgs_huc2_acf_sf$MK_Combi_Trend),' (',
                                    table(usgs_huc2_acf_sf$MK_Combi_Trend),'-',
                                    round(table(usgs_huc2_acf_sf$MK_Combi_Trend)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_fill_manual(values=c('dodgerblue4','grey40','saddlebrown'),
                     labels = paste(levels(usgs_huc2_acf_sf$MK_Combi_Trend),' (',
                                    table(usgs_huc2_acf_sf$MK_Combi_Trend),'-',
                                    round(table(usgs_huc2_acf_sf$MK_Combi_Trend)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_color_manual(values=c('dodgerblue4','grey40','saddlebrown'),
                    labels = paste(levels(usgs_huc2_acf_sf$MK_Combi_Trend),' (',
                                   table(usgs_huc2_acf_sf$MK_Combi_Trend),'-',
                                   round(table(usgs_huc2_acf_sf$MK_Combi_Trend)/nrow(usgs_huc2_acf_sf)*100,1)
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
usgs_huc2_acf_sf$Ori_Test_Trend<-factor(usgs_huc2_acf_sf$Ori_Test_Trend, levels=c("no trend","increasing", "decreasing"))

usgs_huc2_states_3btrend_ori_plot <- ggplot()+
  geom_point(data=usgs_huc2_acf_sf,aes(Station_Lon,y=Station_Lat,
                                       shape=Ori_Test_Trend,
                                       color=Ori_Test_Trend,
                                       fill=Ori_Test_Trend))+
  theme_minimal() +
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(1, 24, 25),
                     labels = paste(levels(usgs_huc2_acf_sf$Ori_Test_Trend),' (',
                                    table(usgs_huc2_acf_sf$Ori_Test_Trend),'-',
                                    round(table(usgs_huc2_acf_sf$Ori_Test_Trend)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_color_manual(values=c('grey', 'blue', 'red'),
                     labels = paste(levels(usgs_huc2_acf_sf$Ori_Test_Trend),' (',
                                    table(usgs_huc2_acf_sf$Ori_Test_Trend),'-',
                                    round(table(usgs_huc2_acf_sf$Ori_Test_Trend)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_fill_manual(values=c('grey', 'blue', 'red'),
                     labels = paste(levels(usgs_huc2_acf_sf$Ori_Test_Trend),' (',
                                    table(usgs_huc2_acf_sf$Ori_Test_Trend),'-',
                                    round(table(usgs_huc2_acf_sf$Ori_Test_Trend)/nrow(usgs_huc2_acf_sf)*100,1)
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
usgs_huc2_acf_sf$HR_Modif_Trend<-factor(usgs_huc2_acf_sf$HR_Modif_Trend, levels=c("no trend","increasing", "decreasing"))

usgs_huc2_states_3btrend_hr_plot <- ggplot()+
  geom_point(data=usgs_huc2_acf_sf,aes(Station_Lon,y=Station_Lat,
                                       shape=HR_Modif_Trend,
                                       color=HR_Modif_Trend,
                                       fill=HR_Modif_Trend,
                                       ))+
  theme_minimal() +
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
   
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(1, 24, 25),
                     labels = paste(levels(usgs_huc2_acf_sf$HR_Modif_Trend),' (',
                                    table(usgs_huc2_acf_sf$HR_Modif_Trend),'-',
                                    round(table(usgs_huc2_acf_sf$HR_Modif_Trend)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_color_manual(values=c('grey', 'blue', 'red'),
                     labels = paste(levels(usgs_huc2_acf_sf$HR_Modif_Trend),' (',
                                    table(usgs_huc2_acf_sf$HR_Modif_Trend),'-',
                                    round(table(usgs_huc2_acf_sf$HR_Modif_Trend)/nrow(usgs_huc2_acf_sf)*100,1)
                                    ,'%)', sep = ""))+
  scale_fill_manual(values=c('grey', 'blue', 'red'),
                     labels = paste(levels(usgs_huc2_acf_sf$HR_Modif_Trend),' (',
                                    table(usgs_huc2_acf_sf$HR_Modif_Trend),'-',
                                    round(table(usgs_huc2_acf_sf$HR_Modif_Trend)/nrow(usgs_huc2_acf_sf)*100,1)
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

