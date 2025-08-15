## Modifications

## 8/8/2025
## Table values at last not correct
## Added colClasses = c("STAID" = "character") while reading hcdn dataset

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


## Clear all variables
rm(list=ls())

## Clear Console
cat("\014")

alpha_value='0.05'

year_usgs_start='1950' #(Options: "all", "1950", "1981")
year_usgs_end='2014'   #(Options: "all", "2014", "2021")

## Change it as required
folder_name_absolute<-"C:\\Users\\joseph57\\Box\\01_SharedBox_Jibin\\JournalDataRepo\\"

setwd(paste0(folder_name_absolute,"\\Code_R"))

## Plots Output Folder
folder_out<-paste0(folder_name_absolute,"\\Plots_IntermediateResults\\plots_field_significance_v2_",
                   year_usgs_start,
                   "_",
                   year_usgs_end,
                   "\\")

dir.create(folder_out, showWarnings = TRUE)
fileConn<-file(paste0(folder_out,"00_code_full_path_",Sys.Date(),".txt"))

text_to_be_written<- c(rstudioapi::getSourceEditorContext()$path,
                       "\n##############################################\tSession Info\t##############################################\n",
                       capture.output(sessionInfo()))

writeLines(text_to_be_written,
          con=fileConn,
          sep='\n',
          useBytes = FALSE) #default is FALSE)
close(fileConn)
rm(fileConn)
rm(text_to_be_written)




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

####################################
## Read trend-stationary results####
####################################

if (year_usgs_start=="all" | year_usgs_end=="all"){
  usgs_results_30<-read.csv(paste0(folder_name_absolute,"/Data_Intermediate/FFA_Analysis/FFA_EntireUS/z_Result10e/",
                                   "B_Results_Trend_Stationarity_",
                                   year_usgs_start,"_",year_usgs_end,
                                   #"_missing_5percent",
                                   "_alpha_0.05.txt"),
                            sep='\t',
                            colClasses = c("USGS_Station" = "character"))
}else{
  usgs_results_30<-read.csv(paste0(folder_name_absolute,"/Data_Intermediate/FFA_Analysis/FFA_EntireUS/z_Result10e/",
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
          paste0(folder_name_absolute,"/merged_result_usgs_analysis.csv"), 
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
usgs_huc2_acf_sf<-st_intersection(usgs_results_sf,huc2_sf)

rm(usgs_results_sf)
colnames(usgs_huc2_acf_sf)


data_combined_all<-usgs_huc2_acf_sf%>%
  select(USGS_Station,SlNo,Station_Name,State_Name,Station_Lat,Station_Lon,
  Drain_Area,CountyCode,State_Code,HUC_Code,Basin_Code,
  PeakBeginDate,PeakEndDate,USGS_Count,Calc_Count,
  MK_Combi_Trend,MK_Combi_h,MK_Combi_pvalue,MK_Combi_z,MK_Combi_tau,
  MK_Combi_score,MK_Combi_var_s,MK_Combi_slope,MK_Combi_intercept,
  HUC2,NAME,CLASS,HCDN_2009)


huc2_sf_map_with_data<-huc2_sf_map%>%
  arrange(HUC2)

huc2_sf_map_with_data <- huc2_sf_map_with_data%>% mutate(score = NA,
                                                         var_s=NA,
                                                         norm_score=NA,
                                                         p_value=NA,
                                                         sen_slope=NA)
#sites_type_list<-c("AllSites------",
 #                  "RefSites------",
  #                 "Non-refSites")
#count_file<-1
#for (sites_type in sites_type_list){
#
#if (sites_type == "AllSites------"){
  
 # data_combined<-data_combined_all
#  title_name<-'All USGS Sites'
  
#} else if (sites_type=="RefSites------"){
  
 # data_combined<-data_combined_all%>%
  #  filter(CLASS=="Ref")
  #title_name<-'GAGES-II Reference Sites'
  
#}else if (sites_type=="Non-refSites"){
  
 # data_combined<-data_combined_all%>%
#    filter(CLASS=="Non-ref")
#  title_name<-'GAGES-II Non-reference Sites'
#}


library(stringr)

df_list<-list()

for (huc2_number in sort(unique(huc2_sf_map_with_data$huc_numer))){
  #huc2_number<-1
  
  #print(huc2_number)
  huc2_number_modified<-str_pad(huc2_number, 2, pad = "0")
  
  print(huc2_number_modified)
  
  data_for_each_huc2<-data_combined_all%>%
    st_drop_geometry()%>%
    filter(HUC2==huc2_number_modified)
  
  print(nrow(data_for_each_huc2))
  
  # Extract p-values from the data frame
  p_values <- data_for_each_huc2$MK_Combi_pvalue
  
  # Step 2: Sort the p-values in ascending order
  sorted_p_values <- sort(p_values)
  
  # Step 3: Determine PFDR
  alpha_global <- 0.05
  K <- length(sorted_p_values)
  thresholds <- (1:K) / K * alpha_global
  
  # Find the largest p-value that is less than or equal to the corresponding threshold
  P_FDR <- max(sorted_p_values[sorted_p_values <= thresholds])
  
  # If no p-value meets the condition, P_FDR should be set to the smallest value above the threshold
  if(is.na(P_FDR)) {
    P_FDR <- min(sorted_p_values[sorted_p_values > alpha_global])
  }
  
  # Step 4: Determine which p-values are field significant
  field_significant <- p_values <= P_FDR
  
  # Add the field significance results back to the data frame
  data_for_each_huc2$HUC2_Field_Significant <- field_significant
  
  data_for_each_huc2$HUC2_P_FDR=P_FDR
  
  df_list<-append(df_list, list(data_for_each_huc2))
  
}

merged_df <- do.call(rbind, df_list)

######################
### FDR FOR CONUS ####
######################

# Extract p-values from the data frame
p_values <- merged_df$MK_Combi_pvalue

# Step 2: Sort the p-values in ascending order
sorted_p_values <- sort(p_values)

# Step 3: Determine PFDR
alpha_global <- 0.05
K <- length(sorted_p_values)
thresholds <- (1:K) / K * alpha_global

# Find the largest p-value that is less than or equal to the corresponding threshold
P_FDR <- max(sorted_p_values[sorted_p_values <= thresholds])

# If no p-value meets the condition, P_FDR should be set to the smallest value above the threshold
if(is.na(P_FDR)) {
  P_FDR <- min(sorted_p_values[sorted_p_values > alpha_global])
}

# Step 4: Determine which p-values are field significant
field_significant <- p_values <= P_FDR

# Add the field significance results back to the data frame
merged_df$CONUS_Field_Significant <- field_significant

merged_df$CONUS_P_FDR=P_FDR


data_for_merge<-merged_df%>%
  select(USGS_Station,
         HUC2_Field_Significant, HUC2_P_FDR,
         CONUS_Field_Significant,CONUS_P_FDR)

data_combined_merged <- data_combined_all %>% 
  left_join(data_for_merge)


data_combined_merged$HUC2_Field_Significant <-factor(data_combined_merged$HUC2_Field_Significant, 
                                              levels=c("FALSE","TRUE"))

site_type<-'all'

usgs_huc2_states_5bstat_mar_plot <- ggplot() + 
  theme_minimal() +
  geom_point(data=data_combined_merged, aes(x=Station_Lon,y=Station_Lat,
                                        shape=HUC2_Field_Significant,
                                        color=HUC2_Field_Significant,
                                        fill=HUC2_Field_Significant
  ))+
  
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  #geom_sf(data = regions_sf_map, colour="grey60", fill="transparent", linetype = "solid", linewidth=0.5) +
  geom_sf(data = huc2_sf_map, colour="black", fill="transparent", linetype = "solid", linewidth=0.5) +
  
  scale_shape_manual(values = c(0, 21),
                     labels = paste(levels(data_combined_merged$HUC2_Field_Significant),' (',
                                    table(data_combined_merged$HUC2_Field_Significant),'-',
                                    round(table(data_combined_merged$HUC2_Field_Significant)/nrow(data_combined_merged)*100,1)
                                    ,'%)', sep = ""))+
  scale_color_manual(values=c('grey','dodgerblue4'),
                     labels = paste(levels(data_combined_merged$HUC2_Field_Significant),' (',
                                    table(data_combined_merged$HUC2_Field_Significant),'-',
                                    round(table(data_combined_merged$HUC2_Field_Significant)/nrow(data_combined_merged)*100,1)
                                    ,'%)', sep = ""))+
  scale_fill_manual(values=c('grey','dodgerblue4'),
                    labels = paste(levels(data_combined_merged$HUC2_Field_Significant),' (',
                                   table(data_combined_merged$HUC2_Field_Significant),'-',
                                   round(table(data_combined_merged$HUC2_Field_Significant)/nrow(data_combined_merged)*100,1)
                                   ,'%)', sep = ""))+
  #ggtitle(paste("Stationarity Results (Majority Rule Approach) for",nrow(data_combined_merged),"USGS stations with",years_count,"and more years")) + 
  labs(y= "Latitude", x = "Longitude")+
  ggtitle(paste0("Plot of ",nrow(data_combined_merged%>%st_drop_geometry())," (",site_type, ") USGS  sites for alpha = ", alpha_value))+ # for the main title+
  
  ## Change legend title
  labs(shape="Field Significance") +
  labs(color="Field Significance") +
  labs(fill="Field Significance") +
  
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
  paste0(folder_out,site_type,"_usgs_huc2_states_field_significance_plot.png"),
  plot = usgs_huc2_states_5bstat_mar_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 900,
  limitsize = TRUE
)
rm(usgs_huc2_states_5bstat_mar_plot)



#####################
## Estimating the number of stations with field significant increasing and decreasing

ncount<-data_combined_merged%>%
  nrow()
print("All USGS Sites")

df<-data_combined_merged%>% st_drop_geometry()
tot<-nrow(df)

a<- df %>% count(MK_Combi_Trend, sort = FALSE)


a$percent<-round(a$n/nrow(df)*100,1)
print(nrow(df))
print("All USGS sites")
print(a)
print((a$n/tot*100))

print(ncount)

data_combined_merged%>%
  filter(MK_Combi_Trend=='increasing')%>%
  nrow()

data_combined_merged%>%
  filter(MK_Combi_Trend=='decreasing')%>%
  nrow()

data_combined_merged%>%
  filter(MK_Combi_Trend=='no trend')%>%
  nrow()
print("All USGS sites")
a2<- data_combined_merged %>% 
  st_drop_geometry() %>% 
  count(MK_Combi_Trend,HUC2_Field_Significant, sort = FALSE)
a2$percent<-round(a2$n/nrow(df)*100,1)
print("All USGS sites")
print(a2%>%arrange(HUC2_Field_Significant))

data_combined_merged%>%
  filter(MK_Combi_Trend=='increasing')%>%
  filter(HUC2_Field_Significant=="TRUE")%>%
  nrow()

data_combined_merged%>%
  filter(MK_Combi_Trend=='decreasing')%>%
  filter(HUC2_Field_Significant=="TRUE")%>%
  nrow()

data_combined_merged%>%
  filter(MK_Combi_Trend=='no trend')%>%
  filter(HUC2_Field_Significant=="TRUE")%>%
  nrow()


## HCDN

print("HCDN Sites")
a2<- data_combined_merged %>% 
  st_drop_geometry() %>%
  filter(HCDN_2009=="yes")%>%
  count(MK_Combi_Trend,HUC2_Field_Significant, sort = FALSE)

count<-data_combined_merged %>% 
  st_drop_geometry() %>%
  filter(HCDN_2009=="yes")%>%
  nrow()

a2$percent<-round(a2$n/count*100,1)
print("HCDN Sites")
print(a2%>%arrange(HUC2_Field_Significant))

data_combined_merged%>%
  filter(HCDN_2009=="yes")%>%
  nrow()

data_combined_merged%>%
  filter(HCDN_2009=="yes")%>%
  filter(MK_Combi_Trend=='increasing')%>%
  nrow()

data_combined_merged%>%
  filter(HCDN_2009=="yes")%>%
  filter(MK_Combi_Trend=='decreasing')%>%
  nrow()

data_combined_merged%>%
  filter(HCDN_2009=="yes")%>%
  filter(MK_Combi_Trend=='increasing')%>%
  filter(HUC2_Field_Significant=="TRUE")%>%
  nrow()

data_combined_merged%>%
  filter(HCDN_2009=="yes")%>%
  filter(MK_Combi_Trend=='decreasing')%>%
  filter(HUC2_Field_Significant=="TRUE")%>%
  nrow()

## GAGES-II Ref


print("GAGES-II Ref")
a2<- data_combined_merged %>%
  st_drop_geometry() %>% 
  filter(CLASS=="Ref")%>%
  count(MK_Combi_Trend,HUC2_Field_Significant, sort = FALSE)

count<-data_combined_merged %>% 
  st_drop_geometry() %>%
  filter(CLASS=="Ref")%>%
  nrow()

a2$percent<-round(a2$n/count*100,1)
print("GAGES-II Ref")
print(a2%>%arrange(HUC2_Field_Significant))


data_combined_merged%>%
  filter(CLASS=="Ref")%>%
  nrow()

data_combined_merged%>%
  filter(CLASS=="Ref")%>%
  filter(MK_Combi_Trend=='increasing')%>%
  nrow()

data_combined_merged%>%
  filter(CLASS=="Ref")%>%
  filter(MK_Combi_Trend=='decreasing')%>%
  nrow()

data_combined_merged%>%
  filter(CLASS=="Ref")%>%
  filter(MK_Combi_Trend=='increasing')%>%
  filter(HUC2_Field_Significant=="TRUE")%>%
  nrow()

data_combined_merged%>%
  filter(CLASS=="Ref")%>%
  filter(MK_Combi_Trend=='decreasing')%>%
  filter(HUC2_Field_Significant=="TRUE")%>%
  nrow()


## GAGES-II Non-ref

print("GAGES-II Non-ref")
a2<- data_combined_merged %>%
  st_drop_geometry() %>% 
  filter(CLASS=="Non-ref")%>%
  count(MK_Combi_Trend,HUC2_Field_Significant, sort = FALSE)

count<-data_combined_merged %>% 
  st_drop_geometry() %>%
  filter(CLASS=="Non-ref")%>%
  nrow()

a2$percent<-round(a2$n/count*100,1)
print("GAGES-II Non-ref")
print(a2%>%arrange(HUC2_Field_Significant))


data_combined_merged%>%
  filter(CLASS=="Non-ref")%>%
  nrow()

data_combined_merged%>%
  filter(CLASS=="Non-ref")%>%
  filter(MK_Combi_Trend=='increasing')%>%
  nrow()

data_combined_merged%>%
  filter(CLASS=="Non-ref")%>%
  filter(MK_Combi_Trend=='decreasing')%>%
  nrow()

data_combined_merged%>%
  filter(CLASS=="Non-ref")%>%
  filter(MK_Combi_Trend=='increasing')%>%
  filter(HUC2_Field_Significant=="TRUE")%>%
  nrow()

data_combined_merged%>%
  filter(CLASS=="Non-ref")%>%
  filter(MK_Combi_Trend=='decreasing')%>%
  filter(HUC2_Field_Significant=="TRUE")%>%
  nrow()

## Other USGS

print("Other USGS")
a2<- data_combined_merged %>%
  st_drop_geometry() %>% 
  filter(is.na(HCDN_2009))%>%
  filter(is.na(CLASS))%>%
  count(MK_Combi_Trend,HUC2_Field_Significant, sort = FALSE)

count<-data_combined_merged %>% 
  st_drop_geometry() %>%
  filter(is.na(HCDN_2009))%>%
  filter(is.na(CLASS))%>%
  nrow()

a2$percent<-round(a2$n/count*100,1)
print("Other USGS")
print(a2%>%arrange(HUC2_Field_Significant))

data_combined_merged%>%
  filter(is.na(HCDN_2009))%>%
  filter(is.na(CLASS))%>%
  nrow()

data_combined_merged%>%
  filter(is.na(HCDN_2009))%>%
  filter(is.na(CLASS))%>%
  filter(MK_Combi_Trend=='increasing')%>%
  nrow()

data_combined_merged%>%
  filter(is.na(HCDN_2009))%>%
  filter(is.na(CLASS))%>%
  filter(MK_Combi_Trend=='decreasing')%>%
  nrow()

data_combined_merged%>%
  filter(is.na(HCDN_2009))%>%
  filter(is.na(CLASS))%>%
  filter(MK_Combi_Trend=='increasing')%>%
  filter(HUC2_Field_Significant=="TRUE")%>%
  nrow()

data_combined_merged%>%
  filter(is.na(HCDN_2009))%>%
  filter(is.na(CLASS))%>%
  filter(MK_Combi_Trend=='decreasing')%>%
  filter(HUC2_Field_Significant=="TRUE")%>%
  nrow()


#####################


