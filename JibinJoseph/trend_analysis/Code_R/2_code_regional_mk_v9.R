## Modifications

## 2025-08-09
## Removed "usgs_huc2_states_" from output filenames
## Added "pad_x = unit(1.2, "in"), pad_y = unit(0.2, "in")," in north symbol

## 2024-08-16
## While reading the csv hcdn_gages_df, included "colClasses = c("STAID" = "character")" to capture the leading 0 in site number
## Without this data_combined_all was not able to capture HUC2 and Ref site details


## 2024-01-15
## Add trend results on top of RAMK results


## 2024-01-05
## Normalized slope by std deviation
## Trying to find whether the unit of slope is cfs/year or /year
## 


## Revision 2023-07-27
# show all trends in color where you found significant changes
## show colors for all sens slope numbers and mask out the color plotting if the value is between -1.96 and  +1.96 in the z value

## Revision 2023-07-27
## Added white region in legend for -1.96 to +1.96 and -5 to +5


library(sf)
library(ggplot2)
library(ggsn)
library(ggrepel)
library(dplyr)
library(tmap)
library(ggpattern)
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

year_usgs_start='all' #(Options: "all", "1981", "1950")
year_usgs_end='all'   #(Options: "all", "2021", "2014")

## Change it as required
folder_name_absolute<-"C:\\Users\\joseph57\\Box\\01_SharedBox_Jibin\\JournalDataRepo\\"

setwd(paste0(folder_name_absolute,"\\Code_R"))

## Plots Output Folder
folder_out<-paste0(folder_name_absolute,"\\Plots_IntermediateResults\\plots_regional_mk_v9_",
                   year_usgs_start,"_",
                   year_usgs_end,"\\"
                   )
fileConn<-file(paste0(folder_out,"00_code_full_path_",Sys.Date(),".txt"))
dir.create(folder_out, showWarnings = TRUE)
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

if (year_usgs_start=="all"){
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

if (year_usgs_start=="all"){
  usgs_results<-filter(usgs_results_cont,Calc_Count>=years_count)
}else{
  usgs_results<-usgs_results_cont
}
head(usgs_results)
colnames(usgs_results)

rm(usgs_results_30,usgs_results_cont)

hcdn_gages_df<-read.csv(paste0(folder_name_absolute,"/Data_Intermediate/hcdn_gages_df.csv"),
                        colClasses = c("STAID" = "character"))

x<-usgs_results
y<-hcdn_gages_df

usgs_results_30_merged_result<-merge(x, y, 
                                     by.x = "USGS_Station", 
                                     by.y = "STAID", 
                                     all.x = TRUE, 
                                     all.y = FALSE)

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

#####################################
## Intersection with huc 2 level
## Takes time
#####################################
st_crs(usgs_results_sf)
st_crs(huc2_sf)
usgs_huc2_acf_sf<-st_intersection(usgs_results_sf,
                                  huc2_sf)

#####################################
## Above Takes time
#####################################


rm(usgs_results_sf)

#####################################
## Normalize trend slope by standard deviation

usgs_huc2_acf_sf <- usgs_huc2_acf_sf%>%
  filter(!is.na(StdDev))
usgs_huc2_acf_sf$Norm_Trend_slope<-usgs_huc2_acf_sf$MK_Combi_slope/usgs_huc2_acf_sf$StdDev


#####################################

colnames(usgs_huc2_acf_sf)

data_combined_all<-usgs_huc2_acf_sf%>%
  select(USGS_Station,SlNo,Station_Name,State_Name,Station_Lat,Station_Lon,
  Drain_Area,CountyCode,State_Code,HUC_Code,Basin_Code,
  PeakBeginDate,PeakEndDate,USGS_Count,Calc_Count,
  MK_Combi_Trend,MK_Combi_h,MK_Combi_pvalue,MK_Combi_z,MK_Combi_tau,
  MK_Combi_score,MK_Combi_var_s,MK_Combi_slope,MK_Combi_intercept,
  HUC2,NAME,CLASS,HCDN_2009,Norm_Trend_slope)


huc2_sf_map_with_data<-huc2_sf_map%>%
  arrange(HUC2)


sites_type_list<-c("AllSites------"
                   ,"HCDNSites----"
                   ,"RefSites------"
                   ,"Non-refSites"
                   ,"Non-refSites+OtherUSGS"
                   )

###############################
## Below should be outside the loop ##
###############################

min_score<-0
max_score<-0
min_senslope<-0
max_senslope<-0
min_norm_senslope<-0
max_norm_senslope<-0



###############################
## Loop for sites types ##
###############################

for (sites_type in sites_type_list){
  #sites_type<-"AllSites------"
  #sites_type<-"RefSites------"
  #sites_type<-"HCDNSites----"
  #sites_type<-"Non-refSites"
  
  if (sites_type == "AllSites------"){
    
    data_combined<-data_combined_all
    title_name<-'All USGS Sites'
    
  } else if (sites_type=="HCDNSites----"){
    
    data_combined<-data_combined_all%>%
      filter(HCDN_2009=="yes")
    title_name<-'HCDN 2009 Sites'
    
  }else if (sites_type=="RefSites------"){
    
    data_combined<-data_combined_all%>%
      filter(CLASS=="Ref")
    title_name<-'GAGES-II Reference Sites'
    
  }else if (sites_type=="Non-refSites"){
    
    data_combined<-data_combined_all%>%
      filter(CLASS=="Non-ref")
    title_name<-'GAGES-II Non-reference Sites'
  }else if (sites_type=="Non-refSites+OtherUSGS"){
    
    data_combined<-data_combined_all%>%
      filter(CLASS%in%c(NA,"Non-ref"))
    title_name<-'GAGES-II Non-reference + Other Sites'
  }
  
  huc2_sf_map_with_data <- huc2_sf_map_with_data%>% mutate(score = NA,
                                                           var_s=NA,
                                                           norm_score=NA,
                                                           p_value=NA,
                                                           sen_slope=NA,
                                                           norm_sen_slope=NA,
                                                           #norm_sen_slope_5000=NA,
                                                           norm_sen_slope_6000=NA,
  )
  
  
  library(stringr)
  
  ###############################
  ## Inner Loop for getting values ##
  ###############################  

  for (huc2_number in sort(unique(huc2_sf_map_with_data$huc_numer))){
    #print(huc2_number)}
    #huc2_number<-1
    score_for_huc2_region<--99999
    var_s_for_huc2_region<--99999
    norm_score_for_huc2_region<--99999
    p_value_for_huc2_region<--99999
    sen_slope_for_huc2_region<--99999
    norm_sen_slope_for_huc2_region<--99999
    norm_sen_slope_6000_for_huc2_region<--99999
  
  
    #print(huc2_number)
    huc2_number_modified<-str_pad(huc2_number, 2, pad = "0")
    
    data_for_each_huc2<-data_combined%>%
      st_drop_geometry()%>%
      filter(HUC2==huc2_number_modified)
    
    
    score_for_huc2_region<-sum(data_for_each_huc2$MK_Combi_score)
    var_s_for_huc2_region<-(sum((data_for_each_huc2$MK_Combi_var_s)))
    #var_s_for_huc2_region<-(sum((data_for_each_huc2$MK_Combi_var_s)^(2)))^(0.5)
    
    if(score_for_huc2_region>0){
      norm_score_for_huc2_region<-(score_for_huc2_region-1)/var_s_for_huc2_region^0.5
    } else if (score_for_huc2_region==0) {
      norm_score_for_huc2_region<-0
    } else if(score_for_huc2_region<0){
      norm_score_for_huc2_region<-(score_for_huc2_region+1)/var_s_for_huc2_region^0.5
      
    }
  
    p_value_for_huc2_region <- (1 - pnorm(abs(norm_score_for_huc2_region))) * 2
    sen_slope_for_huc2_region<-median(data_for_each_huc2$MK_Combi_slope)
    norm_sen_slope_for_huc2_region<-median(data_for_each_huc2$Norm_Trend_slope)
    norm_sen_slope_6000_for_huc2_region<-norm_sen_slope_for_huc2_region*6000
  
    #Inspired from
    #df <- data.frame(id = 1:10, key = 1:10)
    #replace_key <- c(2,5)
    #replace_id <- c(9,3)
    #df$key[match(replace_id, df$id)] <- replace_key
    
    cat(huc2_number,":",nrow(data_for_each_huc2),"-",p_value_for_huc2_region,sen_slope_for_huc2_region,norm_sen_slope_for_huc2_region,norm_sen_slope_6000_for_huc2_region,"\n")
    
    
    huc2_sf_map_with_data$score[match(huc2_number_modified,huc2_sf_map_with_data$HUC2)]<-score_for_huc2_region
    huc2_sf_map_with_data$var_s[match(huc2_number_modified,huc2_sf_map_with_data$HUC2)]<-var_s_for_huc2_region
    huc2_sf_map_with_data$norm_score[match(huc2_number_modified,huc2_sf_map_with_data$HUC2)]<-norm_score_for_huc2_region
    huc2_sf_map_with_data$p_value[match(huc2_number_modified,huc2_sf_map_with_data$HUC2)]<-p_value_for_huc2_region
    huc2_sf_map_with_data$sen_slope[match(huc2_number_modified,huc2_sf_map_with_data$HUC2)]<-sen_slope_for_huc2_region
    huc2_sf_map_with_data$norm_sen_slope[match(huc2_number_modified,huc2_sf_map_with_data$HUC2)]<-norm_sen_slope_for_huc2_region
    huc2_sf_map_with_data$norm_sen_slope_6000[match(huc2_number_modified,huc2_sf_map_with_data$HUC2)]<-norm_sen_slope_6000_for_huc2_region
    
    assign(as.vector(paste0("data_",sites_type)),huc2_sf_map_with_data)
    
  }
  


  if (max_score<max(huc2_sf_map_with_data$norm_score)){
    max_score<-max(huc2_sf_map_with_data$norm_score)
    
  }
  
  if (min_score>min(huc2_sf_map_with_data$norm_score)){
    min_score<-min(huc2_sf_map_with_data$norm_score)
    
  }
  
  if (max_senslope<max(huc2_sf_map_with_data$sen_slope, na.rm = TRUE)){
    max_senslope<-max(huc2_sf_map_with_data$sen_slope)
    
  }
  
  if (min_senslope>min(huc2_sf_map_with_data$sen_slope, na.rm = TRUE)){
    min_senslope<-min(huc2_sf_map_with_data$sen_slope)
    
  }
  
  if (max_norm_senslope<max(huc2_sf_map_with_data$norm_sen_slope_6000, na.rm = TRUE)){
    max_norm_senslope<-max(huc2_sf_map_with_data$norm_sen_slope_6000)
    
  }
  
  if (min_norm_senslope>min(huc2_sf_map_with_data$norm_sen_slope_6000, na.rm = TRUE)){
    min_norm_senslope<-min(huc2_sf_map_with_data$norm_sen_slope_6000)
    
  }
  
}





print_data<-huc2_sf_map_with_data%>%
  st_drop_geometry()%>%
  select(HUC2,p_value)
print_data

###############################
## Loop for creating figures ##
###############################

count_file<-1

for (sites_type in sites_type_list){
  #sites_type<-"AllSites------"
  if (sites_type == "AllSites------"){
    
    data_combined<-data_combined_all
    title_name<-'All USGS Sites'
    
    #plot_trend_data<-usgs_huc2_acf_sf%>%
     # select(Station_Lon,
       #      Station_Lat,
        #     MK_Combi_Trend)
    
  } else if (sites_type=="HCDNSites----"){
    
    data_combined<-data_combined_all%>%
      filter(HCDN_2009=="yes")
    title_name<-'HCDN 2009 Sites'
    } else if (sites_type=="RefSites------"){
    
    data_combined<-data_combined_all%>%
      filter(CLASS=="Ref")
    title_name<-'GAGES-II Reference Sites'
    
  }else if (sites_type=="Non-refSites"){
    
    data_combined<-data_combined_all%>%
      filter(CLASS=="Non-ref")
    title_name<-'GAGES-II Non-reference Sites'
  }else if (sites_type=="Non-refSites+OtherUSGS"){
    
    data_combined<-data_combined_all%>%
      filter(CLASS%in%c(NA,"Non-ref"))
    title_name<-'GAGES-II Non-reference + Other Sites'
  }

  
  ## Specify the factor levels in the order you want for legend
  #plot_trend_data$MK_Combi_Trend<-factor(plot_trend_data$MK_Combi_Trend, 
   #                                    levels=c("increasing","no trend", "decreasing"))
##################################
## Normalized Score ##############
##################################
  no_range<-1.96
  
  
  data_each_case<-get(paste0("data_",sites_type))
  
  selected_huc2<- subset(data_each_case, p_value<=0.05)
  selected_huc2<- subset(data_each_case, norm_score>1.96 | norm_score< (-1.96))
  
  library (dplyr)
  
  data_each_case<-data_each_case%>%
    mutate(sen_slope=ifelse(norm_score > (-1.96) & norm_score < (+1.96), NA, sen_slope))
  
  limit<-ceiling(max(abs(min_score),abs(max_score)))
  
  num_low<-(-limit)
  num_high<-(+limit)
  
  
  
  values_calc<-c(num_low,-no_range,-no_range+0.1,no_range-0.1,no_range,num_high)
  values_calc<-(values_calc-num_low)/(num_high-num_low)

usgs_huc2_states_1_norm_score_plot <- ggplot(data=data_each_case)+
  geom_sf(aes(fill = norm_score),color=NA) +
  theme_minimal() +
  geom_sf(data = huc2_sf_map, color="black", fill=NA)+
  geom_sf(data = conus_sf_map, color="grey60", fill=NA,linetype = "dashed") +
  #scale_fill_gradientn(colours= c('grey','dodgerblue4'),name = "Increasing\nTrend",limit=c(0,70))+
  #scale_fill_gradient2(midpoint=0, 
  #                      low="saddlebrown", 
  #                      mid="grey",
  #                      high="dodgerblue4", 
  #                      space ="Lab",
  #                     limit=c(-25,+25))+
  
  #geom_sf_pattern(data = selected_huc2, 
  #                #aes(fill=HUC2),
  #                pattern="circle", 
  #                #pattern_aspect_ratio = 3,
  #                #pattern_scale = 0.5,
  #                fill='transparent',
  #                #pattern_size=0.1
  #                pattern_spacing=0.03)+
  
  scale_fill_gradientn(colours=c("saddlebrown",'tan1',"white","white","deepskyblue","dodgerblue"),
                       values=values_calc,
                       #na.value="white", 
                       #guide="colourbar",
                       name='RAMK\nNormalized\nScore (Z)',
                       limits=c(num_low,num_high),
                       breaks=c(num_low,-no_range,no_range,num_high), 
                       labels=c(num_low,-no_range,no_range,num_high))+
  
  #geom_point(data=plot_trend_data,
   #          aes(Station_Lon,
   #              y=Station_Lat,
    #             fill=MK_Combi_Trend,
    #             shape=MK_Combi_Trend,
     #            color=MK_Combi_Trend))+
  
  #guides(fill = guide_colourbar(barheight = 20, direction = "vertical",
  #                              title.position="top", title.hjust = 0.5,title.vjust = 0.5, nbin = 50))+
  
  labs(y= "Latitude", x = "Longitude")+
  #labs(fill='RAMK - Normalized Score') +
  geom_sf_label(aes(label = paste(round(norm_score,2), " ")))+
  ggtitle(paste0("Plot of ",nrow(data_combined%>%st_drop_geometry())," ", title_name, " for alpha = ", alpha_value))+
  #labs(color='Site Type (\u2265 60 years)') +
  coord_sf()#xlim = c(-130, -60),
#ylim = c(23, 53),expand=FALSE)
usgs_huc2_states_1_norm_score_plot
rm(limit,
   num_low,num_high,no_range,values_calc)

## Change legend title
#usgs_huc2_states_1_norm_score_plot <- usgs_huc2_states_1_norm_score_plot+labs(shape="Trend")

# Right -> inside the plot area
usgs_huc2_states_1_norm_score_plot <-usgs_huc2_states_1_norm_score_plot + theme(
  #legend.position = c(.995, .35),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6),
  legend.position = "right",
  legend.box.margin = margin(0, 20, 0, 0),
  legend.title = element_text(size = 14),  # Title size
  legend.text = element_text(size = 12),   # Label size
  legend.key.size = unit(1.5, "lines")     # Key box size
)

## add a north symbol and a scale bar with segments of 200 km
## Load ggspatial and ggsn package
usgs_huc2_states_1_norm_score_plot <- usgs_huc2_states_1_norm_score_plot+
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

#usgs_huc2_states_1_norm_score_plot
ggsave(
  paste0(folder_out,"1_norm_score_plot","_site_type_",count_file,"_",sites_type,"_alpha_",alpha_value,".png"),
  plot = usgs_huc2_states_1_norm_score_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(usgs_huc2_states_1_norm_score_plot)



###################################
## Sen Slope Plot #################
###################################



limit<-ceiling(max(abs(min_senslope),abs(max_senslope)))

num_low<-(-limit)
num_high<-(+limit)

no_range<-5

#values_calc<-c(num_low,-no_range,-no_range+0.1,no_range-0.1,no_range,num_high)
values_calc<-c(num_low,-0.1,0,+0.1,num_high)
values_calc<-(values_calc-num_low)/(num_high-num_low)

library(tidyverse)

usgs_huc2_states_2_sen_slope_plot <- ggplot(data=data_each_case%>% 
                                              tidyr::drop_na(sen_slope))+
  geom_sf(aes(fill = sen_slope), color = NA) +
  theme_minimal() +
  geom_sf(data = huc2_sf_map, colour="black", fill=NA)+
  #geom_sf_pattern(data = data_each_case%>%
  #          filter(is.na(sen_slope)), 
  #          pattern="circle", 
  #          pattern_aspect_ratio = 0.25,
  #          fill=NA)+
  geom_sf(data = conus_sf_map, colour="grey60", fill=NA,linetype = "dashed") +
  #scale_fill_gradientn(colours= c('grey','dodgerblue4'),name = "Increasing\nTrend",limit=c(0,70))+
  #scale_fill_gradient2(midpoint=0, 
  #                     low="saddlebrown", 
  #                     mid="grey",
  #                     high="dodgerblue4", 
  #                     space ="Lab",
  #                     limit=c(-25,+25))+
  
  #geom_sf_pattern(data = selected_huc2, 
                  #aes(fill=HUC2),
   #               pattern="circle", 
                  #pattern_aspect_ratio = 3,
                  #pattern_scale = 0.5,
    #              fill='transparent',
                  #pattern_size=0.1
     #             pattern_spacing=0.03
  #)+
  scale_fill_gradientn(colours=c("saddlebrown",'tan1',"white","deepskyblue","dodgerblue"),
                       values=values_calc,
                       #na.value="white", 
                       #guide="colourbar",
                       name='RAMK - Sen Slope\n(cfs per year)',
                       limits=c(num_low,num_high),
                       breaks=c(num_low,0,num_high), 
                       labels=c(num_low,0,num_high))+
  
  labs(y= "Latitude", x = "Longitude")+
  labs(fill='RAMK - Sen Slope (cfs per year)') +
  geom_sf_label(aes(label = if (class(sen_slope)=="numeric") paste(format(round(sen_slope,1), nsmall = 1), " ") else sen_slope))+
  ggtitle(paste0("Plot of ",nrow(data_combined%>%st_drop_geometry())," ", title_name, " for alpha = ", alpha_value))+
  #labs(color='Site Type (\u2265 60 years)') +
  coord_sf()#xlim = c(-130, -60),
#ylim = c(23, 53),expand=FALSE)
usgs_huc2_states_2_sen_slope_plot
rm(limit,
   num_low,num_high,no_range,values_calc)

## Change legend title
#usgs_huc2_states_2_sen_slope_plot <- usgs_huc2_states_2_sen_slope_plot+labs(shape="Trend")

# Right -> inside the plot area
usgs_huc2_states_2_sen_slope_plot <-usgs_huc2_states_2_sen_slope_plot + theme(
  legend.position = c(.995, .35),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)

## add a north symbol and a scale bar with segments of 200 km
## Load ggspatial and ggsn package
usgs_huc2_states_2_sen_slope_plot <- usgs_huc2_states_2_sen_slope_plot+
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

#usgs_huc2_states_2_sen_slope_plot
ggsave(
  paste0(folder_out,"2_sen_slope_plot_","site_type_",count_file,"_",sites_type,"_alpha_",alpha_value,".png"),
  plot = usgs_huc2_states_2_sen_slope_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(usgs_huc2_states_2_sen_slope_plot)


########################################
## Norm Sen Slope Plot #################
########################################



limit<-ceiling(max(abs(min_norm_senslope),abs(max_norm_senslope)))

num_low<-(-limit)
num_high<-(+limit)

no_range<-5

#values_calc<-c(num_low,-no_range,-no_range+0.1,no_range-0.1,no_range,num_high)
values_calc<-c(num_low,-0.1,0,+0.1,num_high)
values_calc_act<-(values_calc-num_low)/(num_high-num_low)

library(tidyverse)

usgs_huc2_states_3_norm_sen_slope_plot <- ggplot(data=data_each_case%>% 
                                              tidyr::drop_na(norm_sen_slope_6000))+
  geom_sf(aes(fill = norm_sen_slope_6000), color = NA) +
  theme_minimal() +
  geom_sf(data = huc2_sf_map, colour="black", fill=NA)+
  
  geom_sf(data = conus_sf_map, colour="grey60", fill=NA,linetype = "dashed") +
  #geom_sf(data = selected_huc2, 
  #             pattern = 'circle',
  #             fill = 'transparent',
  #             pattern_fill = 'blue',
  #             color = 'black') +
  
 # geom_sf_pattern(data = selected_huc2, 
  #                #aes(fill=HUC2),
   #               pattern="circle", 
                  #pattern_aspect_ratio = 3,
                  #pattern_scale = 0.5,
    #              fill='transparent',
                  #pattern_size=0.1
     #             pattern_spacing=0.03
      #            )+
  

  
  #scale_fill_gradientn(colours= c('grey','dodgerblue4'),name = "Increasing\nTrend",limit=c(0,70))+
  #scale_fill_gradient2(midpoint=0, 
  #                     low="saddlebrown", 
  #                     mid="grey",
  #                     high="dodgerblue4", 
  #                     space ="Lab",
  #                     limit=c(-25,+25))+
  scale_fill_gradientn(colours=c("saddlebrown",'tan1',"white","deepskyblue","dodgerblue"),
                       values=values_calc_act,
                       #na.value="white", 
                       #guide="colourbar",
                       name='RAMK - Sen Slope\n(%std dev change in 60 years)',
                       limits=c(num_low,num_high),
                       breaks=c(num_low,0,num_high), 
                       labels=c(num_low,0,num_high))+
  
  labs(y= "Latitude", x = "Longitude")+
  labs(fill='RAMK - Norm Sen Slope (%std dev change in 60 years)') +
  geom_sf_label(aes(label = if (class(norm_sen_slope_6000)=="numeric") paste(format(round(norm_sen_slope_6000,1), nsmall = 1), " ") else norm_sen_slope_6000))+
  ggtitle(paste0("Plot of ",nrow(data_combined%>%st_drop_geometry())," ", title_name, " for alpha = ", alpha_value))+
  #labs(color='Site Type (\u2265 60 years)') +
  coord_sf()#xlim = c(-130, -60),
#ylim = c(23, 53),expand=FALSE)
#usgs_huc2_states_3_norm_sen_slope_plot
rm(limit,
   num_low,num_high,no_range,values_calc,values_calc_act)

## Change legend title
#usgs_huc2_states_3_norm_sen_slope_plot <- usgs_huc2_states_3_norm_sen_slope_plot+labs(shape="Trend")

# Right -> inside the plot area
usgs_huc2_states_3_norm_sen_slope_plot <-usgs_huc2_states_3_norm_sen_slope_plot + theme(
  legend.position = c(.995, .35),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)

## add a north symbol and a scale bar with segments of 200 km
## Load ggspatial and ggsn package
usgs_huc2_states_3_norm_sen_slope_plot <- usgs_huc2_states_3_norm_sen_slope_plot+
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

#usgs_huc2_states_3_norm_sen_slope_plot
ggsave(
  paste0(folder_out,"3_norm_sen_slope_plot_","site_type_",count_file,"_",sites_type,"_alpha_",alpha_value,".png"),
  plot = usgs_huc2_states_3_norm_sen_slope_plot,
  width = 245,
  height = 150,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
rm(usgs_huc2_states_3_norm_sen_slope_plot)

rm(data_combined)
count_file<-count_file+1
}

#########################################################
#########################################################


rm(min_score, max_score)
rm(min_senslope, max_senslope)
rm(min_norm_senslope, max_norm_senslope)
rm(count_file)


