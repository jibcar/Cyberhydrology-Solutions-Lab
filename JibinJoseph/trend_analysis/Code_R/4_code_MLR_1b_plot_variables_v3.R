## Modifications
## 2024-02-24 - Automatic loop for number of classes

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
library(ggtext)

library(profvis)

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

##  Direction to Output Folder and also save the path the R code
folder_out<-paste0(folder_name_absolute,"\\Plots_IntermediateResults\\plots_multiple_regression_drivers_v3\\")

dir.create(folder_out, showWarnings = TRUE)


fileConn<-file(paste0(folder_out,"00_code_full_path.txt"))
writeLines(rstudioapi::getSourceEditorContext()$path, 
           fileConn)
close(fileConn)
rm(fileConn)

#############################
## Plotting the variables ###
#############################

huc2_sf<-st_read(paste0(folder_name_absolute,"/Data/Shapefiles/HUC2_modified/HUC2_modified5.shp"))
huc2_sf_map<-st_read(paste0(folder_name_absolute,"/Data/Shapefiles/HUC2_modified/HUC2_modified6.shp"))

conus_sf<-st_read(paste0(folder_name_absolute,"/Data/Shapefiles/cb_2018_us_conus_5m/cb_2018_us_conus_5m.shp"))
conus_sf_map<-st_read(paste0(folder_name_absolute,"/Data/Shapefiles/states_conterminous/states_conterminous.shp"))

regression_input_data5<-read.csv(paste0(folder_name_absolute,
                                        "\\Data_Intermediate\\data_multiple_regression_v1_",
                                        year_usgs_start,"_",
                                        year_usgs_end,
                                        "\\",
                                        "regression_input_data5_USGS_new2_",
                                        year_usgs_start,"_",year_usgs_end,
                                        "_dam_",
                                        year_dam_start,"_",year_dam_end,
                                        ".csv"),
                                 sep=',',
                                 colClasses = c("USGS_St" = "character",
                                                "HUC2" = "character"))

unique(regression_input_data5$HUC2)
length(unique(regression_input_data5$HUC2))

hcdn_gages_df<-read.csv(paste0(folder_name_absolute,"/Data_Intermediate/hcdn_gages_df_openedinMSExcel.csv"),
                        colClasses = c("STAID" = "character"))

unique(hcdn_gages_df$HUC02)
length(unique(hcdn_gages_df$HUC02))

# Load required library
library(stringr)
## Replace "10U" and "10L" with "10"
hcdn_gages_df$HUC02 <- str_replace(hcdn_gages_df$HUC02, "[UL]", "")
unique(hcdn_gages_df$HUC02)
length(unique(hcdn_gages_df$HUC02))

regression_input_data6<-merge(regression_input_data5,
                              hcdn_gages_df%>%dplyr::select(STAID,CLASS),
                              by.x = "USGS_St", 
                              by.y = "STAID", 
                              all.x = TRUE, 
                              all.y = FALSE)

class(regression_input_data6)
unique(regression_input_data6$CLASS)

regression_input_data6$norm_max_stor_percent<-regression_input_data6$norm_max_stor*100

# Load required library
library(ggplot2)

# Reshape data for plotting
data_long <- reshape2::melt(regression_input_data6%>%
                              dplyr::select(HUC2,CLASS,
                                     LU2019_Urb),
                            id.vars = "HUC2")



# Convert HUC2 to factor
regression_input_data5$HUC2 <- as.factor(regression_input_data5$HUC2)


## Did not work
## Create a dictionary mapping unique character values to new values
# mapping <- c("Ref" = "GAGES-II R",
#              "Non-ref" = "GAGES-II NR + O",
#              "NA" = "GAGES-II NR + O"
# )
unique(regression_input_data6$CLASS)
table(regression_input_data6$CLASS)
## Replace the values using the dictionary
regression_input_data6 <- regression_input_data6%>%
  mutate(CLASS2=recode(CLASS,"Ref" = "GAGES-II R",
                              "Non-ref" = "GAGES-II NR + O"))
                              
unique(regression_input_data6$CLASS2)

# Handling NA values separately
regression_input_data6$CLASS2[is.na(regression_input_data6$CLASS)] <- "GAGES-II NR + O"
unique(regression_input_data6$CLASS2)


## Specify the factor levels in the order you want for legend
regression_input_data6$CLASS2<-factor(regression_input_data6$CLASS2,
                                                  levels=c("GAGES-II R", 
                                                           "GAGES-II NR + O"))

# Define the variables you want to create boxplots for
variables <- c("Norm_Trend_slope","LU2019_Urb",'LU2019_Agr',"LU2019_For",
               'percent_max_decrease','percent_mean_decrease',
               "percent_mean_increase","percent_max_increase",
               "norm_max_stor","norm_max_stor_percent")

#variables <- c("Norm_Trend_slope")

## Make sure each variable in each class and each HUC2 region has 10 observations
 filtered_data_CONUS <- regression_input_data6 %>%
   dplyr::group_by(HUC2, CLASS) %>%
   # Filter out groups with fewer than 10 observations for each variable
   dplyr::filter(
     sum(!is.na(Norm_Trend_slope)) >= 10 &
       sum(!is.na(LU2019_Urb)) >= 10 &
       sum(!is.na(LU2019_Agr)) >= 10 &
       sum(!is.na(percent_max_decrease)) >= 10 &
       sum(!is.na(percent_mean_decrease)) >= 10 &
       sum(!is.na(percent_mean_increase)) >= 10 &
       sum(!is.na(percent_max_increase)) >= 10 &
       sum(!is.na(norm_max_stor)) >= 10
   )
 
 filtered_data_CONUS <-filtered_data_CONUS %>%
   arrange(DA_sqkm)

#############################################
 
 ## Box plot of drivers
 
#############################################
  


# Define custom colors for classes
class_colors <- c("Ref" = "lightgreen", "Non-ref" = "red")

# define the summary function
fun_manual_percentiles <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

num_obs <- 10 ## Number of observations in each class in different HUC2 regions

my_dict <- list(
  Norm_Trend_slope = "Norm Trend Slope",
  LU2019_Urb = "Urbanisation (%)",
  LU2019_Agr = "Agriculture (%)",
  LU2019_For="Forest (%)",
  percent_max_decrease="PCP Max Decrease (%)",
  percent_mean_decrease="PCP Mean Decrease (%)",
  percent_mean_increase="PCP Mean Increase(%)",
  percent_max_increase="PCP Max Increase (%)",
  norm_max_stor="Norm Max Storage",
  norm_max_stor_percent="Norm Max Storage (%)")

my_dict_boxplot_labels <- list(
  Norm_Trend_slope = c("0.0001","0.0010","0.0100","0.1000","1.0000"),
  LU2019_Urb = "Urbanisation (%)",
  LU2019_Agr = "Agriculture (%)",
  LU2019_For="Forest (%)",
  percent_max_decrease="PCP Max Decrease",
  percent_mean_decrease="PCP Mean Decrease",
  percent_mean_increase="PCP Mean Increase",
  percent_max_increase="PCP Max Increase",
  norm_max_stor="Norm Max Storage")

my_dict_num <- list(
  Norm_Trend_slope = "1",
  LU2019_Urb = "2",
  LU2019_Agr = "3",
  LU2019_For="4",
  percent_max_decrease="5",
  percent_mean_decrease="6",
  percent_mean_increase="7",
  percent_max_increase="8",
  norm_max_stor="9",
  norm_max_stor_percent="10")

###################
## Loop
###################



for (variable_selected in variables){
  
  #variable_selected<-"percent_max_increase"
  
  ###############################################
  ## Filter data for minimum number of sites ####
  ###############################################
  
  # Initialize an empty list to store filtered data
  filtered_data_list <- list()
  
  # Loop over each HUC2 region and class
  for (HUC02_region in unique(regression_input_data6$HUC2)) {
    for (class_type in unique(regression_input_data6$CLASS2)) {
      # Filter data for the current HUC2 region and class
      filtered_data <- regression_input_data6%>%
        select(HUC2,CLASS2,{{variable_selected}}) %>%
        filter(HUC2 == HUC02_region, CLASS2 == class_type)
      
      # Check if filtered data has at least 10 observations
      if (nrow(filtered_data) >= num_obs) {
        filtered_data_list[[paste(HUC02_region, class_type, sep = "_")]] <- filtered_data
      }
    }
  }
  
  # Combine filtered data from all iterations
  filtered_data_combined <- bind_rows(filtered_data_list)
  
  ###############################################
  ###############################################
  
# Create the boxplot
plot_driver<-ggplot(filtered_data_combined, 
          aes(x = HUC2, y = !!sym(variable_selected), group=HUC2
           #, fill = HUC2
           )) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = paste0("Boxplot of ",variable_selected," by HUC2 regions"),
       x = "HUC2",
       y = variable_selected) +
  scale_fill_brewer(palette = "Set3")  # Using a color palette

# Save the plot as a JPEG file
ggsave(paste0(folder_out,"Type1_boxplot_",my_dict_num[[variable_selected]],variable_selected,"_",
              year_usgs_start,"_",year_usgs_end,
              "_dam_",
              year_dam_start,"_",year_dam_end,".jpeg"), 
       plot_driver, 
       device = "jpeg",
       width = 20, height = 12, units = "cm")


# Create the boxplot
plot_driver_type2<-ggplot(filtered_data_combined, 
                          aes(x = HUC2, y = !!sym(variable_selected), 
                              color = CLASS2),
                              #,group=factor(HUC2)
                          drop=FALSE) +
  #geom_boxplot() +
  stat_summary(fun.data = fun_manual_percentiles, 
               geom="boxplot",
               position=position_dodge2(preserve = "single"))+
  stat_summary(fun.data = fun_manual_percentiles,
               geom = "errorbar",
               position=position_dodge2(preserve = "single"),
               aes(color=CLASS2))+
  # stat_summary(aes(fill = CLASS2),colour = "black",fun = mean,
  #              size = .4, shape = 5,
  #              #position = position_dodge2(preserve = "single")
  #              )+
  #scale_fill_manual(values = c("white","white")) +  # Specify custom colors
  scale_color_manual(values = c("blue", "red"))+
  scale_y_continuous(trans='log10')+
  #facet_wrap() +
  theme_minimal() +
  
  labs(title = paste0("<span style='font-size: 08px;'>",
                      paste0("DateTime:", Sys.time ()),
                      "</span><br/><span style='font-size: 18px;'>",
                      paste0("Boxplot of ",variable_selected," by grouped by HUC2 regions and GAGES-II classes"),
                      "</span><br/><span style='font-size: 12px;'>",
                      paste0("\nUSGS flow between:",year_usgs_start,"-",year_usgs_end," years, Dams completed between years:",year_dam_start,"_",year_dam_end," years."),
                      "</span><br/><span style='font-size: 12px;'>",
                      paste0("Each class in different HUC2 regions are plotted if ",num_obs," sites are present."),
                      "</span>"),
       x = "HUC02 Regions",
       y = my_dict[[variable_selected]],
       fill = "GAGES-II Class",
       color = "GAGES-II Class") +
  #coord_cartesian(ylim = c(0, 0.5))+
  theme(plot.title = element_markdown(lineheight = 1))


  

# Save the plot as a JPEG file
ggsave(paste0(folder_out,"Type2_boxplot_",my_dict_num[[variable_selected]],variable_selected,"_",
              year_usgs_start,"_",year_usgs_end,
              "_dam_",
              year_dam_start,"_",year_dam_end,".jpeg"), 
       plot_driver_type2, 
       device = "jpeg",
       width = 20, height = 14, units = "cm")

# Create the boxplot
plot_driver_type3<-ggplot(filtered_data_combined, 
                          aes(x = HUC2, y = !!sym(variable_selected), 
                              color = CLASS2),
                          #,group=factor(HUC2)
                          drop=FALSE) +
  #geom_boxplot() +
  stat_summary(fun.data = fun_manual_percentiles, 
               geom="boxplot",
               position=position_dodge2(preserve = "single"))+
  stat_summary(fun.data = fun_manual_percentiles,
               geom = "errorbar",
               position=position_dodge2(preserve = "single"),
               aes(color=CLASS2))+
  # stat_summary(aes(fill = CLASS2),colour = "black",fun = mean,
  #              size = .4, shape = 5,
  #              #position = position_dodge2(preserve = "single")
  #              )+
  #scale_fill_manual(values = c("white","white")) +  # Specify custom colors
  scale_color_manual(values = c("blue", "red"))+
  scale_y_continuous(trans='log10',
                     labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  #annotation_logticks(sides = 'lr') +
  #facet_wrap() +
  theme_minimal() +
  
  labs(title = paste0("<span style='font-size: 08px;'>",
                      paste0("DateTime:", Sys.time ()),
                      "</span><br/><span style='font-size: 18px;'>",
                      paste0("Boxplot of ",variable_selected," by grouped by HUC2 regions and GAGES-II classes"),
                      "</span><br/><span style='font-size: 12px;'>",
                      paste0("\nUSGS flow between:",year_usgs_start,"-",year_usgs_end," years, Dams completed between years:",year_dam_start,"_",year_dam_end," years."),
                      "</span><br/><span style='font-size: 12px;'>",
                      paste0("Each class in different HUC2 regions are plotted if ",num_obs," sites are present."),
                      "</span>"),
       x = "HUC02 Regions",
       y = my_dict[[variable_selected]],
       fill = "GAGES-II Class",
       color = "GAGES-II Class") +
  
  #coord_cartesian(ylim = c(0, 0.5))+
  theme(plot.title = element_markdown(lineheight = 1))




# Save the plot as a JPEG file
ggsave(paste0(folder_out,"Type3_boxplot_",my_dict_num[[variable_selected]],variable_selected,"_",
              year_usgs_start,"_",year_usgs_end,
              "_dam_",
              year_dam_start,"_",year_dam_end,".jpeg"), 
       plot_driver_type3, 
       device = "jpeg",
       width = 20, height = 14, units = "cm")


# Create the boxplot
plot_driver_type4<-ggplot(filtered_data_combined, 
                          aes(x = HUC2, y = !!sym(variable_selected), 
                              color = CLASS2),
                          #,group=factor(HUC2)
                          drop=FALSE) +
  #geom_boxplot() +
  stat_summary(fun.data = fun_manual_percentiles, 
               geom="boxplot",
               position=position_dodge2(preserve = "single"))+
  stat_summary(fun.data = fun_manual_percentiles,
               geom = "errorbar",
               position=position_dodge2(preserve = "single"),
               aes(color=CLASS2))+
  # stat_summary(aes(fill = CLASS2),colour = "black",fun = mean,
  #              size = .4, shape = 5,
  #              #position = position_dodge2(preserve = "single")
  #              )+
  #scale_fill_manual(values = c("white","white")) +  # Specify custom colors
  scale_color_manual(values = c("blue", "red"))+
  scale_y_continuous(trans='log10',
                     labels = function(x) sprintf("%.5f", 10^x))+
  #annotation_logticks(sides = 'lr') +
  #facet_wrap() +
  theme_minimal() +
  
  labs(title = paste0("<span style='font-size: 08px;'>",
                      paste0("DateTime:", Sys.time ()),
                      "</span><br/><span style='font-size: 18px;'>",
                      paste0("Boxplot of ",variable_selected," by grouped by HUC2 regions and GAGES-II classes"),
                      "</span><br/><span style='font-size: 12px;'>",
                      paste0("\nUSGS flow between:",year_usgs_start,"-",year_usgs_end," years, Dams completed between years:",year_dam_start,"_",year_dam_end," years."),
                      "</span><br/><span style='font-size: 12px;'>",
                      paste0("Each class in different HUC2 regions are plotted if ",num_obs," sites are present."),
                      "</span>"),
       x = "HUC02 Regions",
       y = my_dict[[variable_selected]],
       fill = "GAGES-II Class",
       color = "GAGES-II Class") +
  
  #coord_cartesian(ylim = c(0, 0.5))+
  theme(plot.title = element_markdown(lineheight = 1))




# Save the plot as a JPEG file
ggsave(paste0(folder_out,"Type4_boxplot_",my_dict_num[[variable_selected]],variable_selected,"_",
              year_usgs_start,"_",year_usgs_end,
              "_dam_",
              year_dam_start,"_",year_dam_end,".jpeg"), 
       plot_driver_type4, 
       device = "jpeg",
       width = 20, height = 14, units = "cm")

}



######################################################


######################################################
## Stats dataframe #################
######################################################


######################################################
library(dplyr)

# Define a function to calculate statistics
calculate_stats <- function(data) {
  mean_value <- mean(data, na.rm = TRUE)
  median_value <- median(data, na.rm = TRUE)
  fifth_percentile <- quantile(data, 0.05, na.rm = TRUE)
  ninety_fifth_percentile <- quantile(data, 0.95, na.rm = TRUE)
  num_obs_region_class<-length(data)
  standard_deviation<-sd(data,na.rm = TRUE)
  standard_error<-sd(data,na.rm = TRUE)/sqrt(length(data))*2
  
  return(data.frame(mean = mean_value, 
                    median = median_value, 
                    num_obs_region_class=num_obs_region_class,
                    fifth_percentile = fifth_percentile, 
                    ninety_fifth_percentile = ninety_fifth_percentile,
                    standard_deviation=standard_deviation,
                    standard_error=standard_error))
}

#######
## Loop Start #
#######



#ii<-1

for (variable_selected in variables){
  #variable_selected<-"Norm_Trend_slope"

  # Initialize an empty dataframe to store results
  stats_df <- data.frame(HUC02 = character(), CLASS = character(), 
                         num_obs = numeric(),
                         fifth_percentile = numeric(), 
                         mean = numeric(), 
                         median = numeric(), 
                         ninety_fifth_percentile = numeric(),
                         standard_deviation = numeric(), 
                         standard_error = numeric())
  
  for (class_type in unique(regression_input_data6$CLASS2)) {
    #class_type<-"GAGES-II R"
    filtered_data_CONUS2<-filtered_data_CONUS%>%
      ungroup()%>%
      select(CLASS2,{{variable_selected}})%>%
      filter(CLASS2 == class_type)
  
  
  
    # Calculate statistics
    stats <- calculate_stats(filtered_data_CONUS2[[2]])
    # Store results in dataframe
    stats_df <- rbind(stats_df, data.frame(HUC02 = "00", 
                                           CLASS = class_type,
                                           num_obs=stats$num_obs_region_class,
                                           fifth_percentile = stats$fifth_percentile,
                                           mean = stats$mean,
                                           median = stats$median,
                                           ninety_fifth_percentile = stats$ninety_fifth_percentile,
                                           standard_deviation = stats$standard_deviation,
                                           standard_error = stats$standard_error))
    
  
  # Loop over each HUC2 region and class
  for (HUC02_region in unique(regression_input_data6$HUC2)) {
    
      #HUC02_region<-"06"
      #class_type<-"Ref"
      # Filter data for the current HUC2 region and class
      # Initialize an empty list to store filtered data
      filtered_data_list <- list()
      
      # Loop over each HUC2 region and class
      
          # Filter data for the current HUC2 region and class
          filtered_data <- regression_input_data6%>%
            select(HUC2,CLASS2,{{variable_selected}}) %>%
            filter(HUC2 == HUC02_region, CLASS2 == class_type)
          
          # Check if filtered data has at least 10 observations
          if (nrow(filtered_data) >= num_obs) {
            filtered_data_list[[paste(HUC02_region, class_type, sep = "_")]] <- filtered_data
            # Combine filtered data from all iterations
            filtered_data_combined <- bind_rows(filtered_data_list)
            
            filtered_data <- filtered_data_combined %>%
              filter(HUC2 == HUC02_region, CLASS2 == class_type) %>%
              select({{variable_selected}})
            
            # Calculate statistics
            stats <- calculate_stats(filtered_data[[1]])
            # Store results in dataframe
            stats_df <- rbind(stats_df, data.frame(HUC02 = HUC02_region, 
                                                   CLASS = class_type,
                                                   num_obs=stats$num_obs_region_class,
                                                   fifth_percentile = stats$fifth_percentile,
                                                   mean = stats$mean,
                                                   median = stats$median,
                                                   ninety_fifth_percentile = stats$ninety_fifth_percentile,
                                                   standard_deviation = stats$standard_deviation,
                                                   standard_error = stats$standard_error))
            
          } else {
            stats_df <- rbind(stats_df, data.frame(HUC02 = HUC02_region, 
                                                   CLASS = class_type,
                                                   num_obs="<10",
                                                   fifth_percentile = NA,
                                                   mean = NA,
                                                   median = NA,
                                                   ninety_fifth_percentile = NA,
                                                   standard_deviation = NA, 
                                                   standard_error = NA))
          }
        
      
      
      
      
      
      
      #cat("Done for",HUC02_region," and ",class_type,"\n")
  }
  
}

# Print the dataframe
#write(stats_df)

stats_df <- stats_df %>%
  arrange(HUC02, desc(CLASS))

# Write the dataframe to an Excel file
openxlsx::write.xlsx(stats_df,
           paste0(folder_out,"drivers_boxplot_numbers2_",my_dict_num[[variable_selected]],"_",variable_selected,"_",
              year_usgs_start,"_",year_usgs_end,
              "_dam_",
              year_dam_start,"_",year_dam_end,".xlsx"), 
           sheetName = paste0(my_dict_num[[variable_selected]],"_",variable_selected), 
           rowNames = FALSE)

#ii<-ii+1
rm(filtered_data_CONUS2)
}

######################################################
######################################################
######################################################



######################################################
## Spatial plots of driver variables #################
######################################################
df<-filtered_data_CONUS%>%
  rename(USGS_SiteNo=USGS_St)

class(df$USGS_SiteNo)
# Convert integer values to character
df$USGS_SiteNo<- as.character(df$USGS_SiteNo)

class(df$USGS_SiteNo)

# Add leading zeros if necessary
#df$USGS_SiteNo <- sprintf("%08s", df$USGS_SiteNo)

df$USGS_SiteNo2 <- as.character(rep(NA, nrow(df)))


# Check each row and add leading zero if necessary
for (i in 1:nrow(df)) {
  cat(nchar(df$USGS_SiteNo[i]))
  if (nchar(df$USGS_SiteNo[i]) == 7) {
    df$USGS_SiteNo2[i] <- paste0("0", df$USGS_SiteNo[i])
  } else {
    df$USGS_SiteNo2[i] <- df$USGS_SiteNo[i]
  }
  
}

#class(filtered_data_CONUS$USGS_SiteNo)
# Print the modified dataframe
print(df)
write.csv(df,
          paste0(folder_out,"filtered_data_CONUS.csv"),
          row.names = FALSE)

usgs_sites_sf<-st_read(paste0(folder_name_absolute,"/Data_Intermediate/LULC_data/nlcd_2019_usgs_sites_3890.shp"))

usgs_sites_sf2<-usgs_sites_sf%>%
  select(USGS_St)%>%
  dplyr::filter(USGS_St %in% (df$USGS_SiteNo2))

usgs_sites_sf2$DA_sqmi <- units::set_units(st_area(usgs_sites_sf2), mi^2)

# Perform left join
usgs_sites_sf3 <- dplyr::left_join(usgs_sites_sf2, df, by = c("USGS_St" = "USGS_SiteNo2"))

usgs_sites_sf3$norm_max_stor<-usgs_sites_sf3$norm_max_stor*100

save_shapefile="Yes"

aaa<-usgs_sites_sf3%>%arrange(DA_sqkm)
bbb<-usgs_sites_sf3%>%arrange(desc(DA_sqkm))

if(save_shapefile=="Yes"){
  sf::st_write(aaa,
               paste0(folder_out,
                      "filtered_shapefile_CONUS_ascend.gpkg"),
               driver="gpkg",
               append=FALSE)
  sf::st_write(bbb,
               paste0(folder_out,
                      "filtered_shapefile_CONUS_descend.gpkg"),
               driver="gpkg",
               append=FALSE)
}

# Load the shapefile
#usgs_sites_sf3 <- st_read(paste0(folder_out,"filtered_shapefile_CONUS.shp"))
variables <- c("Norm_Trend_slope",
               "LU2019_Urb",
               'LU2019_Agr',
               'LU2019_For',
               'percent_max_decrease','percent_mean_decrease',
               "percent_mean_increase","percent_max_increase",
               "norm_max_stor"
  )

#variables <- c("LU2019_Urb")

# Create a dictionary-like named list




my_dict_decimals <- list(
  Norm_Trend_slope = 3,
  LU2019_Urb = 2,
  LU2019_Agr = 2,
  LU2019_For=2,
  percent_max_decrease=2,
  percent_mean_decrease=2,
  percent_mean_increase=2,
  percent_max_increase=2,
  norm_max_stor=3)

my_dict_classes <- list(
  Norm_Trend_slope = 6,
  LU2019_Urb = 8,
  LU2019_Agr = 6,
  LU2019_For=6,
  percent_max_decrease=6,
  percent_mean_decrease=6,
  percent_mean_increase=6,
  percent_max_increase=6,
  norm_max_stor=6)



class(usgs_sites_sf3$Norm_Trend_slope)

for (i in 1:nrow(usgs_sites_sf3)) {
  cat(class(usgs_sites_sf3$LU2019_Urb[i]))
  
}

#usgs_sites_sf3$Norm_Trend_slope<-as.numeric(usgs_sites_sf3$Norm_Trend_slope)

#data<-usgs_sites_sf3%>%
#  select(Norm_Trend_slope)

#summary(data$Norm_Trend_slope)

# Determine the number of classes for natural breaks
num_classes <- 6  # You can adjust this value based on your data




ii<-1
for (variable_selected in variables){
  
  #variable_selected <- "LU2019_Urb"
  
  # Create a gradient palette from yellow to grey to dark blue to black
  gradient_palette <- colorRampPalette(c("black", "darkblue", "grey", "yellow"))
  
  # Generate 6 colors transitioning between the specified colors
  colors2 <- gradient_palette(my_dict_classes[[variable_selected]])
  
  # Print the generated colors
  print(colors2)
  
  barplot(rep(1, my_dict_classes[[variable_selected]]), col = colors2, border = NA)

#class(usgs_sites_sf3$Norm_Trend_slope)



# Define your two colors
#color_low <- "blue"
#color_high <- "red"

# Calculate natural breaks
values_4_breaks<-usgs_sites_sf3[variable_selected]%>% na.omit()%>%
  st_drop_geometry()
breaks_jenks <- classInt::classIntervals(values_4_breaks[[1]], 
                                         n = my_dict_classes[[variable_selected]], 
                                         style = "jenks")$brks
breaks_quantile <- classInt::classIntervals(values_4_breaks[[1]], 
                                            n = my_dict_classes[[variable_selected]], 
                                            style = "quantile")$brks

# Calculate differences between consecutive breaks
break_diff <- diff(breaks_quantile)

# Check if all differences are positive
if (all(break_diff > 0)) {
  print("Breaks are non-overlapping")
  
} else {
  print("Breaks are overlapping")
  breaks_quantile<-breaks_jenks
}

if(variable_selected=="LU2019_Urb"){
  breaks_quantile<-breaks_jenks
}

data_4_plot <- usgs_sites_sf3%>%
  select({{variable_selected}})

variable_selected_category<-paste0(variable_selected,"_groups")



#factor(data_4_plot2[[variable_selected_category]])



data_4_plot_list <- list()

# Iterate over each class
for (i in 1:(my_dict_classes[[variable_selected]])){
  #i=num_classes - 1
  print(i)
  # Initialize an empty list to store filtered data
  
  # Create the condition for the current class
  if (i==1){
    condition <- paste0(variable_selected," >= breaks_quantile[", i, "] & ",variable_selected," <= breaks_quantile[", i + 1, "]")
    cat(i,"condition1\n")
  } else if(i==my_dict_classes[[variable_selected]]){
    condition <- paste0(variable_selected," > breaks_quantile[", i, "]")
    cat(i,"condition3\n")
    
  }else{
    condition <- paste0(variable_selected," > breaks_quantile[", i, "] & ",variable_selected," <= breaks_quantile[", i + 1, "]")
    cat(i,"condition2\n")
  }#condition2 <- paste0("!!variable_selected")
  #print(eval(parse(text = condition)))
  #print(rlang::parse_expr(condition))
  
  #print(eval(parse(text =paste0("!!sym(variable_selected)" ))))
  
  # Mutate the dataframe based on the condition
  data_4_plot_add<- data_4_plot %>%
    dplyr::filter(eval(parse(text = condition)))%>%
    mutate(
      !!variable_selected_category := case_when(
        eval(parse(text = condition)) ~ paste0(round(breaks_quantile[i], my_dict_decimals[[variable_selected]]), " to ", round(breaks_quantile[i + 1], my_dict_decimals[[variable_selected]])),
        TRUE ~ "Other"
      )
    )
  data_4_plot_list[[paste(variable_selected, i, sep = "_")]]<-data_4_plot_add
  
}

data_4_plot_combined<- bind_rows(data_4_plot_list)


cbu<-(unique(data_4_plot_combined[[variable_selected_category]]))
print(cbu)
data_4_plot_combined[[variable_selected_category]]<-factor(data_4_plot_combined[[variable_selected_category]],
                                              levels=cbu
                                               )



#profvis({
plot_polygon<- # Plot the shapefile using ggplot
  ggplot()+
  geom_sf(data = data_4_plot_combined%>% na.omit(),
              aes(#x=Station_Lon, #
                #y=Station_Lat,
                fill = !!sym(variable_selected_category)),
          colour=NA) +
  theme_minimal()+
  geom_sf(data = conus_sf_map, 
          colour="grey", 
          fill=NA,
          linetype = "dashed", 
          linewidth=0.5) +
  #geom_sf(data = regions_sf_map, colour="grey60", fill="transparent", linetype = "solid", linewidth=0.5) +
  geom_sf(data = huc2_sf_map, 
          colour="#800020",##3EAD92", 
          fill="transparent", 
          linetype = "solid", 
          linewidth=0.5)+
  coord_sf()+
  
  #scale_fill_gradient2(name=variable_selected,
                        #low='white', high='grey20'
   #low = "lightblue", mid="blue", high = "pink") +  # Adjust color scale as needed
  #scale_fill_viridis_c(name = variable_selected, option = "C") +
  #scale_fill_gradientn(colours = c("cyan", "black", "red"))+
  #scale_fill_gradientn(name = variable_selected, breaks = breaks, limits = range(breaks),
  #                    colorlow = "lightblue", middle = "blue", high = "pink") +
  # scale_fill_gradientn(name = variable_selected, 
  #                      colours = c("lightblue", "blue", "red"),  # Adjust color scale as needed
  #                      breaks = breaks, 
  #                      #limits = range(breaks),
  #                      na.value = "grey50") +  # Specify break points and colors
  # scale_fill_gradientn(colors = c("lightgreen", "blue", "red"),
  #                      values = scales::rescale(breaks_quantile),
  #                      breaks = breaks,
  #                      #labels = scales::scientific,
  #                      #scipen = 1
  #)+
  scale_fill_manual(values=colors2
                      #c("yellow","#FFCC00",
    #'#ff0000','#cc0000','#990000',"#660000"
    #"#CCCCCC","#666666","#000666","#000333")
    ,labels = paste(levels(data_4_plot_combined[[variable_selected_category]])
                    #,' ('
                    #,table(data_4_plot2[[variable_selected_category]])
                    #,'-'
                    #,round(table(usgs_huc2_acf_sf_full$site_classification)/nrow(usgs_huc2_acf_sf_full)*100,1)
                    #,"%"
                    #,")"
                    , sep = ""))+
  labs(fill=my_dict[[variable_selected]]) +
  labs(y= "Latitude", x = "Longitude")+




  ## Right -> inside the plot area
  theme(
    legend.position = c(.999, .40),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )+
  
  ggspatial::annotation_north_arrow(
    location = "tr", 
    which_north = "true",
    pad_x = unit(1, "in"), 
    pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = ""
    ))+
  ggsn::scalebar(conus_sf_map, dist = 1000, dist_unit = "km", 
                 st.size = 3,location = "bottomleft",
                 transform = TRUE, model = "WGS84")

#plot_polygon

# Save the plot as an image file
ggsave(paste0(folder_out,"Type5_spatial_",my_dict_num[[variable_selected]],
              variable_selected,"_driver_plot.png"),
       plot_polygon,
       width = 245,
       height = 150,
       units = c("mm"),
       dpi = 300,
       limitsize = TRUE)
#})

ii<-ii+1

}
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################


# Create the boxplot with facets
ggplot(regression_input_data6, 
       aes(x = HUC2, y = LU2019_Urb, fill = CLASS
           #,group=factor(HUC2)
           )) +
  geom_boxplot() +
  scale_fill_manual(values = class_colors) +  # Specify custom colors
  #facet_wrap() +
  theme_minimal() +
  labs(title = "Boxplot of LU2019_Urb by HUC2 and Class",
       x = "HUC2",
       y = "LU2019_Urb",
       fill = "Class")

# Create the boxplot without facets
ggplot(regression_input_data6, aes(x = HUC2, y = LU2019_Urb, fill = factor(CLASS),group=factor(HUC2))) +
  geom_boxplot(position = "dodge") +  # Dodge positions the boxes side by side
  theme_minimal() +
  labs(title = "Boxplot of LU2019_Urb by HUC2 and Class",
       x = "HUC2",
       y = "LU2019_Urb",
       fill = "Class") +
  scale_fill_brewer(palette = "Set3")  # Using a color palette


######################
######################

# Create the boxplot
ggplot(data_long
                , aes(x = variable, y = value, fill = HUC2)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of Variables by HUC2 Regions",
       x = "Variables",
       y = "Values") +
  scale_fill_brewer(palette = "Set3")  # Using a color palette

data_sf<-merge(usgs_sites2_sf,
               regression_input_data5,
               by.x="USGS_St",
               by.y="USGS_SiteNo",
               all.y=TRUE)

#sf::plot_sf(data_sf)
colnames(data_sf)
#plot(data_sf["Norm_Trend_slope"],breaks='jenks', key.pos = 1, axes = TRUE, key.width = lcm(1.3), key.length = 1.0)
#plot(data_sf["LU_Urb"],breaks='jenks', key.pos = 1, axes = TRUE, key.width = lcm(1.3), key.length = 1.0)
#plot(data_sf["LU_Agr"],breaks='jenks', key.pos = 1, axes = TRUE, key.width = lcm(1.3), key.length = 1.0)
#plot(data_sf["LU_For"],breaks='jenks', key.pos = 1, axes = TRUE, key.width = lcm(1.3), key.length = 1.0)
#plot(data_sf["percent_max_increase"],breaks='jenks', key.pos = 1, axes = TRUE, key.width = lcm(1.3), key.length = 1.0)
#plot(data_sf["percent_max_decrease"],breaks='jenks', key.pos = 1, axes = TRUE, key.width = lcm(1.3), key.length = 1.0)
#plot(data_sf["percent_mean_increase"],breaks='jenks', key.pos = 1, axes = TRUE, key.width = lcm(1.3), key.length = 1.0)
#plot(data_sf["percent_mean_decrease"],breaks='jenks', key.pos = 1, axes = TRUE, key.width = lcm(1.3), key.length = 1.0)
#plot(data_sf["norm_max_stor"], key.pos = 1, axes = TRUE, key.width = lcm(1.3), key.length = 1.0)


colnames(data_sf)

# Define your two colors
color_low <- "blue"
color_high <- "red"

# Calculate the midpoint based on your data
midpoint <- (max(data_sf$percent_mean_decrease) + min(data_sf$percent_mean_decrease)) / 2


plot_mlr_var <- ggplot() + 
  geom_sf(data = data_sf, 
          aes(fill = percent_mean_decrease)) + 
  scale_fill_viridis_c() 

data_sf<-data_sf%>%dplyr::arrange(desc(percent_mean_decrease))

# Determine the number of quantiles for breaks
num_quantiles <- 5  # You can adjust this value based on your data

# Calculate quantiles for breaks
breaks <- quantile(data_sf$Norm_Trend_slope, probs = seq(0, 1, length.out = num_quantiles + 1))

# Create the custom gradient color scale with quantile breaks
plot_mlr_var <- ggplot() + 
  geom_sf(data = data_sf, aes(fill = Norm_Trend_slope)) + 
  geom_sf(data = conus_sf_map, colour = "grey", fill = NA, linetype = "dashed", linewidth = 0.5) +
  geom_sf(data = huc2_sf_map, colour = "grey30", fill = "transparent", linetype = "solid", linewidth = 0.5) +
  scale_fill_gradientn(colors = c("blue","green4"),
                       values = scales::rescale(breaks),
                       breaks = breaks,
                       labels = scales::scientific) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5))


ggsave(paste0(folder_out,"plot_Norm_Trend_Slope.png"), 
       plot_mlr_var, 
       width = 6, height = 4, 
       dpi = 300)



# Determine the number of classes for natural breaks
num_classes <- 5  # You can adjust this value based on your data

# Calculate natural breaks
breaks <- classInt::classIntervals(data_sf$percent_mean_decrease, n = num_classes)$brks

# Create the custom gradient color scale with natural breaks
plot_mlr_var2 <- ggplot() + 
  geom_sf(data = data_sf, aes(fill = percent_mean_decrease)) + 
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  #geom_sf(data = regions_sf_map, colour="grey60", fill="transparent", linetype = "solid", linewidth=0.5) +
  geom_sf(data = huc2_sf_map, colour="#3EAD92", fill="transparent", linetype = "solid", linewidth=0.5)+
  scale_fill_gradientn(#colors = c(color_low, "white", color_high),
    colors = c(high=color_low, low=color_high),
    values = scales::rescale(breaks),
    breaks = breaks,
    labels = scales::number_format(accuracy = 1),
    trans = "log1p")

# Determine the number of quantiles for breaks
num_quantiles <- 5  # You can adjust this value based on your data

# Calculate quantiles for breaks
breaks <- quantile(data_sf$norm_max_stor, probs = seq(0, 1, length.out = num_quantiles + 1))

# Create the custom gradient color scale with quantile breaks
plot_mlr_var3 <- ggplot() + 
  geom_sf(data = data_sf, aes(fill = norm_max_stor)) + 
  geom_sf(data = conus_sf_map, colour="grey", fill=NA,linetype = "dashed", linewidth=0.5) +
  #geom_sf(data = regions_sf_map, colour="grey60", fill="transparent", linetype = "solid", linewidth=0.5) +
  geom_sf(data = huc2_sf_map, colour="grey20", fill="transparent", linetype = "solid", linewidth=0.5)+
  #scale_fill_gradientn(colors = c(color_low, "white", color_high),
  #                    values = scales::rescale(breaks),
  #                     breaks = breaks,
  #labels = scales::number_format(accuracy = 3,
  #                              scientific=TRUE)
  scale_fill_gradientn(colors = c(color_low, "white", color_high),
                       values = scales::rescale(breaks),
                       breaks = breaks,
                       labels = scales::scientific,
                       #scipen = 1
  ) + 
  
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5))



ggsave(paste0(folder_out,"plot_norm_max_stor_quantiles.png"), plot_mlr_var3, width = 6, height = 4, dpi = 300)

summary(data_sf$norm_max_stor)

quantile(data_sf$norm_max_stor,0.95)




###########################
library(ggforce)

p<-ggplot() + 
  geom_point(data = regression_input_data5, 
             aes(x=Station_Lon,
                 y=Station_Lat,
                 color = as.numeric(norm_max_stor)),
             #size=3
  ) + 
  theme_minimal()
#scale_y_continuous(breaks = 34:36)
#scale_y_continuous()+

p
p<-p+scale_color_gradient(low = "yellow", high = "darkblue")# low = "blue", high = "#CB454A")#scale_fill_gradient2(low = "red",mid = scales::muted("purple"),high = "blue")
p
p<-p+scale_fill_gradient(low = "yellow", high = "darkblue")#low = "blue", high = "#CB454A")
p

library("ggExtra")
p_a<-ggMarginal(p, 
                #groupColour = TRUE, 
                #groupFill = TRUE,
                type="histogram")
p_a
summary(regression_input_data5$norm_max_stor)

rm(p,p_a)

p1<-ggplot() + 
  geom_point(data = regression_input_data5, 
             aes(x=Station_Lon,
                 y=Station_Lat,
                 color = MaxStorage,
                 fill = MaxStorage),
             size=3) + 
  #scale_y_continuous(breaks = 34:36)
  #scale_y_continuous()+
  theme_minimal()






p1
p1<-p1+scale_color_gradient(low = "yellow", high = "darkblue")# low = "blue", high = "#CB454A")#scale_fill_gradient2(low = "red",mid = scales::muted("purple"),high = "blue")
p1
p1<-p1+scale_fill_gradient(low = "yellow", high = "darkblue")#low = "blue", high = "#CB454A")
p1


library("ggExtra")
p1_a<-ggMarginal(p1, 
                 #groupColour = TRUE, 
                 #groupFill = TRUE,
                 type="histogram")
p1_a
summary(regression_input_data5$MaxStorage)

rm(p1,p1_a)


## Using Natural Jenks
# create Natural Jenks function
jenks_natural <- function(data, var, breaks){
  
  # data <- copy(iris)
  # var <- "Petal.Length"
  # breaks <- 5
  
  
  # convert df to data.table  
  setDT(data)
  
  # name of new column
  newvar <- paste0(var,"_jenks")
  
  # calculate jenks natural breaks
  data[, paste0(newvar) := as.character(cut(get(var), breaks= getJenksBreaks(get(var), breaks), include.lowest = TRUE, dig.lab=3)) ]
  
  # Edit factor text
  data[, paste0(newvar) := str_replace_all(get(newvar), "\\[|\\(|\\]", "") ]
  data[, paste0(newvar) := stri_replace_all_regex(get(newvar), "[,]", " - ") ]
  
  # get factor labels
  jenks_labels  <- data[, get(newvar)]  %>% table %>% names() %>% sort(decreasing = F) 
  
  # recode variable
  data[, paste0(newvar) := factor(get(newvar), levels = jenks_labels)]
  
  return(data)
}


natural_breaks <- function(df, var) {
  var_breaks <- BAMMtools::getJenksBreaks(df[[var]], k = 6)
  df[[paste0('jenks_', var)]] <- findInterval(df[[var]], var_breaks)
  df
}


# apply function
regr_shp <- natural_breaks(regression_input_data5, "norm_max_stor")

library(classInt)
classes <- classIntervals(regression_input_data5$norm_max_stor, n = 6, style = "fisher")

regr_shp2 <- regression_input_data5 %>% mutate(jenks_plot = cut(regression_input_data5$norm_max_stor, breaks=classes$brks, include.lowest = T,
                                                                labels=scales::comma(classes$brks[2:length(classes$brks)]),0) )


p2 <- ggplot() + 
  geom_point(data=regr_shp2,
             aes(Station_Lon,
                 y=Station_Lat,
                 color=factor(jenks_plot),
                 fill=factor(jenks_plot)),
             size=3)
p2

# Install RColorBrewer package
library("RColorBrewer")                            # Load RColorBrewer
p2<-p2+
  scale_fill_brewer(palette = "PiYG", direction=-1, name="norm_max_stor")+
  scale_color_brewer(palette = "PiYG", direction=-1, name="norm_max_stor")+
  theme_void() 


p2

rm(regr_shp)




usgs_huc2_states_0summary_plot <- ggplot()+
  geom_point(data=data_sf,
             aes(Station_Lon,y=Station_Lat,
                 color=site_classification,shape=site_classification))+
  theme_minimal() +
  geom_sf(data = huc2_sf_map, colour="black", fill=NA)+
  geom_sf(data = conus_sf_map, colour="grey60", fill=NA,linetype = "dashed") +
  scale_shape_manual(values = c(15,16,3,5)
                     ,labels = paste(levels(usgs_huc2_acf_sf$site_classification)
                                     ,' ('
                                     ,table(usgs_huc2_acf_sf$site_classification)
                                     #,'-'
                                     #,round(table(usgs_huc2_acf_sf$site_classification)/nrow(usgs_huc2_acf_sf)*100,1)
                                     #,"%"
                                     ,")"
                                     , sep = "")
  )+
  scale_color_manual(values=c('saddlebrown','sandybrown','deepskyblue','dodgerblue3')
                     ,labels = paste(levels(usgs_huc2_acf_sf$site_classification)
                                     ,' ('
                                     ,table(usgs_huc2_acf_sf$site_classification)
                                     #,'-'
                                     #,round(table(usgs_huc2_acf_sf$site_classification)/nrow(usgs_huc2_acf_sf)*100,1)
                                     #,"%"
                                     ,")"
                                     , sep = ""))+
  labs(y= "Latitude", x = "Longitude")+
  ggtitle(paste0("Plot of ",nrow(usgs_huc2_acf_sf%>%st_drop_geometry()), " USGS sites for alpha = ", alpha_value))+ # for the main title+
  
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




