

library(dplyr)
library(car) ##for avPlots
library(sf)

## Clear all variables
rm(list=ls())

## Clear Console
cat("\014")


username="joseph57"
user="user"

year_dam_start="all"
year_dam_end="all"

year_usgs_start="1950"
year_usgs_end="2014"

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

#####################################
#####################################


regression_input_data5<-read.csv(paste0("C:\\Users\\",
                                        username,
                                        "\\Dropbox\\01_SharedFolder_Jibin\\Code_R\\code_multiple_regression\\data\\",
                                        "regression_input_data5_USGS_new_",
                                        year_usgs_start,"_",year_usgs_end,
                                        "_dam_",
                                        year_dam_start,"_",year_dam_end,
                                        ".csv"),
                                 sep=',',
                                 colClasses = c("USGS_St" = "character",
                                                "HUC2" = "character"))

colnames(regression_input_data5)

##################################
#### Entire US ###################
## Multiple Linear Regression ####
##################################

# Create a vector with row names repeating 7 times
row_names <- rep(c("LU2019_Urb",
                   "LU2019_Agr",
                   #"LU2019_For",
                   "percent_max_increase",
                   "percent_max_decrease",
                   "percent_mean_increase",
                   "percent_mean_decrease",
                   "norm_max_stor","(Intercept)"))


# Create a dataframe with 126 rows and 5 columns
barchart_data <- data.frame(
  HUC2_code = rep(NA, 8),
  HUC2_name = rep(NA, 8),
  Variable_Name = rep(NA, 8),
  Significance = rep(NA, 8),
  VarianceExplained_percent = rep(NA, 8),
  Number_Sites = rep(NA, 8),
  Multiple_Rsquared = rep(NA, 8)
)

rownames(barchart_data) <- row_names

data_mlr<-regression_input_data5%>%
  #st_drop_geometry()%>%
  filter(MK_Combi_Trend%in%c("decreasing","increasing"))%>% #,"increasing"
  #filter(CLASS=='Ref')%>%
  #filter(HUC2 %in% c('18')) %>%
  select(Norm_Trend_slope,
         MK_Combi_slope,
         #Trend_Result,
         LU2019_Urb, LU2019_Agr, LU2019_For,
         percent_max_increase,percent_max_decrease,
         percent_mean_increase,percent_mean_decrease,
         norm_max_stor)


model <- lm(Norm_Trend_slope ~ 
  #MK_Combi_slope ~
    LU2019_Urb #+ 
  +LU2019_Agr
  #+LU2019_For#
  +percent_max_increase
  +percent_max_decrease
  +percent_mean_increase
  +percent_mean_decrease
  +norm_max_stor
  ,data = data_mlr)

# Get the coefficient matrix
coefs <- summary(model)$coefficients
#colnames(coefs)

# Identify the variables with "3 stars"
vars <- rownames(coefs)[which(coefs[, 4] < 0.05)]
#vars

#class(vars)


#anova(model)
af <- anova(model)
afss <- af$"Sum Sq"

#print(cbind(af,PctExp=afss/sum(afss)*100))
df2<-as.data.frame(cbind(af,PctExp=afss/sum(afss)*100))


for (name in row_names) {
  #print(name)
  #print(barchart_data[name,'Significance'])
  
  if (coefs[name,"Pr(>|t|)"] < 0.05){
    #print("Yes")
    barchart_data[name,'Significance']<-"sig"
  }else{
    barchart_data[name,'Significance']<-"insig"
  }
  barchart_data[name,'Variable_Name']<-name
  
  
  if(name=="(Intercept)"){
    barchart_data[name,'VarianceExplained_percent']<-df2['Residual','PctExp']
    
  }else{
    barchart_data[name,'VarianceExplained_percent']<-df2[name,'PctExp']
  }
  
}
barchart_data$HUC2_code<-"00"
barchart_data$HUC2_name<-"CONUS"

barchart_data$Number_Sites<-nrow(data_mlr)
#barchart_data$Number_Sites<-nrow(data_mlr)

## Returning multiple R-squared
barchart_data$Multiple_Rsquared<-summary(model)$r.squared                      

## Create an empty list
df_list<-list()
df_list<-append(df_list, list(barchart_data))


#########################################
## HUC2 analysis 
## To find the drivers and its values
##########################################


huc2_sf<-st_read(paste0("C:/Users/",
                        username,
                        "/Box/01_SharedBox_Jibin/Data/Shapefiles/HUC2_modified/HUC2_modified5.shp"))
huc2_sf_code_name<-huc2_sf%>%
  st_drop_geometry()%>%
  select(HUC2,NAME)

rownames(huc2_sf_code_name) <- huc2_sf_code_name$HUC2


huc_num_list <-huc2_sf_code_name$HUC2


for (huc_num in huc_num_list){
  
  # Create a vector with row names repeating 7 times
  row_names <- rep(c("LU2019_Urb",
                     "LU2019_Agr",
                     #"LU2019_For",
                     "percent_max_increase",
                     "percent_max_decrease",
                     "percent_mean_increase",
                     "percent_mean_decrease",
                     "norm_max_stor","(Intercept)"))
  
  
  # Create a dataframe with 126 rows and 5 columns
  barchart_data <- data.frame(
    HUC2_code = rep(NA, 8),
    HUC2_name = rep(NA, 8),
    Variable_Name = rep(NA, 8),
    Significance = rep(NA, 8),
    VarianceExplained_percent = rep(NA, 8),
    Number_Sites = rep(NA, 8),
    Multiple_Rsquared = rep(NA, 8)
  )
  
  rownames(barchart_data) <- row_names
  
  rm(model)
  
  
  data_mlr<-regression_input_data5%>%
    #st_drop_geometry()%>%
    filter(MK_Combi_Trend%in%c("decreasing","increasing"))%>% #,"increasing"
    #filter(CLASS=='Ref')%>%
    filter(HUC2==huc_num)%>%
    select(Norm_Trend_slope,
           #Trend_Result,
           LU2019_Urb, LU2019_Agr, LU2019_For,
           percent_max_increase,percent_max_decrease,
           percent_mean_increase,percent_mean_decrease,
           norm_max_stor)
  
    model <- lm(Norm_Trend_slope ~ LU2019_Urb #+ 
              +LU2019_Agr
              #+LU2019_For
              +percent_max_increase
              +percent_max_decrease
              +percent_mean_increase
              +percent_mean_decrease
              +norm_max_stor
              ,data = data_mlr)
  
  
  # Get the coefficient matrix
  coefs <- summary(model)$coefficients
  
  # Identify the variables with "3 stars"
  vars <- rownames(coefs)[which(coefs[, 4] < 0.05)]
  #vars
  
  
  af <- anova(model)
  afss <- af$"Sum Sq"
  df_results<-cbind(af,PctExp=afss/sum(afss)*100)

  
  for (name in row_names) {
    #print(name)
    #print(barchart_data[name,'Significance'])
    if (name %in% vars){
      if (coefs[name,"Pr(>|t|)"] < 0.05){
        #print("Yes")
        barchart_data[name,'Significance']<-"sig"
      }
    }else{
      barchart_data[name,'Significance']<-"insig"
    }
    
    barchart_data[name,'Variable_Name']<-name
    
    
    if(name=="(Intercept)"){
      barchart_data[name,'VarianceExplained_percent']<-df_results['Residual','PctExp']
      
    }else{
      barchart_data[name,'VarianceExplained_percent']<-df_results[name,'PctExp']
    }
    
    }
  
  barchart_data$HUC2_code<-huc_num
  barchart_data$HUC2_name<-huc2_sf_code_name[huc_num,"NAME"]
  
  barchart_data$Number_Sites<-nrow(data_mlr)
  barchart_data$Number_Sites<-nrow(data_mlr)
  ## Returning multiple R-squared
  barchart_data$Multiple_Rsquared<-summary(model)$r.squared                      
  
  
 
  df_list<-append(df_list, list(barchart_data))
  
 
}
merged_df <- do.call(rbind, df_list)
write.csv(merged_df, 
          paste0("C:\\Users\\",
                 username,
                 "\\Dropbox\\01_SharedFolder_Jibin\\Code_R\\code_multiple_regression\\data\\",
                 "barchart_data_type1_USGS_",
                 year_usgs_start,"_",year_usgs_end,
                 "_dam_",
                 year_dam_start,"_",year_dam_end,
                 ".txt"), 
          row.names = F)
