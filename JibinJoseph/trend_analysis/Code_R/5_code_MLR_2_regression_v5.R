## Modifications
## 2024-03-08
## Added Pearson correlation coefficient
## Added Correlation Coefficient,SE,tstat,pvalue in the table
## 2024-02-17
## Split (data prep + plot) again into two subsets
## 2024-01-13
## Split data prep + plot + MLR into two subsets

library(dplyr)
library(car) ##for avPlots
library(sf)

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
folder_plot_out<-paste0(folder_name_absolute,"\\Plots_IntermediateResults\\plots_multiple_regression_v1_",
                        year_usgs_start,"_",
                        year_usgs_end,
                        "\\")
folder_data_out<-paste0(folder_name_absolute,"\\Data_Intermediate\\data_multiple_regression_v1_",
                        year_usgs_start,"_",
                        year_usgs_end,
                        "\\")

fileConn<-file(paste0(folder_plot_out,"00_code_full_path.txt"))

writeLines(rstudioapi::getSourceEditorContext()$path, 
           fileConn)
close(fileConn)
rm(fileConn)

#####################################
#####################################

#filename_part='1950_2014'##"all"



regression_input_data5<-read.csv(paste0(folder_data_out,
                                        #"regression_input_data5_USGS_new_",
                                        "regression_input_data5_USGS_new2_",
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
# row_names <- rep(c("LU2019_Urb",
#                    "LU2019_Agr",
#                    #"LU2019_For",
#                    "percent_max_increase",
#                    "percent_max_decrease",
#                    "percent_mean_increase",
#                    "percent_mean_decrease",
#                    "norm_max_stor","(Intercept)"))
row_names <- rep(c("LU2019_Urb",
                   "LU2019_Agr",
                   #"LU2019_For",
                   "Weigh_Norm_Max_TS",
                   "Weigh_Norm_Mean_TS",
                   #"percent_mean_increase",
                   #"percent_mean_decrease",
                   "norm_max_stor",
                   "(Intercept)"))


# Create a dataframe with 126 rows and 5 columns
barchart_data <- data.frame(
  HUC2_code = rep(NA, length(row_names)),
  HUC2_name = rep(NA, length(row_names)),
  Variable_Name = rep(NA, length(row_names)),
  CorrelationCoefficient = rep(NA, length(row_names)),
  StandardError = rep(NA, length(row_names)),
  tStat = rep(NA, length(row_names)),
  pvalue = rep(NA, length(row_names)),
  Significance = rep(NA, length(row_names)),
  VarianceExplained_percent = rep(NA, length(row_names)),
  Number_Sites = rep(NA, length(row_names)),
  Multiple_Rsquared = rep(NA, length(row_names)),
  PearsonCorr_with_NormTrendSlope = rep(NA, length(row_names))
)

rownames(barchart_data) <- row_names

data_mlr<-regression_input_data5%>%
  #st_drop_geometry()%>%
  filter(MK_Combi_Trend%in%c("decreasing"))%>% #,"increasing",,"increasing"
  #filter(CLASS=='Ref')%>%
  #filter(HUC2 %in% c('18')) %>%
  select(Norm_Trend_slope,
         MK_Combi_slope,
         #Trend_Result,
         LU2019_Urb, LU2019_Agr, LU2019_For,
         #percent_max_increase,percent_max_decrease,
         #percent_mean_increase,percent_mean_decrease,
         Weigh_Norm_Max_TS,
         Weigh_Norm_Mean_TS,
         norm_max_stor)

#data_mlr%>%write.csv("data_mlr.csv")



##################################################
## Trend_slope = a0 + 
##               a1 x LU_Urb+
##               a2 x LU_Agr+
##               a3 x percent_max_increase+
##               a4 x percent_max_decrease+
##               a5 x percent_mean_increase+
##               a6 x percent_mean_decrease+
##               a7 x norm_max_stor
#################################################

model <- lm(Norm_Trend_slope ~ 
  #MK_Combi_slope ~
    LU2019_Urb #+ 
  +LU2019_Agr
  #+LU2019_For#
  #+percent_max_increase
  #+percent_max_decrease
  #+percent_mean_increase
  #+percent_mean_decrease
  +Weigh_Norm_Max_TS
  +Weigh_Norm_Mean_TS
  +norm_max_stor
  ,data = data_mlr)

# Get the coefficient matrix
coefs <- summary(model)$coefficients
coefs2 <- as.data.frame(summary(model)$coefficients)
#colnames(coefs)

# Identify the variables with "3 stars"
vars <- rownames(coefs)[which(coefs[, 4] < 0.05)]
#vars

#class(vars)


#roduce added variable plots
#car::avPlots(model)


#anova(model)
af <- anova(model)
afss <- af$"Sum Sq"

#print(cbind(af,PctExp=afss/sum(afss)*100))
df2<-as.data.frame(cbind(af,PctExp=afss/sum(afss)*100))

#df2
#nrow(data_mlr)

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
  barchart_data[name,'CorrelationCoefficient']<-coefs2[name,'Estimate']
  barchart_data[name,'StandardError']<-coefs2[name,'Std. Error']
  barchart_data[name,'tStat']<-coefs2[name,'t value']
  barchart_data[name,'pvalue']<-coefs2[name,'Pr(>|t|)']
  
  barchart_data[name,'PearsonCorr_with_NormTrendSlope']<-cor(data_mlr['Norm_Trend_slope'],data_mlr[[name]],
                                                             use="complete.obs",
                                                             method='pearson')
  
  
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
barchart_data$Multiple_Rsquared<-summary(model)$r.squared                      # Returning multiple R-squared


df_list<-list()
df_list<-append(df_list, list(barchart_data))

#write.csv(data_mlr,"data_conus_mlr.csv")

# ###############################
# ###############################
# ###############################
# 
# df3<-df2[!(row.names(df2) %in% c("Residuals")),]%>%
#   arrange(desc(PctExp))
# row_name<-row.names(df3)[1]
# value<-df3[["PctExp"]][[1]]
# 
# m8 = model
# m7 = update(m8, ~ . - norm_max_stor)
# m6 = update(m7, ~ . - percent_mean_decrease)
# m5 = update(m6, ~ . - percent_mean_increase)
# m4 = update(m5, ~ . - percent_max_decrease)
# m3 = update(m4, ~ . - percent_max_increase)
# m2 = update(m3, ~ . - LU_Agr)
# m1 = update(m2, ~ . - LU_Urb)
# 
# anova(m1,m2,m3,m4,m5,m6,m7,m8)
# 
# ###############################
# ## Correlation values#########
# ###############################
# 
# 
# print("data_mlr$Norm_Trend_slope,data_mlr$LU2019_Urb")
# print(cor(data_mlr$Norm_Trend_slope,data_mlr$LU2019_Urb,
#     use="complete.obs",
#     method='pearson')# kendall,spearman)
#     )
#cor(data_mlr$Norm_Trend_slope,data_mlr$LU2019_Agr,
#     use="complete.obs",
 #    method='pearson')
# #cor(data_mlr$Norm_Trend_slope,data_mlr$LU_For)
# cor(data_mlr$Norm_Trend_slope,data_mlr$percent_max_increase,
#     use="complete.obs",
#     method='pearson')
# cor(data_mlr$Norm_Trend_slope,data_mlr$percent_max_decrease,
#     use="complete.obs",
#     method='pearson')
# cor(data_mlr$Norm_Trend_slope,data_mlr$percent_mean_increase,
#     use="complete.obs",
#     method='pearson')
# cor(data_mlr$Norm_Trend_slope,data_mlr$percent_mean_decrease,
#     use="complete.obs",
#     method='pearson')
# cor(data_mlr$Norm_Trend_slope,data_mlr$norm_max_stor,
#     use="complete.obs",
#     method='pearson')
# 
# cor(data_mlr,
#     use="complete.obs")
# 
# 
# ##################################################
# ## HUC wise results
# #################################################
# 
# 
# huc_num_list <-c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18")
# 
# df <- data.frame(matrix(ncol = 9, nrow = length(huc_num_list)))
# # assigning new names to the columns of the data frame
# colnames(df) <- c('HUC2_Code','Count','LU_Urb','LU_Urb',
#                   '%_max_increase','%_max_decrease',
#                   '%_mean_increase','%_mean_decrease',
#                   'norm_max_stor')
# 
# x<-1
# for (huc_num in huc_num_list){
#   data_mlr<-regression_input_data5%>%
#     #st_drop_geometry()%>%
#     filter(MK_Combi_Trend%in%c("decreasing","increasing"))%>% #,"increasing"
#     #filter(CLASS=='Non-ref')%>%
#     filter(HUC2==huc_num)%>%
#     select(Norm_Trend_slope,
#            #Trend_Result,
#            LU2019_Urb, LU2019_Agr, LU2019_For,
#            percent_max_increase,percent_max_decrease,
#            percent_mean_increase,percent_mean_decrease,
#            norm_max_stor)
#   
#   
#   df[[1]][[x]]<-huc_num
#   df[[2]][[x]]<-nrow(data_mlr)
#   df[[3]][[x]]<-round(cor(data_mlr$Norm_Trend_slope,data_mlr$LU_Urb),2)
#   df[[4]][[x]]<-round(cor(data_mlr$Norm_Trend_slope,data_mlr$LU_Agr),2)
#   df[[5]][[x]]<-round(cor(data_mlr$Norm_Trend_slope,data_mlr$percent_max_increase),2)
#   df[[6]][[x]]<-round(cor(data_mlr$Norm_Trend_slope,data_mlr$percent_max_decrease),2)
#   df[[7]][[x]]<-round(cor(data_mlr$Norm_Trend_slope,data_mlr$percent_mean_increase),2)
#   df[[8]][[x]]<-round(cor(data_mlr$Norm_Trend_slope,data_mlr$percent_mean_decrease),2)
#   df[[9]][[x]]<-round(cor(data_mlr$Norm_Trend_slope,data_mlr$norm_max_stor),2)
#   
#   x<-x+1
# }
# 
# df

huc2_sf<-st_read(paste0(folder_name_absolute,"/Data/Shapefiles/HUC2_modified/HUC2_modified5.shp"))
huc2_sf_code_name<-huc2_sf%>%
  st_drop_geometry()%>%
  select(HUC2,NAME)

rownames(huc2_sf_code_name) <- huc2_sf_code_name$HUC2


huc_num_list <-huc2_sf_code_name$HUC2
#huc_num_list <-c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18")

#df_model <- data.frame(matrix(ncol = 4, nrow = length(huc_num_list)))

#df_imp_variables <- data.frame(matrix(ncol = 10, nrow = length(huc_num_list)))
# assigning new names to the columns of the data frame
#colnames(df_model) <- c('HUC2_Code','Count','Variable','Value')

#x<-1
#########################################

### Change HUC2 number here!
### To find the drivers and its values

##########################################
#huc_num_list <-c("16")

for (huc_num in huc_num_list){
  
    # Create a dataframe with 126 rows and 5 columns
  barchart_data <- data.frame(
    HUC2_code = rep(NA, length(row_names)),
    HUC2_name = rep(NA, length(row_names)),
    Variable_Name = rep(NA, length(row_names)),
    CorrelationCoefficient = rep(NA, length(row_names)),
    StandardError = rep(NA, length(row_names)),
    tStat = rep(NA, length(row_names)),
    pvalue = rep(NA, length(row_names)),
    Significance = rep(NA, length(row_names)),
    VarianceExplained_percent = rep(NA, length(row_names)),
    Number_Sites = rep(NA, length(row_names)),
    Multiple_Rsquared = rep(NA, length(row_names)),
    PearsonCorr_with_NormTrendSlope = rep(NA, length(row_names))
  )
  
  rownames(barchart_data) <- row_names
  
  #huc_num='1'
  rm(model)
  
  #print(huc_num)

  
  data_mlr<-regression_input_data5%>%
    #st_drop_geometry()%>%
    filter(MK_Combi_Trend%in%c("decreasing","increasing"))%>% #,"increasing"
    #filter(CLASS=='Ref')%>%
    filter(HUC2==huc_num)%>%
    select(Norm_Trend_slope,
           #Trend_Result,
           LU2019_Urb, LU2019_Agr, LU2019_For,
           #percent_max_increase,percent_max_decrease,
           #percent_mean_increase,percent_mean_decrease,
           Weigh_Norm_Max_TS,
           Weigh_Norm_Mean_TS,
           norm_max_stor)
  
  ##################################################
  ## Trend_slope = a0 + 
  ##               a1 x LU_Urb+
  ##               a2 x LU_Agr+
  ##               a3 x percent_max_increase+
  ##               a4 x percent_max_decrease+
  ##               a5 x percent_mean_increase+
  ##               a6 x percent_mean_decrease+
  ##               a7 x norm_max_stor
  #################################################
  
  model <- lm(Norm_Trend_slope ~ LU2019_Urb #+ 
              +LU2019_Agr
              #+LU2019_For#
              #+percent_max_increase
              #+percent_max_decrease
              #+percent_mean_increase
              #+percent_mean_decrease
              +Weigh_Norm_Max_TS
              +Weigh_Norm_Mean_TS
              +norm_max_stor
              ,data = data_mlr)
  #print(summary(model))
  
  # Get the coefficient matrix
  coefs <- summary(model)$coefficients
  coefs2 <- as.data.frame(summary(model)$coefficients)
  
  # Identify the variables with "3 stars"
  vars <- rownames(coefs)[which(coefs[, 4] < 0.05)]
  #vars
  
  
  
  #load car package
  #library(car)
  
  #produce added variable plots
  #avPlots(model)
  
  
  #anova(model)
  af <- anova(model)
  afss <- af$"Sum Sq"
  #print(vars)
  #vars1<-vars[!vars=="(Intercept)"]
  #print(vars1)
  #print(cbind(af,PctExp=afss/sum(afss)*100))
  df_results<-cbind(af,PctExp=afss/sum(afss)*100)

  
  #df_results_filtered<-df_results %>%
    #filter(row.names(df_results) %in% vars1)
  
  #print(df_results_filtered)
  
  
  #print(nrow(data_mlr))
  
  
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
    barchart_data[name,'CorrelationCoefficient']<-coefs2[name,'Estimate']
    barchart_data[name,'StandardError']<-coefs2[name,'Std. Error']
    barchart_data[name,'tStat']<-coefs2[name,'t value']
    barchart_data[name,'pvalue']<-coefs2[name,'Pr(>|t|)']
    
    barchart_data[name,'PearsonCorr_with_NormTrendSlope']<-cor(data_mlr['Norm_Trend_slope'],data_mlr[[name]],
                                                               use="complete.obs",
                                                               method='pearson')
    
    
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
  barchart_data$Multiple_Rsquared<-summary(model)$r.squared                      # Returning multiple R-squared
  
  
 
  df_list<-append(df_list, list(barchart_data))
  
  #print("data_mlr$Norm_Trend_slope,data_mlr$LU2019_Urb")
  #print(cor(data_mlr$Norm_Trend_slope,data_mlr$LU2019_Urb,
   #         use="complete.obs",
    #        method='pearson')# kendall,spearman)
  #)
  #print("data_mlr$Norm_Trend_slope,data_mlr$LU2019_Agr")
  #print(cor(data_mlr$Norm_Trend_slope,data_mlr$LU2019_Agr,
  #           use="complete.obs",
  #           method='pearson')# kendall,spearman)
  # )
  # print("data_mlr$Norm_Trend_slope,data_mlr$percent_max_increase")
  # print(cor(data_mlr$Norm_Trend_slope,data_mlr$percent_max_increase,
  #           use="complete.obs",
  #           method='pearson')# kendall,spearman)
  # )
  # print("data_mlr$Norm_Trend_slope,data_mlr$percent_mean_increase")
  # print(cor(data_mlr$Norm_Trend_slope,data_mlr$percent_mean_increase,
  #           use="complete.obs",
  #           method='pearson')# kendall,spearman)
  # )
  # print("data_mlr$Norm_Trend_slope,data_mlr$percent_max_decrease")
  # print(cor(data_mlr$Norm_Trend_slope,data_mlr$percent_max_decrease,
  #           use="complete.obs",
  #           method='pearson')# kendall,spearman)
  # )
  # print("data_mlr$Norm_Trend_slope,data_mlr$percent_mean_decrease")
  # print(cor(data_mlr$Norm_Trend_slope,data_mlr$percent_mean_decrease,
  #           use="complete.obs",
  #           method='pearson')# kendall,spearman)
  # )
  # print("data_mlr$Norm_Trend_slope,data_mlr$norm_max_stor")
  # print(cor(data_mlr$Norm_Trend_slope,data_mlr$norm_max_stor,
  #           use="complete.obs",
  #           method='pearson')# kendall,spearman)
  # )
  
  
  
  
}
merged_df <- do.call(rbind, df_list)
write.csv(merged_df, 
          paste0(folder_data_out,
                 "barchart_data_type2_USGS_",
                 year_usgs_start,"_",year_usgs_end,
                 "_dam_",
                 year_dam_start,"_",year_dam_end,
                 ".txt"), 
          row.names = F)



##############################
##############################



