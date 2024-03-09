## Modifications
## 204-02-16
## Changed varible name from Trend-Result to MK_Combi_Trend
## filenmame pattern chnaged to "data_USGS_",file_name,"_",variable_selected,"_",start_year,"_",end_year,"_
## 2024-02-07
## Added a fun 'f' to manually calculate the percentile 

########################
## Importing Packages
########################
library(sf)
library(ggplot2)
library(ggsn)
library(ggrepel)
library(dplyr)
library(tmap)
## https://www.statology.org/summary-table-in-r/
#install.packages("psych")
library(psych)
library(dplyr)
library(data.table)

########################
########################

## Clear all variables
rm(list=ls())

## Clear Console
cat("\014")

username<-'joseph57' # Office computer
#username="Jibin Joseph" # Apartment computer
#username<-'carji' # Laptop
variable_selected<-"KPSS_Result"#"Var_2Divide"#"KPSS_Result" #"Trend_Result"

start_year="1950"
end_year="2014"
##############################
## Plot Type #################
if(variable_selected=="MK_Combi_Trend"){
trend_type="significant"
y_up_input=75
variable_selected2="Trend_Result"

}else if (variable_selected=="KPSS_Result"){
  stat_type="NS"
  y_up_input=75
}else if (variable_selected=="Var_2Divide"){
  var_result_type="not equal"
  y_up_input=60
}else{
  print("Check")
}

var_type=TRUE
##############################
alpha_value<-"0.05"
if(variable_selected=="MK_Combi_Trend"){
  if(trend_type=="increasing"){
    
    file_name<-'1original'
    title_extra<-''
  } else if(trend_type=="decreasing"){
      file_name<-'1original'
      title_extra<-''
  }else if(trend_type=="significant"){
    file_name<-'2modified'
    title_extra<-" (increasing and decreasing)"
  }
}else if (variable_selected=="KPSS_Result"){
  file_name<-'1original'
}else if (variable_selected=="Var_2Divide"){
  file_name<-'1original'
}else{
  print("Check")
}


## Set input directory
setwd(paste0("C:\\Users\\",
             username,"\\Dropbox\\01_SharedFolder_Jibin\\Code_R\\code_boxplot_usgs_cesm2"))

## Plots Output Folder
folder_out<-paste0("C:\\Users\\",username,"\\Box\\01_SharedBox_Jibin\\Plots_IntermediateResults\\plots_boxplot_usgs_cesm2_v7\\")


df_list<-vector("list")
df_list[[1]]<-read.csv(paste0("data_USGS_",file_name,"_",variable_selected,"_",start_year,"_",end_year,"_all_df.csv"))
df_list[[2]]<-read.csv(paste0("data_CESM2_",file_name,"_",variable_selected,"_",start_year,"_",end_year,"_hist_df.csv"))%>%dplyr::select(HUC2,all_of(variable_selected),Percent,Type)
#df_list[[3]]<-read.csv(paste0("data_",variable_selected,"_",file_name,"_cesm2_futu_df.csv"))%>%dplyr::select(HUC2,all_of(variable_selected),Percent,Type)
#df_list[[4]]<-read.csv(paste0("data_",variable_selected,"_",file_name,"_cesm1_pre-industrial_df.csv"))%>%dplyr::select(HUC2,all_of(variable_selected),Percent,Type)

my_merge_func <- function(df1, df2){                                # Create own merging function
  merge(df1, df2, 
        by=c('HUC2',variable_selected,'Percent','Type'),all=TRUE)
}
merged_df_1<-Reduce(my_merge_func, df_list)


## Should be 1620 rows in the below data_cesm2 (NOT INCLUDING USGS) for 90 ensembles file representing 18 HUC2 regions

if(variable_selected=="MK_Combi_Trend"){
data_cesm1<-merged_df_1%>%
  dplyr::filter(Type!="USGS")%>%
  #dplyr::filter(Var_2Divide=="not equal")
  dplyr::filter(get({{variable_selected}})==trend_type) 
## Some article say get should be avoided - https://stackoverflow.com/questions/50913673/use-of-get-in-dplyr
## The recommendation now is to use {{}} rather than !!
}else if (variable_selected=="KPSS_Result"){
  data_cesm1<-merged_df_1%>%
    dplyr::filter(Type!="USGS")%>%
    #dplyr::filter(Var_2Divide=="not equal")
    dplyr::filter(get({{variable_selected}})==stat_type) 
  ## Some article say get should be avoided - https://stackoverflow.com/questions/50913673/use-of-get-in-dplyr
  ## The recommendation now is to use {{}} rather than !!
}else if (variable_selected=="Var_2Divide"){
  data_cesm1<-merged_df_1%>%
    dplyr::filter(Type!="USGS")%>%
    #dplyr::filter(Var_2Divide=="not equal")
    dplyr::filter(get({{variable_selected}})==var_result_type) 
}


unique(data_cesm1[[variable_selected]])  

unique(data_cesm1$Type)

## Should be 18 rows in the below data_usgs file representing 18 HUC2 regions

if(variable_selected=="MK_Combi_Trend"){
data_usgs<-merged_df_1%>%
  dplyr::filter(Type=="USGS")%>%
  dplyr::filter(get({{variable_selected}})==trend_type)
}else if (variable_selected=="KPSS_Result"){
  data_usgs<-merged_df_1%>%
    dplyr::filter(Type=="USGS")%>%
    dplyr::filter(get({{variable_selected}})==stat_type)
}else if (variable_selected=="Var_2Divide"){
  data_usgs<-merged_df_1%>%
    dplyr::filter(Type=="USGS")%>%
    dplyr::filter(get({{variable_selected}})==var_result_type)
}
colnames(data_usgs)
unique(data_usgs$Type)

if(variable_selected=="MK_Combi_Trend"){
data_ensemble001<-read.csv(paste0("data_CESM2_",file_name,"_",variable_selected,"_",start_year,"_",end_year,"_hist_df.csv"))%>%
  dplyr::filter(Ensemble==paste0(variable_selected,"_1231_001"))%>%
  dplyr::filter(get({{variable_selected}})==trend_type) %>%
  dplyr::select(HUC2,all_of(variable_selected),Percent,Type,Freq)
}else if (variable_selected=="KPSS_Result"){
  data_ensemble001<-read.csv(paste0("data_CESM2_",file_name,"_",variable_selected,"_",start_year,"_",end_year,"_hist_df.csv"))%>%
    dplyr::filter(Ensemble==paste0(variable_selected,"_1231_001"))%>%
    dplyr::filter(get({{variable_selected}})==stat_type) %>%
    dplyr::select(HUC2,all_of(variable_selected),Percent,Type,Freq)
}else if (variable_selected=="Var_2Divide"){
  data_ensemble001<-read.csv(paste0("data_CESM2_",file_name,"_",variable_selected,"_",start_year,"_",end_year,"_hist_df.csv"))%>%
    dplyr::filter(Ensemble==paste0(variable_selected,"_1231_001"))%>%
    dplyr::filter(get({{variable_selected}})==var_result_type) %>%
    dplyr::select(HUC2,all_of(variable_selected),Percent,Type,Freq)
}
  
  
data_ensemble001['Type']="CESM2 Historical\n Ensemble 1231 001"
write.csv(data_ensemble001,paste0("data_CESM2_",file_name,"_",variable_selected,"_",start_year,"_",end_year,"_ensemble001.csv"))

rm(df_list,my_merge_func)

## Specify the factor levels in the order you want for legend
data_cesm1$Type<-factor(data_cesm1$Type,levels=c(#"CESM1 Pre-industrial", 
                                                      "CESM2 Historical"
                                                      #,"CESM2 Future"
                                                      ))
#############################################
#############################################
library(gg.layers)
library(ggplot2)
myboxplot_trial <- ggplot(data_cesm1, 
                    aes(x=factor(HUC2), 
                        y=Percent,
                        color=factor(Type)
                        #,fill=factor(Type)
                    )) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot2(width=0.9,width.errorbar=0.5
    #outlier.shape = NA, shape=23,
               #fatten = NULL,
               #coef = 1,
               #fill = NA
  ) +
  coord_cartesian(ylim=c(0,y_up_input))+
  theme(legend.position=c(0.05,.90), 
        legend.justification='left')+
  theme_classic()

myboxplot_trial


# define the summary function
fun_manual_percentiles <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

myboxplot_trial2<-ggplot(data_cesm1, 
                         aes(x=factor(HUC2), 
                             y=Percent,
                             color=factor(Type))) + 
  stat_summary(fun.data = fun_manual_percentiles, 
               geom="boxplot")+
  stat_summary(fun.data = fun_manual_percentiles,
               geom = "errorbar") 
myboxplot_trial2

#ggplot(PlantGrowth, aes(group, weight))+
#  stat_boxplot( aes(group, weight), 
#                geom='errorbar', linetype=1, width=0.5)#+  #whiskers
  #geom_boxplot( aes(group, weight),outlier.shape=1) +    
  #stat_summary(fun.y=mean, geom="point", size=2) #+ 
  #stat_summary(fun.data = mean_se, geom = "errorbar")

#############################################
#############################################


myboxplot <- ggplot(data_cesm1, 
               aes(x=factor(HUC2), 
                   y=Percent,
                   color=factor(Type)
                   #,fill=factor(Type)
                   )) +
  stat_summary(fun.data = fun_manual_percentiles, 
               geom="boxplot")+
  stat_summary(fun.data = fun_manual_percentiles,
               geom = "errorbar") +
  #geom_boxplot(outlier.shape = NA, shape=23,
               #fatten = NULL,
               #coef = 1,
               #fill = NA
               #) +
  coord_cartesian(ylim=c(0,y_up_input))+
  theme(legend.position=c(0.05,.90), 
        legend.justification='left')+
  theme_classic()
  ## Adding mean value to boxplots
  #+stat_summary(fun = "mean", geom = "point", shape = 2, size = 3, color = "blue")

dev.off()

myboxplot

myboxplot<-myboxplot+
  scale_color_manual(values=c(#"gray40",
    "#003366","red1","black","blue1","blue1","blue1","red1","red1"))
#+
  #scale_fill_manual(values=c("gray90","dodgerblue","sienna1","black","blue1","gray40","gray40","red1","red1")
#                    ,
                    #limits = c('CESM1 Pre-industrial','CESM1 Historical', 'CESM1 Future')
                    #)
myboxplot

if(variable_selected=="MK_Combi_Trend"){
plot_filename<-paste0(folder_out,
                      paste0("boxplot_",
                             variable_selected,
                             "_",start_year,"_",end_year,
                              "_",
                              trend_type,
                              "_cesm2_usgs_for_getting_colors_plot.png"))
}else if (variable_selected=="KPSS_Result"){
  plot_filename<-paste0(folder_out,
                        paste0("boxplot_",
                               variable_selected,
                               "_",start_year,"_",end_year,
                               "_",
                               stat_type,
                               "_cesm2_usgs_for_getting_colors_plot.png"))
}else if (variable_selected=="Var_2Divide"){
  plot_filename<-paste0(folder_out,
                        paste0("boxplot_",
                               variable_selected,
                               "_",start_year,"_",end_year,
                               "_",
                               var_result_type,
                               "_cesm2_usgs_for_getting_colors_plot.png"))
}
ggsave(
  plot_filename,
  plot = myboxplot,
  width = 300,
  height = 170,
  #height = 100,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)
myboxplot2<-myboxplot+
  geom_point(data = data_usgs%>%select(HUC2,Percent,Type),
             aes(x=HUC2,
                 y=Percent),
             color="blue1",
             shape=8,
             size=3)

myboxplot2

if(variable_selected=="MK_Combi_Trend"){
  plot_filename<-paste0(folder_out,
                        paste0("boxplot_",
                               variable_selected,
                               "_",start_year,"_",end_year,
                               "_",
                               trend_type,
                               "_cesm2_usgs_for_getting_symbols&USGS_text_plot_v2.png"))
}else if (variable_selected=="KPSS_Result"){
  plot_filename<-paste0(folder_out,
                        paste0("boxplot_",
                               variable_selected,
                               "_",start_year,"_",end_year,
                               "_",
                               stat_type,
                               "_cesm2_usgs_for_getting_symbols&USGS_text_plot_v2.png"))
}else if (variable_selected=="Var_2Divide"){
  plot_filename<-paste0(folder_out,
                        paste0("boxplot_",
                               variable_selected,
                               "_",start_year,"_",end_year,
                               "_",
                               var_result_type,
                               "_cesm2_usgs_for_getting_symbols&USGS_text_plot_v2.png"))
}

ggsave(
  plot_filename,
  plot = myboxplot2,
  width = 300,
  height = 170,
  #height = 100,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)

myboxplot<-myboxplot+
  geom_point(data = data_usgs%>%select(HUC2,Percent,Type),
             aes(x=HUC2,
                 y=Percent),
             color='black',
             shape=19,
             size=3,
             position=position_nudge(x = 0, y = 0),
             alpha=10)  
myboxplot

myboxplot2<-myboxplot+
  geom_point(data = data_usgs%>%select(HUC2,Percent,Type),
             aes(x=HUC2,
                 y=Percent),
             color="blue1",
             shape=8,
             size=3)

myboxplot2
if(variable_selected=="MK_Combi_Trend"){
  plot_filename<-paste0(folder_out,
                        paste0("boxplot_",
                               variable_selected,
                               "_",start_year,"_",end_year,
                               "_",
                               trend_type,
                               "_cesm2_usgs_for_getting_symbols&USGS_text_plot_v2.png"))
}else if (variable_selected=="KPSS_Result"){
  plot_filename<-paste0(folder_out,
                        paste0("boxplot_",
                               variable_selected,
                               "_",start_year,"_",end_year,
                               "_",
                               stat_type,
                               "_cesm2_usgs_for_getting_symbols&USGS_text_plot_v2.png"))
}else if (variable_selected=="Var_2Divide"){
  plot_filename<-paste0(folder_out,
                        paste0("boxplot_",
                               variable_selected,
                               "_",start_year,"_",end_year,
                               "_",
                               var_result_type,
                               "_cesm2_usgs_for_getting_symbols&USGS_text_plot_v2.png"))
}

ggsave(
  plot_filename,
  plot = myboxplot2,
  width = 300,
  height = 170,
  #height = 100,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)

myboxplot<-myboxplot+
  geom_point(data = data_ensemble001%>%select(HUC2,Percent,Type),
             aes(x=HUC2,
                 y=Percent),
             color='#003366',
             shape=1,
             size=3,
             position=position_nudge(x = 0, y = 0),
             alpha=10)  
myboxplot

############################
## Adding HCDN points######
############################

if (variable_selected=="MK_Combi_Trend"){
data_hcdn<-read.csv(paste0("data_USGS_",file_name,"_",variable_selected,"_",start_year,"_",end_year,"_hcdn_df.csv"))%>%dplyr::select(HUC2,all_of(variable_selected),Percent,Type,Freq)%>%filter(get({{variable_selected}})==trend_type)
}else if (variable_selected=="KPSS_Result"){
  data_hcdn<-read.csv(paste0("data_USGS_",file_name,"_",variable_selected,"_",start_year,"_",end_year,"_hcdn_df.csv"))%>%dplyr::select(HUC2,all_of(variable_selected),Percent,Type,Freq)%>%filter(get({{variable_selected}})==stat_type)
}else if (variable_selected=="Var_2Divide"){
  data_hcdn<-read.csv(paste0("data_USGS_",file_name,"_",variable_selected,"_",start_year,"_",end_year,"_hcdn_df.csv"))%>%dplyr::select(HUC2,all_of(variable_selected),Percent,Type,Freq)%>%filter(get({{variable_selected}})==var_result_type)
}

colnames(data_usgs)
colnames(data_hcdn)

#myboxplot<-myboxplot+
#  geom_point(data = data_hcdn%>%select(HUC2,Percent,Type),
#             aes(x=HUC2,
#                 y=Percent),
#             color='sandybrown',
#             shape=16,
#             size=3,
#             position=position_nudge(x = 0, y = 0),
#             alpha=10)  
#myboxplot

############################
## Adding GAGES II Reference ##
############################
if (variable_selected=="MK_Combi_Trend"){
data_gages_ref<-read.csv(paste0("data_USGS_",file_name,"_",variable_selected,"_",start_year,"_",end_year,"_gages_Ref_df.csv"))%>%dplyr::select(HUC2,all_of(variable_selected),Percent,Type,Freq)%>%filter(get({{variable_selected}})==trend_type)
}else if (variable_selected=="KPSS_Result"){
  data_gages_ref<-read.csv(paste0("data_USGS_",file_name,"_",variable_selected,"_",start_year,"_",end_year,"_gages_Ref_df.csv"))%>%dplyr::select(HUC2,all_of(variable_selected),Percent,Type,Freq)%>%filter(get({{variable_selected}})==stat_type)
}else if (variable_selected=="Var_2Divide"){
  data_gages_ref<-read.csv(paste0("data_USGS_",file_name,"_",variable_selected,"_",start_year,"_",end_year,"_gages_Ref_df.csv"))%>%dplyr::select(HUC2,all_of(variable_selected),Percent,Type,Freq)%>%filter(get({{variable_selected}})==var_result_type)
}

colnames(data_usgs)
colnames(data_hcdn)
colnames(data_gages_ref)
#colnames(data_gages_nonref)

myboxplot<-myboxplot+
  geom_point(data = data_gages_ref%>%select(HUC2,Percent,Type),
             aes(x=HUC2,
                 y=Percent),
             color='sandybrown',
             shape=15,
             size=3,
             position=position_nudge(x = -0.18, y = 0),
             alpha=10)  
myboxplot

############################
## Adding GAGES II Non-reference ##
############################
if (variable_selected=="MK_Combi_Trend"){
data_gages_nonref<-read.csv(paste0("data_USGS_",file_name,"_",variable_selected,"_",start_year,"_",end_year,"_gages_Non-ref_df.csv"))%>%dplyr::select(HUC2,all_of(variable_selected),Percent,Type,Freq)%>%filter(get({{variable_selected}})==trend_type)
}else if (variable_selected=="KPSS_Result"){
  data_gages_nonref<-read.csv(paste0("data_USGS_",file_name,"_",variable_selected,"_",start_year,"_",end_year,"_gages_Non-ref_df.csv"))%>%dplyr::select(HUC2,all_of(variable_selected),Percent,Type,Freq)%>%filter(get({{variable_selected}})==stat_type)  
}else if (variable_selected=="Var_2Divide"){
  data_gages_nonref<-read.csv(paste0("data_USGS_",file_name,"_",variable_selected,"_",start_year,"_",end_year,"_gages_Non-ref_df.csv"))%>%dplyr::select(HUC2,all_of(variable_selected),Percent,Type,Freq)%>%filter(get({{variable_selected}})==var_result_type)  
}
  
colnames(data_usgs)
colnames(data_hcdn)
colnames(data_gages_ref)
colnames(data_gages_nonref)

myboxplot<-myboxplot+
  geom_point(data = data_gages_nonref%>%select(HUC2,Percent,Type),
             aes(x=HUC2,
                 y=Percent),
             color='blue',
             shape=3,
             size=2,
             position=position_nudge(x = +0.18, y = 0),
             alpha=10)  
myboxplot


## If you see the error: Error in `f()`: ! Insufficient values in manual scale. 6 needed but only 5 provided.
## Add an extra color in scale_manual_color and _fill earlier
#####################################

if(variable_selected=="MK_Combi_Trend"){
  plot_filename<-paste0(folder_out,
                        paste0("boxplot_",
                               variable_selected,
                               "_",start_year,"_",end_year,
                               "_",
                               trend_type,
                               "_cesm2_usgs_for_getting_symbols&USGS_text_plot.png"))
}else if (variable_selected=="KPSS_Result"){
  plot_filename<-paste0(folder_out,
                        paste0("boxplot_",
                               variable_selected,
                               "_",start_year,"_",end_year,
                               "_",
                               stat_type,
                               "_cesm2_usgs_for_getting_symbols&USGS_text_plot.png"))
}else if (variable_selected=="Var_2Divide"){
  plot_filename<-paste0(folder_out,
                        paste0("boxplot_",
                               variable_selected,
                               "_",start_year,"_",end_year,
                               "_",
                               var_result_type,
                               "_cesm2_usgs_for_getting_symbols&USGS_text_plot.png"))
}
  


ggsave(
  plot_filename,
  plot = myboxplot,
  width = 300,
  height = 170,
  #height = 100,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)

myboxplot<-myboxplot+
  #stat_boxplot(geom ='errorbar') + ## This was adding vertical line inside the box, so used coef = 0 in the initial boxplot creation
  xlab("Different HUC2 Regions") + 
  ylab("Percentage of Grid Points/ Sites")

myboxplot

#myboxplot<-myboxplot+
#  scale_color_manual(values=c("gray40","blue1","red1","black"),
#                     limits = c('CESM1 Pre-industrial',
#                       'CESM1 Historical', 'CESM1 Future'))
  #+scale_fill_manual(values=c("gray90","dodgerblue","sienna1","black"),
  #                  limits = c('CESM1 Pre-industrial','CESM1 Historical', 'CESM1 Future'))
#myboxplot  

if(variable_selected=="MK_Combi_Trend"){
plot_title<-paste0("Percentage of CESM2 Grid Points or USGS sites (",start_year," and ",end_year,") showing ",trend_type," trend",title_extra," for different\nHUC2 regions using Mann Kendall (using original and modified versions) with alpha = ",alpha_value,".") # Assumed the variance of\nsample for t-test as ", var_type,".")) 
}else if (variable_selected=="KPSS_Result"){
  plot_title<-paste0("Percentage of CESM2 Grid Points or USGS sites (",start_year," and ",end_year,") showing ",stat_type," stationarity type"," for different\nHUC2 regions using KPSS Test with alpha = ",alpha_value,".") # Assumed the variance of\nsample for t-test as ", var_type,".")) 
}else if (variable_selected=="Var_2Divide"){
  plot_title<-paste0("Percentage of CESM2 Grid Points or USGS sites (",start_year," and ",end_year,") showing ",var_result_type," varaince type"," for different\nHUC2 regions using KPSS Test with alpha = ",alpha_value,".") # Assumed the variance of\nsample for t-test as ", var_type,"."))
}
myboxplot<-myboxplot +
  labs(color='Types of Data')+
  #labs(fill='Types of Data')
ggtitle(plot_title) 

                                             
myboxplot


if(variable_selected=="MK_Combi_Trend"){
  plot_filename<-paste0(folder_out,paste0("boxplot_",variable_selected,"_",start_year,"_",end_year,"_",trend_type,"_cesm2_usgs_Height100_var-equal-",var_type,"_plot.png"))
  plot_filename1<-paste0(folder_out,paste0("boxplot_",variable_selected,"_",start_year,"_",end_year,"_",trend_type,"_cesm2_usgs_Height170_var-equal-",var_type,"_plot.png"))
  }else if (variable_selected=="KPSS_Result"){
  plot_filename<-paste0(folder_out,paste0("boxplot_",variable_selected,"_",start_year,"_",end_year,"_",stat_type,"_cesm2_usgs_Height100_var-equal-",var_type,"_plot.png"))
  plot_filename1<-paste0(folder_out,paste0("boxplot_",variable_selected,"_",start_year,"_",end_year,"_",stat_type,"_cesm2_usgs_Height170_var-equal-",var_type,"_plot.png"))
  }else if (variable_selected=="Var_2Divide"){
    plot_filename<-paste0(folder_out,paste0("boxplot_",variable_selected,"_",start_year,"_",end_year,"_",var_result_type,"_cesm2_usgs_Height100_var-equal-",var_type,"_plot.png"))
    plot_filename1<-paste0(folder_out,paste0("boxplot_",variable_selected,"_",start_year,"_",end_year,"_",var_result_type,"_cesm2_usgs_Height170_var-equal-",var_type,"_plot.png"))
  }
    

ggsave(
  plot_filename,
  plot = myboxplot,
  width = 300,
  #height = 170,
  height = 100,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)


ggsave(
  plot_filename1,
  plot = myboxplot,
  width = 300,
  height = 170,
  #height = 100,
  units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)

data_ensemble001['Type']="Ensemble 1231 001 (CESM2 Historical)"

full_data<-data_cesm1%>%
  dplyr::bind_rows(data_ensemble001)%>%
  dplyr::bind_rows(data_usgs)%>%
  dplyr::bind_rows(data_hcdn)%>%
  dplyr::bind_rows(data_gages_ref)%>%
  dplyr::bind_rows(data_gages_nonref)

if(variable_selected=="MK_Combi_Trend"){
  csv_filename<-paste0(folder_out,paste0("data_FULL_",variable_selected,"_",start_year,"_",end_year,"_",trend_type,"_cesm2_usgs.csv"))
  csv_filename1<-paste0(folder_out,paste0("data_FULL_",variable_selected,"_",start_year,"_",end_year,"_",trend_type,"_cesm2_usgs.csv"))
}else if (variable_selected=="KPSS_Result"){
  csv_filename<-paste0(folder_out,paste0("data_FULL_",variable_selected,"_",start_year,"_",end_year,"_",stat_type,"_cesm2_usgs.csv"))
  csv_filename1<-paste0(folder_out,paste0("data_FULL_",variable_selected,"_",start_year,"_",end_year,"_",stat_type,"_cesm2_usgs.csv"))
}else if (variable_selected=="Var_2Divide"){
  csv_filename<-paste0(folder_out,paste0("data_FULL_",variable_selected,"_",start_year,"_",end_year,"_",var_result_type,"_cesm2_usgs.csv"))
  csv_filename1<-paste0(folder_out,paste0("data_FULL_",variable_selected,"_",start_year,"_",end_year,"_",var_result_type,"_cesm2_usgs.csv"))
}

write.csv(full_data,
          csv_filename,
          row.names = FALSE)
#############################

huc_num='3'

data_ensemble001%>%
  dplyr::filter(HUC2==huc_num)

data_usgs%>%
  dplyr::filter(HUC2==huc_num)

data_gages_ref%>%
  dplyr::filter(HUC2==huc_num)
data_gages_nonref%>%
  dplyr::filter(HUC2==huc_num)
data_hcdn%>%
  dplyr::filter(HUC2==huc_num)  

data_cesm_selected<-data_cesm1%>%
  dplyr::filter(HUC2==huc_num)

Summary<-boxplot(data_cesm_selected$Percent)$stats
colnames(Summary)<-c("x")#,"y","z")
rownames(Summary)<-c("Min","First Quartile","Median","Third Quartile","Maximum")
Summary


boxplot.stats(data_cesm_selected$Percent)$out


############################################
# Get boxplot statistics
boxplot_stats <- boxplot.stats(data_cesm_selected$Percent)
boxplot_stats$stats
# Change upper and lower limits
boxplot_stats$stats[1] <- quantile(data_cesm_selected$Percent, 0.05)  # Change lower limit to 10th percentile
boxplot_stats$stats[5] <- quantile(data_cesm_selected$Percent, 0.95)  # Change upper limit to 90th percentile

# View the modified boxplot statistics
print(boxplot_stats)

print

############################################


# define the summary function
f <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

quantile(data_cesm_selected$Percent, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))


a<-data_cesm_selected$Percent;b<-quantile(data_cesm_selected$Percent, 0.05);c<-quantile(data_cesm_selected$Percent, 0.95)
a[a<b]
a[a>c]


# do it
p<-ggplot(data = data_cesm_selected%>%select(HUC2,Percent,Type),
       aes(x=HUC2,
           y=Percent
           ))+
  stat_summary(fun.data = f, geom="boxplot")

p
# example with outliers
# define outlier as you want    
o <- function(x) {
  subset(x, x < quantile(x)[2] | quantile(x)[4] < x)
}

# do it
ggplot(data_cesm_selected$Percent)#, aes(x, y)) 
+ 
  stat_summary(fun.data=f, geom="boxplot") + 
  stat_summary(fun.y = o, geom="point")

#################
#################

rm(myboxplot,trend_type,stat_type,var_result_type)

data_cesm1_v2<-data_cesm1
data_cesm1_v2$Type<-as.character(data_cesm1_v2$Type)
rm(data_cesm1, data_ensemble001)
plot<-boxplot(Percent~Type+HUC2,data=data_cesm1_v2%>%filter(Type=="CESM1 Future"),outline=FALSE)
summary<-plot$stats

df <- data.frame(matrix(ncol = 18, nrow = 3))

for (x in 1:18) {


a<-data_cesm1_v2%>%filter(Type=="CESM1 Historical",HUC2==x)%>%select(Percent)
a1<-data_cesm1_v2%>%filter(Type=="CESM1 Historical",HUC2==x)%>%select(HUC2,Percent)
a2<-split(a1, a1$HUC2)

print(median(a$Percent))


df[[x]][[2]]<-median(a$Percent)
b<-quantile(a,probs=c(0.025, 0.975),na.rm=TRUE)
df[[x]][[1]]<-b[[1]]
df[[x]][[3]]<-b[[2]]

}



## Outliers

a<-data_cesm1_v2%>%filter(Type=="CESM1 Historical",HUC2==x)%>%select(Percent)
Q <- quantile(a, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(a$Percent, na.rm = TRUE)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
a_trimmed<- subset(a, a$Percent > (Q[1] - 1.5*iqr) & warpbreaks$breaks < (Q[2]+1.5*iqr))
a_trimmed2<-na.omit(a_trimmed)


row.names(df) <- c("2.5%", "Median", "97.5%")


usgs_<-data_usgs%>%select(Percent)
usgs_v2<-transpose(usgs_)
write.csv(usgs_v2,"usgs.csv")

rm(usgs_,usgs_v2,data_cesm1_v2,plot,summary,data_usgs)
rm(folder_out,username)

