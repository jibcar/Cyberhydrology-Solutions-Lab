
library(ggplot2)
library(ggpattern)
library(viridis)
library(hrbrthemes)
library(ggtext)
library(dplyr)


## Clear all variables
rm(list=ls())

## Clear Console
cat("\014")


version="v1"

year_dam_start="all"
year_dam_end="all"

year_usgs_start="all"
year_usgs_end="all"

setwd(paste0(".\\Code_R"))

## Plots Output Folder
folder_plot_out<-paste0(".\\Plots_IntermediateResults\\plots_multiple_regression_",
                   version,
                   "\\")
dir.create(folder_plot_out, showWarnings = TRUE)
folder_data_out<-paste0(".\\Data_Intermediate\\data_multiple_regression_",
                   version,
                   "\\")

fileConn<-file(paste0(folder_plot_out,"00_code_full_path.txt"))
writeLines(rstudioapi::getSourceEditorContext()$path, 
           fileConn)
close(fileConn)
rm(fileConn)



########################
########################

file_name=paste0(folder_data_out,"/barchart_data_type1_USGS_",year_usgs_start,"_",year_usgs_end,"_dam_",year_dam_start,"_",year_dam_end,".txt")

data_full <- read.csv(file_name,
                      sep=',',
                      colClasses = c("HUC2_code" = "character"),
                      row.names=NULL)
num_of_sites<-data_full[["Number_Sites"]][[1]]

title_name=paste0("Spatial determinants identified behind the peakflow trends in the HUC2 regions",
                  "\nusing Multiple linear regression using Peakflow data of " ,num_of_sites," significant sites",
                  "\nduring the period from ", year_usgs_start," to ",year_usgs_end,
                  " & using Max storage from dams completed from ",year_dam_start," to ",year_dam_end)

if (year_dam_start=="all"){
  if(year_dam_start=="all"){
    #file_name=paste0("./data/barchart_data_type1_USGS_",year_usgs_start,"_",year_usgs_end,"_dam_",year_dam_start,"_",year_dam_end,".txt")
    
    title_name=paste0("Spatial determinants identified behind the peakflow trends in the HUC2 regions",
                      "\nusing Multiple linear regression using Peakflow data of " ,num_of_sites," significant sites",
                      "\nduring the period from ", year_usgs_start," to ",year_usgs_end,
                      " & using Max storage from all dams")
    y_up=65
  }}
if (year_dam_start=="1950"){
  if(year_dam_start=="2014"){
    #file_name=paste0("./data/barchart_data_type1_USGS_",year_usgs_start,"_",year_usgs_end,"_dam_",year_dam_start,"_",year_dam_end,".txt")
    
    title_name=paste0("Spatial determinants identified behind the peakflow trends in the HUC2 regions",
                      "\nusing Multiple linear regression using Peakflow data of " ,num_of_sites," significant sites",
                      "\nduring the period from ", year_usgs_start," to ",year_usgs_end,
                      " & using Max storage from dams completed between ", year_dam_start," to ",year_dam_end)
    y_up=77
  }}


##############################
#############################



data <- data_full %>%dplyr::filter(HUC2_code!="00")

data<-data%>%
  dplyr::filter(Significance=="sig")

data<-data%>%
  dplyr::filter(Variable_Name!="(Intercept)")

data$HUC2_Code_Name<-paste0(data$HUC2_code," - ", data$HUC2_name)

data<-data%>%
  arrange(HUC2_code)

## Generation of Labels
labs <- data %>% group_by(HUC2_Code_Name) %>%
  #summarise(count_N=mean(Count_Sites_in_HUC2),
  summarise(count_N=mean(Number_Sites),
            #location_N=sum(Value)) %>% 
            location_N=sum(VarianceExplained_percent)) %>% 
  
  mutate(Variable_Name=NA)


levels(data$Variable_Name)
datetime_text<-paste0("DateTime:", Sys.time ())

# Find unique character values
unique_values <- sort(unique(data$Variable_Name))


## Create a dictionary mapping unique character values to new values
mapping <- c("LU2019_Agr" = "Agriculture",
             "LU2019_Urb" = "Urbanization",
             "norm_max_stor" ="Norm Max Storage",
             "percent_max_decrease"="PCP Max Decrease",
             "percent_mean_decrease"="PCP Mean Decrease",
             "percent_mean_increase"="PCP Mean Increase",
             "percent_max_increase"="PCP Max Increase"
             )

## Replace the values using the dictionary
data$Variable_Name <- mapping[data$Variable_Name]


data$Variable_Name<-factor(data$Variable_Name, 
                                             levels=c("Agriculture",
                                                      "Urbanization",
                                                      "Norm Max Storage",
                                                      "PCP Max Decrease",
                                                      "PCP Mean Decrease",
                                                      "PCP Mean Increase",
                                                      "PCP Max Increase"
                                                      ))
unique(data$Variable_Name)
levels(data$Variable_Name)

colnames(data)

datetime_text<-paste0("DateTime: ", Sys.time ())
# Small multiple
mlr_plot<-ggplot(data, mapping=aes(x=HUC2_Code_Name,
                         y=VarianceExplained_percent,
                         group=Variable_Name))+
                 
  geom_bar(aes(fill = Variable_Name),
           stat="identity") +
  # geom_text(data=labs,
  #           aes(x=HUC2_Code_Name,
  #               y=location_N,
  #               label=paste0(count_N," sites"),
  #               group=Variable_Name),
  #           #position=position_stack(vjust=.5),
  #           position = position_dodge(width = 0.9),
  #           vjust = 0,
  #           hjust=1.5,
  #           col="black",
  #           angle=90
  #           )+

  geom_text(data=labs,
    aes(x=HUC2_Code_Name,
        #y=location_N,
        label = paste0("(",count_N,")"),
        y = location_N+5),
    position = position_dodge(0.9),
    vjust = 0,
    #hjust=-2,
    angle=0
  )+
  geom_text(aes(label = round(VarianceExplained_percent,1)), position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = c("#FFFF00", "#99ff33",
                               "#00CC99",
                               "#FF0000","#FF6666",
                               "#3399FF","#0000FF"))+ 
  
  ggtitle(title_name) +
  #theme_ipsum() +
  xlab("HUC2 Region")+
  ylab("% Variance Explained")+
  
  scale_x_discrete(guide = guide_axis(angle = 45))+
  ## Add text
  coord_cartesian(ylim=c(0,y_up+15),clip = "off")+
  annotate("text", 
           x = 16, 
           y = (y_up+15)*1.2, 
           label = paste0("DateTime:", Sys.time ()),
           size=1.5
          )+
  annotate("text", 
           x = 10, 
           y = (y_up+0)*1.2, 
           label = paste0("Number of significant sites in each regions in shown in parenthesis",
                          "\nSome HUC2 regions do not show up in MLR and hence total number of sites (",
                          sum(labs[["count_N"]]),") does not add up to ",
                          num_of_sites
                          ),
           size=3
  )
  
mlr_plot
ggsave(
  paste0(folder_plot_out,
         paste0("mlr_USGS_new_",
                year_usgs_start,"_",year_usgs_end,
                "_dam_",
                year_dam_start,"_",year_dam_end,
                "_type1_y_up_",y_up,".png")),
  plot = mlr_plot,
  width = 8.93,
  height = 5.20,
  #units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)

rm(mlr_plot)

######################
## Type 2 plot with shading
#####################
levels(data$Variable_Name)

mlr_plot2<-ggplot(data, mapping=aes(x=HUC2_code,
                                   y=VarianceExplained_percent,
                                   #fill=SignificantVariable,
                                   #pattern=SignificantVariable
                                   ))+
  geom_bar_pattern(position="stack",
                   stat="identity",
    mapping=aes(fill=Variable_Name,
                pattern=Variable_Name,
                pattern_spacing=Variable_Name,
                pattern_density=Variable_Name,
                ),
    
    #pattern_density = 0.2,
    #pattern_spacing=0.025
    )+
  theme_bw() +
  #labs(title = "Use 'stripe' and 'wave' patterns") + 
  #theme(legend.key.size = unit(1.5, 'cm')) +
  scale_fill_manual(values = c("#FFFF00", "#99ff33",
                               "#00CC99",
                               "#FF0000","#FF6666",
                               "#3399FF","#0000FF"))+ 
  scale_pattern_manual(values=c("none", 'weave',
                                'none',
                                'none','none',
                                'none','none'))+ 
  scale_pattern_spacing_manual(values = c(0.05,0.05, 0.05, 0.05, 0.05, 0.05, 0.05))+
  scale_pattern_density_manual(values = c(0.05,0.4, 0.05, 0.05, 0.05, 0.05, 0.05))+
  
  ## Change legend title
  labs(fill='Significant Variables') +
  labs(pattern='Significant Variables') +
  labs(pattern_manual='Significant Variables') +
  labs(pattern_spacing='Significant Variables') +
  labs(pattern_density='Significant Variables')+
  
coord_cartesian(ylim=c(0,y_up),clip = "off")+
  annotate("text", 
         x = 10, 
         y = y_up*1.2, 
         label = paste0("DateTime:", Sys.time ()),
         size=1.5)+
  

  
  #+scale_pattern_type_manual(values=c(NA, 'triangle', 'sine',NA, 'triangle', 'sine',NA))
  ggtitle(title_name) +
  #theme_ipsum() +
  xlab("HUC2 Region")+
  ylab("% Variance Explained")+
  
  ## Legend
  theme(#legend.key.width = unit(1, "cm"), 
        legend.position = "right",#"right"
        legend.direction = "vertical"#"vertical"
        )

ggsave(
  paste0(folder_plot_out,
         paste0("mlr_USGS_new_",year_usgs_start,"_",year_usgs_end,"_dam_",year_dam_start,"_",year_dam_end,"_type2_y_up_",y_up,".png")),
  plot = mlr_plot2,
  width = 8.93,
  height = 5.20,
  units = "in",
  #units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)


mlr_plot3 <- ggplot(data, aes(x = HUC2_code, y = VarianceExplained_percent)) +
  geom_bar_pattern(
    position = "stack",
    stat = "identity",
    aes(
      fill = Variable_Name,
      pattern = Variable_Name,
      pattern_spacing = Variable_Name,
      pattern_density = Variable_Name
    )
  ) +
  scale_fill_manual(values = c("#FFFF00", "#99ff33", "#00CC99", "#FF0000", "#FF6666", "#3399FF", "#0000FF")) +
  scale_pattern_manual(values = c("none", "weave", "none", "crosshatch", "none", "none", "none")) +
  scale_pattern_spacing_manual(values = c(0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05)) +
  scale_pattern_density_manual(values = c(0.05, 0.4, 0.05, 0.1, 0.05, 0.05, 0.05)) +
  labs(
    title = title_name,
    x = "HUC2 Region",
    y = "% Variance Explained",
    fill = 'Significant Variables',
    pattern = 'Significant Variables',
    pattern_spacing = 'Significant Variables',
    pattern_density = 'Significant Variables'
  ) +
  coord_cartesian(ylim = c(0, y_up), clip = "off") +
  annotate("text", 
           x = 1,  # Adjust x and y as needed
           y = y_up * 1.2, 
           label = paste0("DateTime: ", Sys.time()), 
           size = 4) +
  theme_bw() +
  theme(
    legend.position = "right",
    legend.direction = "vertical"
  )
#mlr_plot3

ggsave(
  paste0(folder_plot_out,
         paste0("mlr_USGS_new_",year_usgs_start,"_",year_usgs_end,"_dam_",year_dam_start,"_",year_dam_end,"_type3_",y_up,".png")),
  plot = mlr_plot3,
  width = 8.93,
  height = 5.20,
  units = "in",
  #units = c("mm"),
  dpi = 300,
  limitsize = TRUE
)

