#The object of this script is to tidy soils data from STA1E and STA34. 


rm(list=ls())


library(readr)
library(readxl)
library(scales)
library(dplyr)
library(ggpmisc)
library(ggplot2)
library(lubridate)
library(tidyr)
library(maptools)
library(broom)
library(stringr)
library(fmsb)
library(ggrepel)

# Step 1: Import and Tidy Flow  Data from CSV --------


STA1E_SoilsData_Complete_032222 <- read_excel("Data/STA1E_SoilsData_Complete_032222.xls")  %>% mutate(STA="STA-1E")  
STA34_SoilsData_Complete_032222 <- read_excel("Data/STA34_SoilsData_Complete_032222.xls")  %>% mutate(STA="STA-34") 



# Step 2 Tidy and Join Data -----------------------------------------------

Soils_Wide_Tidy <- rbind(STA1E_SoilsData_Complete_032222,STA34_SoilsData_Complete_032222 ) %>% #create wide DF
filter(str_detect(SOIL_SECTION,"FLOC")) %>%  #filter to floc layer
group_by (STA,STATION) %>%
summarise(across(where(is.numeric),mean))  #calculate mean of replicate samples from same station

write_csv(Soils_Wide_Tidy, "./Data/Soils_Wide_Tidy.csv")

Soils_Long_Tidy <- Soils_Wide_Tidy %>%   #create long DF
pivot_longer(names_to="Parameter",values_to="Value",3:24)  #pivot to long DF

write_csv(Soils_Long_Tidy, "./Data/Soils_Long_Tidy.csv")

Soils_Wide_Scale_Bad_Names <- Soils_Wide_Tidy %>%    
mutate(across(where(is.numeric),scale))   #scale values (creates column names with special characters)

Soils_Long_Scale_Tidy <- Soils_Wide_Scale_Bad_Names   %>% 
pivot_longer(names_to="Parameter",values_to="Value",3:24) %>%  #pivot to long DF 
mutate(Value=Value[,1])  #remove special characters from column name

write_csv(Soils_Long_Scale_Tidy, "./Data/Soils_Long_Scale_Tidy.csv")

Soils_Wide_Scale_Tidy <- Soils_Long_Scale_Tidy %>%   #recreate wide format DF without special charaters in column names
pivot_wider(names_from="Parameter",values_from="Value")   

write_csv(Soils_Wide_Scale_Tidy,"./Data/Soils_Wide_Scale_Tidy.csv")

Soils_Storage_Wide_Tidy <- Soils_Wide_Tidy %>%
rowwise()  %>%
mutate( across(contains('mg/Kg'),funs(Storage = .*`BD, g/cc`*`THICKNESS, cm`/100))) %>%  #convert mg/kg to g/m^2: 10,000cm^2/1m^2 * 1kg/1,000,000mg * thickness cm
rename_at(vars(contains('Storage')), funs(sub(", mg/Kg_Storage", " g/m^2", .)))

write_csv(Soils_Storage_Wide_Tidy, "./Data/Soils_Storage_Wide_Tidy.csv")


# Visualize ---------------------------------------------------------------

#Boxplot
ggplot(Soils_Long_Tidy,aes(STATION,Value,color=STA))+geom_boxplot()+
facet_wrap(~Parameter,scales="free")+theme_bw()

#Paretto chart TP mg/kg
ggplot(Soils_Wide_Tidy ,aes(stats::reorder(STATION,-`TOT-P, mg/Kg`),`TOT-P, mg/Kg`,fill=STA))+geom_col()+theme_bw()+scale_fill_manual(values=c("#7fcdbb","#2c7fb8"))+
theme(plot.margin = unit(c(.2,.2,.1,.75), "cm"),legend.position = "bottom",axis.text.x = element_text(angle = 60, vjust = 1, hjust=1,size=12),axis.text.y = element_text(size=12),axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14))+
labs(x="TOT-P",y=expression(mg~kg-1))+scale_y_continuous(breaks= pretty_breaks(n=10))

#Paretto chart TP g/m^2
ggplot(Soils_Storage_Wide_Tidy ,aes(stats::reorder(STATION,-`TOT-P g/m^2`),`TOT-P g/m^2`,fill=STA))+geom_col()+theme_bw()+scale_fill_manual(values=c("#7fcdbb","#2c7fb8"))+
theme(plot.margin = unit(c(.2,.2,.1,.75), "cm"),legend.position = "bottom",axis.text.x = element_text(angle = 60, vjust = 1, hjust=1,size=12),axis.text.y = element_text(size=12),axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14))+
labs(x="TOT-P",y=expression(mg~kg-1))+scale_y_continuous(breaks= pretty_breaks(n=10))

#Bar chart of scaled values
ggplot(Soils_Long_Scale_Tidy, aes(x=Parameter, y=Value,fill=STA)) +
geom_bar(stat="identity", width=0.6) +scale_fill_manual(values=c("#7fcdbb","#2c7fb8"))+
coord_flip() +facet_wrap(~STATION,ncol=9)

#Bulk Density vs TP
ggplot(Soils_Wide_Tidy,aes(`THICKNESS, cm`,`BD, g/cc`,size=(`TOT-P, mg/Kg`),fill=STA,label=STATION))+geom_point(shape=21)+geom_label_repel(size=3,fill="grey90",box.padding=.5)
theme_bw()



