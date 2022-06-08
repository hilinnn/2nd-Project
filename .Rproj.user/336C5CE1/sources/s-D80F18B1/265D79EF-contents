##Summary statistics table
library(tidyverse)
library(writexl)

#####Functions of summarizing table
Sum_func <- function(table, row_ord){
  Out <- table %>%
    mutate(Nets = factor(Nets, levels = row_ord))%>%
    group_by(Treatment)%>%
    summarize(Mor.Tot = round(sum(Dead.Tot)/sum(Total),  3), 
              Morfed.Tot = round(sum(bf_dead.Tot)/sum(Fed.Tot),  3), 
              Bf.Tot = round(sum(Fed.Tot)/sum(Total),   3),
              Mean_mospn.Tot = round(mean(Total),3),
              Mor24.Tot = round(sum(dead24.Tot)/sum(Total),  3), 
              Mor.Room = round(sum(Dead.Room)/sum(Total.Room),  3), 
              Morfed.Room = round(sum(bf_dead.Room)/sum(Fed.Room),  3),
              Bf.Room = round(sum(Fed.Room)/sum(Total.Room),  3),
              Mean_mospn.Room = round(mean(Total.Room),3),
              Mor24.Room = round(sum(dead24.Room)/sum(Total.Room),  3), 
              Mor.Net = round(sum(Dead.Net)/sum(Total.Net),  3), 
              Morfed.Net = round(sum(bf_dead.Net)/sum(Fed.Net),  3),
              Bf.Net = round(sum(Fed.Net)/sum(Total.Net),  3),
              Mean_mospn.Net = round(mean(Total.Net),3),
              Mor24.Net = round(sum(dead24.Net)/sum(Total.Net),  3), 
              Mor.Ver = round(sum(Dead.Ver)/sum(Total.Ver),  3), 
              Morfed.Ver = round(sum(bf_dead.Ver)/sum(Fed.Ver),  3),
              Bf.Ver = round(sum(Fed.Ver)/sum(Total.Ver),  3),
              Mean_mospn.Ver = round(mean(Total.Ver),3),
              Mor24.Ver = round(sum(dead24.Ver)/sum(Total.Ver),  3),
              Spread.Room = round(sum(Total.Room)/sum(Total),  3),
              Spread.Net = round(sum(Total.Net)/sum(Total),  3),
              Spread.Ver = round(sum(Total.Ver)/sum(Total),  3),
              n = n())%>%
    ungroup()
  return(Out)
}


#####Boxplots function according to the location
box_4loc <- function(Round, ti){
  
  ###rows for Mortality
  mor_data <- Round %>%
    group_by(Treatment) %>%
    summarise(Room = round(Dead.Room/Total.Room,2),
              Net = round(Dead.Net/Total.Net,2),
              Veranda = round(Dead.Ver/Total.Ver,2),
              Total = round(Dead.Tot/Total,2)) %>%
    gather("Location", "val", c("Room", "Net", "Veranda", "Total")) %>%
    mutate(Location = factor(Location, levels = c("Veranda","Net","Room", "Total"))) %>%
    ungroup()%>%
    mutate(Category = rep("Mortality",nrow(Round)*4))
  
  ###Rows of mosquito dead within 24 hours
  mor24_data <- Round %>%
    group_by(Treatment) %>%
    summarise(Room = round(dead24.Room/Total.Room,2),
              Net = round(dead24.Net/Total.Net,2),
              Veranda = round(dead24.Ver/Total.Ver,2),
              Total = round(dead24.Tot/Total,2)) %>%
    gather("Location", "val", c("Room", "Net", "Veranda", "Total")) %>%
    mutate(Location = factor(Location, levels = c("Veranda","Net","Room", "Total"))) %>%
    ungroup()%>%
    mutate(Category = rep("Mortality in 24 hours",nrow(Round)*4))
  
  ###rows for blood-fed
  bf_data <-  Round %>%
    group_by(Treatment) %>%
    summarise(Room = round(Fed.Room/Total.Room,2),
              Net = round(Fed.Net/Total.Net,2),
              Veranda = round(Fed.Ver/Total.Ver,2),
              Total = round(Fed.Tot/Total,2)) %>%
    gather("Location", "val", c("Room", "Net", "Veranda", "Total")) %>%
    mutate(Location = factor(Location, levels = c("Veranda","Net","Room", "Total"))) %>%
    ungroup()%>%
    mutate(Category = rep("Blood-fed",nrow(Round)*4))
  
  ###rows for Mean no.mosquito
  Mean_Mospn_data <- select(Round, c("Treatment","Total","Total.Room","Total.Net","Total.Ver"))
  colnames(Mean_Mospn_data) <- c("Treatment","Total", "Room", "Net", "Veranda")
  Mean_Mospn_data <-  Mean_Mospn_data %>%
    gather("Location", "val", c("Room", "Net", "Veranda", "Total")) %>%
    mutate(Location = factor(Location, levels = c("Veranda","Net","Room", "Total"))) %>%
    mutate(Category = rep("Mean mosquito per night",nrow(Round)*4))
  
  ###Rows of mortality conditional on feeding activity
  Morcfed_data <- Round %>%
    group_by(Treatment) %>%
    summarise(Room = round(bf_dead.Room/Fed.Room,2),
              Net = round(bf_dead.Net/Fed.Net,2),
              Veranda = round(bf_dead.Ver/Fed.Ver,2),
              Total = round(bf_dead.Tot/Fed.Tot,2)) %>%
    gather("Location", "val",c("Room", "Net", "Veranda", "Total")) %>%
    mutate(Location = factor(Location, levels = c("Veranda","Net","Room", "Total"))) %>%
    ungroup()%>%
    mutate(Category = rep("Mortality conditional on blood-fed",nrow(Round)*4))
  
  
  ###Rows of the spread of mosquitoes by location
  Spread_data <- Round %>%
    group_by(Treatment) %>%
    summarise(Room = round(Total.Room/Total,2),
              Net = round(Total.Net/Total,2),
              Veranda = round(Total.Ver/Total,2)) %>%
    gather("Location", "val",c("Room", "Net", "Veranda")) %>%
    mutate(Location = factor(Location, levels = c("Veranda","Net","Room"))) %>%
    ungroup()%>%
    mutate(Category = rep("Spread",nrow(Round)*3))
  
  ###Show the proportion of dead mosquito each night in different locations
  
  
  p_data <- rbind(mor_data, bf_data, Mean_Mospn_data, mor24_data,Morcfed_data,Spread_data)
  
  p1 <- p_data %>%
    mutate(Category = factor(Category, levels = c("Mortality","Mortality in 24 hours",
                                                  "Blood-fed", "Mean mosquito per night",
                                                  "Mortality conditional on blood-fed", "Spread"))) %>%
    ggplot(aes(x=val, y=Location)) +
    geom_boxplot(aes(color = Treatment))+
    labs(title = ti, x="Value")+
    theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))+
    facet_wrap(vars(Category), scales = "free_x",ncol = 2)+
    guides(color = guide_legend(reverse = TRUE))
  
  return(p1)
}







###Round 1, Tengrela: After removing the 'error' lines
Tengrela_R1_rm <- as.data.frame(dfW_rm[dfW_rm$Village == 'Tengrela' & dfW_rm$Round ==1,])

#dim(Tengrela_R1)
# 432 44

dim(Tengrela_R1_rm)
# 432 48

########################################################################
###########################Round 1A#####################################
########################################################################

Tengrela_R1A_rm <- subset(Tengrela_R1_rm, Tengrela_R1_rm$Date < "2020-01-01")
View(Tengrela_R1A_rm)
range(Tengrela_R1A_rm$Date)
#"2019-09-08" "2019-10-18"


##Modify the two columns into factors so easier for the following plots
Tengrela_R1A_rm$Nets <- as.factor(Tengrela_R1A_rm$Nets)
Tengrela_R1A_rm$Treatment <- as.factor(Tengrela_R1A_rm$Treatment)



##General
###Desire order of rows
row_ord <- fct_rev(factor(levels(Tengrela_R1A_rm$Treatment)))

###Summary statistics estimate
Tengrela_R1A_Sum <-Sum_func(Tengrela_R1A_rm, row_ord)

###Validity testing
sum(Tengrela_R1A_Sum[1:6,22:24])
###should be equal to 6, could also sum up by row
View(Tengrela_R1A_Sum)

write_xlsx(Tengrela_R1A_Sum, "summary0506.xlsx", col_names = TRUE)



###Boxplot of the statistics by treatments
box_4loc(Tengrela_R1A_rm,
         ti = "Data of trials in Tengrela: Round 1 - 2019")
ggsave("Tengrela R1A plot 0506.jpeg", device = "jpeg")


