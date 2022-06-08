########Visualization functions#########
library(tidyverse)
library(ggplot2)

#####Boxplots according to the location
box_4loc <- function(Round, ti){
  
  n <- sum(Tengrela_R1A_rm$Total!= 0)+ sum(Tengrela_R1_rm$Total.Room != 0) +sum(Tengrela_R1_rm$Total.Net != 0) +sum(Tengrela_R1_rm$Total.Ver != 0)   

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




