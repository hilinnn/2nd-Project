##Summary statistics table
library(tidyverse)
library(writexl)

#####Functions of summarizing table
Sum_func <- function(table, row_ord){
  Out <- table %>%
    group_by(Treatment) %>%
    mutate(Treatment = factor(Treatment, levels = fct_rev(row_ord))) %>%
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



###Round 1, Tengrela
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
Tengrela_R1A_rm$Nets <- as.factor(Tengrela_R1A_rm$Nets)
Tengrela_R1A_rm$Treatment <- as.factor(Tengrela_R1A_rm$Treatment)

a <- subset(Tengrela_R1A_rm, Tengrela_R1A_rm$Treatment == "IG2.wash")


##General
###Desire order of rows
row_ord <- fct_rev(factor(levels(Tengrela_R1A_rm$Treatment)))

###Summary statistics estimate
Tengrela_R1A_Sum <-Sum_func(Tengrela_R1A_rm, row_ord)
Tengrela_R1A_Sum_test <-Sum_func(Tengrela_R1A_rm, row_ord)

###Validity testing
sum(Tengrela_R1A_Sum[1:6,22:24])
###should be equal to 6, could also sum up by row
View(Tengrela_R1A_Sum)
View(Tengrela_R1A_Sum_test)


###Export the summary table to a excel spreadsheet
write_xlsx(Tengrela_R1A_Sum, "summary0506.xlsx", col_names = TRUE)


###Boxplot of the statistics by treatments
box_4loc(Tengrela_R1A_rm,
         ti = "Data of trials in Tengrela: Round 1 - 2019")
ggsave("Tengrela R1A plot.jpeg", device = "jpeg")






###Round 2, Tengrela
Tengrela_R2 <- as.data.frame(dfW_new[dfW$Village == 'Tengrela' & dfW$Round ==2,])

###Round 1, Tiefora
Tiefora_R1 <- as.data.frame(dfW_new[dfW$Village == 'Tiefora' & dfW$Round ==1,s])

###Round 1, Tiefora
Tiefora_R2 <- as.data.frame(dfW_new[dfW$Village == 'Tiefora' & dfW$Round ==2,])
