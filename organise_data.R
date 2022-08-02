library(readxl)
library(here)
library(ggplot2)
library(reshape2)

#Read in Excel file
dataCB <- read_excel(here("F5_EHT-Data 2019-2020_Feb21.xlsx")) 
table(dataCB$`Village `)
#Update this variable name!
colnames(dataCB)[colnames(dataCB) == 'Village '] <- 'Village'
table(dataCB$Nets)
table(dataCB$Round)
table(dataCB$Day)
table(dataCB$nDay)

#optional: convert Date to a string
dataCB$Date <- as.character(dataCB$Date)
dataCB <- as.data.frame(dataCB)

#Data from two locations
table(dataCB$Round[dataCB$Village=='Tengrela'])
table(dataCB$Round[dataCB$Village=='Tiefora'])

#Two trials carried out in each location. 'UTN' = untreated net
table(dataCB$Nets[dataCB$Village=='Tiefora' & dataCB$Round==1])
table(dataCB$Nets[dataCB$Village=='Tiefora' & dataCB$Round==2])

table(dataCB$Treatment[dataCB$Village=='Tiefora' & dataCB$Round==1])
table(dataCB$Treatment[dataCB$Village=='Tiefora' & dataCB$Round==2])

table(dataCB$Nets[dataCB$Village=='Tengrela' & dataCB$Round==1])
table(dataCB$Nets[dataCB$Village=='Tengrela' & dataCB$Round==2])

table(dataCB$Treatment[dataCB$Village=='Tengrela' & dataCB$Round==1])
table(dataCB$Treatment[dataCB$Village=='Tengrela' & dataCB$Round==2])

#If I've understood data correctly, there's a typo...
dataCB[dataCB$Row.ID=='Ten.533',]$Cum_72H <- 8 #???

#Each data point spread over 24 rows! Here's one way to summarise data into 1 row. 
#However, this will ignore data on the location in which mosquito was found
#Task: find a combination of variables that uniquely identify each data point
uni <- unique(dataCB[,c('Village','Round','Treatment','Sleeper','nDay','Date')])
ld <- dim(uni)[1]
#A unique ID
uni$marker <- seq(1,ld,1)

#Transfer this identifier to the dataframe
dataCB$marker <- NA
for(i in 1:ld){
  vill <- uni[i,"Village"]
  roun <- uni[i,"Round"]
  treat <- uni[i,"Treatment"]
  sleep <- uni[i,"Sleeper"]
  nday <- uni[i,"nDay"]
  d8 <- uni[i,"Date"]
  dataCB[dataCB$Village == vill & dataCB$Round == roun  & dataCB$Treatment == treat & 
           dataCB$Sleeper == sleep & dataCB$nDay == nday & dataCB$Date == d8, ]$marker <- uni$marker[i]
}


#This is important- Cum72 only useful for Alive status
table(dataCB[dataCB$Status=='Dead',]$Cum_72H)

#####################################   IMPORTANT   ####################################
# There is a complication here! The variable 'Status' records whether mosquitoes were dead or alive when they were
# collected. However, if a mosquito died within 72 hours of collection, it should be recorded as dead. Therefore,
# we must use information in column 'Cum_72H' to make adjustments to the number of mosquitoes that are dead & alive
# See Antoine's email 29th Oct 2021

#Now I want to collect summary information for each data point. We ignore other mosquito species, 
#I just used information from 'Tot.gamb.follow' 


#Start with empty dataframe?
# dfW_new <- data.frame('Village'=character(), 'Round' = integer(), 'Treatment' = character(),
#                   'Nets' = character(), 'Date' = character(), 
#                   'Total'=integer(), 
#                   'Dead' = integer(), 'Alive' = integer(), 'Sleeper' = character(),
#                   'Fed' = integer(), 'Unfed' = integer(),
#                   'unf_live' = integer(), 'unf_dead' = integer(),
#                   'bf_live' = integer(), 'bf_dead' = integer(),
#                   'Total.Room'=integer(), 
#                   'Dead.Room' = integer(), 'Alive.Room' = integer(), 'Sleeper.Room' = character(),
#                   'Fed.Room' = integer(), 'Unfed.Room' = integer(),
#                   'unf_live.Room' = integer(), 'unf_dead.Room' = integer(),
#                   'bf_live.Room' = integer(), 'bf_dead.Room' = integer(),
#                   'Total.Net'=integer(), 
#                   'Dead.Net' = integer(), 'Alive.Net' = integer(), 'Sleeper.Net' = character(),
#                   'Fed.Net' = integer(), 'Unfed.Net' = integer(),
#                   'unf_live.Net' = integer(), 'unf_dead.Net' = integer(),
#                   'bf_live.Net' = integer(), 'bf_dead.Net' = integer(),
#                   'Total.Ver'=integer(), 
#                   'Dead.Ver' = integer(), 'Alive.Ver' = integer(), 'Sleeper.Ver' = character(),
#                   'Fed.Ver' = integer(), 'Unfed.Ver' = integer(),
#                   'unf_live.Ver' = integer(), 'unf_dead.Ver' = integer(),
#                   'bf_live.Ver' = integer(), 'bf_dead.Ver' = integer(),
#                   'Hut' = character(), 'marker' = integer())
# 
# 
# 
# 
# for(i in 1:ld){
#   #Make a small dataframe ('aux') for each data point
#   aux <- dataCB[dataCB$marker == i,] 
#   dead <- sum(aux[aux$Status == 'Dead',]$Tot.gamb.follow) + 
#     sum(aux[aux$Status == 'Alive',]$Cum_72H)
#   live <- sum(aux[aux$Status == 'Alive',]$Tot.gamb.follow) - 
#     sum(aux[aux$Status == 'Alive',]$Cum_72H)
#   fed <- sum(aux[aux$Physiology == 'Fed',]$Tot.gamb.follow) + 
#     sum(aux[aux$Physiology == 'Partially-fed',]$Tot.gamb.follow) 
#   unfed <- sum(aux[aux$Physiology == 'Unfed',]$Tot.gamb.follow) + 
#     sum(aux[aux$Physiology == 'Gravid',]$Tot.gamb.follow)
#   unf_live <- sum(aux[aux$Status == 'Alive' & aux$Physiology=='Unfed',]$Tot.gamb.follow) - 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Unfed',]$Cum_72H) + 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Gravid',]$Tot.gamb.follow) - 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Gravid',]$Cum_72H)
#   unf_dead <- sum(aux[aux$Status == 'Dead' & aux$Physiology=='Unfed' ,]$Tot.gamb.follow) + 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Unfed',]$Cum_72H) + 
#     sum(aux[aux$Status == 'Dead' & aux$Physiology=='Gravid' ,]$Tot.gamb.follow) + 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Gravid',]$Cum_72H) 
#   bf_live <- sum(aux[aux$Status == 'Alive' & aux$Physiology=='Fed',]$Tot.gamb.follow) - 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Fed',]$Cum_72H) + 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Partially-fed',]$Tot.gamb.follow) - 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Partially-fed',]$Cum_72H) 
#   bf_dead <- sum(aux[aux$Status == 'Dead' & aux$Physiology=='Fed' ,]$Tot.gamb.follow) + 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Fed',]$Cum_72H) + 
#     sum(aux[aux$Status == 'Dead' & aux$Physiology=='Partially-fed' ,]$Tot.gamb.follow) + 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Partially-fed',]$Cum_72H) 
#   total <- sum(aux$Tot.gamb.follow)
#   
#   ##In the room
#   dead.Room <- sum(aux[aux$Status == 'Dead' & aux$Area == 'Room',]$Tot.gamb.follow) + 
#     sum(aux[aux$Status == 'Alive' & aux$Area == 'Room',]$Cum_72H)
#   live.Room <- sum(aux[aux$Status == 'Alive' & aux$Area == 'Room',]$Tot.gamb.follow) - 
#     sum(aux[aux$Status == 'Alive' & aux$Area == 'Room',]$Cum_72H)
#   fed.Room <- sum(aux[aux$Physiology == 'Fed' & aux$Area == 'Room',]$Tot.gamb.follow) + 
#     sum(aux[aux$Physiology == 'Partially-fed' & aux$Area == 'Room',]$Tot.gamb.follow) 
#   unfed.Room <- sum(aux[aux$Physiology == 'Unfed' & aux$Area == 'Room',]$Tot.gamb.follow)+ 
#     sum(aux[aux$Physiology == 'Gravid' & aux$Area == 'Room',]$Tot.gamb.follow)
#   unf_live.Room <- sum(aux[aux$Status == 'Alive' & aux$Physiology=='Unfed' & aux$Area == 'Room',]$Tot.gamb.follow) - 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Unfed' & aux$Area == 'Room',]$Cum_72H)+ 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Gravid' & aux$Area == 'Room',]$Tot.gamb.follow) - 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Gravid' & aux$Area == 'Room',]$Cum_72H)
#   unf_dead.Room <- sum(aux[aux$Status == 'Dead' & aux$Physiology=='Unfed' & aux$Area == 'Room',]$Tot.gamb.follow) + 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Unfed' & aux$Area == 'Room',]$Cum_72H) + 
#     sum(aux[aux$Status == 'Dead' & aux$Physiology=='Gravid'  & aux$Area == 'Room',]$Tot.gamb.follow) + 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Gravid' & aux$Area == 'Room',]$Cum_72H) 
#   bf_live.Room <- sum(aux[aux$Status == 'Alive' & aux$Physiology=='Fed' & aux$Area == 'Room',]$Tot.gamb.follow) - 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Fed' & aux$Area == 'Room',]$Cum_72H) + 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Partially-fed' & aux$Area == 'Room',]$Tot.gamb.follow) - 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Partially-fed' & aux$Area == 'Room',]$Cum_72H) 
#   bf_dead.Room <- sum(aux[aux$Status == 'Dead' & aux$Physiology=='Fed' & aux$Area == 'Room',]$Tot.gamb.follow) + 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Fed' & aux$Area == 'Room',]$Cum_72H) + 
#     sum(aux[aux$Status == 'Dead' & aux$Physiology=='Partially-fed' & aux$Area == 'Room',]$Tot.gamb.follow) + 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Partially-fed' & aux$Area == 'Room',]$Cum_72H)
#   total.Room <- sum(aux[aux$Area == 'Room',]$Tot.gamb.follow)
#   
#   ##In the Net
#   dead.Net <- sum(aux[aux$Status == 'Dead' & aux$Area == 'Net',]$Tot.gamb.follow) + 
#     sum(aux[aux$Status == 'Alive' & aux$Area == 'Net',]$Cum_72H)
#   live.Net <- sum(aux[aux$Status == 'Alive' & aux$Area == 'Net',]$Tot.gamb.follow) - 
#     sum(aux[aux$Status == 'Alive' & aux$Area == 'Net',]$Cum_72H)
#   fed.Net <- sum(aux[aux$Physiology == 'Fed' & aux$Area == 'Net',]$Tot.gamb.follow) + 
#     sum(aux[aux$Physiology == 'Partially-fed' & aux$Area == 'Net',]$Tot.gamb.follow) 
#   unfed.Net <- sum(aux[aux$Physiology == 'Unfed' & aux$Area == 'Net',]$Tot.gamb.follow)+ 
#     sum(aux[aux$Physiology == 'Gravid' & aux$Area == 'Net',]$Tot.gamb.follow)
#   unf_live.Net <- sum(aux[aux$Status == 'Alive' & aux$Physiology=='Unfed' & aux$Area == 'Net',]$Tot.gamb.follow) - 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Unfed' & aux$Area == 'Net',]$Cum_72H)+ 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Gravid' & aux$Area == 'Net',]$Tot.gamb.follow) - 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Gravid' & aux$Area == 'Net',]$Cum_72H)
#   unf_dead.Net <- sum(aux[aux$Status == 'Dead' & aux$Physiology=='Unfed' & aux$Area == 'Net',]$Tot.gamb.follow) + 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Unfed' & aux$Area == 'Net',]$Cum_72H)+ 
#     sum(aux[aux$Status == 'Dead' & aux$Physiology=='Gravid'  & aux$Area == 'Net',]$Tot.gamb.follow) + 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Gravid' & aux$Area == 'Net',]$Cum_72H) 
#   bf_live.Net <- sum(aux[aux$Status == 'Alive' & aux$Physiology=='Fed' & aux$Area == 'Net',]$Tot.gamb.follow) - 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Fed' & aux$Area == 'Net',]$Cum_72H) + 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Partially-fed' & aux$Area == 'Net',]$Tot.gamb.follow) - 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Partially-fed' & aux$Area == 'Net',]$Cum_72H) 
#   bf_dead.Net <- sum(aux[aux$Status == 'Dead' & aux$Physiology=='Fed' & aux$Area == 'Net',]$Tot.gamb.follow) + 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Fed' & aux$Area == 'Net',]$Cum_72H) + 
#     sum(aux[aux$Status == 'Dead' & aux$Physiology=='Partially-fed' & aux$Area == 'Net',]$Tot.gamb.follow) + 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Partially-fed' & aux$Area == 'Net',]$Cum_72H) 
#   total.Net <- sum(aux[aux$Area == 'Net',]$Tot.gamb.follow)
#   
#   ##In the Veranda
#   dead.Ver <- sum(aux[aux$Status == 'Dead' & aux$Area == 'Veranda',]$Tot.gamb.follow) + 
#     sum(aux[aux$Status == 'Alive' & aux$Area == 'Veranda',]$Cum_72H)
#   live.Ver <- sum(aux[aux$Status == 'Alive' & aux$Area == 'Veranda',]$Tot.gamb.follow) - 
#     sum(aux[aux$Status == 'Alive' & aux$Area == 'Veranda',]$Cum_72H)
#   fed.Ver <- sum(aux[aux$Physiology == 'Fed' & aux$Area == 'Veranda',]$Tot.gamb.follow) + 
#     sum(aux[aux$Physiology == 'Partially-fed' & aux$Area == 'Veranda',]$Tot.gamb.follow) 
#   unfed.Ver <- sum(aux[aux$Physiology == 'Unfed' & aux$Area == 'Veranda',]$Tot.gamb.follow)+ 
#     sum(aux[aux$Physiology == 'Gravid' & aux$Area == 'Veranda',]$Tot.gamb.follow)
#   unf_live.Ver <- sum(aux[aux$Status == 'Alive' & aux$Physiology=='Unfed' & aux$Area == 'Veranda',]$Tot.gamb.follow) - 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Unfed' & aux$Area == 'Veranda',]$Cum_72H) + 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Gravid' & aux$Area == 'Veranda',]$Tot.gamb.follow) - 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Gravid' & aux$Area == 'Veranda',]$Cum_72H)
#   unf_dead.Ver <- sum(aux[aux$Status == 'Dead' & aux$Physiology=='Unfed' & aux$Area == 'Veranda',]$Tot.gamb.follow) + 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Unfed' & aux$Area == 'Veranda',]$Cum_72H) + 
#     sum(aux[aux$Status == 'Dead' & aux$Physiology=='Gravid'  & aux$Area == 'Veranda',]$Tot.gamb.follow) + 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Gravid' & aux$Area == 'Veranda',]$Cum_72H) 
#   bf_live.Ver <- sum(aux[aux$Status == 'Alive' & aux$Physiology=='Fed' & aux$Area == 'Veranda',]$Tot.gamb.follow) - 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Fed' & aux$Area == 'Veranda',]$Cum_72H) + 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Partially-fed' & aux$Area == 'Veranda',]$Tot.gamb.follow) - 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Partially-fed' & aux$Area == 'Veranda',]$Cum_72H)
#   bf_dead.Ver <- sum(aux[aux$Status == 'Dead' & aux$Physiology=='Fed' & aux$Area == 'Veranda',]$Tot.gamb.follow) + 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Fed' & aux$Area == 'Veranda',]$Cum_72H) + 
#     sum(aux[aux$Status == 'Dead' & aux$Physiology=='Partially-fed' & aux$Area == 'Veranda',]$Tot.gamb.follow) + 
#     sum(aux[aux$Status == 'Alive' & aux$Physiology=='Partially-fed' & aux$Area == 'Veranda',]$Cum_72H)
#   total.Ver <- sum(aux[aux$Area == 'Veranda',]$Tot.gamb.follow)
# 
#   
#   dff <- data.frame('Village'=aux$Village[1], 'Round' = aux$Round[1], 
#                     'Treatment' = aux$Treatment[1],
#                     'Nets' = aux$Nets[1], 'Date' = aux$Date[1], 
#                     'Total'=total, 
#                     'Dead.Tot' = dead, 'Alive.Tot' = live, 'Fed.Tot' = fed, 'Unfed.Tot' = unfed, 
#                     'unf_live.Tot' = unf_live, 'unf_dead.Tot' = unf_dead,
#                     'bf_live.Tot' = bf_live, 'bf_dead.Tot' = bf_dead, 
#                     'Total.Room'=total.Room,
#                     'Dead.Room' = dead.Room, 'Alive.Room' = live.Room, 'Fed.Room' = fed.Room, 
#                     'Unfed.Room' = unfed.Room, 'unf_live.Room' = unf_live.Room, 
#                     'unf_dead.Room' = unf_dead.Room, 'bf_live.Room' = bf_live.Room, 
#                     'bf_dead.Room' = bf_dead.Room, 
#                     'Total.Net'=total.Net,
#                     'Dead.Net' = dead.Net, 'Alive.Net' = live.Net, 'Fed.Net' = fed.Net, 
#                     'Unfed.Net' = unfed.Net, 'unf_live.Net' = unf_live.Net, 
#                     'unf_dead.Net' = unf_dead.Net, 'bf_live.Net' = bf_live.Net, 
#                     'bf_dead.Net' = bf_dead.Net, 
#                     'Total.Ver'=total.Ver,
#                     'Dead.Ver' = dead.Ver, 'Alive.Ver' = live.Ver, 'Fed.Ver' = fed.Ver, 
#                     'Unfed.Ver' = unfed.Ver, 'unf_live.Ver' = unf_live.Ver, 
#                     'unf_dead.Ver' = unf_dead.Ver, 'bf_live.Ver' = bf_live.Ver, 
#                     'bf_dead.Ver' = bf_dead.Ver,
#                     'Sleeper' = aux$Sleeper[1],
#                     'Hut' = aux$Hut[1], 'marker' = i) 
#   dfW_new <- rbind(dfW_new,dff)
# }
# dim(dfW_new)
# ###1080  44
# head(dfW_new)


###Problem-ish rows
###Subset them
prob <- dataCB%>%
  summarise(prob = Tot.gamb.follow - Cum_72H) %>%
  subset(prob < 0)
View(dataCB[c(79,1144,5167,23447),])

View(dataCB_rm)
####Remove the 4 rows with errors
dataCB_rm <- dataCB

dataCB_rm[c(79,1144,5167,23447),17:23] <- rep(0,7)

error_find <- dataCB_rm %>% subset(Date < "2020-01-01")
error_find[c(79,1144,5167),]

dfW_rm <- data.frame('Village'=character(), 'Round' = integer(), 'Treatment' = character(),
                     'Nets' = character(), 'Date' = character(), 
                     'Total'=integer(), 
                     'Dead' = integer(), 'Alive' = integer(), 'Sleeper' = character(),
                     'Fed' = integer(), 'Unfed' = integer(),
                     'unf_live' = integer(), 'unf_dead' = integer(),
                     'bf_live' = integer(), 'bf_dead' = integer(), 'dead24.Tot' = interger(),
                     'Total.Room'=integer(), 
                     'Dead.Room' = integer(), 'Alive.Room' = integer(), 'Sleeper.Room' = character(),
                     'Fed.Room' = integer(), 'Unfed.Room' = integer(),
                     'unf_live.Room' = integer(), 'unf_dead.Room' = integer(),
                     'bf_live.Room' = integer(), 'bf_dead.Room' = integer(),
                     'dead24.Room' = interger(),
                     'Total.Net'=integer(), 
                     'Dead.Net' = integer(), 'Alive.Net' = integer(), 'Sleeper.Net' = character(),
                     'Fed.Net' = integer(), 'Unfed.Net' = integer(),
                     'unf_live.Net' = integer(), 'unf_dead.Net' = integer(),
                     'bf_live.Net' = integer(), 'bf_dead.Net' = integer(),
                     'dead24.Net' = interger(),
                     'Total.Ver'=integer(), 
                     'Dead.Ver' = integer(), 'Alive.Ver' = integer(), 'Sleeper.Ver' = character(),
                     'Fed.Ver' = integer(), 'Unfed.Ver' = integer(),
                     'unf_live.Ver' = integer(), 'unf_dead.Ver' = integer(),
                     'bf_live.Ver' = integer(), 'bf_dead.Ver' = integer(),
                     'dead24.Ver' = interger(),
                     'Hut' = character(), 'marker' = integer())


for(i in 1:ld){
  #Make a small dataframe ('aux') for each data point
  aux <- dataCB_rm[dataCB_rm$marker == i,] 
  dead <- sum(aux[aux$Status == 'Dead',]$Tot.gamb.follow) + 
    sum(aux[aux$Status == 'Alive',]$Cum_72H)
  live <- sum(aux[aux$Status == 'Alive',]$Tot.gamb.follow) - 
    sum(aux[aux$Status == 'Alive',]$Cum_72H)
  fed <- sum(aux[aux$Physiology == 'Fed',]$Tot.gamb.follow) + 
    sum(aux[aux$Physiology == 'Partially-fed',]$Tot.gamb.follow) 
  unfed <- sum(aux[aux$Physiology == 'Unfed',]$Tot.gamb.follow) + 
    sum(aux[aux$Physiology == 'Gravid',]$Tot.gamb.follow)
  unf_live <- sum(aux[aux$Status == 'Alive' & aux$Physiology=='Unfed',]$Tot.gamb.follow) - 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Unfed',]$Cum_72H) + 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Gravid',]$Tot.gamb.follow) - 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Gravid',]$Cum_72H)
  unf_dead <- sum(aux[aux$Status == 'Dead' & aux$Physiology=='Unfed' ,]$Tot.gamb.follow) + 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Unfed',]$Cum_72H) + 
    sum(aux[aux$Status == 'Dead' & aux$Physiology=='Gravid' ,]$Tot.gamb.follow) + 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Gravid',]$Cum_72H) 
  bf_live <- sum(aux[aux$Status == 'Alive' & aux$Physiology=='Fed',]$Tot.gamb.follow) - 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Fed',]$Cum_72H) + 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Partially-fed',]$Tot.gamb.follow) - 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Partially-fed',]$Cum_72H) 
  bf_dead <- sum(aux[aux$Status == 'Dead' & aux$Physiology=='Fed' ,]$Tot.gamb.follow) + 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Fed',]$Cum_72H) + 
    sum(aux[aux$Status == 'Dead' & aux$Physiology=='Partially-fed' ,]$Tot.gamb.follow) + 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Partially-fed',]$Cum_72H) 
  dead24 <- sum(aux[aux$Status == 'Dead',]$Tot.gamb.follow)
  total <- sum(aux$Tot.gamb.follow)
  
  ##In the room
  dead.Room <- sum(aux[aux$Status == 'Dead' & aux$Area == 'Room',]$Tot.gamb.follow) + 
    sum(aux[aux$Status == 'Alive' & aux$Area == 'Room',]$Cum_72H)
  live.Room <- sum(aux[aux$Status == 'Alive' & aux$Area == 'Room',]$Tot.gamb.follow) - 
    sum(aux[aux$Status == 'Alive' & aux$Area == 'Room',]$Cum_72H)
  fed.Room <- sum(aux[aux$Physiology == 'Fed' & aux$Area == 'Room',]$Tot.gamb.follow) + 
    sum(aux[aux$Physiology == 'Partially-fed' & aux$Area == 'Room',]$Tot.gamb.follow) 
  unfed.Room <- sum(aux[aux$Physiology == 'Unfed' & aux$Area == 'Room',]$Tot.gamb.follow)+ 
    sum(aux[aux$Physiology == 'Gravid' & aux$Area == 'Room',]$Tot.gamb.follow)
  unf_live.Room <- sum(aux[aux$Status == 'Alive' & aux$Physiology=='Unfed' & aux$Area == 'Room',]$Tot.gamb.follow) - 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Unfed' & aux$Area == 'Room',]$Cum_72H)+ 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Gravid' & aux$Area == 'Room',]$Tot.gamb.follow) - 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Gravid' & aux$Area == 'Room',]$Cum_72H)
  unf_dead.Room <- sum(aux[aux$Status == 'Dead' & aux$Physiology=='Unfed' & aux$Area == 'Room',]$Tot.gamb.follow) + 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Unfed' & aux$Area == 'Room',]$Cum_72H) + 
    sum(aux[aux$Status == 'Dead' & aux$Physiology=='Gravid'  & aux$Area == 'Room',]$Tot.gamb.follow) + 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Gravid' & aux$Area == 'Room',]$Cum_72H) 
  bf_live.Room <- sum(aux[aux$Status == 'Alive' & aux$Physiology=='Fed' & aux$Area == 'Room',]$Tot.gamb.follow) - 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Fed' & aux$Area == 'Room',]$Cum_72H) + 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Partially-fed' & aux$Area == 'Room',]$Tot.gamb.follow) - 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Partially-fed' & aux$Area == 'Room',]$Cum_72H) 
  bf_dead.Room <- sum(aux[aux$Status == 'Dead' & aux$Physiology=='Fed' & aux$Area == 'Room',]$Tot.gamb.follow) + 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Fed' & aux$Area == 'Room',]$Cum_72H) + 
    sum(aux[aux$Status == 'Dead' & aux$Physiology=='Partially-fed' & aux$Area == 'Room',]$Tot.gamb.follow) + 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Partially-fed' & aux$Area == 'Room',]$Cum_72H)
  dead24.Room <- sum(aux[aux$Status == 'Dead'& aux$Area == 'Room',]$Tot.gamb.follow)
  total.Room <- sum(aux[aux$Area == 'Room',]$Tot.gamb.follow)
  
  ##In the Net
  dead.Net <- sum(aux[aux$Status == 'Dead' & aux$Area == 'Net',]$Tot.gamb.follow) + 
    sum(aux[aux$Status == 'Alive' & aux$Area == 'Net',]$Cum_72H)
  live.Net <- sum(aux[aux$Status == 'Alive' & aux$Area == 'Net',]$Tot.gamb.follow) - 
    sum(aux[aux$Status == 'Alive' & aux$Area == 'Net',]$Cum_72H)
  fed.Net <- sum(aux[aux$Physiology == 'Fed' & aux$Area == 'Net',]$Tot.gamb.follow) + 
    sum(aux[aux$Physiology == 'Partially-fed' & aux$Area == 'Net',]$Tot.gamb.follow) 
  unfed.Net <- sum(aux[aux$Physiology == 'Unfed' & aux$Area == 'Net',]$Tot.gamb.follow)+ 
    sum(aux[aux$Physiology == 'Gravid' & aux$Area == 'Net',]$Tot.gamb.follow)
  unf_live.Net <- sum(aux[aux$Status == 'Alive' & aux$Physiology=='Unfed' & aux$Area == 'Net',]$Tot.gamb.follow) - 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Unfed' & aux$Area == 'Net',]$Cum_72H)+ 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Gravid' & aux$Area == 'Net',]$Tot.gamb.follow) - 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Gravid' & aux$Area == 'Net',]$Cum_72H)
  unf_dead.Net <- sum(aux[aux$Status == 'Dead' & aux$Physiology=='Unfed' & aux$Area == 'Net',]$Tot.gamb.follow) + 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Unfed' & aux$Area == 'Net',]$Cum_72H)+ 
    sum(aux[aux$Status == 'Dead' & aux$Physiology=='Gravid'  & aux$Area == 'Net',]$Tot.gamb.follow) + 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Gravid' & aux$Area == 'Net',]$Cum_72H) 
  bf_live.Net <- sum(aux[aux$Status == 'Alive' & aux$Physiology=='Fed' & aux$Area == 'Net',]$Tot.gamb.follow) - 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Fed' & aux$Area == 'Net',]$Cum_72H) + 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Partially-fed' & aux$Area == 'Net',]$Tot.gamb.follow) - 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Partially-fed' & aux$Area == 'Net',]$Cum_72H) 
  bf_dead.Net <- sum(aux[aux$Status == 'Dead' & aux$Physiology=='Fed' & aux$Area == 'Net',]$Tot.gamb.follow) + 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Fed' & aux$Area == 'Net',]$Cum_72H) + 
    sum(aux[aux$Status == 'Dead' & aux$Physiology=='Partially-fed' & aux$Area == 'Net',]$Tot.gamb.follow) + 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Partially-fed' & aux$Area == 'Net',]$Cum_72H) 
  dead24.Net <- sum(aux[aux$Status == 'Dead'& aux$Area == 'Net',]$Tot.gamb.follow)
  total.Net <- sum(aux[aux$Area == 'Net',]$Tot.gamb.follow)
  
  ##In the Veranda
  dead.Ver <- sum(aux[aux$Status == 'Dead' & aux$Area == 'Veranda',]$Tot.gamb.follow) + 
    sum(aux[aux$Status == 'Alive' & aux$Area == 'Veranda',]$Cum_72H)
  live.Ver <- sum(aux[aux$Status == 'Alive' & aux$Area == 'Veranda',]$Tot.gamb.follow) - 
    sum(aux[aux$Status == 'Alive' & aux$Area == 'Veranda',]$Cum_72H)
  fed.Ver <- sum(aux[aux$Physiology == 'Fed' & aux$Area == 'Veranda',]$Tot.gamb.follow) + 
    sum(aux[aux$Physiology == 'Partially-fed' & aux$Area == 'Veranda',]$Tot.gamb.follow) 
  unfed.Ver <- sum(aux[aux$Physiology == 'Unfed' & aux$Area == 'Veranda',]$Tot.gamb.follow)+ 
    sum(aux[aux$Physiology == 'Gravid' & aux$Area == 'Veranda',]$Tot.gamb.follow)
  unf_live.Ver <- sum(aux[aux$Status == 'Alive' & aux$Physiology=='Unfed' & aux$Area == 'Veranda',]$Tot.gamb.follow) - 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Unfed' & aux$Area == 'Veranda',]$Cum_72H) + 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Gravid' & aux$Area == 'Veranda',]$Tot.gamb.follow) - 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Gravid' & aux$Area == 'Veranda',]$Cum_72H)
  unf_dead.Ver <- sum(aux[aux$Status == 'Dead' & aux$Physiology=='Unfed' & aux$Area == 'Veranda',]$Tot.gamb.follow) + 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Unfed' & aux$Area == 'Veranda',]$Cum_72H) + 
    sum(aux[aux$Status == 'Dead' & aux$Physiology=='Gravid'  & aux$Area == 'Veranda',]$Tot.gamb.follow) + 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Gravid' & aux$Area == 'Veranda',]$Cum_72H) 
  bf_live.Ver <- sum(aux[aux$Status == 'Alive' & aux$Physiology=='Fed' & aux$Area == 'Veranda',]$Tot.gamb.follow) - 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Fed' & aux$Area == 'Veranda',]$Cum_72H) + 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Partially-fed' & aux$Area == 'Veranda',]$Tot.gamb.follow) - 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Partially-fed' & aux$Area == 'Veranda',]$Cum_72H)
  bf_dead.Ver <- sum(aux[aux$Status == 'Dead' & aux$Physiology=='Fed' & aux$Area == 'Veranda',]$Tot.gamb.follow) + 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Fed' & aux$Area == 'Veranda',]$Cum_72H) + 
    sum(aux[aux$Status == 'Dead' & aux$Physiology=='Partially-fed' & aux$Area == 'Veranda',]$Tot.gamb.follow) + 
    sum(aux[aux$Status == 'Alive' & aux$Physiology=='Partially-fed' & aux$Area == 'Veranda',]$Cum_72H)
  dead24.Ver <- sum(aux[aux$Status == 'Dead'& aux$Area == 'Veranda',]$Tot.gamb.follow)
  total.Ver <- sum(aux[aux$Area == 'Veranda',]$Tot.gamb.follow)
  
  
  dff <- data.frame('Village'=aux$Village[1], 'Round' = aux$Round[1], 
                    'Treatment' = aux$Treatment[1],
                    'Nets' = aux$Nets[1], 'Date' = aux$Date[1], 
                    'Total'=total, 
                    'Dead.Tot' = dead, 'Alive.Tot' = live, 'Fed.Tot' = fed, 'Unfed.Tot' = unfed, 
                    'unf_live.Tot' = unf_live, 'unf_dead.Tot' = unf_dead,
                    'bf_live.Tot' = bf_live, 'bf_dead.Tot' = bf_dead, 
                    'dead24.Tot' = dead24,
                     'Total.Room'=total.Room,
                    'Dead.Room' = dead.Room, 'Alive.Room' = live.Room, 'Fed.Room' = fed.Room, 
                    'Unfed.Room' = unfed.Room, 'unf_live.Room' = unf_live.Room, 
                    'unf_dead.Room' = unf_dead.Room, 'bf_live.Room' = bf_live.Room, 
                    'bf_dead.Room' = bf_dead.Room, 
                    'dead24.Room' = dead24.Room,
                    'Total.Net'=total.Net,
                    'Dead.Net' = dead.Net, 'Alive.Net' = live.Net, 'Fed.Net' = fed.Net, 
                    'Unfed.Net' = unfed.Net, 'unf_live.Net' = unf_live.Net, 
                    'unf_dead.Net' = unf_dead.Net, 'bf_live.Net' = bf_live.Net, 
                    'bf_dead.Net' = bf_dead.Net, 
                    'dead24.Net' = dead24.Net,
                    'Total.Ver'=total.Ver,
                    'Dead.Ver' = dead.Ver, 'Alive.Ver' = live.Ver, 'Fed.Ver' = fed.Ver, 
                    'Unfed.Ver' = unfed.Ver, 'unf_live.Ver' = unf_live.Ver, 
                    'unf_dead.Ver' = unf_dead.Ver, 'bf_live.Ver' = bf_live.Ver, 
                    'bf_dead.Ver' = bf_dead.Ver,
                    'dead24.Ver' = dead24.Ver,
                    'Sleeper' = aux$Sleeper[1],
                    'Hut' = aux$Hut[1], 'marker' = i) 
  dfW_rm <- rbind(dfW_rm,dff)
}
View(dfW_rm)
dim(dfW_rm)
###1080  44
head(dfW_rm)



#Tidy data for exporting
dfWz <- dfW_new
dfWz$Hut2 <- as.numeric(substr(dfW_new$Hut, 5,5))
dfWz$Sleeper2 <- as.numeric(substr(dfW_new$Sleeper, 5,5))
colnames(dfWz)[colnames(dfWz) == 'Dead'] <- 'tot_dead'
dfWz2 <- dplyr::select(dfWz, Village, Round, Treatment,Date,Total,tot_dead,unf_live,unf_dead,bf_live,bf_dead,Sleeper2,Hut2)
colnames(dfWz2)[colnames(dfWz2) == 'Sleeper2'] <- 'sleeper'
colnames(dfWz2)[colnames(dfWz2) == 'Hut2'] <- 'hut'
colnames(dfWz2)[colnames(dfWz2) == 'Total'] <- 'total'
colnames(dfWz2)[colnames(dfWz2) == 'Treatment'] <- 'treatment'

saveRDS(dfWz2, 'cleaned_EHT_data_four_trials.rds')









