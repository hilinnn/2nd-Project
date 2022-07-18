source("~/Desktop/Second project/R/0610 loop.R")
library(lme4)
library(tidyverse)
library(DHARMa)
library(lubridate)
library(MASS)
library(merTools)
library(ggpubr)
library(ggpmisc)
library(fitdistrplus)
library(broom)


####################################################################
#### Models for the mortality of mosquitoes by location  ###########
####################################################################


###Adding individual random effect to model 1
View(Tengrela_R1A_rm)

m_mos_df <- as_tibble(Tengrela_R1A_rm)%>% 
  mutate(Room = dead24.Room/Total.Room,
         Net = dead24.Net/Total.Net,
         Veranda = dead24.Ver/Total.Ver)%>%
  gather("Location", "Mortality24", c("Room", "Net", "Veranda"))%>%
  dplyr::select(c("Location","Mortality24","Treatment","Date"))


m_mos <- as_tibble(Tengrela_R1A_rm)%>% 
  mutate(Total.Mor = Dead.Tot/Total,
         Room = Dead.Room/Total.Room,
         Net = Dead.Net/Total.Net,
         Veranda = Dead.Ver/Total.Ver)%>%
  gather("Location", "Mortality", c("Room", "Net", "Veranda"))%>%
  dplyr::select(starts_with(c("Village","Date","Treatment","Location", "Mortality",
                              "Total","Dead","dead","Hut","Sleeper","marker"))) %>%
  mutate(WashedStatus = case_when( Treatment == "IG2.unwash" ~ "unwashed",
                                   Treatment == "P3.unwash" ~ "unwashed",
                                   Treatment == "P2.unwash" ~ "unwashed",
                                   Treatment == "IG2.wash" ~ "washed",
                                   Treatment == "P3.wash" ~ "washed",
                                   Treatment == "UTN" ~ "UTN"),
         Insecticide = case_when(Treatment == "IG2.unwash" ~ "alpha cypermethrin",
                                 Treatment == "P3.unwash" ~ "deltamethrin",
                                 Treatment == "P2.unwash" ~ "deltamethrin",
                                 Treatment == "IG2.wash" ~ "alpha cypermethrin",
                                 Treatment == "P3.wash" ~ "deltamethrin",
                                 Treatment == "UTN" ~ "UTN"),
         Week = ceiling((yday(Date) - min(yday(Date))+1)/7),
         Nets = case_when(Treatment == "IG2.unwash" ~ "IG2",
                          Treatment == "P3.unwash" ~ "P3",
                          Treatment == "P2.unwash" ~ "P2",
                          Treatment == "IG2.wash" ~ "IG2",
                          Treatment == "P3.wash" ~ "P3",
                          Treatment == "UTN" ~ "UTN"))

m_mos$Location <- as.factor(m_mos$Location)
m_mos$Date <- as.Date(m_mos$Date)


head(m_mos)
View(m_mos) 


###Generate the data used in the model fitting
mor_reg <- gen_mo(m_mos)
mor_reg$WashedStatus <- as.factor(mor_reg$WashedStatus)
mor_reg$Insecticide <- as.factor(mor_reg$Insecticide)
mor_reg$Nets <- as.factor(mor_reg$Nets)
mor_reg$Location <- as.factor(mor_reg$Location)


View(mor_reg)

mor_reg$WashedStatus <- relevel(mor_reg$WashedStatus, ref = "UTN")
mor_reg$Nets <- relevel(mor_reg$Nets, ref = "UTN")
mor_reg$Treatment <- relevel(mor_reg$Treatment, ref = "UTN")




###Plot the data to find the appropriate distribution for the data
ggplot(m_mos, aes(x=Mortality, fill = Treatment))+
  geom_histogram(binwidth = 0.1, boundary = 0, alpha=0.5)+
  geom_freqpoly(aes(color = Treatment), binwidth = 0.1, boundary = 0)+
  facet_wrap(vars(Treatment))+
  labs(title  = "Histogram and line graph of mortality of mosquitoes by Treatment")+
  theme(plot.title = element_text(size=13, hjust = 0.5))+
  scale_x_continuous(limits = c(0,1))

ggsave("Histogram and line graph of mortality of mosquitoes by Treatment.jpeg", device = jpeg,
       width = 8, height = 5.5)

ggplot(m_mos, aes(x=Mortality, after_stat(density)))+
  geom_freqpoly(aes(color = Location),binwidth = 0.1)+
  facet_wrap(vars(Treatment))+
  labs(title = "Frequency line graph of mortality of mosquitoes by Location and Treatment")+
  theme(plot.title = element_text(size=13, hjust = 0.5))+
  scale_x_continuous(limits = c(0,1))

ggsave("Frequency line graph of mortality of mosquitoes by Location and Treatment.jpeg", 
       device = jpeg, width = 8, height = 5.5)

####Dotplot of the mortality

ggplot(m_mos, aes(y = Mortality, x = Date, color = Nets))+
  geom_point()+
  facet_grid(vars(Location), vars(Treatment))+
  scale_x_date(date_breaks = "1 week", date_labels = "%W",
               date_minor_breaks = "1 day")+
  labs(title = "Daily mortality by location and treatment",
       x = "Week")+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Dotplot of daily mortality by location and treatment.jpeg", 
       device = jpeg, width = 8, height = 5.5)

ggplot(m_mos, aes(y = Mortality, x=Location, color = Location,  fill = Location))+
  geom_boxplot(alpha=0.5)+
  stat_summary(fun = mean, geom="point", shape=18, size=6, alpha = 0.7, color = "red") +
  facet_wrap(vars(Treatment))+
  labs(title = "Mortality by location and treatment")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal_hgrid()

ggsave("Mortality by location and treatment.jpeg", 
       device = jpeg, width = 8, height = 5.5)


####Plot mortality against the number of mosquitoes to observe the relationship
ggplot(mor_reg, aes(x = Total, y = Dead, color = Nets, fill = Nets))+
  geom_point()+
  facet_grid(vars(Location), vars(Treatment))+
  labs(title = "Mosquito status correponding to number of mosquitoes",
       x = "Number of mosquitoes")+
  scale_y_continuous( breaks = c(0,1))+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Mosquito status correponding to number of mosquito.jpeg", device = jpeg,
       height = 5, width = 7.5)


####Plot mortality against the number of mosquitoes to observe the relationship
m_mos_df <- as.tibble(Tengrela_R1A_rm) %>%
  gather("Loc", "Total.Loc", c("Total.Room","Total.Net","Total.Ver")) %>%
  mutate(Loc.Tot = case_when(Loc == "Total.Room" ~ "Room",
                             Loc == "Total.Net" ~ "Net",
                             Loc == "Total.Ver" ~ "Veranda"))%>%
  dplyr::select(c("Village", "Date", "Treatment", "Loc.Tot", "Total.Loc",
           "Hut", "Sleeper", "marker"))

m_mos_df <- left_join(m_mos, m_mos_df, by = c("Village", "Date", "Treatment", "Hut", "Sleeper", "marker", 
                                              "Location" ="Loc.Tot"))

View(m_mos_df)

ggplot(m_mos_df, aes(x = Total.Loc, y = Mortality, color = Nets, fill = Nets))+
  geom_point()+
  facet_grid(vars(Location), vars(Treatment))+
  labs(title = "Mortality correponding to number of mosquitoes",
       x = "Number of mosquitoes", y = "Mortality")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

ggsave("1.jpeg", device = jpeg)


m_f_mos <- m_mos %>%
  dplyr::select(c("Village", "Date", "Location", "Treatment","WashedStatus", "Mortality", "marker", "Hut", "Sleeper", "Nets")) %>%
  left_join(bf_mos, by = c("Village", "Date", "Location", "Treatment","WashedStatus", "marker", "Hut", "Sleeper", "Nets"))
  
ggplot(m_f_mos, aes(y=Mortality, x=Bloodfed, color = Nets)) +
  geom_point()+
  facet_grid( vars(Location), vars(Treatment))+
  theme_bw()+
  labs(title = "Mortality rate against blood-feeding rate")+
  scale_x_continuous(labels = c(0,0.25,0.5,0.75,1))+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 11))

ggsave("Mortality rate against blood-feeding rate.jpeg", device = "jpeg")

ggplot(mor_fed, aes(y=Dead, x=Fed, color = Nets)) +
  geom_point()+
  facet_grid( vars(Location), vars(Treatment))+
  geom_smooth(formula = y~x, method = "glm",method.args= list(family="binomial"))
  
####Run logistic regression for the mortality
###Fixed effect model
prop.test()

####Model 0
mor_0 <- glm(Dead~1, data = mor_reg, family = binomial("log"))
summary(mor_0)
##AIC: 4819.5

mor_0_ran <- glmer(Mortality~(1|marker), 
                   data = m_mos, family = binomial("logit"))
summary(mor_0_ran)ww
##AIC: 560.5

mor_0_ran_id <- glmer(Mortality~(1|id), 
                      data = m_mos, family = binomial("logit"))
##AIC:560.8
##Singular fit


####Model 1: Location 
mor_1_1 <- glm(Dead~Location, data = mor_reg, family = binomial("logit"))
summary(mor_1_1)
##AIC: 4627.9


####Model 2: WashedStatus 
mor_1_2 <- glm(Dead~WashedStatus, data = mor_reg, family = binomial("logit"))
summary(mor_1_2)
##AIC: 4685.3


####Model 3: Treatment 
mor_1_3 <- glm(Dead~Treatment, data = mor_reg, family = binomial("logit"))
summary(mor_1_3)
##AIC: 4410.3

####Model 4: Insecticide 
mor_1_4 <- glm(Dead~Insecticide, data = mor_reg, family = binomial("logit"))
summary(mor_1_4)
##AIC: 4689.4

####Model 5: Nets 
mor_1_5 <- glm(Dead~Nets, data = mor_reg, family = binomial("logit"))
summary(mor_1_5)
##AIC: 4689.6





###Variables: Location and others 
mor_2_1 <- glm(Dead~Location+Treatment, 
             data = mor_reg, family = binomial("logit"))
summary(mor_2_1)
##AIC: 4251


mor_2_2 <- glm(Dead~Location+WashedStatus, 
               data = mor_reg, family = binomial("logit"))
summary(mor_2_2)
##AIC: 4495.3


mor_2_3 <- glm(Dead~Location+Insecticide,
               data = mor_reg, family = binomial("logit"))
summary(mor_2_3)
##AIC: 4500.7


mor_2_4 <- glm(Dead~Location+Nets, 
               data = mor_reg, family = binomial("logit"))
summary(mor_2_4)
##AIC: 4501.1


###Variables: Location and others with interactions
mor_3_1 <- glm(Dead~Location+Treatment + Location*Treatment, 
               data = mor_reg, family = binomial("logit"))
summary(mor_3_1)
##AIC: 4225.8

mor_3_2 <- glm(Dead~Location+WashedStatus + Location*WashedStatus, 
               data = mor_reg, family = binomial("logit"))
summary(mor_3_2)
##AIC: 4469

mor_3_3 <- glm(Dead~Location+Insecticide + Location*Insecticide, 
               data = mor_reg, family = binomial("logit"))
summary(mor_3_3)
##AIC: 4485.4

mor_3_4 <- glm(Dead~Location+Nets + Location*Nets, 
               data = mor_reg, family = binomial("logit"))
summary(mor_3_4)
##AIC: 4488.4


### 3 Variables: Location and WashedStatus and Nets
mor_4_1 <- glm(Dead~Location + WashedStatus + Nets, 
               data = mor_reg, family = binomial("logit"))
summary(mor_4_1)
##AIC: 4460.6

mor_4_1_ran <- glmer(Dead~Location + WashedStatus + Nets + (1|marker), 
               data = mor_reg, family = binomial("logit"))
summary(mor_4_1_ran)
##AIC: 4043.9
##Variance of the random effect: 1.279 

mor_4_1_Hut <- glmer(Dead~Location + WashedStatus + Nets + (1|Hut), 
                     data = mor_reg, family = binomial("logit"))
summary(mor_4_1_Hut)
##AIC: 4413.4
##Variance of the random effect: 0.08696 

mor_4_1_Mix <- glmer(Dead~Location + WashedStatus + Nets + (1|Hut)+ (1|marker), 
                     data = mor_reg, family = binomial("logit"))
summary(mor_4_1_Mix)
##AIC: 4045.2
##Variance of the observational random effect: 1.2453
##Variance of the Hut random effect: 0.0285 


mor_4_2 <- glm(Dead~Location + WashedStatus + Nets + WashedStatus * Nets , 
               data = mor_reg, family = binomial("logit"))
summary(mor_4_2)
##AIC: 4251


mor_4_2_ran <- glmer(Dead~Location + WashedStatus + Nets + WashedStatus * Nets + (1|marker), 
               data = mor_reg, family = binomial("logit"))
mor_4_2_ran <- update(mor_4_2_ran, control = glmerControl(optimizer = "bobyqa"))
summary(mor_4_2_ran)
##AIC: 3995.1
##Variance of the random effect: 0.8805 


mor_4_2_hut <- glmer(Dead~Location + WashedStatus + Nets + WashedStatus * Nets + (1|Hut), 
                     data = mor_reg, family = binomial("logit"))

summary(mor_4_2_hut)
##AIC: 4210.8
##Variance of the random effect: 0.08135 

mor_4_2_Mix <- glmer(Dead~Location + WashedStatus + Nets + WashedStatus * Nets 
                     + (1|Hut) + (1|marker), 
                     data = mor_reg, family = binomial("logit"))
mor_4_2_Mix <- update(mor_4_2_Mix, control = glmerControl(optimizer = "bobyqa"))

summary(mor_4_2_Mix)

write.csv(tidy(mor_4_2_Mix), "Mortality 3 var best model.csv")
##AIC: 3995.3
##Variance of the observational random effect: 0.83637
##Variance of the Hut random effect: 0.03794 

###Predict the model outcome
predict.glm()


###Mixed effect model

################################################################################
############Random effect: individual random effect (observational)#############
################################################################################

####Model 1: Location 
mor_1_1_ran <- glmer(Dead~Location  + (1|marker), 
                   data = mor_reg, family = binomial("logit"))
summary(mor_1_1_ran)
##AIC: 4067.5
##Variance of the random effect: 1.596 


####Model 2: WashedStatus 
mor_1_2_ran <- glmer(Dead~WashedStatus+ (1|marker), 
                     data = mor_reg, family = binomial("logit"))
summary(mor_1_2_ran)
##AIC: 4145.6
##Variance of the random effect: 1.474 


####Model 3: Treatment 
mor_1_3_ran <- glmer(Dead~Treatment + (1|marker), 
               data = mor_reg, family = binomial("logit"))
summary(mor_1_3_ran)
##AIC: 4093.1
##Variance of the random effect: 0.9805 

####Model 4: Insecticide 
mor_1_4_ran <- glmer(Dead~Insecticide + (1|marker), 
                   data = mor_reg, family = binomial("logit"))
summary(mor_1_4_ran)
##AIC: 4147.7
##Variance of the random effect: 1.494 

####Model 5: Nets 
mor_1_5_ran <- glmer(Dead~Nets + (1|marker), 
                   data = mor_reg, family = binomial("logit"))
summary(mor_1_5_ran)
##AIC: 4147.3
##Variance of the random effect: 1.485 


################################################################################
####################Random effect: individual random effect (Hut)###############
################################################################################

####Model 1: Location 
mor_1_1_Hut <- glmer(Dead~Location  + (1|Hut), 
                     data = mor_reg, family = binomial("logit"))
summary(mor_1_1_Hut)
##AIC: 4571.3
##Variance of the random effect: 0.09573 


####Model 2: WashedStatus 
mor_1_2_Hut <- glmer(Dead~WashedStatus+ (1|Hut), 
                     data = mor_reg, family = binomial("logit"))
summary(mor_1_2_Hut)
##AIC: 4623.9
##Variance of the random effect: 0.102 


####Model 3: Treatment 
mor_1_3_Hut <- glmer(Dead~Treatment + (1|Hut), 
                     data = mor_reg, family = binomial("logit"))
summary(mor_1_3_Hut)
##AIC: 4366.4
##Variance of the random effect: 0.08424 

####Model 4: Insecticide 
mor_1_4_Hut <- glmer(Dead~Insecticide + (1|Hut), 
                     data = mor_reg, family = binomial("logit"))
summary(mor_1_4_Hut)
##AIC: 4631.8
##Variance of the random effect: 0.09726 

####Model 5: Nets 
mor_1_5_Hut <- glmer(Dead~Nets + (1|Hut), 
                     data = mor_reg, family = binomial("logit"))
summary(mor_1_5_Hut)
##AIC: 4631.1
##Variance of the random effect: 0.09869 



################################################################################
################Random effect: individual random effect (Sleeper)###############
################################################################################

####Model 1: Location 
mor_1_1_Sleeper <- glmer(Dead~Location  + (1|Sleeper), 
                     data = mor_reg, family = binomial("logit"))
summary(mor_1_1_Sleeper)
##AIC: 4610.3
##Variance of the random effect: 0.04278 


####Model 2: WashedStatus 
mor_1_2_Sleeper <- glmer(Dead~WashedStatus+ (1|Sleeper), 
                     data = mor_reg, family = binomial("logit"))
summary(mor_1_2_Sleeper)
##AIC: 4654.7
##Variance of the random effect: 0.06182 


####Model 3: Treatment 
mor_1_3_Sleeper <- glmer(Dead~Treatment + (1|Sleeper), 
                     data = mor_reg, family = binomial("logit"))
summary(mor_1_3_Sleeper)
##AIC: 4372.0
##Variance of the random effect: 0.08367 

####Model 4: Insecticide 
mor_1_4_Sleeper <- glmer(Dead~Insecticide + (1|Sleeper), 
                     data = mor_reg, family = binomial("logit"))
summary(mor_1_4_Sleeper)
##AIC: 4645.5
##Variance of the random effect: 0.0835 

####Model 5: Nets 
mor_1_5_Sleeper <- glmer(Dead~Nets + (1|Sleeper), 
                     data = mor_reg, family = binomial("logit"))
summary(mor_1_5_Sleeper)
##AIC: 4646.5
##Variance of the random effect: 0.08241 




###Variables: Location and others with observational random effect
mor_2_1_ran <- glmer(Dead~Location+Treatment + (1|marker), 
               data = mor_reg, family = binomial("logit"))
summary(mor_2_1_ran)
##AIC: 3995.1
##Variance of the random effect: 0.8808 

mor_2_2_ran <- glmer(Dead~Location+WashedStatus + (1|marker), 
                     data = mor_reg, family = binomial("logit"))
summary(mor_2_2_ran)
##AIC: 4047.5
##Variance of the random effect: 1.344 

mor_2_3_ran <- glmer(Dead~Location+Insecticide + (1|marker), 
                     data = mor_reg, family = binomial("logit"))
summary(mor_2_3_ran)
##AIC: 4050.3
##Variance of the random effect: 1.37 

mor_2_4_ran <- glmer(Dead~Location+Nets + (1|marker), 
                     data = mor_reg, family = binomial("logit"))
summary(mor_2_4_ran)
##AIC: 4050.4
##Variance of the random effect: 1.364 



###Variables: Location and others with interaction and observational random effect
mor_3_1_ran <- glmer(Dead~Location+Treatment + Location*Treatment +(1|marker), 
                     data = mor_reg, family = binomial("logit"))

mor_3_1_ran <- update(mor_3_1_ran, control = glmerControl(optimizer = "bobyqa"))

summary(mor_3_1_ran)
##AIC: 3979.8
##Variance of the random effect: 0.9021

mor_3_1_mix <- glmer(Dead~Location+Treatment + Location*Treatment +(1|marker) + (1|Hut), 
                     data = mor_reg, family = binomial("logit"))
mor_3_1_mix <- update(mor_3_1_mix, control = glmerControl(optimizer = "bobyqa"))

summary(mor_3_1_mix)
##AIC: 3980.2
##Variance of the random effect (Observational): 0.86114
##Variance of the random effect (Hut): 0.03548
write.csv(tidy(mor_3_1_mix), "Mortality 2 var best model fit.csv")



mor_3_2_ran <- glmer(Dead~Location+WashedStatus+Location*WashedStatus + (1|marker), 
                     data = mor_reg, family = binomial("logit"))
mor_3_2_ran <- update(mor_3_2_ran, control = glmerControl(optimizer = "bobyqa"))

summary(mor_3_2_ran)
##AIC: 4035.2
##Variance of the random effect: 1.348 

mor_3_2_mix <- glmer(Dead~Location+WashedStatus + Location*WashedStatus +(1|marker) + (1|Hut), 
                     data = mor_reg, family = binomial("logit"))
mor_3_2_mix <- update(mor_3_2_mix, control = glmerControl(optimizer = "bobyqa"))

summary(mor_3_2_mix)
##AIC: 4036.7
##Variance of the random effect (Observational): 1.31807
##Variance of the random effect (Hut): 0.02477


mor_3_3_ran <- glmer(Dead~Location+Insecticide + Location*Insecticide +(1|marker), 
                     data = mor_reg, family = binomial("logit"))
mor_3_3_ran <- update(mor_3_3_ran, control = glmerControl(optimizer = "bobyqa"))

summary(mor_3_3_ran)
##AIC: 4035.9
##Variance of the random effect: 1.378 


mor_3_3_mix <- glmer(Dead~Location+Insecticide + Location*Insecticide +(1|marker) + (1|Hut), 
                     data = mor_reg, family = binomial("logit"))
mor_3_3_mix <- update(mor_3_3_mix, control = glmerControl(optimizer = "bobyqa"))

summary(mor_3_3_mix)
##AIC: 4037.3
##Variance of the random effect (Observational): 1.31807
##Variance of the random effect (Hut): 0.02477


mor_3_4_ran <- glmer(Dead~Location+Nets + Location*Nets +(1|marker), 
                     data = mor_reg, family = binomial("logit"))
mor_3_4_ran <- update(mor_3_4_ran, control = glmerControl(optimizer = "bobyqa"))

summary(mor_3_4_ran)
##AIC: 4038.6
##Variance of the random effect: 1.378 

mor_3_4_mix <- glmer(Dead~Location+Nets + Location*Nets +(1|marker) + (1|Hut), 
                     data = mor_reg, family = binomial("logit"))
mor_3_4_mix <- update(mor_3_4_mix, control = glmerControl(optimizer = "bobyqa"))

summary(mor_3_4_mix)
##AIC: 4040.1
##Variance of the random effect (Observational): 1.34632
##Variance of the random effect (Hut): 0.02637



############################################################
####################Do not run##############################
############################################################
# 
# mor_2_ran <- glmer(Mortality~Location+Treatment +  Location*Treatment + (1|marker), 
#                    data = m_mos, family = binomial("logit"))
# summary(mor_2_ran)
# ##AIC: 501.7
# ##Variance of the random effect: 0.0004968 (small)
# 
# ##Additional warnings: failed to converge
# 
# ##Tried to solve the warnings but unsuccessful
# m_mos <- m_mos %>%
#   mutate(id = seq(1, nrow(m_mos),1))
# View(m_mos)
# 
# 
# 
# model_1_rand <- glmer.nb(formula = Count~Location+(1|id), data = n_mos)
# summary(model_1_rand)
# ###AIC: 3659.1
# plot(model_1_rand)
# 
# ###Plot the residuals
# plot(model_1_rand_resid)
# model_1_rand_resid <- simulateResiduals(fittedModel = model_1_rand, n = nrow(n_mos)*5, refit = TRUE)
# plot(model_1_rand_resid)
# testDispersion(model_1_rand_resid)
# 
# rs_1 <- simulateResiduals(fittedModel = model_1)
# a <- testOutliers(rs_1, type = 'bootstrap')
# plot(a)
# glm.diag.plots(rs_1)
# 
# 
# sum(n_mos$Count == 0)


##########################################################################################
##########################Log odds ratio plot for the best models#########################
##########################################################################################

#####Best mortality model: mor_3_1_mix
mor_reg$marker <- as.factor(mor_reg$marker)
levels(mor_reg$marker)


mor_pred_conf_int <- plot_mor(model = mor_3_1_mix, data = mor_reg)
View(mor_pred_conf_int)

ggplot(m_mos, aes(y = Mortality, x=Treatment, color = Nets))+
  geom_boxplot(aes(y = Mortality, x=Treatment))+
  stat_summary(aes(fill = Nets), fun = mean, geom="point", shape=16, size=6, alpha = 0.7) +
  facet_wrap(vars(Location), ncol = 1)+
  labs(title = "Mortality by location and treatment")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  geom_errorbar(mor_pred_conf_int, aes(y = mean, x = Treatment, ymin = lwr, ymax = upr))

ggplot(mor_pred_conf_int, aes(x = Treatment,colour = Nets))+
  geom_point(aes(y = mean))+
  geom_errorbar(aes(y = mean, ymin = lwr, ymax = upr))+
  facet_grid(Location~.)+
  geom_point(aes(y=True.mean), size = 6, shape = 23, alpha = 0.5, color = "red",  fill = "red")+
  labs(title = "Best fitted model predicted mortality", y = "Mortality")+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Best fitted model predicted mortality.jpeg", device = "jpeg",
       height = 6, width = 7)





####Additional variable: total mosquitoes caught in the hut

####Use the number of mosquitoes in each location first
  
View(m_tot_mos)


mor_t_0 <- glm(Dead ~ Total, data = mor_reg, family = binomial("logit"))
summary(mor_t_0)
###AIC:4821.5

mor_t_1 <- glm(Dead ~ Location + Treatment + Location * Treatment + Total, 
               data = mor_reg, family = binomial("logit"))
summary(mor_t_1)
###AIC:4224.9

mor_t_2 <- glm(Dead ~  Location + Treatment + Location * Treatment + Total * Treatment, 
             data = mor_reg, family = binomial("logit"))
summary(mor_t_2)
###AIC:4225.1


mor_t_3 <- glm(Dead ~  Location + Treatment + Location * Treatment + Total * Location, 
               data = mor_reg, family = binomial("logit"))
summary(mor_t_3)
###AIC:4224.4




mor_tot_obs <- glmer(Dead ~  Location + Treatment + Location * Treatment + Total * Location + (1|marker), 
                     data = mor_reg, family = binomial("logit"))
ss <- getME(mor_tot_obs,c("theta","fixef"))
mor_tot_obs <- update(mor_tot_obs,start=ss,control=glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e5)))
summary(mor_tot_obs)
###AIC:3982.5
###Var:0.904


mor_tot_Hut <- glmer(Dead ~  Location + Treatment + Location * Treatment + Total * Location  + (1|Hut), 
                     data = mor_reg, family = binomial("logit"))
ss <- getME(mor_tot_Hut,c("theta","fixef"))
mor_tot_Hut <- update(mor_tot_Hut,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                optCtrl=list(maxfun=4e5)))
summary(mor_tot_Hut)

###AIC:4187.9
###Var:0.07748


mor_tot_Slp <- glmer(Dead ~  Location + Treatment + Location * Treatment + Total * Location  + (1|Sleeper), 
                     data = mor_reg, family = binomial("logit"))
ss <- getME(mor_tot_Slp,c("theta","fixef"))
mor_tot_Slp <- update(mor_tot_Slp,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                optCtrl=list(maxfun=4e5)))
summary(mor_tot_Slp)
###AIC:4191.8
###Var:0.08221


mor_tot_Rand <- glmer(Dead ~  Location + Treatment + Location * Treatment + Total * Location  
                      + (1|marker)+ (1|Hut) + (1|Sleeper), 
                      data = mor_reg, family = binomial("logit"))
ss <- getME(mor_tot_Rand,c("theta","fixef"))
mor_tot_Rand <- update(mor_tot_Rand,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                optCtrl=list(maxfun=5e5)))
summary(mor_tot_Rand)
###AIC:3980.3
###Var:0.79211(observational) 0.04664 (Hut) 0.07460 (Sleeper)



###With Location
View(m_num_mos)


mor_num <- glm(Dead ~ Total.Loc, data = mor_reg, family = binomial("logit"))
summary(mor_num)
###AIC:4817.8

mor_num_0 <- glm(Dead ~ Location + Treatment + Location * Treatment + Total.Loc, 
                 data = mor_reg,  family = binomial("logit"))
summary(mor_num_0)
###AIC:4220.3


mor_num_1 <- glm(Dead ~ Location + Treatment + Location * Treatment + Total.Loc + Treatment * Total.Loc, 
                 data = mor_reg,  family = binomial("logit"))
summary(mor_num_1)
###AIC:4220.1


mor_num_obs <- glmer(Dead ~ Location + Treatment + Location * Treatment + Total.Loc + Treatment * Total.Loc + (1|marker), 
                     data = mor_reg,  family = binomial("logit"))
ss <- getME(mor_num_obs,c("theta","fixef"))
mor_num_obs <- update(mor_num_obs,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                  optCtrl=list(maxfun=5e5)))

summary(mor_num_obs)
###AIC:3985.3
###Var:0.9126



mor_num_Hut <- glmer(Dead ~ Location + Treatment + Location * Treatment + Total.Loc + Treatment * Total.Loc + (1|Hut), 
                     data = mor_reg,  family = binomial("logit"))
mor_num_Hut <- update(mor_num_Hut, control = glmerControl(optimizer = "bobyqa"))

ss <- getME(mor_num_Hut,c("theta","fixef"))
mor_num_Hut <- update(mor_num_Hut,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                optCtrl=list(maxfun=5e5)))

summary(mor_num_Hut)
###AIC:4190.6
###Var:0.06789


mor_num_Slp <- glmer(Dead ~ Location + Treatment + Location * Treatment + Total.Loc + Treatment * Total.Loc + (1|Sleeper), 
                     data = mor_reg,  family = binomial("logit") )
ss <- getME(mor_num_Slp,c("theta","fixef"))
mor_num_Slp <- update(mor_num_Slp,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                optCtrl=list(maxfun=5e5)))

summary(mor_num_Slp)
###AIC:4184.4
###Var:0.08792


mor_num_mS <- glmer(Dead ~ Location + Treatment + Location * Treatment + Total.Loc + Treatment * Total.Loc 
                    + (1|marker) + (1|Sleeper), 
                      data = mor_reg,  family = binomial("logit"))
ss <- getME(mor_num_mS,c("theta","fixef"))
mor_num_mS <- update(mor_num_mS,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                optCtrl=list(maxfun=5e5)))

summary(mor_num_mS)
###AIC:3982.8
###Var:0.84137(observational) 0.07822(Hut)




mor_num_Rand <- glmer(Dead ~ Location + Treatment + Location * Treatment + Total.Loc + Treatment * Total.Loc 
                      + (1|marker) + (1|Hut) + (1|Sleeper), 
                      data = mor_reg,  family = binomial("logit"))

ss <- getME(mor_num_Rand,c("theta","fixef"))
mor_num_Rand <- update(mor_num_Rand,start=ss,control=glmerControl(optimizer="bobyqa",
                                                              optCtrl=list(maxfun=5e5)))
summary(mor_num_Rand)
###AIC:3983.2
###Var:0.80256(observational) 0.08294(Sleeper) 0.03722(Hut)




####Additional variable: Blood-feeding
####Test the relationship between the mortality and blood-feeding based on the best model: mor_3_1_mix

###Sanity check
sum(mor_reg[,c(1:9,11)] != bf_reg[,1:10])

mor_fed <- cbind(mor_reg, bf_reg[,"Fed"])
View(mor_fed)
colnames(mor_fed)[14] <- "Fed"


mor_fed$Treatment <- relevel(as.factor(mor_fed$Treatment), ref = "UTN")
mor_fed$Location <- relevel(as.factor(mor_fed$Location), ref = "Net")

mor_f <- glm(Dead~Fed,data = mor_fed, family = binomial("logit"))
summary(mor_f)
#AIC: 4785.8


mor_fed_0 <- glm(Dead ~ Fed + Location + Treatment + Location * Treatment,  
                  data = mor_fed, family = binomial("logit"))
summary(mor_fed_0)
#AIC: 4207.3

mor_fed_obs <- glmer(Dead ~ Fed + Location + Treatment + Location * Treatment + 
                     (1 | marker), data = mor_fed, family = binomial("logit"))
mor_fed_obs <- update(mor_fed_obs, control = glmerControl(optimizer = "bobyqa"))

summary(mor_fed_obs)
#AIC: 3829.1
#Var: 1.68

mor_fed_hut <- glmer(Dead ~ Fed + Location + Treatment + Location * Treatment + 
                       (1 | Hut), data = mor_fed, family = binomial("logit"))
mor_fed_hut <- update(mor_fed_hut, control = glmerControl(optimizer = "bobyqa"))

summary(mor_fed_hut)
#AIC: 4160.5
#Var: 0.09682


mor_fed_Slp <- glmer(Dead ~ Fed + Location + Treatment + Location * Treatment + 
                       (1 | Sleeper), data = mor_fed, family = binomial("logit"))

summary(mor_fed_Slp)
#AIC: 4159.9
#Var: 0.1134

mor_fed_obs_Slp <- glmer(Dead ~ Fed + Location + Treatment + Location * Treatment + 
                           (1 | marker)+ (1 | Sleeper), data = mor_fed, family = binomial("logit"))
mor_fed_obs_Slp <- update(mor_fed_obs_Slp, control = glmerControl(optimizer = "bobyqa"))

summary(mor_fed_obs_Slp)
#AIC: 3823.0
#Var: 1.5246 obs 0.1814 sleeper


mor_fed_obs_Hut <- glmer(Dead ~ Fed + Location + Treatment + Location * Treatment + 
                           (1 | marker)+ (1 | Hut), data = mor_fed, family = binomial("logit"))
mor_fed_obs_Hut <- update(mor_fed_obs_Hut, control = glmerControl(optimizer = "bobyqa"))

summary(mor_fed_obs_Hut)
#AIC: 3829.9
#Var: 1.62025 obs 0.05051 Hut


mor_fed_Hut_Slp <- glmer(Dead ~ Fed + Location + Treatment + Location * Treatment + 
                           (1 | Sleeper)+ (1 | Hut), data = mor_fed, family = binomial("logit"))
mor_fed_Hut_Slp <- update(mor_fed_Hut_Slp, control = glmerControl(optimizer = "bobyqa"))

summary(mor_fed_Hut_Slp)
#AIC: 4110.2
#Var: 0.1291 Sleeper 0.1120 Hut



mor_fed_rand <- glmer(Dead ~ Fed + Location + Treatment + Location * Treatment + (1 | marker) + 
                           (1 | Sleeper)+ (1 | Hut), data = mor_fed, family = binomial("logit"))
mor_fed_rand <- update(mor_fed_rand, control = glmerControl(optimizer = "bobyqa"))

summary(mor_fed_rand)
#AIC: 3822.8
#Var: 1.45140 obs 0.19054 Sleeper 0.06855 Hut

write.csv(tidy(mor_fed_rand), "Mortality n blood-fed best model.csv")



######Aggregate both blood feeding and number of mosquitoes by location into the model
mor_fed_num_ms <- glmer(Dead~Fed + Location + Treatment + Location * Treatment + Total.Loc + Total.Loc*Treatment +
                          (1 | marker)+ (1 | Sleeper), data = mor_fed, family = binomial("logit"))
ss <- getME(mor_fed_num_ms,c("theta","fixef"))
mor_fed_num_ms <- update(mor_fed_num_ms,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                  optCtrl=list(maxfun=5e5)))
summary(mor_fed_num_ms)
#AIC: 3819.9
#Var: 1.578 obs 0.206 Sleeper 

write.csv(tidy(mor_fed_num_ms), "Mortality n blood-fed n number best model.csv")


