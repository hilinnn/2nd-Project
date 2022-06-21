source("~/Desktop/Second project/R/0610 loop.R")
library(lme4)
library(tidyverse)
library(DHARMa)
library(lubridate)
library(MASS)
library(merTools)
library(ggpubr)
library(ggpmisc)



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

ggplot(m_mos, aes(y = Mortality, x=Village, color = Nets))+
  geom_boxplot()+
  stat_summary(aes(fill = Nets), fun = mean, geom="point", shape=16, size=6, alpha = 0.7) +
  facet_grid(vars(Location), vars(Treatment))+
  labs(title = "Mortality by location and treatment")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave("Mortality by location and treatment.jpeg", 
       device = jpeg, width = 8, height = 5.5)


View(m_mos)

####Run logistic regression for the mortality
###Fixed effect model
prop.test()

mor_reg$Treatment <- relevel(mor_reg$Treatment, ref = "UTN")

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



