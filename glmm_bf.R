###Adding individual random effect to model 1
View(Tengrela_R1A_rm)


bf_mos <- as_tibble(Tengrela_R1A_rm)%>% 
  mutate(Total.Bf = Fed.Tot/Total,
         Room = Fed.Room/Total.Room,
         Net = Fed.Net/Total.Net,
         Veranda = Fed.Ver/Total.Ver)%>%
  gather("Location", "Bloodfed", c("Room", "Net", "Veranda"))%>%
  dplyr::select(starts_with(c("Village","Date","Treatment","Location", "Bloodfed",
                              "Total","Fed","Hut","Sleeper","marker"))) %>%
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

bf_mos$Location <- as.factor(bf_mos$Location)
bf_mos$Date <- as.Date(bf_mos$Date)


head(bf_mos)
View(bf_mos) 

bf_reg <- gen_bf(bf_mos)
View(bf_reg) 


bf_reg$Location <- as.character(bf_reg$Location)
bf_reg$Location <- as.factor(bf_reg$Location)

bf_reg$Treatment <- relevel(bf_reg$Treatment, ref = "UTN")
bf_reg$WashedStatus <- relevel(bf_reg$WashedStatus, ref = "UTN")
bf_reg$Nets <- relevel(bf_reg$Nets, ref = "UTN")






#########Graphs
ggplot(bf_mos, aes(x=Location, y = Bloodfed, colour = Location, fill = Location))+
  geom_boxplot(alpha = 0.5)+
  facet_wrap(vars(Treatment))+
  stat_summary(fun = mean, geom="point", shape=18, size=5, alpha = 0.7, color = "red") +
  labs(title = "Blood feeding by location and treatment",
       y = "Blood-feeding rate")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal_hgrid(12)

ggsave("Blood feeding by Location and Treatment.jpeg", device = jpeg,
       width = 8, height = 5.5)


ggplot(bf_mos, aes(x=Location, y = Bloodfed, colour = Location))+
  geom_boxplot()+
  facet_grid(vars(Insecticide))+
  stat_summary(fun = mean, geom="point", shape=16, size=5, alpha = 0.5, color = "red") +
  labs(title = "Blood feeding by location and insecticide",
       y = "Blood feeding rate")+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Blood feeding by Location and Insecticide.jpeg", device = jpeg,
       width = 8, height = 5.5)

ggplot(bf_mos, aes(x=Date, y = Bloodfed, color = WashedStatus))+
  geom_point()+
  facet_grid(vars(Location), vars(Insecticide))+
  labs(title = "Daily blood feeding rate by location and insecticide",
       y = "Blood feeding rate")+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Daily blood feeding rate by Location and Insecticide.jpeg", device = jpeg,
       width = 8, height = 5.5)


ggplot(bf_mos, aes(x=Total.Room, y = Bloodfed, color = WashedStatus))+
  geom_point()+
  facet_grid(vars(Location), vars(Insecticide))+
  labs(title = "Blood feeding rate by number of mosquitoes",
       y = "Blood feeding rate", x = "Number of mosquitoes")+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Blood feeding rate by number of mosquitoes.jpeg", device = jpeg,
       width = 8, height = 5.5)



ggplot(mor_fed, aes(x=Total.Loc, y = Fed, color = WashedStatus))+
  geom_point()+
  facet_grid(vars(WashedStatus), vars(Location))+
  labs(title = "Blood feeding rate by number of mosquitoes in different location",
       y = "Blood-fed status", x = "Number of mosquitoes")+
  geom_smooth(formula = y~x,method = glm, method.args= list(family="binomial"))+
  theme_bw()

ggsave("Blood feeding status by number of mosquitoes.jpeg", device = jpeg,
       width = 8, height = 5.5)




bf_mos_df <- as.tibble(Tengrela_R1A_rm) %>%
  gather("Loc", "Total.Loc", c("Total.Room","Total.Net","Total.Ver")) %>%
  mutate(Loc.Tot = case_when(Loc == "Total.Room" ~ "Room",
                             Loc == "Total.Net" ~ "Net",
                             Loc == "Total.Ver" ~ "Veranda"))%>%
  dplyr::select(c("Village", "Date", "Treatment", "Loc.Tot", "Total.Loc",
                  "Hut", "Sleeper", "marker"))

bf_mos_df <- left_join(bf_mos, bf_mos_df, by = c("Village", "Date", "Treatment", "Hut", "Sleeper", "marker", 
                                              "Location" ="Loc.Tot"))
View(bf_mos_df)

ggplot(bf_mos_df, aes(x = Total.Loc, y = Bloodfed, color = Nets, fill = Nets))+
  geom_point()+
  facet_grid(vars(Location), vars(Insecticide))+
  labs(title = "Blood feeding rate correponding to number of mosquitoes",
       x = "Number of mosquitoes", y = "blood feeding rate")+
  scale_y_continuous( breaks = seq(0,1,0.25), limits = c(0,1))+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_smooth(method = "lm", formula = y~x)

ggsave("Blood feeding rate correponding to number of mosquitoe.jpeg", device = jpeg)


###Intercept only model
bf_0 <- glm(Fed~1, data = bf_reg, family =  binomial("logit"))
summary(bf_0)
#AIC: 5367.4

bf_0_obs <- glmer(Fed ~1+(1|marker), data = bf_reg, family =  binomial("logit"))
summary(bf_0_obs)
#AIC: 4084.7
#Var: 2.965

bf_0_hut <- glmer(Fed ~1+(1|Hut), data = bf_reg, family =  binomial("logit"))
summary(bf_0_hut)
#AIC: 5310.5
#Var: 0.0755

bf_0_rand <- glmer(Fed ~1+(1|marker)+(1|Hut), data = bf_reg, family =  binomial("logit"))
summary(bf_0_rand)
#AIC: 4086.7
#Var: 2.965; 0





###One variable model
bf_1_1 <- glm(Fed~Location, data = bf_reg, family =  binomial("logit"))
summary(bf_1_1)
#AIC: 3888.3



bf_1_2 <- glm(Fed~WashedStatus, data = bf_reg, family =  binomial("logit"))
summary(bf_1_2)
#AIC: 5306.7


bf_1_3 <- glm(Fed~Treatment, data = bf_reg, family =  binomial("logit"))
summary(bf_1_3)
#AIC: 5250.7

bf_1_4 <- glm(Fed~Insecticide, data = bf_reg, family =  binomial("logit"))
summary(bf_1_4)
#AIC: 5306.3

bf_1_5 <- glm(Fed~Nets, data = bf_reg, family =  binomial("logit"))
summary(bf_1_5)
#AIC: 5305.1



###One variable model with random effect
bf_1_1_obs <- glmer(Fed~Location + (1|marker), data = bf_reg, family =  binomial("logit"))
summary(bf_1_1_obs)
#AIC: 3198.7
##2.355



bf_1_2_obs <- glmer(Fed~WashedStatus+ (1|marker), data = bf_reg, family =  binomial("logit"))
summary(bf_1_2_obs)
#AIC: 4080.6

bf_1_3_obs <- glmer(Fed~Treatment+ (1|marker), data = bf_reg, family =  binomial("logit"))
summary(bf_1_3_obs)
#AIC: 4079.6

bf_1_4_obs <- glmer(Fed~Insecticide+ (1|marker), data = bf_reg, family =  binomial("logit"))
summary(bf_1_4_obs)
#AIC: 4080.5

bf_1_5_obs <- glmer(Fed~Nets+ (1|marker), data = bf_reg, family =  binomial("logit"))
summary(bf_1_5_obs)
#AIC: 4080.3


###One variable model with Hut random effect
bf_1_1_hut <- glmer(Fed~Location + (1|Hut), data = bf_reg, family =  binomial("logit"))
summary(bf_1_1_hut)
#AIC: 3198.7
##2.355

bf_1_2_hut <- glmer(Fed~WashedStatus+ (1|Hut), data = bf_reg, family =  binomial("logit"))
summary(bf_1_2_hut)
#AIC: 4080.6


bf_1_3_hut <- glmer(Fed~Treatment+ (1|Hut), data = bf_reg, family =  binomial("logit"))
summary(bf_1_3_hut)
#AIC: 4079.6

bf_1_4_hut <- glmer(Fed~Insecticide+ (1|Hut), data = bf_reg, family =  binomial("logit"))
summary(bf_1_4_hut)
#AIC: 4080.5

bf_1_5_hut <- glmer(Fed~Nets+ (1|Hut), data = bf_reg, family =  binomial("logit"))
summary(bf_1_5_hut)
#AIC: 4080.3


###Two variable model
bf_2_1 <- glm(Fed~Location + Treatment, data = bf_reg, family =  binomial("logit"))
summary(bf_2_1)
#AIC: 3800.7

bf_2_2 <- glm(Fed~Location +WashedStatus, data = bf_reg, family =  binomial("logit"))
summary(bf_2_2)
#AIC: 3816.6


bf_2_3 <- glm(Fed~Location +Insecticide, data = bf_reg, family =  binomial("logit"))
summary(bf_2_3)
#AIC: 3817.3

bf_2_4 <- glm(Fed~Location +Nets, data = bf_reg, family =  binomial("logit"))
summary(bf_2_4)
#AIC: 3817



###Two variable model with interaction
bf_2_1_int <- glm(Fed~Location + Treatment+Location * Treatment, data = bf_reg, family =  binomial("logit"))
summary(bf_2_1_int)
#AIC: 3808.8

bf_2_2_int <- glm(Fed~Location +WashedStatus + Location *WashedStatus, data = bf_reg, family =  binomial("logit"))
summary(bf_2_2_int)
#AIC: 3818.1


bf_2_3_int <- glm(Fed~Location +Insecticide +Location*Insecticide, data = bf_reg, family =  binomial("logit"))
summary(bf_2_3_int)
#AIC: 3822.3

bf_2_4_int <- glm(Fed~Location +Nets + Location*Nets, data = bf_reg, family =  binomial("logit"))
summary(bf_2_4_int)
#AIC: 3822.1
anova(bf_2_1_int,bf_2_2_int,bf_2_3_int,bf_2_4_int)

###Two variable model with observational random effect
bf_2_1_obs <- glmer(Fed~Location + Treatment + (1|marker), data = bf_reg, family =  binomial("logit"))
bf_2_1_obs <- update(bf_2_1_obs, control = glmerControl(optimizer = "bobyqa"))
summary(bf_2_1_obs)
#AIC: 3194.5
#Var: 2.178
write.csv(tidy(bf_2_1_obs), "Blood fed Treatment model.csv")

bf_2_2_obs <- glmer(Fed~Location +WashedStatus + (1|marker), data = bf_reg, family =  binomial("logit"))
summary(bf_2_2_obs)
#AIC: 3192.5
#Var: 2.178


bf_2_3_obs <- glmer(Fed~Location +Insecticide + (1|marker), data = bf_reg, family =  binomial("logit"))
summary(bf_2_3_obs)
#AIC: 3192.6
#Var: 2.215

write.csv(rbind(tidy(bf_2_3_obs),tidy(bf_2_2_obs)), "Blood feeding best model.csv")

bf_2_4_obs <- glmer(Fed~Location +Nets + (1|marker), data = bf_reg, family =  binomial("logit"))
summary(bf_2_4_obs)
#AIC: 3193.4
#Var: 2.207


###Two variable model with interaction and observational random effect
bf_2_1_int_obs <- glmer(Fed~Location + Treatment+Location * Treatment + (1|marker), data = bf_reg, family =  binomial("logit"))
bf_2_1_int_obs <- update(bf_2_1_int_obs, control = glmerControl(optimizer = "bobyqa"))
summary(bf_2_1_int_obs)
#AIC: 3200.2
#Var: 2.219

bf_2_2_int_obs <- glmer(Fed~Location +WashedStatus + Location *WashedStatus+ (1|marker), data = bf_reg, family =  binomial("logit"))
bf_2_2_int_obs <- update(bf_2_2_int_obs, control = glmerControl(optimizer = "bobyqa"))
summary(bf_2_2_int_obs)
#AIC: 3199.3
#Var: 2.196

bf_reg$Insecticide <- relevel(as.factor(bf_reg$Insecticide), ref = "UTN")
bf_2_3_int_obs <- glmer(Fed~Location +Insecticide +Location*Insecticide+ (1|marker), data = bf_reg, family =  binomial("logit"))
summary(bf_2_3_int_obs) 
#AIC: 3192.8
#Var: 2.278

write.csv(tidy(bf_2_3_int_obs), "Blood feeding best model.csv")


bf_2_4_int_obs <- glmer(Fed~Location +Nets + Location*Nets+ (1|marker), data = bf_reg, family =  binomial("logit"))
bf_2_4_int_obs <- update(bf_2_4_int_obs, control = glmerControl(optimizer = "bobyqa"))
summary(bf_2_4_int_obs)
#AIC: 3196.4
#Var: 2.256



###Two variable model with interaction and observational and hut random effect
bf_2_1_int_rand <- glmer(Fed~Location + Treatment+Location * Treatment + (1|marker) + (1|Hut), 
                         data = bf_reg, family =  binomial("logit"))
bf_2_1_int_rand <- update(bf_2_1_int_rand, control = glmerControl(optimizer = "bobyqa"))
summary(bf_2_1_int_rand)
#AIC: 3202.2
#Var: 2.219 

bf_2_2_int_rand <- glmer(Fed~Location +WashedStatus + Location *WashedStatus+ (1|marker)+ (1|Hut),
                        data = bf_reg, family =  binomial("logit"))
bf_2_2_int_rand <- update(bf_2_2_int_rand, control = glmerControl(optimizer = "bobyqa"))
summary(bf_2_2_int_rand)
#AIC: 3199.3
#Var: 2.196


bf_2_3_int_rand <- glmer(Fed~Location +Insecticide +Location*Insecticide+ (1|marker) + (1|Hut),
                        data = bf_reg, family =  binomial("logit"))
bf_2_3_int_rand <- update(bf_2_3_int_rand, control = glmerControl(optimizer = "bobyqa"))
summary(bf_2_3_int_rand) 
#AIC: 3194.8
#Var: 2.278


bf_2_4_int_rand <- glmer(Fed~Location +Nets + Location*Nets+ (1|marker) + (1|Hut),
                        data = bf_reg, family =  binomial("logit"))
bf_2_4_int_rand <- update(bf_2_4_int_rand, control = glmerControl(optimizer = "bobyqa"))
summary(bf_2_4_int_rand)
#AIC: 3198.4
#Var: 2.256

summary(glmer(Fed~Location +Insecticide +WashedStatus+ Insecticide *WashedStatus+  (1|marker),
              data = bf_reg, family =  binomial("logit")))


#### Model blood feeding with the number of mosquitoes
###Total regardless of location
bf_num_1 <- glm(Fed~Total, data = mor_fed, family =  binomial("logit"))
summary(bf_num_1)
##AIC: 5368.4

bf_num_2 <- glm(Fed~Total+ Location + WashedStatus, data = mor_fed, family =  binomial("logit"))
summary(bf_num_2)
##AIC: 3818

bf_num_3 <- glm(Fed~Total+ Location + WashedStatus + Total * WashedStatus, data = mor_fed, family =  binomial("logit"))
summary(bf_num_3)
##AIC: 3811.1


bf_num_4 <- glm(Fed~Total+ Location + WashedStatus + Total * Location, data = mor_fed, family =  binomial("logit"))
summary(bf_num_4)
##AIC: 3801.5


###Add random effect in the Total*Treatment model
bf_num_3_obs <- glmer(Fed~Total+ Location + WashedStatus + Total * WashedStatus+ (1|marker), 
                      data = mor_fed, family =  binomial("logit"))
ss <- getME(bf_num_3_obs,c("theta","fixef"))
bf_num_3_obs <- update(bf_num_3_obs,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                  optCtrl=list(maxfun=5e5)))
summary(bf_num_3_obs)
##AIC: 3196.9
###Var: 2.199

bf_num_3_Hut <- glmer(Fed~Total+ Location + WashedStatus + Total * WashedStatus+ 
                        (1|Hut), data = mor_fed, family =  binomial("logit"))
ss <- getME(bf_num_3_Hut,c("theta","fixef"))
bf_num_3_Hut <- update(bf_num_3_Hut,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                  optCtrl=list(maxfun=5e5)))
summary(bf_num_3_Hut)
##AIC: 3767.2
###Var: 0.09347


bf_num_3_Slp <- glmer(Fed~Total+ Location + WashedStatus + Total * WashedStatus+ 
                        (1|Sleeper), data = mor_fed, family =  binomial("logit"))
ss <- getME(bf_num_3_Slp,c("theta","fixef"))
bf_num_3_Slp <- update(bf_num_3_Slp,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                  optCtrl=list(maxfun=6e5)))
summary(bf_num_3_Slp)
##AIC: 3610.1
###Var: 0.4007

bf_num_3_MS <- glmer(Fed~Total+ Location + Treatment + Total * Treatment+ 
                         (1|marker) +  (1|Sleeper), 
                       data = mor_fed, family =  binomial("logit"))
ss <- getME(bf_num_3_MS,c("theta","fixef"))
bf_num_3_MS <- update(bf_num_3_MS,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                    optCtrl=list(maxfun=3e5)))
summary(bf_num_3_MS)
##AIC: 3172.5
###Var: 1.6922(observational) 0.5049(Sleeper)

bf_num_3_Rand <- glmer(Fed~Total+ Location + WashedStatus + Total * WashedStatus+ 
                         (1|marker) + (1|Hut)+ (1|Sleeper), 
                       data = mor_fed, family =  binomial("logit"))
ss <- getME(bf_num_3_Rand,c("theta","fixef"))
bf_num_3_Rand <- update(bf_num_3_Rand,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                  optCtrl=list(maxfun=3e5)))
summary(bf_num_3_Rand)
##AIC: 3174.5
###Var: 1.6922(observational) 0.0000(Hut) 0.5049(Sleeper)



#### Model blood feeding with the number of mosquitoes
###Total by location
bf_num_loc_1 <- glm(Fed~Total.Loc, data = mor_fed, family =  binomial("logit"))
summary(bf_num_loc_1)
##AIC: 5361.8

bf_num_loc_2 <- glm(Fed~Total.Loc+ Location + WashedStatus, data = mor_fed, family =  binomial("logit"))
summary(bf_num_loc_2)
##AIC: 3807.4


bf_num_loc_3 <- glm(Fed~Total.Loc+ Location + WashedStatus + Total.Loc * WashedStatus, data = mor_fed, family =  binomial("logit"))
summary(bf_num_loc_3)
##AIC: 3801.4

bf_num_loc_4 <- glm(Fed~Total.Loc+ Location + WashedStatus  + Total.Loc * Location,
                    data = mor_fed, family =  binomial("logit"))
summary(bf_num_loc_4)
##AIC: 3740.8

bf_num_loc_5 <- glm(Fed~Total.Loc+ Location + WashedStatus + Total.Loc * WashedStatus + Total.Loc * Location,
                    data = mor_fed, family =  binomial("logit"))
summary(bf_num_loc_5)
##AIC: 3714.6


bf_num_loc_obs <- glmer(Fed~Total.Loc + Location + WashedStatus + Total.Loc * WashedStatus + Total.Loc * Location
                      + (1|marker), data = mor_fed, family =  binomial("logit"))
ss <- getME(bf_num_loc_obs,c("theta","fixef"))
bf_num_loc_obs <- update(bf_num_loc_obs,start=ss,control=glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=5e5)))

summary(bf_num_loc_obs)
##AIC: 3177.4
##Var: 2.178

bf_num_loc_Hut <- glmer(Fed~Total.Loc+ + Location + WashedStatus + Total.Loc * WashedStatus 
                        + Total.Loc * Location + (1|Hut), 
                      data = mor_fed, family =  binomial("logit"))

summary(bf_num_loc_Hut)
##AIC: 3681.6
##Var: 0.07529

bf_num_loc_Hut_nw <- glmer(Fed~Total.Loc+ + Location + WashedStatus +  
                        + Total.Loc * Location + (1|Hut), 
                        data = mor_fed, family =  binomial("logit"))
bf_num_loc_Hut_nw <- update(bf_num_loc_Hut_nw,control=glmerControl(optimizer="bobyqa"))
summary(bf_num_loc_Hut_nw)
##AIC: 3701.7
##Var: 0.08533

bf_num_loc_Slp <- glmer(Fed~Total.Loc+ Location + WashedStatus + Total.Loc * WashedStatus
                        + Total.Loc * Location+ (1|Sleeper), data = mor_fed, family =  binomial("logit"))

summary(bf_num_loc_Slp)
##AIC: 3554.8
##Var: 0.3314

bf_num_loc_Rand <- glmer(Fed~Total.Loc+ Location + WashedStatus + Total.Loc * WashedStatus + (1|Hut)
                         + Total.Loc * Location+ (1|marker)  + (1|Sleeper), 
                         data = mor_fed, family =  binomial("logit"))
ss <- getME(bf_num_loc_Rand,c("theta","fixef"))
bf_num_loc_Rand <- update(bf_num_loc_Rand,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                    optCtrl=list(maxfun=3e5)))
summary(bf_num_loc_Rand)
##AIC: 3160.9
##Var: 1.727365(observational) 0.005976(Hut) 0.416347(Sleeper)

bf_num_loc_Rand_nw <- glmer(Fed~Total.Loc+ Location + WashedStatus  + (1|Hut)
                         + Total.Loc * Location+ (1|marker)  + (1|Sleeper), 
                         data = mor_fed, family =  binomial("logit"))
ss <- getME(bf_num_loc_Rand_nw,c("theta","fixef"))
bf_num_loc_Rand_nw <- update(bf_num_loc_Rand_nw,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                        optCtrl=list(maxfun=3e5)))
summary(bf_num_loc_Rand_nw)
##AIC: 3156.9
##Var: 1.734571(observational) 0.005949(Hut) 0.415454(Sleeper)

####Test model contains Treatment with or w/o location

bf_best_treat_loc <- glmer(Fed~Total.Loc + Location + Treatment
      + (1|marker), data = mor_fed, family =  binomial("logit"))
ss <- getME(bf_best_treat_loc,c("theta","fixef"))
bf_best_treat_loc <- update(bf_best_treat_loc,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                        optCtrl=list(maxfun=3e5)))
summary(bf_best_treat_loc)
##AIC:3175.9
##Var: 2.236


bf_best_treat <- glmer(Fed~Total.Loc + Treatment
                       + (1|marker), data = mor_fed, family =  binomial("logit"))
ss <- getME(bf_best_treat,c("theta","fixef"))
bf_best_treat <- update(bf_best_treat,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                    optCtrl=list(maxfun=3e5)))
summary(bf_best_treat)
##AIC: 4023.1
##Var: 3.151
 

bf_best_treat_int <- glmer(Fed~Total.Loc + Treatment + Total.Loc * Treatment
                       + (1|marker), data = mor_fed, family =  binomial("logit"))

summary(bf_best_treat_int)
##AIC: 4016.8
##Var: 3.146





