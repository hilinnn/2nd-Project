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
ggplot(bf_mos, aes(x=Location, y = Bloodfed, colour = Location))+
  geom_boxplot()+
  facet_wrap(vars(Treatment))+
  stat_summary(fun = mean, geom="point", shape=16, size=5, alpha = 0.5, color = "red") +
  labs(title = "Blood feeding by location and treatment",
       y = "Blood feeding rate")+
  theme(plot.title = element_text(hjust = 0.5))

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


ggplot(bf_mos, aes(x=Total, y = Total.Bf, color = WashedStatus))+
  geom_point()+
  facet_grid( vars(Insecticide))+
  labs(title = "Blood feeding rate by number of mosquitoes within the hut",
       y = "Blood feeding rate", x = "Number of mosquitoes")+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Blood feeding rate by number of mosquitoes within the hut.jpeg", device = jpeg,
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


