#############################                        
### ERA5 data for Tengrela ###
#############################
BF_data <- as.data.frame(read_csv(file = "temp data/t2m_2019_BF_Tengrela.csv")) %>%
  mutate(Date = as.Date(time),
         time = hour(time),
         Site = "Tengrela",
         Hut = "ERA5",
         temp = t2m -  273.5,
         min_temp = NA,
         max_temp = NA) %>%
  filter(Date >= "2019-09-08" & Date <= "2019-10-18")


View(BF_data)

sum(is.na(BF_data$temp))
###Do not need to interpolate the missing temp data

BF_temp <- BF_data %>%
  group_by(Date) %>%
  mutate(min_temp = min(temp),
         max_temp = max(temp),
         mean_temp = mean(temp),
         diurnal_temp_range = max_temp - min_temp) %>%
  ungroup() %>%
  subset(time >= 20 | time <= 6) %>%
  group_by(Date) %>%
  mutate(trial_min_temp = min(temp),
         trial_max_temp = max(temp),
         trial_mean_temp = mean(temp),
         trial_temp_range = trial_max_temp - trial_min_temp) %>%
  dplyr::select(c("Date", "min_temp", "max_temp", "mean_temp" ,"diurnal_temp_range",
                  "trial_min_temp", "trial_max_temp", "trial_mean_temp","trial_temp_range"))%>%
  distinct()

View(BF_temp)



ggplot(data = BF_data, aes(x = Date, y = temp, col = Site)) +
  geom_line(size = 0.75) +
  theme_bw() + theme(text = element_text(size = 15)) +
  xlab("Time") +
  ylab("Temperature (°C)")

###Plot the diurnal temperature range and the night temperature range together
ggplot(data = BF_data, aes(x = Date, y = temp)) +
  geom_line(size = 0.75, color = "orange") +
  geom_line(data = subset(BF_data, time >= 20 | time <= 6), 
             aes(x = Date, y = temp), col = "grey", size = 0.6, alpha = 0.8) +
  theme_bw() + theme(text = element_text(size = 15)) +
  xlab("Time") +
  ylab("Temperature (°C)")





###Number of mosquitoes

####Combining temp data with trial data
n_mos <- left_join(n_mos, BF_temp, by = "Date")

###Plotting
ggplot(n_mos, aes(y = Count, x=trial_temp_range, col = Location))+
  geom_point()+
  facet_grid(vars(Location), vars(Treatment))+
  labs(y = "Number of mosquitoes", x = "Temperature range during the trial time")

ggplot(n_mos, aes(y = Count, x=trial_mean_temp, col = Location))+
  geom_point()+
  facet_grid(vars(Location), vars(Treatment))+
  labs(y = "Number of mosquitoes", x = "Daily temperature range")


ggplot(n_mos, aes(y = Count, x=mean_temp, col = Location))+
  geom_point()+
  facet_grid(vars(Location), vars(Treatment))+
  labs(y = "Number of mosquitoes", x = "Mean temperature")


ggplot(n_mos, aes(y = Count, x=diurnal_temp_range, col = Location))+
  geom_point()+
  facet_grid(vars(Location), vars(Treatment))+
  labs(y = "Number of mosquitoes", x = "Daily temperature range")



model_5_1_diu_temp <- glmer.nb(formula = Count~Location + WashedStatus+ (1|marker) + diurnal_temp_range, data = n_mos)
summary(model_5_1_diu_temp)
###term insignificant
##AIC:3635.3
###Var:0.1026

model_5_1_mean_temp <- glmer.nb(formula = Count~Location + WashedStatus+ (1|marker) + mean_temp, data = n_mos)
model_5_1_mean_temp <- update(model_5_1_mean_temp, control = glmerControl(optimizer = "bobyqa"))
summary(model_5_1_mean_temp)
###term insignificant
##AIC:3635.3
###Var:0.1026


model_5_1_trial_range <- glmer.nb(formula = Count~Location + WashedStatus+ (1|marker) + trial_temp_range, data = n_mos)
summary(model_5_1_trial_range)
###term significant
##AIC:3630.9
##Var:0.08646


model_5_1_trial_temp <- glmer.nb(formula = Count~Location + WashedStatus+ (1|marker) + trial_mean_temp, data = n_mos)
summary(model_5_1_trial_temp)
###term insignificant
##AIC:3635.5
##Var:0.1028

####Mortality and blood-feeding

####Combining temp data with trial data
mf_mos_temp <- left_join(mf_mos, BF_temp, by = "Date")

ggplot(mf_mos_temp, aes(y=Bloodfed, x = trial_temp_range, col = Location))+
  geom_point()+
  facet_grid(vars(Location), vars(Treatment))+
  labs(y = "Blood-feeding rate", x = "Temperature range during the trial time")


ggplot(mf_mos_temp, aes(y=Bloodfed, x = diurnal_temp_range, col = Location))+
  geom_point()+
  facet_grid(vars(Location), vars(Treatment))+
  labs(y = "Blood-feeding rate", x = "Diurnal temperature range")


ggplot(mf_mos_temp, aes(y=Mortality, x = trial_temp_range, col = Location))+
  geom_point()+
  facet_grid(vars(Location), vars(Treatment))+
  labs(y = "Mortality", x = "Temperature range during the trial time")

ggplot(mf_mos_temp, aes(y=Mortality, x = diurnal_temp_range, col = Location))+
  geom_point()+
  facet_grid(vars(Location), vars(Treatment))+
  labs(y = "Mortality", x = "Diurnal temperature range")




##############################################################
################Blood-feeding modeling########################
##############################################################


bf_num_loc_Rand_mean_temp <- glmer(Fed~Total.Loc+ Location + WashedStatus + Total.Loc * WashedStatus + (1|Hut)
                                    + mean_temp + (1|marker)  + (1|Sleeper), data = mf_mos_temp, family =  binomial("logit"))
bf_num_loc_Rand_mean_temp <- update(bf_num_loc_Rand_mean_temp,control=glmerControl(optimizer="bobyqa",
                                                                                     optCtrl=list(maxfun=3e5)))
summary(bf_num_loc_Rand_mean_temp)
##AIC: 3163.0
##Var: 1.822545(observational) 0.007872(Hut) 0.435544(Sleeper)



bf_num_loc_Rand_trial_temp <- glmer(Fed~Total.Loc+ Location + WashedStatus + Total.Loc * WashedStatus + (1|Hut)
                         + trial_mean_temp + (1|marker)  + (1|Sleeper), data = mf_mos_temp, family =  binomial("logit"))
bf_num_loc_Rand_trial_temp <- update(bf_num_loc_Rand_trial_temp,control=glmerControl(optimizer="bobyqa",
                                                                        optCtrl=list(maxfun=3e5)))
summary(bf_num_loc_Rand_trial_temp)
##AIC: 3163.1
##Var: 1.823451(observational) 0.007609(Hut) 0.433830(Sleeper)


bf_num_loc_Rand_trial_temp_range <- glmer(Fed~Total.Loc+ Location + WashedStatus + Total.Loc * WashedStatus + (1|Hut)
                                    + trial_temp_range + (1|marker)  + (1|Sleeper), data = mf_mos_temp, family =  binomial("logit"))
bf_num_loc_Rand_trial_temp_range <- update(bf_num_loc_Rand_trial_temp_range,control=glmerControl(optimizer="bobyqa",
                                                                                     optCtrl=list(maxfun=3e5)))
summary(bf_num_loc_Rand_trial_temp_range)
##AIC: 3162.8
##Var: 1.818698(observational) 0.007469(Hut) 0.429342(Sleeper)

bf_num_loc_Rand_temp_range <- glmer(Fed~Total.Loc+ Location + WashedStatus + Total.Loc * WashedStatus + (1|Hut)
                                          + diurnal_temp_range + (1|marker)  + (1|Sleeper), data = mf_mos_temp, family =  binomial("logit"))
bf_num_loc_Rand_temp_range <- update(bf_num_loc_Rand_temp_range,control=glmerControl(optimizer="bobyqa",
                                                                                                 optCtrl=list(maxfun=3e5)))
summary(bf_num_loc_Rand_temp_range)
##AIC: 3163.1
##Var: 1.823754(observational) 0.007919(Hut) 0.435774(Sleeper)





##############################################################
####################Mortality modeling########################
##############################################################


ggplot(mf_mos_temp, aes(y=Mortality, x = trial_temp_range, col = Location))+
  geom_point()+
  facet_grid(vars(Location), vars(Treatment))+
  labs(y = "Mortality rate", x = "Temperature range during the trial time")



mor_fed_num_ms_temp <- glmer(Dead~Fed + Location + Treatment + Location * Treatment + Total.Loc + Total.Loc*Treatment +
                               trial_temp_range + (1 | marker)+ (1 | Sleeper), data = mf_mos_temp, family = binomial("logit"))
ss <- getME(mor_fed_num_ms_temp,c("theta","fixef"))
mor_fed_num_ms_temp <- update(mor_fed_num_ms_temp,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                                optCtrl=list(maxfun=5e5)))
summary(mor_fed_num_ms_temp)
###AIC: 3818.9
###Var: 1.5410(observational) 0.2138(Sleeper)


mor_fed_num_ms_mean_temp <- glmer(Dead~Fed + Location + Treatment + Location * Treatment + Total.Loc + Total.Loc*Treatment +
                                    mean_temp + (1 | marker)+ (1 | Sleeper), data = mf_mos_temp, family = binomial("logit"))
ss <- getME(mor_fed_num_ms_mean_temp,c("theta","fixef"))
mor_fed_num_ms_mean_temp <- update(mor_fed_num_ms_mean_temp,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                                     optCtrl=list(maxfun=5e5)))
summary(mor_fed_num_ms_mean_temp)
###AIC: 3821.2
###Var: 1.5647(observational) 0.2082(Sleeper)


mor_fed_num_ms_mean_temp_trial <- glmer(Dead~Fed + Location + Treatment + Location * Treatment + Total.Loc + Total.Loc*Treatment +
                                          trial_mean_temp + (1 | marker)+ (1 | Sleeper), data = mf_mos_temp, family = binomial("logit"))
ss <- getME(mor_fed_num_ms_mean_temp_trial,c("theta","fixef"))
mor_fed_num_ms_mean_temp_trial <- update(mor_fed_num_ms_mean_temp_trial,start=ss, control=glmerControl(optimizer="bobyqa",
                                                                                            optCtrl = list(maxfun=5e5)))
summary(mor_fed_num_ms_mean_temp_trial)
###AIC: 3821.3
###Var: 1.5764(observational) 0.2068(Sleeper)


mor_fed_num_ms_diu_temp_range <- glmer(Dead~Fed + Location + Treatment + Location * Treatment + Total.Loc + Total.Loc*Treatment +
                                    diurnal_temp_range + (1 | marker)+ (1 | Sleeper), data = mf_mos_temp, family = binomial("logit"))
ss <- getME(mor_fed_num_ms_diu_temp_range,c("theta","fixef"))
mor_fed_num_ms_diu_temp_range <- update(mor_fed_num_ms_diu_temp_range,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                                     optCtrl=list(maxfun=5e5)))
summary(mor_fed_num_ms_diu_temp_range)
###AIC: 3817.7
###Var: 1.5223(observational) 0.2152(Sleeper)
















