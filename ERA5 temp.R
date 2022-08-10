#############################                        
### ERA5 data for Tengrela ###
#############################
BF_data <- as.data.frame(read_csv(file = "temp data/t2m_2019_BF_Tengrela.csv")) %>%
  mutate(Date = as.Date(time),
         time = hour(time),
         Site = "Tengrela",
         Hut = "ERA5",
         temp = t2m -  273.5) %>%
  filter(Date >= "2019-09-08" & Date <= "2019-10-19")


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
  dplyr::select(c("Date", "min_temp", "max_temp", "mean_temp" ,"diurnal_temp_range")) %>%
  distinct()
                  

BF_temp_trial <- BF_data %>%
  dplyr::select(!c(Date,t2m))%>%
  subset(time >= 20 | time <= 6) %>%
  mutate(mark_trial = c(rep(0,7),rep(1:41, each = 11), rep(42,4))) %>%
  group_by(mark_trial) %>%
  mutate(trial_min_temp = min(temp),
         trial_max_temp = max(temp),
         trial_mean_temp = mean(temp),
         trial_temp_range = trial_max_temp - trial_min_temp) %>%
  dplyr::select(!c(temp, time)) %>%
  distinct() %>%
  ungroup() %>%
  dplyr::select(c("trial_min_temp", "trial_max_temp", "trial_mean_temp","trial_temp_range"))

Date <-  seq.Date(as.Date("2019-09-07"), as.Date("2019-10-19"), by = "day")

BF_temp_trial <- cbind(Date,BF_temp_trial)[-1,]
colnames(BF_temp_trial)[1] <- "Date"

BF_temp <-  left_join(BF_temp,BF_temp_trial, by = "Date")
  
View(BF_temp)




###Plot the diurnal temperature range and the night temperature range together
ggplot(data = BF_data, aes(x = Date, y = temp)) +
  geom_line(size = 0.75, color = "orange", alpha = 0.8) +
  geom_line(data = subset(BF_data, time >= 20 | time <= 6), 
             aes(x = Date, y = temp), col = "skyblue", size = 0.6) +
  geom_line(data = BF_temp, aes(x=Date, y= trial_mean_temp),
            size = 0.75, color = "red") +
  geom_line(data = BF_temp, aes(x=Date, y= mean_temp),
            size = 0.75, color = "black") +
  theme_bw() + theme(text = element_text(size = 15)) +
  xlab("Date") +
  ylab("Temperature (°C)")

ggsave("Temperature data.jpeg", device = jpeg)





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



model_5_1_diu_temp <- glmer.nb(formula = Count~Location + WashedStatus+ (1|marker)
                               + diurnal_temp_range, data = n_mos)
summary(model_5_1_diu_temp)
###term insignificant
##AIC:3635.3
###Var:0.1026


model_5_1_mean_temp <- glmer.nb(formula = Count~Location + WashedStatus+ (1|marker) + mean_temp, data = n_mos)
model_5_1_mean_temp <- update(model_5_1_mean_temp, control = glmerControl(optimizer = "bobyqa"))
summary(model_5_1_mean_temp)
###term insignificant
##AIC:3635.3
###Var:0.1028


model_5_1_trial_range <- glmer.nb(formula = Count~Location + WashedStatus+ (1|marker) + trial_temp_range, data = n_mos)
model_5_1_trial_range <- update(model_5_1_trial_range, control = glmerControl(optimizer = "bobyqa"))
summary(model_5_1_trial_range)
###term significant
##AIC:3633.4
##Var:0.09561



write.csv(tidy(model_5_1_trial_range), "Best model for count.csv")

model_5_1_trial_temp <- glmer.nb(formula = Count~Location + WashedStatus+ (1|marker) + trial_mean_temp, data = n_mos)
model_5_1_trial_temp <- update(model_5_1_trial_temp, control = glmerControl(optimizer = "bobyqa"))
summary(model_5_1_trial_temp)
###term insignificant
##AIC:3634.3
##Var:0.09908

####Mortality and blood-feeding

####Combining temp data with trial data

mor_fed_temp <- left_join(mor_fed, BF_temp, by = "Date")


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
View(mf_mos_temp)

bf_num_loc_Rand_mean_temp <- glmer(Fed~Total.Loc+ Location + WashedStatus  + (1|Hut)
                                   + Total.Loc * Location+ mean_temp + (1|marker)  + (1|Sleeper), 
                                   data = mor_fed_temp, 
                                   family =  binomial("logit"))
ss <- getME(bf_num_loc_Rand_mean_temp,c("theta","fixef"))
bf_num_loc_Rand_mean_temp <- update(bf_num_loc_Rand_mean_temp,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                 optCtrl=list(maxfun=5e5)))
summary(bf_num_loc_Rand_mean_temp)
##AIC: 3158.8
##Var: 1.732898(observational) 0.006269(Hut) 0.417458(Sleeper)


bf_num_loc_Rand_temp_range <- glmer(Fed~Total.Loc+ Location + WashedStatus  + (1|Hut)
                                    + Total.Loc * Location+ diurnal_temp_range + (1|marker)  + (1|Sleeper), 
                                    data = mor_fed_temp, 
                                    family =  binomial("logit"))
ss <- getME(bf_num_loc_Rand_temp_range,c("theta","fixef"))
bf_num_loc_Rand_temp_range <- update(bf_num_loc_Rand_temp_range,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                                              optCtrl=list(maxfun=5e5)))
summary(bf_num_loc_Rand_temp_range)
##AIC: 3158.9
##Var: 1.734685(observational) 0.006293(Hut) 0.417540(Sleeper)


bf_num_loc_Rand_trial_temp <- glmer(Fed~Total.Loc+ Location + WashedStatus + (1|Hut)
                                    + Total.Loc * Location+ trial_mean_temp+ (1|marker)  + (1|Sleeper), 
                                    data = mor_fed_temp, family =  binomial("logit"))
ss <- getME(bf_num_loc_Rand_trial_temp,c("theta","fixef"))
bf_num_loc_Rand_trial_temp <- update(bf_num_loc_Rand_trial_temp,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                                optCtrl=list(maxfun=5e5)))
summary(bf_num_loc_Rand_trial_temp)
##AIC: 3158.9
##Var: 1.734624(observational) 0.005927(Hut) 0.415383(Sleeper)


bf_num_loc_Rand_trial_temp_range <- glmer(Fed~Total.Loc+ Location + WashedStatus  + (1|Hut)
                                          + Total.Loc * Location+ trial_temp_range + (1|marker)  + (1|Sleeper), 
                                          data = mor_fed_temp, 
                                    family =  binomial("logit"))
ss <- getME(bf_num_loc_Rand_trial_temp_range,c("theta","fixef"))
bf_num_loc_Rand_trial_temp_range <- update(bf_num_loc_Rand_trial_temp_range,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                                              optCtrl=list(maxfun=5e5)))
summary(bf_num_loc_Rand_trial_temp_range)
##AIC: 3158.9
##Var: 1.732868(observational) 0.005903(Hut) 0.414677(Sleeper)







##############################################################
####################Mortality modeling########################
##############################################################


ggplot(mf_mos_temp, aes(y=Mortality, x = diurnal_temp_range, col = Location))+
  geom_point()+
  facet_grid(vars(Location), vars(Treatment))+
  labs(y = "Mortality rate", x = "Temperature range during the trial time")


mor_fed_num_ms_mean_temp <- glmer(Dead~Fed + Location + Treatment + Location * Treatment + Total.Loc + Total.Loc*Location +
                                    mean_temp + (1 | marker)+ (1 | Sleeper) + (1|Hut), 
                                  data = mor_fed_temp, family = binomial("logit"))
ss <- getME(mor_fed_num_ms_mean_temp,c("theta","fixef"))
mor_fed_num_ms_mean_temp <- update(mor_fed_num_ms_mean_temp,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                                          optCtrl=list(maxfun=5e5)))
summary(mor_fed_num_ms_mean_temp)
###AIC: 3820.9
###Var: 1.43495(observational) 0.19014(Sleeper) 0.07023(Hut)



mor_fed_num_ms_diu_temp_range <- glmer(Dead~Fed + Location + Treatment + Location * Treatment + Total.Loc + Total.Loc*Location +
                                         diurnal_temp_range + (1 | marker)+ (1 | Sleeper)+ (1|Hut), data = mor_fed_temp, 
                                       family = binomial("logit"))
ss <- getME(mor_fed_num_ms_diu_temp_range,c("theta","fixef"))
mor_fed_num_ms_diu_temp_range <- update(mor_fed_num_ms_diu_temp_range,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                                                    optCtrl=list(maxfun=5e5)))
summary(mor_fed_num_ms_diu_temp_range)
###AIC: 3817.2
###Var: 1.39494(observational) 0.19681(Sleeper) 0.07232(Hut)



mor_fed_num_ms_mean_temp_trial <- glmer(Dead~Fed + Location + Treatment + Location * Treatment + Total.Loc + Total.Loc*Location +
                                          trial_mean_temp + (1 | marker)+ (1 | Sleeper)+ (1|Hut), data = mor_fed_temp, 
                                        family = binomial("logit"))
ss <- getME(mor_fed_num_ms_mean_temp_trial,c("theta","fixef"))
mor_fed_num_ms_mean_temp_trial <- update(mor_fed_num_ms_mean_temp_trial,start=ss, control=glmerControl(optimizer="bobyqa",
                                                                                                       optCtrl = list(maxfun=5e5)))
summary(mor_fed_num_ms_mean_temp_trial)
###AIC: 3821.6
###Var: 1.44394(observational) 0.18832(Sleeper) 0.07076(Hut)


mor_fed_num_ms_temp <- glmer(Dead~Fed + Location + Treatment + Location * Treatment + Total.Loc + Total.Loc*Location +
                               trial_temp_range + (1 | marker)+ (1 | Sleeper)+ (1|Hut),
                             data = mor_fed_temp, family = binomial("logit"))

ss <- getME(mor_fed_num_ms_temp,c("theta","fixef"))
mor_fed_num_ms_temp <- update(mor_fed_num_ms_temp,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                                optCtrl=list(maxfun=5e5)))
summary(mor_fed_num_ms_temp)
###AIC: 3818.5
###Var: 1.41515(observational) 0.19043(Sleeper) 0.07055(Hut)

























