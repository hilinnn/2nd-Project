#####Store the function of the models and graaphs included in the result part


####Method 

###NB fig
p2 <- ggplot(n_mos, aes(x = Total))+
  geom_histogram(aes(fill = Treatment), binwidth = 3, center = 0, alpha = 0.7)+
  facet_wrap(vars(Treatment), ncol = 2)+
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        legend.position = "none")+
  labs(title = 'Number of mosquitoes per night by Treatment',
       x = "Number of mosquitoes", y = "Frequency")+
  scale_x_continuous(breaks = seq(0, maxTotal, 10))+
  theme_minimal_hgrid()

ggsave("Histogram of the number of mosquitoes by Treatment.jpeg",
       width = 8, height = 6)

###Barplot of the number of mosquitoes by Location
maxCount<- max(n_mos$Count)

p1 <- ggplot(n_mos, aes(x = Count))+
  geom_histogram(aes(fill = Location), binwidth = 1, center = 0)+
  facet_wrap(vars(Location))+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  labs(title = 'Number of mosquitoes per night by Location',
       y = "Frequency", x = "Number of mosquitoes")+
  scale_x_continuous(breaks = seq(0, maxCount, 10))+
  theme_minimal_hgrid(12)

ggsave("Histogram of the number of mosquitoes by Location.jpeg",
       width = 8, height = 4)

plot_grid(p1, p2, labels = c('A', 'B'), label_size = 18)
ggsave("Negative binomial fits.jpeg", device = "jpeg",
       width = 14, height = 9)





####number of mosquitoes

###Model
model_5_1 <- glmer.nb(formula = Count~Location + WashedStatus+ (1|marker), data = n_mos)
summary(model_5_1)


model_5_1_trial_range <- glmer.nb(formula = Count~Location + WashedStatus+ (1|marker) + trial_temp_range, data = n_mos)
summary(model_5_1_trial_range)

write.csv(tidy(model_5_1_trial_range), "Mosquito count best model.csv")


###Summary fig
n_mos %>%
  ggplot(aes(x = Location, y = Count, color = Location))+
  geom_boxplot(aes(fill = Location), alpha = 0.5)+
  stat_summary(fun = mean, geom="point", shape=23, size=4, color = 'red', fill = 'red') +
  #geom_errorbar(aes(x=Location, ymin=min, ymax= max), color = 'steelblue')+
  facet_wrap(vars(Treatment))+
  labs(title = "Number of mosquitoes in each treatment by location",
       y = "Number of mosquitoes")+
  theme(plot.title = element_text(hjust = 0.5, size = 12.5),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 13),
        strip.text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())+
  theme_minimal_hgrid(12)

ggsave("Average of number of mosquitoes by location and treatment.jpeg")




####Blood feeding


####model
standard_bf <- glm(Fed~Treatment, data = mf_mos_temp,
                    family = binomial("logit"))
summary(standard_bf)
##AIC: 5250.7

standard_bf_ws <- glm(Fed~WashedStatus, data = mf_mos_temp,
                   family = binomial("logit"))
summary(standard_bf_ws)
##AIC: 5306.7

bf_num_loc_Rand <- glmer(Fed~Total.Loc+ Location + WashedStatus + Total.Loc * WashedStatus + (1|Hut)
                         + (1|marker)  + (1|Sleeper), data = mor_fed, family =  binomial("logit"))
ss <- getME(bf_num_loc_Rand,c("theta","fixef"))
bf_num_loc_Rand <- update(bf_num_loc_Rand,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                        optCtrl=list(maxfun=3e5)))
summary(bf_num_loc_Rand)
##AIC: 3161.1
##Var: 1.824310(observational) 0.007548(Hut) 0.433516(Sleeper)

write.csv(tidy(bf_num_loc_Rand), "Blood feeding best model.csv")

pred_bf <- predict(bf_num_loc_Rand, mor_fed, type = "response")

###Summary fig
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

###With number of mosquitoes
bf_quant <- bf_mos%>%
  dplyr::select(c("Village","Date","Treatment","Location", "Bloodfed", "Insecticide",
                  "Total","WashedStatus","Sleeper","marker","Nets", "Hut")) %>%
  right_join(mor_fed, by=c("Village","Date","Treatment","Location", "Insecticide",
                           "Total","WashedStatus","Sleeper","marker","Nets", "Hut"))
bf_quant <- cbind(bf_quant, pred_bf)

bf_quant$WashedStatus <-  relevel(as.factor(bf_quant$WashedStatus), ref = "UTN")
  ggplot(bf_quant, aes(color = Location, fill = Location))+
  geom_point(aes(x = Total.Loc, y = Bloodfed))+
  facet_grid(vars(Location), vars(WashedStatus))+
  labs(title = "Blood-feeding correponding to number of mosquitoes",
       x = "Number of mosquitoes", y = "Blood-feeding rate")+
  geom_smooth(aes(x = Total.Loc, y = pred_bf),formula = y~x,method = glm, 
              method.args= list(family="binomial"), col = "black")+
  theme_bw()

ggsave("Blood feeding status by number of mosquitoes.jpeg", device = jpeg,
       width = 8, height = 5.5)




####Mortality

###Model
mf_mos_temp$Treatment <- relevel(mf_mos_temp$Treatment, ref = "UTN")
standard_mor <- glm(Dead~Treatment, data = mf_mos_temp,
                      family = binomial("logit"))
summary(standard_mor)
##AIC: 4410.3

standard_mor_mix <- glmer(Dead~Treatment + (1|marker) + (1|Hut), data = mf_mos_temp,
                    family = binomial("logit"))
summary(standard_mor_mix)
##AIC: 4093.3

mor_fed_num_ms_diu_temp_range <- glmer(Dead~Fed + Location + Treatment + Location * Treatment + Total.Loc + Total.Loc*Treatment +
                                         diurnal_temp_range + (1 | marker)+ (1 | Sleeper), data = mf_mos_temp, family = binomial("logit"))
ss <- getME(mor_fed_num_ms_diu_temp_range,c("theta","fixef"))
mor_fed_num_ms_diu_temp_range <- update(mor_fed_num_ms_diu_temp_range,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                                                    optCtrl=list(maxfun=5e5)))
summary(mor_fed_num_ms_diu_temp_range)
###AIC: 3817.7
###Var: 1.5223(observational) 0.2152(Sleeper)

write.csv(tidy(mor_fed_num_ms_diu_temp_range), "Mortality n blood-fed n number n temp best model.csv")



# n <- mf_mos_temp %>%
#   count(Treatment, Location)
# n <- as.data.frame(n)
# 
# bf_pred <- NULL
# 
# for (i in 1:nrow(n)){
#   Count <- n[i,1] - 1
#   a <- seq(0,1,1/Count)
#   bf_pred <- append(bf_pred,a)
# }
# View(bf_pred)



predict_mor <- predict(mor_fed_num_ms_diu_temp_range, mf_mos_temp, type = "response")
pred_mor <- cbind(mf_mos_temp, predict_mor)

####summary graph
ggplot(m_mos, aes(y = Mortality, x=Location, color = Location,  fill = Location))+
  geom_boxplot(alpha=0.5)+
  stat_summary(fun = mean, geom="point", shape=18, size=6, alpha = 0.7, color = "red") +
  facet_wrap(vars(Treatment))+
  labs(title = "Mortality by location and treatment")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal_hgrid()

ggsave("Mortality by location and treatment.jpeg", 
       device = jpeg, width = 8, height = 5.5)



###Blood feeding rate
mf_mos <- m_mos %>%
  dplyr::select(c("Village","Date","Treatment","Location", "Mortality", "Insecticide",
                  "Total","WashedStatus","Sleeper","marker","Nets", "Hut")) %>%
  right_join(mor_fed, by=c("Village","Date","Treatment","Location", "Insecticide",
                           "Total","WashedStatus","Sleeper","marker","Nets", "Hut"))
mf_mos <- bf_mos %>%
  dplyr::select(c("Village","Date","Treatment","Location", "Bloodfed", "Insecticide",
                  "Total","WashedStatus","Sleeper","marker","Nets", "Hut")) %>%
  right_join(mf_mos, by=c("Village","Date","Treatment","Location", "Insecticide",
                           "Total","WashedStatus","Sleeper","marker","Nets", "Hut"))

mf_mos <- left_join(mf_mos, b, by=c("Village","Date","Treatment","Location", "Insecticide",
                     "Total","WashedStatus","Sleeper","marker","Nets", "Hut",
                     "Mortality", "Dead", "Total.Loc", "Fed","Bloodfed"))

mf_mos$Treatment <- relevel(mf_mos$Treatment, ref = "UTN")

ggplot(mf_mos, aes(color = Nets, fill = Nets))+
  geom_point(aes(x = Bloodfed, y = Mortality))+
  facet_grid(vars(Location), vars(Treatment))+
  labs(title = "Mortality correponding to blood-feeding",
       x = "Blood-feeding rate", y = "Mortality")+
  scale_x_continuous(labels = c(0, 0.25,0.5,0.75,1.0))+
  geom_smooth(data = pred_mor, aes(x=Fed, y = predict_mor), col = "black", method = "glm", 
              method.args = list(family = "binomial"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12))

ggsave("Mortality corresponding blood feeding.jpeg", device = jpeg)

# summary(glm(Dead~Fed, data = mf_mos, family = binomial("logit")))
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -0.9438  -0.9438  -0.7939   1.4305   1.6176  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -0.57775    0.04795 -12.049  < 2e-16 ***
#   Fed         -0.41536    0.06968  -5.961 2.51e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 4817.5  on 3871  degrees of freedom
# Residual deviance: 4781.8  on 3870  degrees of freedom
# AIC: 4785.8





