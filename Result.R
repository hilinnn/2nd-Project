#####Store the function of the models and graaphs included in the result part
library(lme4);library(tidyverse);library(DHARMa);library(lubridate);library(MASS)
library(merTools);library(ggpubr);library(ggpmisc);library(fitdistrplus);library(broom)


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



###Temperature
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

bf_num_loc_Rand_nw <- glmer(Fed~Total.Loc+ Location + WashedStatus  + (1|Hut)
                            + Total.Loc * Location+ (1|marker)  + (1|Sleeper), 
                            data = mor_fed, family =  binomial("logit"))
ss <- getME(bf_num_loc_Rand_nw,c("theta","fixef"))
bf_num_loc_Rand_nw <- update(bf_num_loc_Rand_nw,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                              optCtrl=list(maxfun=3e5)))
summary(bf_num_loc_Rand_nw)
##AIC: 3156.9
##Var: 1.734571(observational) 0.005949(Hut) 0.415454(Sleeper)


write.csv(tidy(bf_num_loc_Rand_nw), "Blood feeding best model.csv")






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
# bf_quant <- bf_mos%>%
#   dplyr::select(c("Village","Date","Treatment","Location", "Bloodfed", "Insecticide",
#                   "Total","WashedStatus","Sleeper","marker","Nets", "Hut")) %>%
#   right_join(mor_fed, by=c("Village","Date","Treatment","Location", "Insecticide",
#                            "Total","WashedStatus","Sleeper","marker","Nets", "Hut"))
# bf_quant <- cbind(bf_quant, pred_bf)
# 
# bf_pred <- bf_quant %>%
#   group_by(WashedStatus, Location, Total.Loc) %>%
#   mutate(sd = sqrt(var(pred_bf)),
#          lwr = pred_bf - 1.96*sd,
#          upr = pred_bf +1.96*sd)
# 
# bf_quant$WashedStatus <-  relevel(as.factor(bf_quant$WashedStatus), ref = "UTN")
# ggplot(bf_quant, aes(color = Location, fill = Location))+
#   geom_point(aes(x = Total.Loc, y = Bloodfed))+
#   facet_grid(vars(Location), vars(WashedStatus))+
#   labs(title = "Blood-feeding correponding to number of mosquitoes",
#        x = "Number of mosquitoes", y = "Blood-feeding rate")+
#   geom_smooth(aes(x = Total.Loc, y = pred_bf), method = "glm", col = "black")+
#   theme_bw()
# 
# ggsave("Blood feeding status by number of mosquitoes.jpeg", device = jpeg,
#        width = 8, height = 5.5)

#####Mortality against temperature predictions and plots

max(mor_fed$Total.Loc)
#52
mor_fed %>%
  count(marker) %>%
  filter(n == max(n))
#133
mor_fed %>%
  count(Sleeper) %>%
  filter(n == max(n))
#Te.S4

mor_fed %>%
  count(Hut) %>%
  filter(n == max(n))
#Te.C3

mor_fed %>%
  count(WashedStatus) %>%
  filter(n == max(n))
#unwashed
WashedStatus_levels <- levels(mor_fed$WashedStatus)

bf_df <-  data.frame(Location  = character(),
                    WashedStatus = character(),
                    Total.Loc = double(),
                    Sleeper= character(),
                    marker = character(),
                    Hut = character())
for (i in 1:3){
  for (j in 1:3){
    df_pred <- data.frame(Location = rep(Loc_levels[j],1000),
                          Total.Loc = rep(seq(0,55,0.5), length.out = 1000),
                          WashedStatus = rep(WashedStatus_levels[i],length.out = 1000),
                          marker = as.character(rep(133, 1000)),
                          Sleeper = rep("Te.S4", length.out =1000),
                          Hut = rep("Te.C3", length.out =1000))
    bf_df <- rbind(bf_df,df_pred)
  }
}

bf_pred <- predictInterval(bf_num_loc_Rand_nw, bf_df, type = "probability",level = 0.95, 
                                 n.sims = 1000,.parallel = FALSE)

bf_pred_plot <- cbind(bf_df,bf_pred)

bf_pred_plot$WashedStatus <- relevel(as.factor(bf_pred_plot$WashedStatus), ref = "UTN")

ggplot(bf_pred_plot, aes(x=Total.Loc, y = fit))+
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5)+
  geom_line(aes(color = Location)) +
  labs(y = "Predicted blood-feeding rate", x = "Number of mosquitoes")+
  facet_grid(vars(Location),vars(WashedStatus))

ggsave("blood-feeding rate n counts.jpeg", device = "jpeg")






####Mortality

###Model
mf_mos_temp$Treatment <- relevel(mf_mos_temp$Treatment, ref = "UTN")
standard_mor <- glm(Dead~Treatment, data = mor_fed_temp,
                      family = binomial("logit"))
summary(standard_mor)
##AIC: 4410.3

standard_mor_mix <- glmer(Dead~Treatment + (1|marker) + (1|Hut), data = mor_fed_temp,
                    family = binomial("logit"))
summary(standard_mor_mix)
##AIC: 4093.3

mor_fed_num_ms_diu_temp_range <- glmer(Dead~Fed + Location + Treatment + Location * Treatment + Total.Loc + Total.Loc*Location +
                                         diurnal_temp_range + (1 | marker)+ (1 | Sleeper)+ (1|Hut), data = mor_fed_temp, 
                                       family = binomial("logit"))
ss <- getME(mor_fed_num_ms_diu_temp_range,c("theta","fixef"))
mor_fed_num_ms_diu_temp_range <- update(mor_fed_num_ms_diu_temp_range,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                                                    optCtrl=list(maxfun=5e5)))
summary(mor_fed_num_ms_diu_temp_range)
###AIC: 3817.2
###Var: 1.39494(observational) 0.19681(Sleeper) 0.07232(Hut)

write.csv(tidy(mor_fed_num_ms_diu_temp_range), "Mortality best model.csv")



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

# a <- pred_mor %>%
#   dplyr::select(c("Treatment", "Location", "mean", "Fed","Nets")) %>%
#   distinct() %>%
#   filter(Fed==0)
# 
# b <- pred_mor %>%
#   dplyr::select(c("Treatment", "Location", "mean", "Fed","Nets")) %>%
#   distinct() %>%
#   filter(Fed==1)
  
pred_mor_mean_line <- left_join(a,b, by = c("Treatment", "Location","Nets"))


mean(mor_fed_temp$diurnal_temp_range)
#7.669743
median(mor_fed_temp$diurnal_temp_range)
#7.911625

mor_bf <-  data.frame(Treatment = character(),
                    Location  = character(),
                    Fed = double(),
                    Total.Loc = double(),
                    Sleeper= character(),
                    marker = character(),
                    Hut = character(),
                    diurnal_temp_range = double())
for (i in 1:6){
  for (j in 1:3){
    df <- data.frame(Treatment = rep(Treatment_levels[i],1000),
                          Location = rep(Loc_levels[j],1000),
                          Total.Loc = rep(15.5, 1000),
                          Fed = rep(0:1, each=500),
                          diurnal_temp_range =  rep(7.8, length.out = 1000),
                          marker = as.character(rep(133, 1000)),
                          Sleeper = rep("Te.S4", length.out =1000),
                          Hut = rep("Te.C3", length.out =1000))
    mor_bf <- rbind(mor_bf,df)
  }
}

mor_bf_pred <- predictInterval(mor_fed_num_ms_diu_temp_range, mor_bf, type = "probability", 
                                 level= 0.95, .parallel = FALSE)

mor_bf_pred_plot <- cbind(mor_bf,mor_bf_pred)

mor_bf_pred_plot$Treatment <- relevel(as.factor(mor_bf_pred_plot$Treatment), ref = "UTN")

mor_bf_pred_mean <- mor_bf_pred_plot %>%
  group_by(Treatment, Location, Fed)%>%
  summarise(Mean = mean(fit),
            Lwr = mean(lwr),
            Upr = mean(upr))



ggplot(mf_mos, aes(color = Location))+
  geom_point(aes(x = Bloodfed, y = Mortality))+
  facet_grid(vars(Location), vars(Treatment))+
  labs(title = "Mortality correponding to blood-feeding",
       x = "Blood-feeding rate", y = "Mortality/Predicted mortality")+
  geom_errorbar(data = mor_bf_pred_mean, aes(x=Fed,ymin = Lwr, ymax = Upr), 
                col = "black", width= 0.3)+
  geom_line(data = mor_bf_pred_mean, aes(x=Fed, y = Mean), col = "black")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12))+
  scale_x_continuous(breaks = c(0,0.5,1))


ggsave("Mortality corresponding blood feeding.jpeg", device = jpeg)



#####Mortality against temperature predictions and plots
mean(mor_fed_temp$Fed)
#0.5121384
mean(mor_fed_temp$Total.Loc)
#15.50155
mor_fed_temp %>%
  count(marker) %>%
  filter(n == max(n))
#133
mor_fed_temp %>%
  count(Sleeper) %>%
  filter(n == max(n))
#Te.S4
mor_fed_temp %>%
  count(Hut) %>%
  filter(n == max(n))
#Te.C3

max(mor_fed_temp$diurnal_temp_range)

mor_temp_df <-  data.frame(Treatment = character(),
                    Location  = character(),
                    Fed = double(),
                    Total.Loc = double(),
                    Sleeper= character(),
                    marker = character(),
                    Hut = character(),
                    diurnal_temp_range = double())
for (i in 1:6){
  for (j in 1:3){
    df_pred <- data.frame(Treatment = rep(Treatment_levels[i],1000),
                       Location = rep(Loc_levels[j],1000),
                       Total.Loc = rep(15.5, 1000),
                       Fed = rep(0.5121384, 1000),
                       diurnal_temp_range =  rep(seq(0,10,0.01), length.out = 1000),
                       marker = as.character(rep(133, 1000)),
                       Sleeper = rep("Te.S4", length.out =1000),
                       Hut = rep("Te.C3", length.out =1000))
    mor_temp_df <- rbind(mor_temp_df,df_pred)
  }
}

mor_temp_pred <- predictInterval(mor_fed_num_ms_diu_temp_range, mor_temp_df, type = "probability", 
                                 level = 0.95,n.sims = 999,.parallel = FALSE)

mor_temp_pred_plot <- cbind(mor_temp_df,mor_temp_pred)

mor_temp_pred_plot$Treatment <- relevel(as.factor(mor_temp_pred_plot$Treatment), ref = "UTN")

ggplot(mor_temp_pred_plot, aes(x=diurnal_temp_range, y = fit))+
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5)+
  geom_line(aes(color = Location)) +
  labs(y = "Predicted mortality", x = "Diurnal temperature range (°C)")+
  facet_grid(vars(Location),vars(Treatment))+
  scale_x_continuous(breaks = c(0,2,4,6,8,10))
  
ggsave("Mortality vs temp range.jpeg", device = "jpeg")

# pred_temp <- cbind(mor_fed_temp, mor_pred_temp)
# pred_temp <- cbind(pred_temp, mor_reg[,"Mortality"])
# colnames(pred_temp)[26] <- "Mortality"
# 
# 
# ggplot(pred_temp, aes(x = diurnal_temp_range, col = Location))+
#   geom_point(aes(y=Mortality))+
#   geom_line(aes(y = fit))+
#   geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Location), alpha = 0.7)+
#   facet_grid(vars(Location), vars(Treatment))+
#   labs(y = "Mortality/predicted mortality", x = "Temperature range during the trial time")

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





