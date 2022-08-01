####glmms 
library(lme4);library(tidyverse);library(DHARMa);library(lubridate);library(MASS);library(merTools);library(ggpubr);
library(ggpmisc);library(nnet);library(cowplot)

source("~/Desktop/Second project/R/glmm_functions.R")


a <- predict(model_5_5, data.frame(Sleeper = factor(levels(n_mos$Sleeper), levels = levels(n_mos$Sleeper)),
                             Week = factor(levels(n_mos$Week), levels = levels(n_mos$Week)),
                             Location = factor(rep(Loc_levels[1],6), levels = Loc_levels),
                             Treatment = factor(rep(Treatment_levels[1],6), levels = Treatment_levels)),
             type = "link", re.form = NULL)

Treatment_levels <- levels(n_mos$Treatment)

pred_WS[1,4]
is.list(pred_WS)

###1st round trial data in Tengrela 
View(Tengrela_R1A_rm) 
Tengrela_R1A_rm$Hut <- as.factor(Tengrela_R1A_rm$Hut)
Tengrela_R1A_rm$Sleeper <- as.factor(Tengrela_R1A_rm$Sleeper)


####################################################################
######Frequency plot of the number of mosquitoes by location########
####################################################################
###Modify data for glm of categorical data
n_mos <- as_tibble(Tengrela_R1A_rm)%>% 
  dplyr::select(starts_with(c("Date","Total","Treatment", "marker","Hut","Sleeper")))
View(n_mos)

##Check the Total column
sum(rowSums(n_mos[,3:5]) != n_mos[,2]) #=0

n_mos <- n_mos %>%
  gather("Location", "Count", Total.Room:Total.Ver)%>%
  ungroup()%>%
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
         Location = case_when(Location == "Total.Net" ~ "Net",
                              Location == "Total.Room" ~ "Room",
                              Location == "Total.Ver" ~ "Veranda")) %>%
  mutate(Week = ceiling((yday(Date) - min(yday(Date))+1)/7))


n_mos$WashedStatus <- as.factor(n_mos$WashedStatus)
n_mos$Insecticide <- as.factor(n_mos$Insecticide)
n_mos$Location <- as.factor(n_mos$Location)
n_mos$Week <- as.factor(n_mos$Week)

n_mos <- n_mos %>%
  mutate(Nets = case_when(Treatment == "IG2.unwash" ~ "IG2",
                          Treatment == "P3.unwash" ~ "P3",
                          Treatment == "P2.unwash" ~ "P2",
                          Treatment == "IG2.wash" ~ "IG2",
                          Treatment == "P3.wash" ~ "P3",
                          Treatment == "UTN" ~ "UTN"))

n_mos$Nets <- as.factor(n_mos$Nets)
head(n_mos)


###Plot of the entire dataset
ggplot(n_mos, aes(x = Date, y = Total))+
  geom_col(fill = 'Orange')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1.5),
        plot.title = element_text(hjust = 0.5, size = 15))+
  labs(title = 'Number of mosquitoes per night')+
  scale_x_date(date_breaks = "1 week")

ggsave("Frequency plot of the number of mosquitoes.jpeg")


###Plot by Treatment
ggplot(n_mos, aes(x = Date, y = Count))+
  geom_col(aes(fill = Location))+
  facet_wrap(vars(Treatment), ncol = 2)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1.5),
        plot.title = element_text(hjust = 0.5, size = 15))+
  labs(title = 'Number of mosquitoes per night by Treatment')+
  scale_x_date(date_breaks = "1 week")
ggsave("Frequency plot of the number of mosquitoes.jpeg")

##Plot by Treatment (Proportion)
prop_n_mos <- n_mos %>%
  mutate(percent = Count/Total)
View(prop_n_mos)

ggplot(prop_n_mos, aes(x = Date, y = percent))+
  geom_col(aes(fill = Location))+
  facet_wrap(vars(Treatment), ncol = 2)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1.5),
        plot.title = element_text(hjust = 0.5, size = 15))+
  labs(title = 'Proportion of number of mosquitoes per night by Treatment')+
  scale_x_date(date_breaks = "1 week")
ggsave("Frequency plot of the proportion of mosquitoes.jpeg")


###Barplot of the number of mosquitoes by Treatment
maxTotal <- max(n_mos$Total)

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


###Barplot of the whole dataset
n_mos %>%
  distinct(Date, Treatment, .keep_all = TRUE) %>%
  group_by(Date) %>%
  mutate(Tot = sum(Total)) %>%
  distinct(Date, Tot, .keep_all = TRUE) %>%
  ggplot(aes(x = Tot))+
  geom_histogram(fill = 'Dark green' ,binwidth = 1, center = 0)+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  labs(title = 'Number of mosquitoes per night')


ggsave("Histogram of the number of mosquitoes.jpeg")

###Barplot of average number of mosquito in each location by Treatment (With/without errorbar)
n_mos %>%
  group_by(Treatment, Location) %>%
  summarise(Avg = mean(Count), max = max(Count), min = min(Count)) %>%
  ggplot(aes(x = Location, y = Avg, color = Location))+
    geom_col(aes(fill= Location))+
    #geom_errorbar(aes(x=Location, ymin=min, ymax= max), color = 'steelblue')+
    facet_wrap(vars(Treatment))+
    labs(title = "Mean and range of number of mosquitoes in each location by treatment")+
    theme(plot.title = element_text(hjust = 0.5, size = 12.5),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          strip.text = element_text(size = 10))

ggsave("Average of number of mosquitoes by treatment and location.jpeg",
       width = 8, height = 5.5)


####Compare number of mosquitoes in each location by different treatment
###Barplot of average number of mosquito in each location by Treatment (With/without errorbar)
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


####Boxplot of the number of mosquitoes by location and treatment and dotplot of the average number
n_mos %>%
  ggplot(aes(x = Location, y = Count, color = Location))+
  geom_boxplot(aes( fill= Location), alpha = 0.5)+
  stat_summary(fun.y = mean, geom="point", shape=23, size=4, color="red", fill="red") +
  facet_wrap(vars(Treatment))+
  labs(title = "Number of mosquitoes in each location by treatment",
       y = "Number of mosquitoes")+
  theme(plot.title = element_text(hjust = 0.5, size = 12.5),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        strip.text = element_text(size = 10))

ggsave("Number of mosquitoes by treatment and location.jpeg",
       width = 8, height = 5.5)



n_mos$Treatment <- relevel(n_mos$Treatment, ref = "UTN")




####################################################################
##################GLM for number of mosquitoes by location##########
####################################################################

####Basic model without any covariate
model_0 <- glm.nb(formula = Count~1, data = n_mos)
summary(model_0)
##AIC: 3702.4

###Fitting multinomial model to test the impact of treatment to the location

##Chagne the reference category
n_mos$Treatment <- relevel(n_mos$Treatment, ref = "UTN")

multimod_Tr_Loc <- multinom(Location ~ Treatment+Count, data = n_mos)
# # weights:  24 (14 variable)
# initial  value 711.900763 
# iter  10 value 682.074194
# iter  20 value 681.618248
# iter  20 value 681.618247
# iter  20 value 681.618247
# final  value 681.618247 
# converged

summary(multimod_Tr_Loc)
z_multimod_Tr_Loc <- summary(multimod_Tr_Loc)$coefficients/summary(multimod_Tr_Loc)$standard.errors
p_multimod_Tr_Loc <- (1-pnorm(abs(z_multimod_Tr_Loc),0,1))*2


multimod_Loc_Tot <- multinom(Location ~ Total, data = n_mos) 
summary(multimod_Loc_Tot)


multimod_Loc_Count <- multinom(Location ~ Count, data = n_mos) 
summary(multimod_Loc_Count)

multimod_Loc_Cou_tot <- multinom(Location ~ Count + Total, data = n_mos) 
summary(multimod_Loc_Cou_tot)

z_model_tot_Loc <- summary(multimod_Loc_Cou_tot)$coefficients/summary(multimod_Loc_Cou_tot)$standard.errors
p_model_tot_Loc <- (1-pnorm(abs(z_model_tot_Loc),0,1))*2


model_tot_l <- glm(Count~Location + Total + Location*Total,  data = n_mos)
summary(model_tot_l)

rn_mos$Nets <- relevel(n_mos$Nets, ref = "UTN")
multimod_Ne_Loc <- multinom(Location ~ Nets+Count, data = n_mos)
# # weights:  18 (10 variable)
# initial  value 711.900763 
# iter  10 value 682.558178
# final  value 681.945688 
# converged

summary(multimod_Ne_Loc)
z_multimod_Ne_Loc <- summary(multimod_Ne_Loc)$coefficients/summary(multimod_Ne_Loc)$standard.errors
p_multimod_Ne_Loc <- (1-pnorm(abs(z_multimod_Ne_Loc),0,1))*2


model_1_1 <- glm.nb(formula = Count~Location, data = n_mos)
summary(model_1_1)
model_1_1$coefficients
###AIC: 3648.4


####Data to model for variables other than Location
n_mos_wt_Loc <- n_mos %>%
  distinct(Date,Treatment,Total, .keep_all = TRUE)

model_1_2 <- glm.nb(formula = Total~WashedStatus, data = n_mos_wt_Loc)
summary(model_1_2)
#AIC: 3694.7
#AIC: 1626.8

model_1_3 <- glm.nb(formula = Total~Treatment, data = n_mos_wt_Loc)
summary(model_1_3)
#AIC: 3699.6
#AIC: 1631.6

model_1_4 <- glm.nb(formula = Total~Insecticide, data = n_mos_wt_Loc)
summary(model_1_4)
#AIC: 3698.5
#AIC: 1630.8

model_1_5 <- glm.nb(formula = Count~Nets, data = n_mos)
summary(model_1_5)
#AIC: 3700.2 0.7726
#AIC: 1632.5


#####Boxplot: 6 vs 4
a <- ggplot(n_mos_wt_Loc, aes(y=Total))+
  geom_boxplot(aes(x=Treatment, fill = Nets))+
  labs(title = "Boxplot of number of mosquitoes by different ITNs")+
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        legend.position = "none")

b <- ggplot(n_mos_wt_Loc, aes(y=Total, fill = Nets))+
  geom_boxplot(aes(x=Nets))+
  labs(title = "Boxplot of number of mosquitoes by types of Nets")+
  theme(plot.title = element_text(size = 12, hjust = 0.5))

a+b

ggsave("Number of mosquitoes found in the hut by different treatment or nets.jpeg", 
       device = "jpeg", width = 10, height = 9)

library(patchwork)

model_2_1 <- glm.nb(formula = Count~Location + WashedStatus, data = n_mos)
summary(model_2_1)
#AIC: 3638

model_2_2 <- glm.nb(formula = Count~Location + Treatment, data = n_mos)
summary(model_2_2)
#AIC: 3643.7

model_2_3 <- glm.nb(formula = Count~Location + Insecticide, data = n_mos)
summary(model_2_3)
#AIC: 3643.8

model_2_4 <- glm.nb(formula = Count~Location + Nets, data = n_mos)
summary(model_2_4)
#AIC: 3645.2


model_3_1 <- glm.nb(formula = Count~Location + WashedStatus + Location*WashedStatus, data = n_mos)
summary(model_3_1)
#AIC: 3638

model_3_2 <- glm.nb(formula = Count~Location + Treatment + Location*Treatment, data = n_mos)
summary(model_3_2)
#AIC: 3650.8

model_3_3 <- glm.nb(formula = Count~Location + Insecticide + Location*Insecticide , data = n_mos)
summary(model_3_3)
#AIC: 3647.3

model_3_4 <- glm.nb(formula = Count~Location + Nets + Location * Nets, data = n_mos)
summary(model_3_4)
#AIC: 3652.2


# Model_1s <- list(model_1_1, model_1_2,model_1_3,model_1_4, model_2_1, model_2_2, model_2_3,
#                  model_3_1, model_3_2, model_3_3)
# names(Model_1s) <- c("model_1_1", "model_1_2","model_1_3","model_1_4", "model_2_1", "model_2_2", 
#                      "model_2_3", "model_3_1", "model_3_2", "model_3_3")
# lapply(Model_1s, coefficients)
# summary(model_2_2)

###########################################################
############Include random effect of Sleeper###############
###########################################################

model_4_1 <- glmer.nb(formula = Count~Location + (1|Sleeper), data = n_mos)
summary(model_4_1)
#AIC: 3646.2


model_4_1_hut <- glmer.nb(formula = Count~Location + (1|Hut), data = n_mos)
summary(rePCA(model_4_1_hut))

View(select(n_mos, c("Treatment", "Hut", "Sleeper", "Location","Date")))

model_4_2 <- glmer.nb(formula = Count~Location + WashedStatus + (1|Sleeper)  , data = n_mos)
summary(model_4_2)
#AIC: 3635.8
#Var: 0.02138

model_4_3 <- glmer.nb(formula = Count~Treatment + Location + (1|Sleeper)  , data = n_mos)
summary(model_4_3)
#AIC: 3641.2
#Var: 0.02259

model_4_4 <- glmer.nb(formula = Count~WashedStatus + Location + WashedStatus * Location 
                      +(1|Sleeper), data = n_mos)
summary(model_4_4)
#AIC: 3642.9
#Var: 0.02133


model_4_5 <- glmer.nb(formula = Count~Location + Treatment + Location*Treatment+ (1|Sleeper), data = n_mos)
summary(model_4_5)
#AIC: 3648.3
#Var: 0.02247


###########################################################
############Include observational random effect############
###########################################################

model_5_1 <- glmer.nb(formula = Count~Location + WashedStatus+ (1|marker), data = n_mos)
summary(model_5_1)
#AIC: 3633.5
##Var: 0.103



model_5_2 <- glmer.nb(formula = Count~Location + Treatment + (1|marker), data = n_mos)
summary(model_5_2)
#AIC: 3639.1
##Var: 0.1046

model_5_3 <- glmer.nb(formula = Count~Location + WashedStatus+ Location * WashedStatus+ (1|marker), data = n_mos)
summary(model_5_3)
#AIC: 3640.5
##Var: 0.1047

model_5_4 <- glmer.nb(formula = Count~Location + Treatment + Location* Treatment + (1|marker), data = n_mos)
model_5_4 <- update(model_5_4, control = glmerControl(optimizer = "bobyqa"))
summary(model_5_4)
#AIC: 3646.7
##Var: 0.1042




####Obtain the coefficients and 95% CI of the selected models
Location_WS <- getcoefsde(model_4_2, "Location+WashedStatus")
Location_Net <- getcoefsde(model_4_3, "Location+Net")
LocationxWS <- getcoefsde(model_4_4, "Location+WashedStatus+Location*WashedStatus")
LocationxNet <- getcoefsde(model_4_5, "Location+Net+Location*Net")



predictInterval(model_4_4, data.frame(Sleeper = factor(levels(n_mos$Sleeper), levels = levels(n_mos$Sleeper)),
                                      Location = factor(rep(Loc_levels[i],6), levels = Loc_levels),
                                      WashedStatus = factor(rep(WS_levels[j],6), levels = WS_levels)),
                type = "linear.prediction", level =  0.95)



Rand_Sleeper <- rbind(Location_WS,Location_Net,LocationxWS,LocationxNet)

ggplot(Rand_Sleeper, aes( y=Point_est, colour = Var))+
  geom_point(aes(x=Var))+
  geom_errorbar(aes(x=Var, ymax = upper, ymin = lower))+
  facet_wrap(vars(Model), ncol = 2)+
  labs(title = "Coefficients and 95% CI in each categorical variable: Models with random effect of Sleeper",
       y = "Point estimates", x = "Variable")+
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.text.x = element_text(angle = 90, hjust =0.95, vjust = 0.2))

ggsave("Coefficients of models with random effect of Sleeper.jpeg", device = 'jpeg',
       height = 8.42, width = 15)



###Plot the predicted categorical mean and actual mean
pred_WS <- pred_num("WashedStatus + Location ", 
                    data = n_mos, mod = model_4_2, fix_eff = "WashedStatus", rand_eff = "Sleeper")
pred_Net <- pred_num("Treatment + Location", 
                    data = n_mos, mod = model_4_3, fix_eff = "Treatment", rand_eff = "Sleeper")
predxWS <- pred_num("WashedStatus + Location + WashedStatus * Location", 
                 data = n_mos, mod = model_4_4, fix_eff = "WashedStatus", rand_eff = "Sleeper")
predxNet <- pred_num("Treatment + Location + Treatment * Location", 
                    data = n_mos, mod = model_4_5, fix_eff = "Treatment", rand_eff = "Sleeper")

pred_p <- rbind(pred_WS, pred_Net, predxWS, predxNet)
View(pred_p)

ggplot(pred_p, aes(x = True.Value, y=Predicted.value))+
  geom_point(color = 'steelblue') + facet_wrap(~Model) +
  geom_smooth(formula = y~ x, method = "lm", se = FALSE, color = 'orange') +
  stat_poly_eq(formula = y ~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, color = 'orange')+
  geom_abline(slope = 1, intercept = 0, linetype = 2, size = 1)+
  labs(title = "Predicted number of mosquitoes and the best fit line from models with random effect of Sleeper",
       x = "True value", y = "Predicted number of mosquitoes")+
  theme(plot.title = element_text(hjust = 0.5, size = 12.5),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        strip.text = element_text(size = 10))

ggsave("Model comparisons with random effect of Sleeper.jpeg", device = 'jpeg',
       width = 9, height = 6.5)

####################################################################
############Include random effect of Sleeper and week###############
####################################################################

model_6_1 <- glmer.nb(formula = Count~Location + (1|marker)+ (1|Sleeper) + (1|Week) , data = n_mos)
summary(model_6_1)
#AIC: 3643.4

model_6_2 <- glmer.nb(formula = Count~Location + WashedStatus + (1|marker) + (1|Sleeper)  + (1|Week) , data = n_mos)
ss <- getME(model_6_2,c("theta","fixef"))
model_6_2 <- update(model_6_2,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                optCtrl=list(maxfun=4e5)))
summary(model_6_2)
#AIC: 3634.2
#Var: 0.06792 (observational) 0.01850 (Sleeper) 0.01133 (Week)


model_6_3 <- glmer.nb(formula = Count~Treatment + Location + (1|marker) + (1|Sleeper)  + (1|Week) , data = n_mos)
ss <- getME(model_6_3,c("theta","fixef"))
model_6_3 <- update(model_6_3,start=ss,control=glmerControl(optimizer="bobyqa",
                                                            optCtrl=list(maxfun=4e5)))
summary(model_6_3)
#AIC: 3639.6
#Var: 0.06838 (observational) 0.01944 (Sleeper) 0.01154 (Week)


model_6_4 <- glmer.nb(formula = Count~WashedStatus + Location + WashedStatus * Location 
                      + (1|marker) +(1|Sleeper) + (1|Week), data = n_mos)
ss <- getME(model_6_4,c("theta","fixef"))
model_6_4 <- update(model_6_4,start=ss,control=glmerControl(optimizer="bobyqa",
                                                            optCtrl=list(maxfun=4e5)))
summary(model_6_4)
#AIC: 3641.1
#Var: 0.06919 (observational) 0.01840 (Sleeper) 0.01154 (Week)


model_6_5 <- glmer.nb(formula = Count~Location + Treatment + Location*Treatment+ 
                         (1|marker)+ (1|Sleeper)  + (1|Week), data = n_mos)
ss <- getME(model_6_5,c("theta","fixef"))
model_6_5 <- update(model_6_5,start=ss,control=glmerControl(optimizer="bobyqa",
                                                            optCtrl=list(maxfun=4e5)))
summary(model_6_5)
#AIC: 3646.8
#Var: 0.06639 (observational) 0.01996 (Sleeper) 0.01270 (Week)


###Export the model fit to a csv file
write.csv(rbind(tidy(model_5_1), tidy(model_6_5)), "Number of mosquitoes (Location and Treatment with interaction).csv")



####Obtain the coefficients and 95% C.I of the selected models
Location_WS <- getcoefsde(model_5_2, "Location+WashedStatus")
Location_Net <- getcoefsde(model_5_3, "Location+Net")
LocationxWS <- getcoefsde(model_5_4, "Location+WashedStatus+Location*WashedStatus")
LocationxNet <- getcoefsde(model_5_5, "Location+Net+Location*Net")

Rand_Sleeper_Week <- rbind(Location_WS,Location_Net,LocationxWS,LocationxNet)


ggplot(Rand_Sleeper_Week, aes( y=Point_est, colour = Var))+
  geom_point(aes(x=Var))+
  geom_errorbar(aes(x=Var, ymax = upper, ymin = lower))+
  facet_wrap(vars(Model), ncol = 2)+
  labs(title = "Coefficients and 95% CI in each categorical variable: Models with random effect of Sleeper and week",
       y = "Point estimates", x = "Variable")+
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.text.x = element_text(angle = 90, hjust =0.95, vjust = 0.2))

ggsave("Coefficients of models with random effect of Sleeper and Week.jpeg", device = 'jpeg',
       height = 8.42, width = 15)



###Plot the predicted categorical mean and actual mean
pred_WS_W <- pred_num("WashedStatus + Location", 
                    data = n_mos, mod = model_5_2, fix_eff = "WashedStatus", rand_eff = "Sleeper+Week")
pred_Net_W <- pred_num("Treatment + Location", 
                     data = n_mos, mod = model_5_3, fix_eff = "Treatment", rand_eff = "Sleeper+Week")
predxWS_W <- pred_num("WashedStatus + Location + WashedStatus * Location", 
                    data = n_mos, mod = model_5_4, fix_eff = "WashedStatus", rand_eff = "Sleeper+Week")
predxNet_W <- pred_num("Treatment + Location + Treatment * Location", 
                     data = n_mos, mod = model_5_5, fix_eff = "Treatment", rand_eff = "Sleeper+Week")


pred_p_W <- rbind(pred_WS_W, pred_Net_W, predxWS_W, predxNet_W)
View(pred_p_W)

###Plot the predicted values from the best model and the corresponding true values
predxNet_W %>% 
  separate(Category, c("Location","Treatment"), sep = "x", remove = FALSE) %>%
  group_by(Category) %>%
  mutate(mean_pred = mean(Predicted.value)) %>%
  ungroup() %>%
  ggplot(aes(x = Model))+
  geom_boxplot(aes(y = True.value), fill = 'seagreen', alpha = 0.6)+
  geom_point(aes(y=mean_pred), color = 'orange', alpha = 0.3)+
  labs(title = "Best model estimates and actual data plot : Treatment + Location + Treatment x Location",
       y= "True/predicted number of mosquitoes")+
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+  
  facet_grid(vars(Location), vars(Treatment))


ggsave("Best model estimates and real data plot (Treatment and Location with interaction).jpeg",
       device = "jpeg", width = 12, height = 9)



####Use the predxNet_W to separate column for Location and Treatment
Describe_loc_tr <- predxNet_W %>%
  separate(Category, c("Location","Treatment"), sep = "x", remove = FALSE) %>%
  group_by(Category) %>%
  mutate(mean_pred = mean(Predicted.value)) %>%
  ungroup()


Describe_loc_tr <- as.data.frame(Describe_loc_tr)
Describe_loc_tr$Treatment <- as.character(Describe_loc_tr$Treatment)

View(Describe_loc_tr)

test <- as_tibble(Describe_loc_tr) %>%
  dplyr::mutate(Nets = case_when(Treatment == "IG2.unwash" ~ "IG2",
                          Treatment == "P3.unwash" ~ "P3",
                          Treatment == "P2.unwash" ~ "P2",
                          Treatment == "IG2.wash" ~ "IG2",
                          Treatment == "P3.wash" ~ "P3",
                          Treatment == "UTN" ~ "UTN"))


# Describe_loc_tr$Nets <- as.factor(`Describe_loc_tr`$Nets)
head(test)




ggplot(Describe_loc_tr, aes(fill = Treatment))+
  geom_boxplot(aes(y=True.value, x=Model), alpha = 0.5)+
  stat_summary(aes(y=True.value, x=Model),fun = mean, geom="point", shape=22, size=6) +
  geom_point(aes(y=mean_pred, x=Model), fill = 'red', color = 'red',size = 3, shape = 23)+
  facet_grid(vars(Location), vars(Treatment))+
  labs(title = "Best model fit of the number of mosquitoes",
       y = "Number of mosquitoes")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'none')

ggsave("Best model fit of the number of mosquitoes.jpeg", device = 'jpeg',
       height = 6.5, width = 9)




ggplot(pred_p_W, aes(x = True.Mean, y = Predicted.value))+
  geom_point(color = 'steelblue') + facet_wrap(~Model) +
  geom_smooth(formula = y~ x, method = "lm", se = FALSE, color = 'orange') +
  geom_abline(slope = 1, intercept = 0, linetype = 2, size = 1)+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE,  color = 'orange')+
  geom_abline(slope = 1, intercept = 0, linetype = 2, size = 1)+
  labs(title = "Predicted number of mosquitoes and the best fit line from models with random effect of Sleeper and Week",
       x = "True mean", y = "Predicted number of mosquitoes")+
  theme(plot.title = element_text(hjust = 0.5, size = 12.5),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        strip.text = element_text(size = 10))

ggsave("Model comparisons with random effect of Sleeper and Week.jpeg", device = 'jpeg',
       width = 9, height = 6.5)


# model_rands <- list(model_4_1, model_4_2, model_4_3, model_4_4, model_4_5,
#                     model_5_1, model_5_2, model_5_3, model_5_4, model_5_5)
# names(model_rands) <- c("model_4_1", "model_4_2", "model_4_3", "model_4_4", "model_4_5",
#                         "model_5_1", "model_5_2", "model_5_3", "model_5_4", "model_5_5")
# View(lapply(model_rands, coefficients))



# ###Truncate the data of count equals to 0 from the model
# trun_data <- n_mos %>%
#   subset(Count >0)
# View(trun_data)
# model_1_trun <- glm.nb(formula = Count~Location, data = trun_data)
# summary(model_1_trun)
# ###AIC: 3115.5
# plot(model_1_trun)










# ####################################################################
# ####Frequency plot of the blood feeding of mosquitoes by location###
# ####################################################################
# bf_mos <- as_tibble(Tengrela_R1A_rm) %>% 
#   dplyr::select(starts_with(c("Date","Treatment", "marker","Hut","Sleeper","Fed","Unf","bf")))
# 
# 
# ###Plot by feeding status
# 
# ###Plot by all 4 status
# 
# 
# View(bf_mos)



