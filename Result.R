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
bf_num_loc_Rand <- glmer(Fed~Total.Loc+ Location + WashedStatus + Total.Loc * WashedStatus + (1|Hut)
                         + (1|marker)  + (1|Sleeper), data = mor_fed, family =  binomial("logit"))
ss <- getME(bf_num_loc_Rand,c("theta","fixef"))
bf_num_loc_Rand <- update(bf_num_loc_Rand,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                        optCtrl=list(maxfun=3e5)))
summary(bf_num_loc_Rand)
##AIC: 3161.1
##Var: 1.824310(observational) 0.007548(Hut) 0.433516(Sleeper)

write.csv(tidy(bf_num_loc_Rand), "Blood feeding best model.csv")


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
bf_quant$WashedStatus <-  relevel(as.factor(bf_quant$WashedStatus), ref = "UTN")
  ggplot(bf_quant, aes(color = Location, fill = Location))+
  geom_point(aes(x = Total.Loc, y = Bloodfed))+
  facet_grid(vars(Location), vars(WashedStatus))+
  labs(title = "Blood-feeding correponding to number of mosquitoes",
       x = "Number of mosquitoes", y = "Blood-feeding rate")+
  geom_smooth(aes(x = Total.Loc, y = Fed),formula = y~x,method = glm, method.args= list(family="binomial"))+
  theme_bw()

ggsave("Blood feeding status by number of mosquitoes.jpeg", device = jpeg,
       width = 8, height = 5.5)




####Mortality

###Model
mor_fed_num_ms <- glmer(Dead~Fed + Location + Treatment + Location * Treatment + Total.Loc + Total.Loc*Treatment +
                          (1 | marker)+ (1 | Sleeper), data = mor_fed, family = binomial("logit"))
ss <- getME(mor_fed_num_ms,c("theta","fixef"))
mor_fed_num_ms <- update(mor_fed_num_ms,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                      optCtrl=list(maxfun=5e5)))
summary(mor_fed_num_ms)
#AIC: 3819.9
#Var: 1.578 obs 0.206 Sleeper 

write.csv(tidy(mor_fed_num_ms), "Mortality n blood-fed n number best model.csv")



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


###Number of mosquitoes
m_mos %>%
  dplyr::select(c("Village","Date","Treatment","Location", "Mortality", "Insecticide",
                    "Total","WashedStatus","Sleeper","marker","Nets", "Hut")) %>%
  right_join(mor_fed, by=c("Village","Date","Treatment","Location", "Insecticide",
                         "Total","WashedStatus","Sleeper","marker","Nets", "Hut")) %>%
  ggplot(aes(color = Nets, fill = Nets))+
  geom_point(aes(x = Total.Loc, y = Mortality))+
  facet_grid(vars(Location), vars(Treatment))+
  labs(title = "Mortality correponding to number of mosquitoes",
       x = "Number of mosquitoes", y = "Mortality")+
  geom_smooth(aes(x = Total.Loc, y = Dead),formula = y~x,method = glm, method.args= list(family="binomial"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12))

ggsave("Mortality corresponding number of mosquitoes.jpeg", device = jpeg)


###Blood feeding rate










