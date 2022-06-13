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

mor_reg <- gen_mo(m_mos)


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
  left_join(m_mos_df, by = c("Location", "Treatment", "Date"))%>%
  dplyr::select(starts_with(c("Village","Date","Treatment","Location", "Mortality",
                              "Total","Dead","dead","Hut","Sleeper","marker")))

m_mos$Location <- as.factor(m_mos$Location)

head(m_mos)
View(m_mos) 


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


####Run logistic regression for the mortality
###Fixed effect model


####Model 0
mor_0 <- glm(Mortality~1, data = m_mos, family = binomial("log"))
summary(mor_0)
##AIC: 578.8
mor_0_ran <- glmer(Mortality~(1|marker), 
                   data = m_mos, family = binomial("logit"))
summary(mor_0_ran)
##AIC: 560.5

mor_0_ran_id <- glmer(Mortality~(1|id), 
                      data = m_mos, family = binomial("logit"))
##AIC:560.8
##Singular fit


####Model 1
###Variables: Location and Treatment
mor_1 <- glm(Mortality~Location+Treatment, data = m_mos, family = binomial("logit"))
summary(mor_1)
##AIC: 522.57

mor_1_q <- glm(Mortality~Location+Treatment, data = m_mos, family = quasibinomial("logit"))
summary(mor_1_q)

###Variables: Location and Treatment with interactions
mor_2 <- glm(Mortality~Location+Treatment + Location*Treatment, 
             data = m_mos, family = binomial("logit"))
summary(mor_2)
##AIC: 533.29

mor_2_q <- glm(Mortality~Location+Treatment + Location*Treatment, 
               data = m_mos, family = quasibinomial("logit"))
summary(mor_2_q)

##Warnings in all Binomial family logit regression: non-integer #successes in a binomial glm!

###Mixed effect model
##Random effect: individual random effect (per hut per night)
mor_1_ran <- glmer(Mortality~Location+Treatment + (1|marker), 
                   data = m_mos, family = binomial("logit"))
summary(mor_1_ran)
##AIC: 495.3
##Variance of the random effect: 0.002516 (small)


mor_2_ran <- glmer(Mortality~Location+Treatment +  Location*Treatment + (1|marker), 
                   data = m_mos, family = binomial("logit"))
summary(mor_2_ran)
##AIC: 501.7
##Variance of the random effect: 0.0004968 (small)

##Additional warnings: failed to converge

##Tried to solve the warnings but unsuccessful
m_mos <- m_mos %>%
  mutate(id = seq(1, nrow(m_mos),1))
View(m_mos)



model_1_rand <- glmer.nb(formula = Count~Location+(1|id), data = n_mos)
summary(model_1_rand)
###AIC: 3659.1
plot(model_1_rand)

###Plot the residuals
plot(model_1_rand_resid)
model_1_rand_resid <- simulateResiduals(fittedModel = model_1_rand, n = nrow(n_mos)*5, refit = TRUE)
plot(model_1_rand_resid)
testDispersion(model_1_rand_resid)

rs_1 <- simulateResiduals(fittedModel = model_1)
a <- testOutliers(rs_1, type = 'bootstrap')
plot(a)
glm.diag.plots(rs_1)


sum(n_mos$Count == 0)