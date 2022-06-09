####glmms 
library(lme4)
library(tidyverse)
library(DHARMa)
library(lubridate)
library(MASS)
library(merTools)
library(ggpubr)
library(ggpmisc)


####Functions

###Obtain boxplots of the estimated coefficients
getcoefsde <- function(model,name){
  a <- sqrt(diag(vcov(model))) #standard error
  b <- as.data.frame(fixef(model)) #Coefficient 
  n <- nrow(b)
  coef_se <- matrix(data = NA, nrow = nrow(b), ncol = 5)
  colnames(coef_se) <- c("Model", "Var","Point_est","lower", "upper")
  coef_se[,1] <- rep(name, nrow(b))
  coef_se[,2] <- rownames(b)
  coef_se[,3] <- round(exp(b[,1]),2)
  coef_se[,4] <- round(exp(b[,1] - 1.96*a),2)
  coef_se[,5] <- round(exp(b[,1] + 1.96*a),2)
  coef_se <- as.data.frame(coef_se)
  
  coef_se$Model <- as.factor(coef_se$Model)
  coef_se$Var <- as.factor(coef_se$Var)
  coef_se$Point_est <- as.numeric(coef_se$Point_est)
  coef_se$lower <- as.numeric(coef_se$lower)
  coef_se$upper <- as.numeric(coef_se$upper)
  
  
  return(coef_se)
}


a <- predict(object = model_4_4, 
             newdata =data.frame(Sleeper = factor(levels(n_mos$Sleeper), levels = levels(n_mos$Sleeper)),
                             Location = factor(rep(Loc_levels[1],6), levels = Loc_levels),
                             WashedStatus = factor(rep(WS_levels[1],6), levels = WS_levels)),
             re.form = NULL, se.fit = TRUE)

pred_WS <- pred_num("WashedStatus + Location ", 
                    data = n_mos, mod = model_4_2, fix_eff = "WashedStatus", rand_eff = "Sleeper")
             
             
###Return a list of actual frequency and the estimated average number of mosquitoes

pred_num <- function(model_name, data, mod, fix_eff, rand_eff){
  Loc_levels <- levels(data$Location)
  nLoc <- length(Loc_levels)
  
  ns <- length(levels(data$Sleeper))
  
  if (fix_eff == "WashedStatus" & rand_eff == "Sleeper"){
    WS_levels <- levels(data$WashedStatus)
    nWS <- length(levels(data$WashedStatus))
    
    l <- matrix(data = NA, ncol = 4, nrow = 0)
    
    for (i in 1:nLoc) {
      for (j in 1:nWS){
        k <- matrix(data = NA, ncol = 4, nrow = ns)
        
        k[,2] <- rep(paste(Loc_levels[i],"x",WS_levels[j]),6)
        
        a <- predict(mod, data.frame(Sleeper = factor(levels(data$Sleeper), levels = levels(data$Sleeper)),
                                           Location = factor(rep(Loc_levels[i],6), levels = Loc_levels),
                                           WashedStatus = factor(rep(WS_levels[j],6), levels = WS_levels)),
                     type = "link", re.form = NULL)
        k[,3] <- exp(a)
        
        t <- data %>%
          filter(Location == Loc_levels[i] & WashedStatus == WS_levels[j]) %>%
          summarise(tv = mean(Count))
        k[,4] <- rep(as.numeric(t), 6)
        
        l <- rbind(l,k)
      }
    }
  } 
  
  
  if (fix_eff == "WashedStatus" & rand_eff == "Sleeper+Week"){
    WS_levels <- levels(data$WashedStatus)
    nWS <- length(levels(data$WashedStatus))
    nw <- length(levels(data$Week))
    for (i in 1:nLoc) {
      for (j in 1:nWS){
        k <- matrix(data = NA, ncol = 4, nrow = ns*nw)
        
        k[,2] <- rep(paste(Loc_levels[i],"x",WS_levels[j]),6)
        
        a <- predict(mod, data.frame(Sleeper = factor(levels(data$Sleeper), levels = levels(data$Sleeper)),
                                           Week = factor(levels(data$Week), levels = levels(data$Week)),
                                           Location = factor(rep(Loc_levels[i],6), levels = Loc_levels),
                                           WashedStatus = factor(rep(WS_levels[j],6), levels = WS_levels)),
                     type = "link",  re.form = NULL)
        k[,3] <- exp(a)
        
        t <- data %>%
          filter(Location == Loc_levels[i] & WashedStatus == WS_levels[j]) %>%
          summarise(tv = mean(Count))
        k[,4] <- rep(as.numeric(t), 6)
        
        l <- rbind(l,k)
      }
    }
  } 
  
  
  if (fix_eff == "Treatment" & rand_eff == "Sleeper"){
    Treatment_levels <- levels(data$Treatment)
    nT <- length(levels(data$Treatment))
    
    for (i in 1:nLoc) {
      for (j in 1:nN){
        k <- matrix(data = NA, ncol = 4, nrow = ns)
        
        k[,2] <- rep(paste(Loc_levels[i],"x",Treatment_levels[j]),6)
        
        a <- predict(mod, data.frame(Sleeper = factor(levels(data$Sleeper), levels = levels(data$Sleeper)),
                                           Location = factor(rep(Loc_levels[i],6), levels = Loc_levels),
                                           Treatment = factor(rep(Treatment_levels[j],6), levels = Treatment_levels)),
                     type = "link", re.form = NULL)
        k[,3] <- exp(a)
        
        t <- data %>%
          filter(Location == Loc_levels[i] & Treatment == Treatment_levels[j]) %>%
          summarise(tv = mean(Count))
        k[,4] <- rep(as.numeric(t), 6)
        
        l <- rbind(l,k)
      }
    }
  } 
  
  
  if (fix_eff == "Treatment" & rand_eff == "Sleeper+Week"){
    Treatment_levels <- levels(data$Treatment)
    nT <- length(levels(data$Treatment))
    nw <- length(levels(data$Week))
    
    for (i in 1:nLoc) {
      for (j in 1:nWS){
        k <- matrix(data = NA, ncol = 4, nrow = ns*nw)
        
        k[,2] <- rep(paste(Loc_levels[i],"x",WS_levels[j]),6)
        
        a <- predict(mod, data.frame(Sleeper = factor(levels(data$Sleeper), levels = levels(data$Sleeper)),
                                     Week = factor(levels(data$Week), levels = levels(data$Week)),
                                     Location = factor(rep(Loc_levels[i],6), levels = Loc_levels),
                                     Treatment = factor(rep(Treatment_levels[j],6), levels = Treatment_levels)),
                     type = "link", re.form = NULL)
        k[,3] <- exp(a)
        
        t <- data %>%
          filter(Location == Loc_levels[i] & Treatment == Treatment_levels[j]) %>%
          summarise(tv = mean(Count))
        k[,4] <- rep(as.numeric(t), 6)
        
        l <- rbind(l,k)
      }
    }
  } 

  
  l[,1] <- rep(model_name, nrow(l))
  colnames(l) <- c("Model", "Category", "Predicted.value", "True.Value")
  l <- as.data.frame(l)
  
  l[,3] <- round(as.double(l[,3]),2)
  l[,4] <- round(as.double(l[,4]),2)
  
  return(l)
}





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

View(n_mos)


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

ggplot(n_mos, aes(x = Total))+
  geom_histogram(aes(fill = Treatment), binwidth = 1, center = 0)+
  facet_wrap(vars(Treatment), ncol = 2)+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  labs(title = 'Number of mosquitoes per night by Treatment')+
  scale_x_continuous(breaks = seq(0, maxTotal, 10))

ggsave("Histogram of the number of mosquitoes by Treatment.jpeg",
       width = 8, height = 6)

###Barplot of the number of mosquitoes by Location
maxCount<- max(n_mos$Count)

ggplot(n_mos, aes(x = Count))+
  geom_histogram(aes(fill = Location), binwidth = 1, center = 0)+
  facet_wrap(vars(Location))+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  labs(title = 'Number of mosquitoes per night by Location')+
  scale_x_continuous(breaks = seq(0, maxCount, 10))

ggsave("Histogram of the number of mosquitoes by Location.jpeg",
       width = 8, height = 4)

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
    geom_errorbar(aes(x=Location, ymin=min, ymax= max), color = 'steelblue')+
    facet_wrap(vars(Treatment))+
    labs(title = "Mean and range number of mosquitoes in each location by treatment")+
    theme(plot.title = element_text(hjust = 0.5, size = 12.5),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          strip.text = element_text(size = 10))

ggsave("Average and range of number of mosquitoes by treatment and location.jpeg",
       width = 9, height = 6.5)
    



####################################################################
##################GLM for number of mosquitoes by location##########
####################################################################

####Basic model without any covariate
model_0 <- glm.nb(formula = Count~1, data = n_mos)
summary(model_0)
##3702.4

model_1_1 <- glm.nb(formula = Count~Location, data = n_mos)
summary(model_1_1)
model_1_1$coefficients
###AIC: 3648.4

model_1_2 <- glm.nb(formula = Count~WashedStatus, data = n_mos)
summary(model_1_2)
#AIC: 3694.7

model_1_3 <- glm.nb(formula = Count~Treatment, data = n_mos)
summary(model_1_3)
#AIC: 3699.6

model_1_4 <- glm.nb(formula = Count~Insecticide, data = n_mos)
summary(model_1_4)
#AIC: 3698.5



model_2_1 <- glm.nb(formula = Count~Location + WashedStatus, data = n_mos)
summary(model_2_1)
#AIC: 3638

model_2_2 <- glm.nb(formula = Count~Location + Treatment, data = n_mos)
summary(model_2_2)
#AIC: 3643.7

model_2_3 <- glm.nb(formula = Count~Location + Insecticide, data = n_mos)
summary(model_2_3)
#AIC: 3643.8


model_3_1 <- glm.nb(formula = Count~Location + WashedStatus + Location*WashedStatus, data = n_mos)
summary(model_3_1)
#AIC: 3638

model_3_2 <- glm.nb(formula = Count~Location + Treatment + Location*Treatment, data = n_mos)
summary(model_3_2)
#AIC: 3650.8

model_3_3 <- glm.nb(formula = Count~Location + Insecticide + Location*Insecticide + (1|marker), data = n_mos)
summary(model_3_3)
#AIC: 3647.3


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

predictInterval(model_4_1, newdata = )

model_4_1_hut <- glmer.nb(formula = Count~Location + (1|Hut), data = n_mos)
summary(rePCA(model_4_1_hut))

View(select(n_mos, c("Treatment", "Hut", "Sleeper", "Location","Date")))

model_4_2 <- glmer.nb(formula = Count~Location + WashedStatus + (1|Sleeper)  , data = n_mos)
summary(model_4_2)
#AIC: 3635.8


model_4_3 <- glmer.nb(formula = Count~Treatment + Location + (1|Sleeper)  , data = n_mos)
summary(model_4_3)
#AIC: 3641.2

model_4_4 <- glmer.nb(formula = Count~WashedStatus + Location + WashedStatus * Location 
                      +(1|Sleeper), data = n_mos)
summary(model_4_4)
#AIC: 3642.9


model_4_5 <- glmer.nb(formula = Count~Location + Treatment + Location*Treatment+ (1|Sleeper), data = n_mos)
summary(model_4_5)
#AIC: 3648.3





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

model_5_1 <- glmer.nb(formula = Count~Location + (1|Sleeper) + (1|Week) , data = n_mos)
summary(model_5_1)
#AIC: 3644.2

model_5_2 <- glmer.nb(formula = Count~Location + WashedStatus + (1|Sleeper)  + (1|Week) , data = n_mos)
summary(model_5_2)
#AIC: 3634.5

model_5_3 <- glmer.nb(formula = Count~Treatment + Location + (1|Sleeper)  + (1|Week) , data = n_mos)
summary(model_5_3)
#AIC: 3639.9

model_5_4 <- glmer.nb(formula = Count~WashedStatus + Location + WashedStatus * Location 
                      +(1|Sleeper) + (1|Week), data = n_mos)
summary(model_5_4)
#AIC: 3641.5

model_5_5 <- glmer.nb(formula = Count~Location + Treatment + Location*Treatment+ 
                        (1|Sleeper)  + (1|Week), data = n_mos)
summary(model_5_5)
#AIC: 3646.9



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

ggplot(pred_p_W, aes(x = True.Value, y=Predicted.value))+
  geom_point(color = 'steelblue') + facet_wrap(~Model) +
  geom_smooth(formula = y~ x, method = "lm", se = FALSE, color = 'orange') +
  geom_abline(slope = 1, intercept = 0, linetype = 2, size = 1)+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE,  color = 'orange')+
  geom_abline(slope = 1, intercept = 0, linetype = 2, size = 1)+
  labs(title = "Predicted number of mosquitoes and the best fit line from models with random effect of Sleeper and Week",
       x = "True value", y = "Predicted number of mosquitoes")+
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



####Model 1
###Variables: Location and Treatment
mor_1 <- glm(Mortality~Location+Treatment, data = m_mos, family = binomial("logit"))
summary(mor_1)
##AIC: 522.57

###Variables: Location and Treatment with interactions
mor_2 <- glm(Mortality~Location+Treatment + Location*Treatment, 
             data = m_mos, family = binomial("logit"))
summary(mor_1)
##AIC: 533.29

##Warnings in all Binomial family logit regression: non-integer #successes in a binomial glm!

###Mixed effect model
##Random effect: individual random effect (per hut per night)
mor_1_ran <- glmer(Mortality~Location+Treatment + (1|marker), 
                   data = m_mos, family = binomial("logit"))
summary(mor_1_ran)
##AIC: 495.3
##Variance of the random effect: 0.002516 (small)

mor_2_ran <- glmer(Mortality~Location+Treatment + + Location*Treatment + (1|id), 
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

###Find the relationship between the count and location and treatment
model_2 <- glm.nb(formula = Count~Location+Treatment, data = n_mos)
summary(model_2)
plot(model_2)

###Adding individual random effect to model 2
model_2_rand <- glmer.nb(formula = Count~Location+Treatment+(1|id), data = n_mos)
summary(model_2_rand)
###AIC: 3652.3

plot(model_2_rand)
###Plot the residuals
plot(simulateResiduals(fittedModel = model_2_rand, refit = TRUE))


###Adding individual random effect and interaction terms to model 2
model_2_rand_int <- glmer.nb(formula = Count~Location+Treatment+Location*Treatment+(1|id), data = n_mos)
##3 Warnings in failed to converge
summary(model_2_rand_int) 
###AIC: 3659
plot(model_2_rand_int)
###Plot the residuals
plot(simulateResiduals(fittedModel = model_2_rand_int, refit = F))





####################################################################
####Frequency plot of the blood feeding of mosquitoes by location###
####################################################################
bf_mos <- as_tibble(Tengrela_R1A_rm) %>% 
  dplyr::select(starts_with(c("Date","Treatment", "marker","Hut","Sleeper","Fed","Unf","bf")))


###Plot by feeding status

###Plot by all 4 status


View(bf_mos)



