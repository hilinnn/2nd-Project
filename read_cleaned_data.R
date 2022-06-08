source('plotting_functions_April1st.R')

dfW <- readRDS('cleaned_EHT_data_four_trials.rds')

#Separate into the 4 trials?
df1 <- dfW[dfW$Round==1 & dfW$Village=='Tengrela',]
df2 <- dfW[dfW$Round==2 & dfW$Village=='Tengrela',]
df3 <- dfW[dfW$Round==1 & dfW$Village=='Tiefora',]
df4 <- dfW[dfW$Round==2 & dfW$Village=='Tiefora',]

dim(df1)
table(df1$treatment) #nb. typos
#Fix typos
df1$treatment[df1$treatment=='Ig1.unwash'] <- 'IG1.unwash'
df1$treatment[df1$treatment=='Ig2.unwash'] <- 'IG2.unwash'
df1$treatment[df1$treatment=='Ig2.wash'] <- 'IG2.wash'

#Here Date is recorded as a string. For plotting, it is useful to convert date into a continuous sequence of integers
#Let's try it for Trial 3
#Warning: For Trial 1, this won't work very well, as the trial was carried out over two years (with a gap in the middle)

#?
vv <- c()
for(i in 1:(length(unique(df3$Date)))){
  vv <- c(vv,rep(i,6))
}
df3$day <- vv #?

#What are the different treatment Arms in Trial 3? Here, 'UTN' means untreated
table(df3$treatment)

#For the plots, it's useful to make the range plotted on the y axis consistent, across the top 3 panels
#Let's choose 3 arms to look at, and check the max number of mosquitoes 
mx <- max(df3[df3$treatment=='UTN'|df3$treatment=='IG2.unwash'|df3$treatment=='IG2.wash',]$total)
mx

plot_grid(pnel1(dataa = df3, arm = 'UTN', arm_title = 'Control', mx=mx),
                    pnel1(dataa = df3, arm = 'IG2.unwash', arm_title = 'IG2 (Unwashed)', mx=mx),
                     pnel1(dataa = df3, arm = 'IG2.wash', arm_title = 'IG2 (Washed)', mx=mx, leg_end = 1), 
                     bfi(data = df3, arm1 = 'UTN', arm2 = 'IG2.unwash', arm3 = 'IG2.wash', deterr = 0,
                         arm_label1 = 'Control', arm_label2 = 'IG2 Unwashed', arm_label3 = 'IG2 Washed'), #see file 'plotting_functions_April1st.R' for info on deterrence
                     error_bar_prop(dataa = df3, arm = 'IG2.unwash', arm_title = 'IG2 (Unwashed)'),
                     error_bar_prop(dataa = df3, arm = 'IG2.wash', arm_title = 'IG2 (Washed)'),
                     nrow=2, labels = c('a','b','c','d','e','f'))
# Saved figure may need to be quite big, to fit everything in!!
#ggsave('Saved_figure.pdf',height = 12.0, width = 17.0)









#Do we need regression output? E.g. for extra pie chart?
library(lme4)

df$observation <- factor(formatC(1:nrow(df), flag="0", width=3))

fit <-
  glmer(
    cbind(tot_dead, total - tot_dead) ~
      treatment + (1 | hut) + (1 | sleeper) + (1 | observation),
    family = binomial, data = df,  control = 
      glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                   optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
summary(fit)