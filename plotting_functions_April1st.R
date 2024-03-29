library(RColorBrewer)
library(cowplot)
library(ggplot2)
library(dplyr)
library(reshape2)

#Color scheme to be used
cb <- brewer.pal(n = 7, name = "Set3")
#display.brewer.pal('Set3', n=7)

#Plotting theme (feel free to modify!)
themeJDC <- 
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white",color = 'black'),
        panel.border = element_rect(colour = "black" ,fill = NA),
        legend.background = element_blank(),
        legend.key = element_blank(), axis.text = element_text(size = 10),
        axis.title = element_text(size=11.5),
        legend.text = element_text(size = 10.5))

# Usually I make figures containing 6 panels from these data. The top row shows nightly data from 3 arms.
# Often I choose to look at the control arm, and 1 ITN, both unwashed & washed. 
# Stacked bar charts are constructed, depicting mosquitoes in one of 4 states: 
#             Alive + Fed, Alive + Unfed, Dead + Fed, Dead + Unfed

# On the bottom row, I first include a summary panel, which just aggregates the bar charts in the top row,
# and also calculated the % of mosquitoes that are blood fed. In this panel, there is an option to calculate deterrence:
# in many hut trials, one finds fewer mosquitoes in huts containing ITNs, compared to those containing untreated nets
# However, this is not always the case, so there is an option to remove deterrence. When I don't show deterrence, 
# I prefer to remove the legend from this panel, which means we need to include a legend in one of the top panels. 
# The last two panels show the fraction of mosquitoes killed each night, to show the variation. 
# Data pts are coloured by hut, if this info is available

################################################################################################
########## 1. Function for plotting the 3 panels on the top row  ###############################
################################################################################################

# Function arguments: arm = trial arm, as labelled in the dataset. 'arm_title' = Your choice for the title of this panel
# I prefer to have a common range on the y axis for all 3 of these panels. This is done by selecting the value of 'mx' 
#mx <- max(df[df$treatment=='C'|df$treatment=='N1u'|df$treatment=='N1w',]$total)

# Setting 'leg_end = 1' adds a legend, and 'legX' & 'legY' determine the position (both in the range [0,1])

# Current issues: (i) Pie chart & legend (if active) can obscure some bars. How to automate positioning?
# (ii) For trials with >1 arms with the same treatment, the data pts may need separating 
# (i.e. can't plot two bars on the same day) 
# (iii) Sometimes it's nice to replace 'Study Day' with the actual month/date. 
# One can do this manually, but it's fiddly 
# (i.e. add 'scale_x_continuous(breaks = c(3,19), labels = c('November','December'))')
# (iv) Pie chart labels can be v close together if certain segments < 1%. I've added some random jitter to improve,
# so you can re-run if labels overlap

pnel1 <- function(dataa = df, arm = 'C', arm_title = 'Control', mx = mx,
                  leg_end = 0, legX = 0.8, legY = 0.8){
  dataCB5 <- dplyr::select(dataa, c('day','treatment','unf_live','unf_dead','bf_live','bf_dead'))
  
  minn <- min(dataCB5$day)
  maxx <- max(dataCB5$day)
  
  dataCB5m <- melt(dataCB5, id.vars = c('day','treatment'))
  
  levels(dataCB5m$variable) <- c('Unfed Alive','Unfed Dead','Fed Alive','Fed Dead')
  
  dataCB5m$variable <- ordered(dataCB5m$variable, 
                               levels = c('Unfed Dead','Fed Dead', 'Unfed Alive','Fed Alive'))
  
  MH_C <- ggplot(dataCB5m[dataCB5m$treatment== arm ,], 
                 aes(fill=variable, y=value, x=day)) + themeJDC + #+ facet_wrap(~treat) 
    geom_bar(position="stack", stat="identity") + ylab('Number of mosquitoes caught per night') +# ylim(c(0,68)) + 
    xlab('Study Day') + scale_fill_manual(name = 'Status', values = c(cb[5],cb[3],cb[6],cb[4])) + ylim(c(0,mx)) +
    theme(legend.position = 'none') + #, axis.title.x = element_blank(), axis.text.x = element_text(size = 11.5)) +
    #scale_x_continuous(breaks = c(3,19), labels = c('November','December'))# + ggtitle('Untreated Control') 
    scale_x_continuous(limits = c(minn, maxx))
  MH_C
  if(leg_end > 0){
    MH_C <- MH_C + theme(legend.position = c(legX,legY))
  }
  
  #Pie chart
  dfc <- data.frame(
    group = c("Unfed Dead", "Fed Dead", "Unfed Alive","Fed Alive"),
    value = c(sum(dataCB5[dataCB5$treatment== arm ,]$unf_dead), 
              sum(dataCB5[dataCB5$treat== arm ,]$bf_dead),
              sum(dataCB5[dataCB5$treatment== arm ,]$unf_live),
              sum(dataCB5[dataCB5$treat== arm ,]$bf_live))
  )
  head(dfc)
  dfc$group <- ordered(dfc$group, 
                       levels = c('Unfed Dead','Fed Dead', 'Unfed Alive','Fed Alive'))
  #dfC, add percentages?
  dfc$valuePC <- paste0(round(100*dfc$value/sum(dfc$value)),'%') #set to '' if equal to "0%"?
  #dfc[dfc$valuePC=='0%',]$valuePC <- ''# Only works if true for at least one element
  dfc$loc <- sum(dfc$value) - cumsum(dfc$value) + 0.67*(dfc$value)
  dfc$x <- rnorm(dim(dfc)[1],1.2,0.04) #random by default??
  
  insetC <- ggplot(dfc, aes(x="", y=value, fill=group))+
    geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + themeJDC + xlab('') + ylab('') +
    scale_fill_manual(name = 'Status', values = c(cb[5],cb[3],cb[6],cb[4])) + 
    theme(axis.ticks = element_blank(), legend.position = 'none', axis.text = element_blank(),
          plot.title = element_text(size = 11.5)) + ggtitle( arm_title ) +
    geom_text(aes(x=x,y=loc, label = valuePC), size = 3, color = 'white')
  insetC
  
  MH_C_i <-
    ggdraw() +
    draw_plot(MH_C) +
    draw_plot(insetC, x = 0.1, y = .56, width = .42, height = .42)
  MH_C_i
}

#pnel1(dataa = df, arm = 'C', arm_title = 'Control', mx=mx)
#pnel1(dataa = df, arm = 'N1u', arm_title = 'ITN (Unwashed)', mx=mx)
#pnel1(dataa = df, arm = 'N1w', arm_title = 'ITN (Washed)', mx=mx)

################################################################################################
########## 2. Nightly variation in mortality (optional hut info)  ##############################
################################################################################################

# Some datasets do not provide data on which hut/sleeper pertains to each data point. 
# If info on hut is not available, set 'hut_info <- 0'

# Current issues: (i) If 0 mosquitoes enter a hut on a particular night, data point isn't plotted. 
# I think this is ok, but could confuse others?

#hut_info <- 1

error_bar_prop <- function(dataa = df, arm = 'C', arm_title = 'Control', hut_info = 1){
  dataa$prop <- dataa$tot_dead / dataa$total
  dataa$ci1 <- 0
  dataa$ci2 <- 0
  #dataa$sd <- 0
  for(i in 1:length(dataa$prop)){
    if(dataa$total[i]>0){
      dataa$ci1[i] <- binom.test(dataa$tot_dead[i], dataa$total[i])$conf.int[1]
      dataa$ci2[i] <- binom.test(dataa$tot_dead[i], dataa$total[i])$conf.int[2]
      #dataa$sd <- dataa$total[i] * dataa$prop * (1 - dataa$prop) #mmm think this is variance??
    }
  }
  minn <- min(dataa$day)
  maxx <- max(dataa$day)
  if(hut_info==1){
    ggplot() + geom_errorbar(data = dataa[dataa$treatment== arm ,],
                             aes(x=day, y = prop, ymin = ci1, ymax = ci2), alpha = .35, width = 0) +
      geom_point(data = dataa[dataa$treatment== arm ,], 
                 aes(x=day, y = prop, size=total, color = factor(hut))) + #guides(size=FALSE) +
      ylab("Proportion of mosquitoes killed") + ggtitle( arm_title) +
      xlab("Study Day") + themeJDC + scale_y_continuous(breaks=c(0,.5,1)) + # + labs(color = "Hut")
      theme(legend.position = 'none') + scale_x_continuous(limits = c(minn,maxx))
  }else{
    ggplot() + geom_errorbar(data = dataa[dataa$treatment== arm ,],
                             aes(x=day, y = prop, ymin = ci1, ymax = ci2), alpha = .35, width = 0) +
      geom_point(data = dataa[dataa$treatment== arm ,], 
                 aes(x=day, y = prop, size=total), color = 'magenta') + #guides(size=FALSE) +
      ylab("Proportion of mosquitoes killed") + ggtitle( arm_title) +
      xlab("Study Day") + themeJDC + scale_y_continuous(breaks=c(0,.5,1)) + # + labs(color = "Hut")
      theme(legend.position = 'none') + scale_x_continuous(limits = c(minn,maxx))
  }
}
#error_bar_prop(dataa = df, arm = 'N1u', arm_title = 'ITN (Unwashed)')
#error_bar_prop(dataa = df, arm = 'N1w', arm_title = 'ITN (Washed)')

################################################################################################
########## 3. Summary panel, with optional deterrence   ########################################
################################################################################################

# Here we summarise the upper panels, and include info on blood feeding
# Additionally, we can add info on deterrence

#However, deterrence only of interest if there're more mosquitoes in huts with untreated nets (compared to those with ITNs)
#Check:
#sum(df[df$treatment == 'C',]$total) - sum(df[df$treatment == 'N1u',]$total)
#sum(df[df$treatment == 'C',]$total) - sum(df[df$treatment == 'N1w',]$total)
#Normally, I only set 'deterr = 1' if both the above quantities are positive

# Current issues: (i) Legend is a squeeze to fit in, saved pdf has to be quite big to accommodate!
# (ii) Above calculation for deterrence only works if there're the same no. of data pts in each arm. But could be
# modified, e.g. by using the mean. I think the same is true inside the function- percentages need to be adjusted to allow for
# different numbers of data pts
# (iii) Code in the function is v messy, I need to tidy!

bfi <- function(dataa = df, arm1 = 'C', arm2 = 'N1u', arm3 = 'N1w', deterr = 0,
                arm_label1 = 'Control', arm_label2 = 'ITN Unwashed', arm_label3 = 'ITN Washed'){
  if(deterr==0){
    #dfc dfVw, dfVu
    #
    dfc <- data.frame(
      group = c("Unfed Dead", "Fed Dead", "Unfed Alive","Fed Alive"),
      value = c(sum(dataa[dataa$treatment==arm1,]$unf_dead), sum(dataa[dataa$treatment==arm1,]$bf_dead),
                sum(dataa[dataa$treatment==arm1,]$unf_live), sum(dataa[dataa$treatment==arm1,]$bf_live))
    )
    head(dfc)
    dfc$group <- ordered(dfc$group, 
                         levels = c('Unfed Dead','Fed Dead', 'Unfed Alive','Fed Alive'))
    #add percentages?
    dfc$valuePC <- paste0(round(100*dfc$value/sum(dfc$value)),'%') #set to '' if equal to "0%"?
    dfc$loc <- sum(dfc$value) - cumsum(dfc$value) + 0.67*(dfc$value)
    #
    dfVu <- data.frame(
      group = c("Unfed Dead", "Fed Dead", "Unfed Alive","Fed Alive"),
      value = c(sum(dataa[dataa$treatment==arm2,]$unf_dead), sum(dataa[dataa$treatment==arm2,]$bf_dead),
                sum(dataa[dataa$treatment==arm2,]$unf_live), sum(dataa[dataa$treatment==arm2,]$bf_live))
    )
    head(dfVu)
    dfVu$group <- ordered(dfVu$group, 
                          levels = c('Unfed Dead','Fed Dead', 'Unfed Alive','Fed Alive'))
    #dfC, add percentages?
    dfVu$valuePC <- paste0(round(100*dfVu$value/sum(dfVu$value)),'%')
    dfVu$loc <- sum(dfVu$value) - cumsum(dfVu$value) + 0.67*(dfVu$value)
    #
    dfVw <- data.frame(
      group = c("Unfed Dead", "Fed Dead", "Unfed Alive","Fed Alive"),
      value = c(sum(dataa[dataa$treatment==arm3,]$unf_dead), sum(dataa[dataa$treatment==arm3,]$bf_dead),
                sum(dataa[dataa$treatment==arm3,]$unf_live),sum(dataa[dataa$treatment==arm3,]$bf_live))
    )
    head(dfVw)
    dfVw$group <- ordered(dfVw$group, 
                          levels = c('Unfed Dead','Fed Dead', 'Unfed Alive','Fed Alive'))
    #dfC, add percentages?
    dfVw$valuePC <- paste0(round(100*dfVw$value/sum(dfVw$value)),'%')
    dfVw$loc <- sum(dfVw$value) - cumsum(dfVw$value) + 0.67*(dfVw$value)
    #
    dfc$net <- 'C'
    dfc$valuePC2 <- dfc$value / sum(dfc$value)
    dfVu$net <- 'Vu'
    dfVu$valuePC2 <- dfVu$value / sum(dfVu$value)
    dfVw$net <- 'Vw'
    dfVw$valuePC2 <- dfVw$value / sum(dfVw$value)
    dfALL <- rbind(dfc,dfVu,dfVw)
    dfALL$group <- ordered(dfALL$group, 
                           levels = c('Fed Alive','Fed Dead','Unfed Dead','Unfed Alive'))
    #print(dfALL)
    sz <- 4.9 #text size of labels
    bfi_plot <- ggplot(dfALL, aes(x=net, fill = group, y=valuePC2)) + geom_bar(position="stack", stat="identity") + 
      scale_fill_manual(name = 'Status', values = c(cb[4],cb[3],cb[5],cb[6])) + themeJDC + coord_flip() + 
      # Indent title, to avoid overlap with panel label
      ggtitle('    Blood Fed per feeding attempt') + theme(legend.position = 'none', axis.ticks = element_blank(),
                                                           axis.title = element_blank(), axis.text = element_blank()) +
      annotate('text', x = 1.0, y = 0.2, label = arm_label1, size = sz) + 
      annotate('text', x = 2.0, y = 0.2, label = arm_label2, size = sz) + 
      annotate('text', x = 3.0, y = 0.2, label = arm_label3, size = sz) + 
      geom_rect(aes(xmin = 0.55, xmax = 1.45, 
                    ymin = 1 - dfALL[dfALL$net=='C' & dfALL$group=='Fed Alive',]$valuePC2 -
                      dfALL[dfALL$net=='C' & dfALL$group=='Fed Dead',]$valuePC2, ymax = 1.0), alpha = .0, color = 'black') + 
      geom_rect(aes(xmin = 1.55, xmax = 2.45, 
                    ymin = 1 - dfALL[dfALL$net=='Vu' & dfALL$group=='Fed Alive',]$valuePC2 -
                      dfALL[dfALL$net=='Vu' & dfALL$group=='Fed Dead',]$valuePC2, ymax = 1.0), alpha = .0, color = 'black') +
      geom_rect(aes(xmin = 2.55, xmax = 3.45, 
                    ymin = 1 - dfALL[dfALL$net=='Vw' & dfALL$group=='Fed Alive',]$valuePC2 -
                      dfALL[dfALL$net=='Vw' & dfALL$group=='Fed Dead',]$valuePC2, ymax = 1.0), alpha = .0, color = 'black') +
      annotate('text', x = 1, y = 1 - 0.5*(dfALL[dfALL$net=='C' & dfALL$group=='Fed Alive',]$valuePC2 +
                                             dfALL[dfALL$net=='C' & dfALL$group=='Fed Dead',]$valuePC2), size = sz,
               label = paste0(round(100*((dfALL[dfALL$net=='C' & dfALL$group=='Fed Alive',]$valuePC2 +
                                            dfALL[dfALL$net=='C' & dfALL$group=='Fed Dead',]$valuePC2))),'%')) +
      annotate('text', x = 2, y = min(0.9,1 - 0.5*(dfALL[dfALL$net=='Vu' & dfALL$group=='Fed Alive',]$valuePC2 +
                                                     dfALL[dfALL$net=='Vu' & dfALL$group=='Fed Dead',]$valuePC2)), size = sz,
               label = paste0(round(100*((dfALL[dfALL$net=='Vu' & dfALL$group=='Fed Alive',]$valuePC2 +
                                            dfALL[dfALL$net=='Vu' & dfALL$group=='Fed Dead',]$valuePC2))),'%')) +
      annotate('text', x = 3, y = min(0.9,1 - 0.5*(dfALL[dfALL$net=='Vw' & dfALL$group=='Fed Alive',]$valuePC2 +
                                                     dfALL[dfALL$net=='Vw' & dfALL$group=='Fed Dead',]$valuePC2)), size = sz,
               label = paste0(round(100*((dfALL[dfALL$net=='Vw' & dfALL$group=='Fed Alive',]$valuePC2 +
                                            dfALL[dfALL$net=='Vw' & dfALL$group=='Fed Dead',]$valuePC2))),'%'))
    bfi_plot
  }else{
    #Or, incorporate deterence in the bfi panel?
    dfc2 <- data.frame(
      group = c("Unfed Dead", "Fed Dead", "Unfed Alive","Fed Alive",'Deterred'),
      value = c(sum(dataa[dataa$treatment==arm1,]$unf_dead), sum(dataa[dataa$treatment==arm1,]$bf_dead),
                sum(dataa[dataa$treatment==arm1,]$unf_live),sum(dataa[dataa$treatment==arm1,]$bf_live),0)
    )
    head(dfc2)
    dfc2$net <- 'C'
    dfc2$valuePC2 <- dfc2$value / sum(dfc2$value)
    
    dfVu2 <- data.frame(
      group = c("Unfed Dead", "Fed Dead", "Unfed Alive","Fed Alive",'Deterred'),
      value = c(sum(dataa[dataa$treatment==arm2,]$unf_dead), sum(dataa[dataa$treatment==arm2,]$bf_dead),
                sum(dataa[dataa$treatment==arm2,]$unf_live),sum(dataa[dataa$treatment==arm2,]$bf_live),
                sum(dataa[dataa$treatment==arm1,]$total) - sum(dataa[dataa$treatment==arm2,]$total))
    )
    head(dfVu2)
    dfVu2$net <- 'Vu'
    dfVu2$valuePC2 <- dfVu2$value / sum(dfVu2$value)
    
    dfVw2 <- data.frame(
      group = c("Unfed Dead", "Fed Dead", "Unfed Alive","Fed Alive",'Deterred'),
      value = c(sum(dataa[dataa$treatment==arm3,]$unf_dead), sum(dataa[dataa$treatment==arm3,]$bf_dead),
                sum(dataa[dataa$treatment==arm3,]$unf_live),sum(dataa[dataa$treatment==arm3,]$bf_live),
                sum(dataa[dataa$treatment==arm1,]$total) - sum(dataa[dataa$treatment==arm3,]$total))
    )
    head(dfVw2)
    dfVw2$net <- 'Vw'
    dfVw2$valuePC2 <- dfVw2$value / sum(dfVw2$value)
    
    #Bind together
    dfALL2 <- rbind(dfc2,dfVu2,dfVw2)
    dfALL2$group <- ordered(dfALL2$group, 
                            levels = c('Fed Alive','Fed Dead','Unfed Dead','Unfed Alive','Deterred'))
    #print(dfALL2)
    sz <- 4.9 #text size of labels
    bfi_plot2 <- ggplot(dfALL2, aes(x=net, fill = group, y=valuePC2)) + geom_bar(position="stack", stat="identity") + 
      scale_fill_manual(name = '', values = c(cb[4],cb[3],cb[5],cb[6],cb[7]), guide = guide_legend(reverse = TRUE)) +
      themeJDC + coord_flip() + 
      # Indent title, to avoid overlap with panel label
      ggtitle('    Blood Fed per feeding attempt') + theme(legend.position = 'bottom', axis.title.x = element_blank(),
                                                           axis.ticks = element_blank(), axis.text = element_blank())+#,
      #axis.title = element_blank()) +
      annotate('text', x = 1.0, y = 0.2, label = arm_label1, size = sz) + 
      annotate('text', x = 2.0, y = 0.2, label = arm_label2, size = sz) + 
      annotate('text', x = 3.0, y = 0.2, label = arm_label3, size = sz) + xlab('') +
      #scale_y_continuous(limits = c(0,6)) +
      geom_rect(aes(xmin = 0.55, xmax = 1.45, 
                    ymin = 1 - dfALL2[dfALL2$net=='C' & dfALL2$group=='Fed Alive',]$valuePC2 -
                      dfALL2[dfALL2$net=='C' & dfALL2$group=='Fed Dead',]$valuePC2, ymax = 1.0), fill = NA, color = 'black') + 
      geom_rect(aes(xmin = 1.55, xmax = 2.45, 
                    ymin = 1 - dfALL2[dfALL2$net=='Vu' & dfALL2$group=='Fed Alive',]$valuePC2 -
                      dfALL2[dfALL2$net=='Vu' & dfALL2$group=='Fed Dead',]$valuePC2, ymax = 1.0), fill = NA, color = 'black') +
      geom_rect(aes(xmin = 2.55, xmax = 3.45, 
                    ymin = 1 - dfALL2[dfALL2$net=='Vw' & dfALL2$group=='Fed Alive',]$valuePC2 -
                      dfALL2[dfALL2$net=='Vw' & dfALL2$group=='Fed Dead',]$valuePC2, ymax = 1.0), fill = NA, color = 'black') +
      annotate('text', x = 1, y = 1 - 0.5*(dfALL2[dfALL2$net=='C' & dfALL2$group=='Fed Alive',]$valuePC2 +
                                             dfALL2[dfALL2$net=='C' & dfALL2$group=='Fed Dead',]$valuePC2), size = sz,
               label = paste0(round(100*((dfALL2[dfALL2$net=='C' & dfALL2$group=='Fed Alive',]$valuePC2 +
                                            dfALL2[dfALL2$net=='C' & dfALL2$group=='Fed Dead',]$valuePC2))),'%')) +
      annotate('text', x = 2, y = min(0.96,1 - 0.5*(dfALL2[dfALL2$net=='Vu' & dfALL2$group=='Fed Alive',]$valuePC2 +
                                                      dfALL2[dfALL2$net=='Vu' & dfALL2$group=='Fed Dead',]$valuePC2)), size = sz,
               label = paste0(round(100*((dfALL2[dfALL2$net=='Vu' & dfALL2$group=='Fed Alive',]$valuePC2 +
                                            dfALL2[dfALL2$net=='Vu' & dfALL2$group=='Fed Dead',]$valuePC2))),'%')) +
      annotate('text', x = 3, y = min(0.96,1 - 0.5*(dfALL2[dfALL2$net=='Vw' & dfALL2$group=='Fed Alive',]$valuePC2 +
                                                      dfALL2[dfALL2$net=='Vw' & dfALL2$group=='Fed Dead',]$valuePC2)), size = sz,
               label = paste0(round(100*((dfALL2[dfALL2$net=='Vw' & dfALL2$group=='Fed Alive',]$valuePC2 +
                                            dfALL2[dfALL2$net=='Vw' & dfALL2$group=='Fed Dead',]$valuePC2))),'%'))
    bfi_plot2
  }
}
#bfi()
#Note how the percentages change, once deterrence is turned on (denominator changes)
#bfi(deterr = 1)

#Note: if deterr = 1, you'd need a legend ('leg_end = 1') in the top row (I often choose top right panel)
#Here, ITN isn't specified in the labels. But could change to e.g. 'IG2 (Unwashed)'
# plot_grid(pnel1(dataa = df, arm = 'C', arm_title = 'Control', mx=mx),
#           pnel1(dataa = df, arm = 'N1u', arm_title = 'ITN (Unwashed)', mx=mx),
#           pnel1(dataa = df, arm = 'N1w', arm_title = 'ITN (Washed)', mx=mx, leg_end = 0), 
#           bfi(deterr = 1),
#           error_bar_prop(dataa = df, arm = 'N1u', arm_title = 'ITN (Unwashed)'),
#           error_bar_prop(dataa = df, arm = 'N1w', arm_title = 'ITN (Washed)'),
#           nrow=2, labels = c('a','b','c','d','e','f'))
# Saved figure may need to be quite big, to fit everything in!!
#ggsave('Saved_figure.pdf',height = 12.0, width = 17.0)

#Do we need regression output? E.g. for extra pie chart?
# library(lme4)
# 
# df$observation <- factor(formatC(1:nrow(df), flag="0", width=3))
# 
# fit <-
#   glmer(
#     cbind(tot_dead, total - tot_dead) ~
#       treatment + (1 | hut) + (1 | sleeper) + (1 | observation),
#     family = binomial, data = df,  control = 
#       glmerControl(optimizer = "optimx", calc.derivs = FALSE,
#                    optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
# summary(fit)
