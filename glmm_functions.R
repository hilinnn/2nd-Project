####glmm functions
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
  
  l <- matrix(data = NA, ncol = 5, nrow = 0)
  
  ns <- length(levels(data$Sleeper))
  
  if (fix_eff == "WashedStatus" & rand_eff == "Sleeper"){
    WS_levels <- levels(data$WashedStatus)
    nWS <- length(levels(data$WashedStatus))
    
    
    for (i in 1:nLoc) {
      for (j in 1:nWS){
        
        t <- data %>%
          subset(Location == Loc_levels[i] & WashedStatus == WS_levels[j]) %>% 
          dplyr::select(Count)
        t <- as.data.frame(t)
        
        len <- nrow(t)
        
        k <- matrix(data = NA, ncol = 5, nrow = len)
        
        k[,5] <- t[,1]         
        
        k[,2] <- rep(paste(Loc_levels[i],"x",WS_levels[j]),len)
        
        a <- predict(mod, data.frame(Sleeper = factor(levels(data$Sleeper), levels = levels(data$Sleeper)),
                                     Location = factor(rep(Loc_levels[i],6), levels = Loc_levels),
                                     WashedStatus = factor(rep(WS_levels[j],6), levels = WS_levels)),
                     type = "link", re.form = NULL)
        k[1:ns,3] <- exp(a)
        
        t <- data %>%
          filter(Location == Loc_levels[i] & WashedStatus == WS_levels[j]) %>%
          summarise(tv = mean(Count))
        k[1:ns,4] <- rep(as.numeric(t), ns)
        
        l <- rbind(l,k)
      }
    }
  } 
  
  
  if (fix_eff == "WashedStatus" & rand_eff == "Sleeper+Week"){
    WS_levels <- levels(data$WashedStatus)
    nWS <- length(levels(data$WashedStatus))
    nw <- length(levels(data$Week))
    n <- ns*nw
    for (i in 1:nLoc) {
      for (j in 1:nWS){
        
        t <- data %>%
          subset(Location == Loc_levels[i] & WashedStatus == WS_levels[j]) %>% 
          dplyr::select(Count)
        t <- as.data.frame(t)
        
        len <- nrow(t)
        
        k <- matrix(data = NA, ncol = 5, nrow = len)
        
        k[,5] <- t[,1]         
        
        k[,2] <- rep(paste(Loc_levels[i],"x",WS_levels[j]), len)
        
        a <- predict(mod, data.frame(Sleeper = factor(levels(data$Sleeper), levels = levels(data$Sleeper)),
                                     Week = factor(levels(data$Week), levels = levels(data$Week)),
                                     Location = factor(rep(Loc_levels[i],6), levels = Loc_levels),
                                     WashedStatus = factor(rep(WS_levels[j],6), levels = WS_levels)),
                     type = "link",  re.form = NULL)
        
        k[1:n,3] <- exp(a)
        
        t <- data %>%
          filter(Location == Loc_levels[i] & WashedStatus == WS_levels[j]) %>%
          summarise(tv = mean(Count))
        k[1:n,4] <- rep(as.numeric(t), n)
        
        l <- rbind(l,k)
      }
    }
  } 
  
  
  if (fix_eff == "Treatment" & rand_eff == "Sleeper"){
    Treatment_levels <- levels(data$Treatment)
    nT <- length(levels(data$Treatment))
    
    for (i in 1:nLoc) {
      for (j in 1:nT){
        
        t <- data %>%
          subset(Location == Loc_levels[i] & Treatment == Treatment_levels[j]) %>% 
          dplyr::select(Count)
        t <- as.data.frame(t)
        
        len <- nrow(t)
        
        k <- matrix(data = NA, ncol = 5, nrow = len)
        
        k[,5] <- t[,1]
        
        k[,2] <- rep(paste(Loc_levels[i],"x",Treatment_levels[j]),len)
        
        a <- predict(mod, data.frame(Sleeper = factor(levels(data$Sleeper), levels = levels(data$Sleeper)),
                                     Location = factor(rep(Loc_levels[i],6), levels = Loc_levels),
                                     Treatment = factor(rep(Treatment_levels[j],6), levels = Treatment_levels)),
                     type = "link", re.form = NULL)
        k[1:ns,3] <- exp(a)
        
        t <- data %>%
          filter(Location == Loc_levels[i] & Treatment == Treatment_levels[j]) %>%
          summarise(tv = mean(Count))
        k[1:ns,4] <- rep(as.numeric(t), ns)
        
        
        l <- rbind(l,k)
      }
    }
  } 
  
  
  if (fix_eff == "Treatment" & rand_eff == "Sleeper+Week"){
    Treatment_levels <- levels(data$Treatment)
    nT <- length(levels(data$Treatment))
    nw <- length(levels(data$Week))
    n <- ns*nw
    for (i in 1:nLoc) {
      for (j in 1:nT){
        
        t <- data %>%
          subset(Location == Loc_levels[i] & Treatment == Treatment_levels[j]) %>% 
          dplyr::select(Count)
        t <- as.data.frame(t)
        
        len <- nrow(t)
        
        k <- matrix(data = NA, ncol = 5, nrow = len)
        
        k[,5] <- t[,1]
        
        a <- predict(mod, data.frame(Sleeper = factor(levels(data$Sleeper), levels = levels(data$Sleeper)),
                                     Week = factor(levels(data$Week), levels = levels(data$Week)),
                                     Location = factor(rep(Loc_levels[i],6), levels = Loc_levels),
                                     Treatment = factor(rep(Treatment_levels[j],6), levels = Treatment_levels)),
                     type = "link", re.form = NULL)
        a <- exp(a)
        
        k[,2] <- rep(paste(Loc_levels[i],"x",Treatment_levels[j]),len)
        
        k[1:n,3] <- a
        
        t <- data %>%
          filter(Location == Loc_levels[i] & Treatment == Treatment_levels[j]) %>%
          summarise(tv = mean(Count))
        k[1:n,4] <- rep(as.numeric(t), n)
        
        
        l <- rbind(l,k)
      }
    }
  } 
  
  
  l[,1] <- rep(model_name, nrow(l))
  colnames(l) <- c("Model", "Category", "Predicted.value", "True.Mean", "True.value")
  l <- as.data.frame(l)
  
  l[,3] <- round(as.double(l[,3]),2)
  l[,4] <- round(as.double(l[,4]),2)
  l[,5] <- as.double(l[,5])
  
  return(l)
  
}

