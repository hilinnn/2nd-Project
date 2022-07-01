####
gen_mo <- function(data){
  out <- data.frame(Village = character(),
                    Date  = as.Date(character()),
                    Treatment = character(),
                    Location = character(),
                    Dead = integer(),
                    Hut= character(),
                    Sleeper= character(),
                    marker = integer(),
                    Nets = character(),
                    WashedStatus = character())
  data <- subset(data, Location == "Room")
  for (i in 1: nrow(data)){
    
    n_dr <- data[i, "Dead.Room"]
    n_ar <- data[i, "Total.Room"] - n_dr
    
    a <- cbind(data[rep(i, n_dr), c("Village", "Date", "Treatment", "Hut", "Sleeper", "marker","WashedStatus", "Insecticide","Nets")], data.frame("Location" = rep("Room", n_dr), "Dead" = rep(1, n_dr)))
    b <- cbind(data[rep(i, n_ar), c("Village", "Date", "Treatment", "Hut", "Sleeper", "marker","WashedStatus", "Insecticide","Nets")], data.frame("Location" = rep("Room", n_ar), "Dead" = rep(0, n_ar)))
    
    n_dn <- data[i, "Dead.Net"]
    n_an <- data[i, "Total.Net"] - n_dn
    
    c <- cbind(data[rep(i, n_dn), c("Village", "Date", "Treatment", "Hut", "Sleeper", "marker","WashedStatus", "Insecticide","Nets")], data.frame("Location" = rep("Net", n_dn), "Dead" = rep(1, n_dn)))
    d <- cbind(data[rep(i, n_an), c("Village", "Date", "Treatment", "Hut", "Sleeper", "marker","WashedStatus", "Insecticide","Nets")], data.frame("Location" = rep("Net", n_an), "Dead" = rep(0, n_an)))
    
    n_dv <- data[i, "Dead.Ver"]
    n_av <- data[i, "Total.Ver"] - n_dv
    
    e <- cbind(data[rep(i, n_dv), c("Village", "Date", "Treatment", "Hut", "Sleeper", "marker","WashedStatus", "Insecticide","Nets")], data.frame("Location" = rep("Veranda", n_dv), "Dead" = rep(1, n_dv)))
    f <- cbind(data[rep(i, n_av), c("Village", "Date", "Treatment", "Hut", "Sleeper", "marker","WashedStatus", "Insecticide","Nets")], data.frame("Location" = rep("Veranda", n_av), "Dead" = rep(0, n_av)))
    
    p <- rbind(a,b,c,d,e,f)
    
    out <- rbind(out, p)
    rm(list = c("a","b","c","d","e","f","p"))
  }
  return(out)
}



####
gen_bf <- function(data){
  out <- data.frame(Village = character(),
                    Date  = as.Date(character()),
                    Treatment = character(),
                    Location = character(),
                    Fed = integer(),
                    Hut= character(),
                    Sleeper= character(),
                    marker = integer(),
                    Nets = character(),
                    WashedStatus = character())
  data <- subset(data, Location == "Room")
  for (i in 1: nrow(data)){
    
    n_dr <- data[i, "Fed.Room"]
    n_ar <- data[i, "Total.Room"] - n_dr
    
    a <- cbind(data[rep(i, n_dr), c("Village", "Date", "Treatment", "Hut", "Sleeper", "marker","WashedStatus", "Insecticide","Nets")], data.frame("Location" = rep("Room", n_dr), "Fed" = rep(1, n_dr)))
    b <- cbind(data[rep(i, n_ar), c("Village", "Date", "Treatment", "Hut", "Sleeper", "marker","WashedStatus", "Insecticide","Nets")], data.frame("Location" = rep("Room", n_ar), "Fed" = rep(0, n_ar)))
    
    n_dn <- data[i, "Fed.Net"]
    n_an <- data[i, "Total.Net"] - n_dn
    
    c <- cbind(data[rep(i, n_dn), c("Village", "Date", "Treatment", "Hut", "Sleeper", "marker","WashedStatus", "Insecticide","Nets")], data.frame("Location" = rep("Net", n_dn), "Fed" = rep(1, n_dn)))
    d <- cbind(data[rep(i, n_an), c("Village", "Date", "Treatment", "Hut", "Sleeper", "marker","WashedStatus", "Insecticide","Nets")], data.frame("Location" = rep("Net", n_an), "Fed" = rep(0, n_an)))
    
    n_dv <- data[i, "Fed.Ver"]
    n_av <- data[i, "Total.Ver"] - n_dv
    
    e <- cbind(data[rep(i, n_dv), c("Village", "Date", "Treatment", "Hut", "Sleeper", "marker","WashedStatus", "Insecticide","Nets")], data.frame("Location" = rep("Veranda", n_dv), "Fed" = rep(1, n_dv)))
    f <- cbind(data[rep(i, n_av), c("Village", "Date", "Treatment", "Hut", "Sleeper", "marker","WashedStatus", "Insecticide","Nets")], data.frame("Location" = rep("Veranda", n_av), "Fed" = rep(0, n_av)))
    
    p <- rbind(a,b,c,d,e,f)
    
    out <- rbind(out, p)
    rm(list = c("a","b","c","d","e","f","p"))
  }
  return(out)
}

