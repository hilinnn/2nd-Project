####
gen_mo <- function(data){
  out <- data.frame(Village = character(),
                    Date  = as.Date(character()),
                    Treatment = character(),
                    Location = character(),
                    Dead = integer(),
                    Total = integer(),
                    Total.Loc = integer(),
                    Hut= character(),
                    Sleeper= character(),
                    marker = integer(),
                    Nets = character(),
                    WashedStatus = character(),
                    Mortality = double())
  data <- subset(data, Location == "Room")
  for (i in 1: nrow(data)){
    
    n_dr <- data[i, "Dead.Room"]
    n_ar <- data[i, "Total.Room"] - n_dr
    room <- data$Total.Room[i]

    a <- cbind(data[rep(i, n_dr), c("Village", "Date", "Treatment", "Hut", "Sleeper", "marker","WashedStatus", "Insecticide","Nets", "Total", "Mortality")], data.frame("Location" = rep("Room", n_dr), "Dead" = rep(1, n_dr), "Total.Loc" = rep(room, n_dr)))
    b <- cbind(data[rep(i, n_ar), c("Village", "Date", "Treatment", "Hut", "Sleeper", "marker","WashedStatus", "Insecticide","Nets", "Total", "Mortality")], data.frame("Location" = rep("Room", n_ar), "Dead" = rep(0, n_ar), "Total.Loc" = rep(room, n_ar)))
    
    n_dn <- data[i, "Dead.Net"]
    n_an <- data[i, "Total.Net"] - n_dn
    net <- data$Total.Net[i]
    
    
    c <- cbind(data[rep(i, n_dn), c("Village", "Date", "Treatment", "Hut", "Sleeper", "marker","WashedStatus", "Insecticide","Nets", "Total", "Mortality")], data.frame("Location" = rep("Net", n_dn), "Dead" = rep(1, n_dn), "Total.Loc" = rep(net, n_dn)))
    d <- cbind(data[rep(i, n_an), c("Village", "Date", "Treatment", "Hut", "Sleeper", "marker","WashedStatus", "Insecticide","Nets", "Total", "Mortality")], data.frame("Location" = rep("Net", n_an), "Dead" = rep(0, n_an), "Total.Loc" = rep(net, n_an)))
    
    n_dv <- data[i, "Dead.Ver"]
    n_av <- data[i, "Total.Ver"] - n_dv
    ver <- data$Total.Ver[i]
    
    e <- cbind(data[rep(i, n_dv), c("Village", "Date", "Treatment", "Hut", "Sleeper", "marker","WashedStatus", "Insecticide","Nets", "Total", "Mortality")], data.frame("Location" = rep("Veranda", n_dv), "Dead" = rep(1, n_dv), "Total.Loc" = rep(ver, n_dv)))
    f <- cbind(data[rep(i, n_av), c("Village", "Date", "Treatment", "Hut", "Sleeper", "marker","WashedStatus", "Insecticide","Nets", "Total", "Mortality")], data.frame("Location" = rep("Veranda", n_av), "Dead" = rep(0, n_av), "Total.Loc" = rep(ver, n_av)))
    
    p <- rbind(a,b,c,d,e,f)
    
    out <- rbind(out, p)
    rm(list = c("a","b","c","d","e","f","p"))
  }
  return(out)
}

mor_reg <- gen_mo(m_mos)

####
gen_bf <- function(data){
  out <- data.frame(Village = character(),
                    Date  = as.Date(character()),
                    Treatment = character(),
                    Location = character(),
                    Total.Loc = integer(),
                    Fed = integer(),
                    Hut= character(),
                    Sleeper= character(),
                    marker = integer(),
                    Nets = character(),
                    WashedStatus = character(),
                    Bloodfed = double())
  data <- subset(data, Location == "Room")
  for (i in 1: nrow(data)){
    
    n_dr <- data[i, "Fed.Room"]
    n_ar <- data[i, "Total.Room"] - n_dr
    room <- data$Total.Room[i]
    
    
    a <- cbind(data[rep(i, n_dr), c("Village", "Date", "Treatment", "Hut", "Sleeper", "marker","WashedStatus", "Insecticide","Nets","Bloodfed")], data.frame("Location" = rep("Room", n_dr), "Fed" = rep(1, n_dr), "Total.Loc" = rep(room, n_dr)))
    b <- cbind(data[rep(i, n_ar), c("Village", "Date", "Treatment", "Hut", "Sleeper", "marker","WashedStatus", "Insecticide","Nets","Bloodfed")], data.frame("Location" = rep("Room", n_ar), "Fed" = rep(0, n_ar), "Total.Loc" = rep(room, n_ar)))
    
    n_dn <- data[i, "Fed.Net"]
    n_an <- data[i, "Total.Net"] - n_dn
    net <- data$Total.Net[i]
    
    
    c <- cbind(data[rep(i, n_dn), c("Village", "Date", "Treatment", "Hut", "Sleeper", "marker","WashedStatus", "Insecticide","Nets","Bloodfed")], data.frame("Location" = rep("Net", n_dn), "Fed" = rep(1, n_dn), "Total.Loc" = rep(net, n_dn)))
    d <- cbind(data[rep(i, n_an), c("Village", "Date", "Treatment", "Hut", "Sleeper", "marker","WashedStatus", "Insecticide","Nets","Bloodfed")], data.frame("Location" = rep("Net", n_an), "Fed" = rep(0, n_an), "Total.Loc" = rep(net, n_an)))
    
    n_dv <- data[i, "Fed.Ver"]
    n_av <- data[i, "Total.Ver"] - n_dv
    ver <- data$Total.Ver[i]
    
    
    e <- cbind(data[rep(i, n_dv), c("Village", "Date", "Treatment", "Hut", "Sleeper", "marker","WashedStatus", "Insecticide","Nets","Bloodfed")], data.frame("Location" = rep("Veranda", n_dv), "Fed" = rep(1, n_dv), "Total.Loc" = rep(ver, n_dv)))
    f <- cbind(data[rep(i, n_av), c("Village", "Date", "Treatment", "Hut", "Sleeper", "marker","WashedStatus", "Insecticide","Nets","Bloodfed")], data.frame("Location" = rep("Veranda", n_av), "Fed" = rep(0, n_av), "Total.Loc" = rep(ver, n_av)))
    
    p <- rbind(a,b,c,d,e,f)
    
    out <- rbind(out, p)
    rm(list = c("a","b","c","d","e","f","p"))
  }
  return(out)
}

bf_reg <- gen_bf(bf_mos)

