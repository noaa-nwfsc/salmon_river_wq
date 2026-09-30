# Functions ----

fncPlotData <- function(dat, lh, xvar, y10 = "Q10", y50 = "Q50", y90 = "Q90",
                        xlb = xvar, ylb = "", mn.lab = met, met = met, si = NA){
  options(warn = -1) #suppress warnings
  
  if(xvar == "River_km"){
    dat <- dat[order(dat[,xvar], decreasing = T),]
  } else{
    dat <- dat[order(dat[,xvar]),]
  }
  xxvar <- dat[,xvar]
  yy10 <- dat[,y10]; yy10 <- yy10[!is.na(xxvar)]
  yy50 <- dat[,y50]; yy50 <- yy50[!is.na(xxvar)]
  yy90 <- dat[,y90]; yy90 <- yy90[!is.na(xxvar)]
  xxvar <- xxvar[!is.na(xxvar)]
  var <- colnames(dat)[ncol(dat)]

  ymin <- as.numeric(min(yy10, na.rm = T)) * 0.9
  ymax <- as.numeric(max(yy90, na.rm = T)) * 1.2
  ylm <- c(ymin, ymax)

  par(mar = c(4,5,2,0.5), las = 1)
  plot(c(xxvar, rev(xxvar)), c(yy10, rev(yy90)), type = "n", las = 1, ylab = "", xlab = xlb,
       ylim = ylm, main = mn.lab, cex.main = 1, cex.axis = 1.3, cex.lab = 1.3)
  title(ylab = ylb, line = 3.5, cex.lab = 1.3)
  arrows(xxvar, as.numeric(yy90), xxvar, as.numeric(yy10), code = 3, length = 0.02, lwd = 2, angle = 90, col = "gray50")

  cx <- ifelse(xvar == "River_km", 0.125, 0.225)
  points(xxvar, yy50, cex = dat[,var] * cx, pch = 21, bg = "#267BB6") ##67bed9
  #points(xxvar, yy50, pch = 19, cex = dat[,var] * 0.05)
  # Get y-axis tick marks and add horizontal guidelines
  y_ticks <- axTicks(2)  # 2 specifies the y-axis
  abline(h = y_ticks, col = "gray", lty = 3)
  
  # Label site
  if(xvar == "River_km" & !is.na(si)){
    y = dat[,y50][dat[,"SiteCode"] %in% si]
    x = dat[,"River_km"][dat[,"SiteCode"] %in% si]
    z <- dat[,"NoYears"][dat[,"SiteCode"] %in% si]
    points(x, y, cex = z * cx * 2, pch = 21, bg = "yellow", col = NA)
    points(x, y, cex = z * cx, pch = 21, bg = "darkred")
  }

  ##Add optimal range ablines
  # if(met %in% c("RNG", "IWI", "AWA", "MWA", "AWM", "MWM", "AWI")){
  #  abline(h = lifestages$Thresh_lo[lifestages$LifeHistory %in% lh & lifestages$Lifestage %in% ls], col = 4, lty = 3, lwd = 2)
  #  abline(h = lifestages$Thresh_hi[lifestages$LifeHistory %in% lh & lifestages$Lifestage %in% ls], col = 2, lty = 3, lwd = 2)
  # }
  
  # legend
  mx <- round(max(dat[,var]))
  mi <- round(min(dat[,var]))
  txt1 <- ifelse(mi == 1, " site", " sites")
  txt2 <- " sites"
  if(xvar == "River_km") txt1 <- ifelse(mi == 1, " year", " years")
  if(xvar == "River_km") txt2 <- " years"
  xx1 <- round(min(dat[,xvar], na.rm = T))
  ypos <- seq(ylm[1], ylm[2], length.out = 21)
  yy1 <- ypos[21]
  yy2 <- ypos[19]

  points(xx1, yy1, cex = (mi * cx), pch = 21, bg = "gray50")
  points(xx1, yy2, cex = (mx * cx), pch = 21, bg = "gray50")
  #points(xx1, yy1, pch = 19, cex = (mi * 0.05))
  #points(xx1, yy2, pch = 19, cex = (mx * 0.05))
  adjvar <- ifelse(xvar == "River_km", 3, 1)
  text((xx1 + adjvar), yy1, paste0(mi, txt1), adj = 0)
  text((xx1 + adjvar), yy2, paste0(mx, txt2), adj = 0)

}

fncMetricName <- function(metric = met){
  if(metric == "temp_mean") return("Mean temperature")
  if(metric == "temp_sd") return("SD of temperature")
  if(metric == "temp_var") return("Variance of temperature")
  if(metric == "temp_range") return("Range of temperature")
  if(metric == "q05") return("5th Percentile of temperature")
  if(metric == "q25") return("25th Percentile of temperature")
  if(metric == "q50") return("50th Percentile of temperature")
  if(metric == "q75") return("75th Percentile of temperature")
  if(metric == "q95") return("95th Percentile of temperature")
  if(metric == "doy_max") return("Day of maximum temperature")
  if(metric == "doy_min") return("Day of minimum temperature")
  if(metric == "days_above_thresh") return("Days above a threshold value")
  if(metric == "days_below_thresh") return("Days below a threshold value")
  if(metric == "degree_days") return("Degree days")
  if(metric == "cumulative_heat") return("Cumulative heat")
  if(metric == "days_in_range") return("Days within suitable range")
  if(metric == "exceed_1st_doy") return("Date first exceeded")
  if(metric == "max_consec_above") return("Max consecutive days above threshold")
  if(metric == "median_consec_above") return("Median consecutive days above threshold")
  if(metric == "n_heat_events") return("Number of heat events (above threshold)")
  if(metric == "max_daily_increase") return("Max daily increase in temperature")
  if(metric == "min_7d") return("Minimum weekly temperature")
  if(metric == "mean_7d") return("Mean weekly temperature")
  if(metric == "max_7d") return("Maximum weekly temperature")
  if(metric == "mean_7d_min") return("Mean of weekly average minimum")
  if(metric == "mean_7d_max") return("Mean of weekly average maximum")
  if(metric == "min_7d_mean") return("Min of weekly average mean")
  if(metric == "max_7d_mean") return("Max of weekly average mean")
  if(metric == "median_weekly_range") return("Median weekly range")
}


fncClrs <- function(x, mi, mx, clnm){
  color_palette <- brewer.pal(7, clnm)
  s <- round(seq(from = mi, to = mx, length.out = 7), 1)
  
  clr = case_when(
    is.na(x) ~ NA,
    (x>=s[1] & x<s[2]) ~ color_palette[1],
    (x>=s[2] & x<s[3]) ~ color_palette[2],
    (x>=s[3] & x<s[4]) ~ color_palette[3],
    (x>=s[4] & x<s[5]) ~ color_palette[4],
    (x>=s[5] & x<s[6]) ~ color_palette[5],
    (x>=s[6] & x<s[7]) ~ color_palette[6],
    x>=s[7] ~ color_palette[7]
  )
  
  legend_labels <- c(
    paste0("< ", s[2]), 
    paste0(s[2], " to ", s[3]), 
    paste0(s[3], " to ", s[4]),
    paste0(s[4], " to ", s[5]),
    paste0(s[5], " to ", s[6]),
    paste0(s[6], " to ", s[7]),
    paste0(">= ", s[7]))
  
  return(list(clr, legend_labels, color_palette))
  
}


