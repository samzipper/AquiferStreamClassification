## NorthAmerica_03-CANOPEX_CalculateStreamflowStatistics.R
#' This script is intended to calculate streamflow statistics from daily data
#' for each CANOPEX watershed.

rm(list=ls())

source("C:/Users/Sam/WorkGits/AquiferStreamClassification/ProcessingScripts/paths+packages.R")

## control parameters
max.gap.days <- 30  # maximum gap to gap-fill
make.plots <- T    # need to make streamflow data availability plots?
n.yr.keep <- 20    # number of years of data to keep

## read in data
# read in catchments selected for processing, from script NorthAmerica_01_SelectCatchments.R
df.in <- read.csv(paste0(dir.data, "NorthAmerica_01_SelectCatchments.csv"), stringsAsFactors=F)
df.in <- subset(df.in, network=="CANOPEX")
df.in$station <- gsub("'", "", df.in$station)  # get rid of quotes in CANOPEX station names

# read in CANOPEX metadata which lists the stations and numbers
df.station.info <- read.delim(paste0(dir.CANOPEX, "CANOPEX_", data.CANOPEX, "_ASCII/CANOPEX_", data.CANOPEX, ".txt"), header=F, 
                              col.names=c("n.station", "lon", "lat", "area.km2", "MMxYYYY.start", "MMxYYYY.end", "state", "station", "name"),
                              stringsAsFactors=F)

# subset station.info to only catchments selected for processing
df.station.info <- subset(df.station.info, station %in% df.in$station)

## scroll through each station and calculate statistics
# make empty columns in df.in to store output
df.in$date.start <- ymd("1900-01-01")      # start date of data
df.in$date.end <- ymd("1900-01-01")        # end date of data
df.in$station.keep <- F      # keep station for final analysis?
df.in$n.yrs <- NaN           # number of years retained in analysis
df.in$n.days.missing <- NaN  # number of total days missing
df.in$n.days.total <- NaN    # number of total days
df.in$n.days.noflow <- NaN   # number of days with flow = 0
df.in$prc.days.noflow <- NaN # percent of days with flow = 0
df.in$BFI.mean <- NaN        # mean baseflow index, entire timeseries
df.in$BFI.max <- NaN         # maximum monthly BFI
df.in$BFI.max.mo <- NaN      # month of maximum monthly BFI
df.in$BFI.min <- NaN         # minimum monthly BFI
df.in$BFI.min.mo <- NaN      # month of minimum monthly BFI
df.in$BF.ann.mm_y <- NaN     # mean annual baseflow
df.in$Q90.mm_d <- NaN        # 10th percentile streamflow
df.in$Q7day.mm_d <- NaN      # mean annual minimum 7-day mean flow
df.in$Qmo.min.mm_d <- NaN    # minimum mean monthly flow flow
df.in$Qmo.min.mo <- NaN      # month of minimum mean monthly flow
df.in$Qann.mm_y <- NaN       # mean annual streamflow
df.in$dQ_dt.slope <- NaN     # slope of lower envelope of log(-dQ/dt) vs log(Q) plot
for (n in df.station.info$n.station){
  # get index of station in df.in
  i <- which(df.in$station==df.station.info$station[df.station.info$n.station==n])
  
  # read in data
  lines.station <- readLines(paste0(dir.CANOPEX, "CANOPEX_", data.CANOPEX, "_ASCII/", n, ".dly"))
  
  # split into columns
  df.station <- data.frame(dates=ymd(gsub(" ", "0", substr(lines.station, 1, 8))), val=as.numeric(substr(lines.station, 30, 39)))  # discharge is in mm
  
  # set nodata
  df.station$val[df.station$val < 0] <- NaN
  
  # fill
  df.station.fill <- fillMissNoPlot(df.station)   ##########NEED TO FIX THIS!!!!!!!
  
  # rename 'val' column to discharge.mm
  colnames(df.station.fill)[2] <- "discharge.mm"
  
  # make year/month columns
  df.station$year <- year(df.station$dates)
  df.station$month <- month(df.station$dates)
  
  
  ## investigate missing data
  df.station.yr.mo <- dplyr::summarize(group_by(df.station, year, month),
                                       n.missing = sum(!(is.finite(val))),
                                       n.total = max(days_in_month(dates)),
                                       prc.missing = 100*n.missing/n.total,
                                       Q.mean = mean(val, na.rm=T))
  
  # determine years/months to retain
  df.station.yr <- dplyr::summarize(group_by(df.station.yr.mo, year),
                                    n.mo.missing = sum(prc.missing>50))
  yrs.keep <- df.station.yr$year[df.station.yr$n.mo.missing==0]
  if (length(yrs.keep>n.yr.keep)) df.in$station.keep[i] <- T

  if (make.plots){
    p.data.mo <- 
      ggplot(df.station.yr.mo, aes(x=factor(month), y=year, fill=Q.mean)) +
      geom_raster() +
      scale_fill_gradient(name="Mean Discharge [mm]", low="red", high="blue") +
      scale_y_reverse(name="Year", expand=c(0,0)) +
      scale_x_discrete(name="Month", expand=c(0,0)) +
      labs(title=paste0(df.in$network[i], " ", df.in$station[i], " (", station.keep, ")"),
           subtitle=paste0(gsub("'", "", df.in$name[i]), ", ", df.in$state[i])) +
      theme_bw() +
      theme(legend.position="bottom")
    
    df.yrs.keep <- data.frame(year = df.station.yr$year,
                              keep = F)
    df.yrs.keep$keep[df.yrs.keep$year %in% yrs.keep] <- T
    
    p.data.keep <- 
      ggplot(df.yrs.keep, aes(x=1, y=year, fill=keep)) +
      geom_raster() +
      scale_fill_discrete(name="Use Year in Analysis?") +
      scale_y_reverse(name="Year", expand=c(0,0)) +
      scale_x_discrete(name="", expand=c(0,0)) +
      theme_bw() +
      theme(legend.position="bottom")
    
    p1 <- ggplotGrob(p.data.mo)
    p2 <- ggplotGrob(p.data.keep)
    p <- cbind(p1, p2, size="first")
    p$heights <- unit.pmax(p1$heights, p2$heights)
    
    ggsave(paste0(dir.plot, "DataAvailability/NorthAmerica_02-CANOPEX_CalculateStreamflowStatistics_", df.in$station[i], ".png"),
           p, width=8, height=6, units="in")
  }
  
  ## trim data to only years to keep
  df.station <- subset(df.station, year(date) %in% yrs.keep)
  
  ## start/end dates
  df.in$date.start[i] <- min(df.station$date[is.finite(df.station$discharge.mm)])
  df.in$date.end[i] <- max(df.station$date[is.finite(df.station$discharge.mm)])
  df.in$n.yrs[i] <- length(yrs.keep)
  
  ## do rest of analysis only if enough years
  if (df.in$station.keep[i]){
    
    ## day counts
    df.in$n.days.missing[i] <- sum(is.na(df.station$discharge.mm))
    df.in$n.days.total[i] <- length(df.station$discharge.mm)
    df.in$n.days.noflow[i] <- sum(df.station$discharge.mm==0, na.rm=T)
    df.in$prc.days.noflow[i] <- round(100*df.in$n.days.noflow[i]/(df.in$n.days.total[i]-df.in$n.days.missing[i]), 2)
    
    ## baseflow index - using hydrostats packages (Lyne & Holick filter)
    # calculate BFI timeseries
    df.station.baseflow <- baseflows(data.frame(Date=df.station$date, Q=df.station$discharge.mm), ts="daily")
    df.station.baseflow$year <- year(df.station.baseflow$Date)
    df.station.baseflow$month <- month(df.station.baseflow$Date)
    
    # mean monthly stats
    df.station.baseflow.mo <- dplyr::summarize(group_by(df.station.baseflow, month),
                                               BF.mm = mean(bf, na.rm=T),
                                               BFI.mo = mean(bfi, na.rm=T))
    df.in$BFI.mean[i] <- sum(df.station.baseflow.mo$BFI.mo*days_in_month(df.station.baseflow.mo$month))/365
    df.in$BFI.max[i] <- max(df.station.baseflow.mo$BFI.mo)
    df.in$BFI.max.mo[i] <- df.station.baseflow.mo$month[which.max(df.station.baseflow.mo$BFI.mo)]
    df.in$BFI.min[i] <- min(df.station.baseflow.mo$BFI.mo)
    df.in$BFI.min.mo[i] <- df.station.baseflow.mo$month[which.min(df.station.baseflow.mo$BFI.mo)]
    df.in$BF.ann.mm_y[i] <- sum(df.station.baseflow.mo$BF.mm*days_in_month(df.station.baseflow.mo$month))
    
    ## flow stats
    # Q90
    df.in$Q90.mm_d[i] <- quantile(df.station$discharge.mm, 0.1, na.rm=T)
    
    # 7-day min
    df.station$Q7day <- as.numeric(stats::filter(df.station$discharge.mm, rep(1/7, 7), sides=1))
    df.station.yr <- dplyr::summarize(group_by(df.station, year),
                                      Q7day.min = min(Q7day, na.rm=T))
    df.in$Q7day.mm_d[i] <- mean(subset(df.station.yr, is.finite(Q7day.min))$Q7day.min)
    
    # monthly and annual flow
    df.station.mo <- dplyr::summarize(group_by(df.station, month),
                                      Q.mm_d = mean(discharge.mm, na.rm=T))
    df.in$Qmo.min.mm_d[i] <- min(df.station.mo$Q.mm_d)
    df.in$Qmo.min.mo[i] <- df.station.mo$month[which.min(df.station.mo$Q.mm_d)]
    df.in$Qann.mm_y[i] <- sum(df.station.mo$Q.mm_d*days_in_month(df.station.mo$month))
    
    ## slope of -dQ/dt vs Q relationship
    # calculate lagged difference (dQ/dt) based on before/after point
    df.station$dQ_dt <- c(NaN, diff(df.station$discharge.mm, lag=2)/2, NaN)
    
    # screen data for which dQ_dt to calculate recession, based on rules in Brutsaert (2008) WRR Section 3.2
    which.negative <- which(df.station$dQ_dt < 0 & df.station$discharge.mm > 0)
    which.positive <- which(df.station$dQ_dt >= 0)
    which.positive.with.buffer <- unique(c(which.positive-2, which.positive-1, which.positive,
                                           which.positive+1, which.positive+2, which.positive+3))  # 2 days before and 3 days after a positive or 0 value
    which.positive.with.buffer <- which.positive.with.buffer[which.positive.with.buffer > 0]  # get rid of negative indices; possible because of 2 days before
    which.keep <- which.negative[!(which.negative %in% which.positive.with.buffer)]
    
    # fit quantile regression
    fit.qr <- rq(log10(-dQ_dt) ~ log10(discharge.mm), data=df.station[which.keep, ], tau=0.05)
    
    # get slope
    df.in$dQ_dt.slope[i] <- coef(fit.qr)[2]
    
    # ## plots
    # p.dQdt <-
    #   ggplot(df.station[which.keep,], aes(x=discharge.mm, y=-dQ_dt)) +
    #   geom_point(shape=21) +
    #   geom_quantile(quantiles=0.10, color="red") +
    #   scale_x_log10() +
    #   scale_y_log10()
    # 
    # p.dQdt.log10 <-
    #   ggplot(df.station[which.keep,], aes(x=log10(discharge.mm), y=log10(-dQ_dt))) +
    #   geom_point() +
    #   geom_quantile(quantiles=0.10)
    
  }
  
  # status update
  print(paste0(i, " of ", dim(df.in)[1], " complete"))
}

## save output dataset
write.csv(df.in, paste0(dir.data, "NorthAmerica_02-CANOPEX_CalculateStreamflowStatistics.csv"), row.names=F)