# CAR-SHARING
library(ggplot2)
library(scales)
library(reshape2)
library(grid)
library(plyr)

## FONT for plots
theme_set(theme_bw(base_size = 20))

myThemeMod <- theme(axis.title.x = element_text(vjust=-1, size=24),
                    axis.title.y = element_text(vjust=-0.1, size=24),
                    plot.margin=unit(c(10,10,10,10),"mm"),
                    plot.title = element_text(vjust=3, size=24,face="bold"),
                    legend.background = element_rect(fill = "white", color="grey"),
                    legend.title = element_blank(),
                                        #legend.title = element_text(vjust=3, size=16, face="bold"),
                                        #legend.direction = "horizzontal",
                    legend.text = element_text(size=16),
                    legend.key.width = unit(1.5, "cm"),
                    legend.key = element_rect(fill = "white", colour = "white")
                    )

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This is does the summary; it's not easy to understand...
    datac <- ddply(data, groupvars, .drop=.drop,
                   .fun= function(xx, col, na.rm) {
                           c( N    = length2(xx[,col], na.rm=na.rm),
                              mean = mean   (xx[,col], na.rm=na.rm),
                              sd   = sd     (xx[,col], na.rm=na.rm),
                              sum  = sum    (xx[,col], na.rm=na.rm)
                              )
                          },
                    measurevar,
                    na.rm
             )

    # Rename the "mean" column
    datac <- rename(datac, c("mean"=measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval:
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}


plotPlayerDecisionsByRound <- function(mydata, filepath=FALSE) {
  # Helper data.frame
  mysize <- 3
  bus <- mydata[mydata$decision == "bus",]
  p <- ggplot(mydata, aes(round, departure.time))
  p <- p + geom_point(aes(color = tried.car.got.it, shape=decision), size=mysize)
  p <- p + geom_point(data = bus, color="tomato4", size=mysize)
  p <- p + geom_line()
  p <- p + facet_grid(player.short ~ .)
  p <- p + xlab("Round") + ylab("Departure Time (10:00 - 11:00)")
  p <- p + scale_y_discrete(breaks=c()) + scale_color_discrete(name="Found car",
                              labels=c("Yes", "No"))
  p <- p + scale_shape_discrete(name="Choice",
                                labels=c("Bus", "Car"))
  p <- p + theme(strip.background = element_blank(),
                 strip.text.y = element_blank())
  p <- p + ggtitle(mydata$condition[1])

  if (filepath != FALSE) {
    ggsave(filepath)
  }
}

# Computes macro statistics on the avg. bus, time for car, and switching.
makeStats <- function(data, write = FALSE) {
  #
  stats.bus <- summarySE(data, "bus", c("payoff.bus", "car.level",
                                        "round"), na.rm=TRUE)
  #
  stats.bus$sum <- NULL
  stats.bus$sd <- NULL
  stats.bus$se <- NULL
  stats.bus$N <- NULL
  colnames(stats.bus) <- c("payoff.bus","car.level", "round", "bus", "bus.ci" )
  #
  stats.time <- summarySE(data[data$decision == "car",], "departure.time",
                          c("payoff.bus", "car.level", "round"), na.rm=TRUE)
  #
  stats.time$sum <- NULL
  stats.time$sd <- NULL
  stats.time$se <- NULL
  stats.time$N <- NULL
  colnames(stats.time) <- c("payoff.bus","car.level", "round", "time", "time.ci")
  #
  if (!("decision.switch" %in% colnames(data))) {
    payoff.bus <- as.numeric(as.character(data[1,]$payoff.bus))
    car.level <- as.numeric(as.character(data[1,]$car.level))
    library(plm)
    #
    decision1 <- data[1,"decision"]
    pdata <- pdata.frame(data, index=c('player','round'))
    # Constants are removed, so put them again, if missing.
    if (!("decision" %in% colnames(pdata))) {
      pdata$istimeout.decision <- decision1
    } else {
      pdata$decision.lag <- lag(pdata$decision, 1)
    }
    pdata$payoff.lag <- lag(pdata$payoff, 1)
    pdata$departure.time.lag <- lag(pdata$departure.time, 1)
    # Constants are removed, so put them again, if missing.
    if (!("istimeout.decision" %in% colnames(pdata))) {
      pdata$istimeout.decision <- 0
    } else {
      pdata$istimeout.decision.lag <- lag(pdata$istimeout.decision, 1)
    }
    pdata$got.car.lag <- lag(pdata$got.car, 1)
    #
    data <- as.data.frame(pdata)
    data$round <- as.numeric(data$round)
    data$decision.switch <- ifelse(data$decision != data$decision.lag, 1, 0)

    if (!("payoff.bus" %in% data)) {
      data$payoff.bus <- payoff.bus
    }
    if (!("car.level" %in% data)) {
      data$car.level <- car.level
    }
  }
  stats.switch <- summarySE(data, "decision.switch",
                            c("payoff.bus", "car.level", "round"), na.rm=TRUE)
  #
  stats.switch$sum <- NULL
  stats.switch$sd <- NULL
  stats.switch$se <- NULL
  stats.switch$N <- NULL
  colnames(stats.switch) <- c("payoff.bus","car.level", "round", "switch", "switch.ci" )
  #
  stats <- merge(stats.bus, stats.time, by=c("payoff.bus","car.level", "round"))
  stats <- merge(stats, stats.switch, by=c("payoff.bus","car.level", "round"))
  #
  if (write) {
    write.table(stats, paste0(DATADIR, 'summary_exp.csv'), col.names=T, sep=",")
  }
  #
  return(stats)
}


## MSD (Mean-Squared Deviation)

computeFit <- function(data) {
  #
  stats.simul <- makeStats(data)
  colnames(stats.simul) <- c("payoff.bus", "car.level", "round", "sim.bus", "sim.bus.ci",
                             "sim.time", "sim.time.ci", "sim.switch", "sim.switch.ci")
  #
  stats.all <- merge(stats, stats.simul)
  #
  stats.all$diff.bus <- stats.all$bus - stats.all$sim.bus
  stats.all$diff.time <- stats.all$time - stats.all$sim.time
  stats.all$diff.switch <- stats.all$switch - stats.all$sim.switch
  #
  stats.all$diff.sq.bus <- 100 * (stats.all$diff.bus^2)
  stats.all$diff.sq.time <- 100 * (stats.all$diff.time^2)
  stats.all$diff.sq.switch <- 100 * (stats.all$diff.switch^2)
  #
  msd.bus <- summarySE(stats.all, "diff.sq.bus", c("payoff.bus", "car.level"), na.rm=TRUE)
  msd.time <- summarySE(stats.all, "diff.sq.time", c("payoff.bus", "car.level"), na.rm=TRUE)
  msd.switch <- summarySE(stats.all, "diff.sq.switch", c("payoff.bus", "car.level"), na.rm=TRUE)
  #
  a <- data[seq(1,nrow(msd.bus)),seq(1,11)]
  a <- cbind(a, msd.bus[,c(1,2,4)])
  a <- cbind(a, msd.time[,4])
  a <- cbind(a, msd.switch[,4])
  #
  colnames(a) <- c("init", "S1", "epsilon", "phi", "rho1", "wPlus", "wMinus",
                   "upsilon", "increase.shock", "decrease.shock",
                   "interval", "payoff.bus", "car.level", "msd.bus",
                   "msd.time", "msd.switch")
  return(a)
}

getComparison <- function(data, P) {
  params <- unique(data[[P]])
  p <- params[1]
  comparison <- computeFit(data[data[[P]] == p,])
  for (param in params) {
    if (param != p) {
      mydata <- data[data[[P]] == param,]
      myfit <- computeFit(mydata)
      comparison <- rbind(comparison, myfit)
    }
  }
  return(comparison)
}

doPlots <- function(P, PHI, EPSILON) {
  #
  title <- paste0(P, '_phi_', PHI, '_epsilon_', EPSILON)
  #
  thisdata <- data[data$phi == PHI & data$epsilon == EPSILON,]
  #
  comparison <- getComparison(thisdata, P)
  #
  p <- ggplot(comparison, aes(S1, msd.bus, color=S1, fill=S1))
  p <- p + geom_bar(alpha=0.5, position="dodge", stat="identity")
  p <- p + facet_grid(payoff.bus ~ car.level)
  p <- p + ggtitle(title)
  #
  ggsave(paste0(IMGDIR, 'bus_', title,  '.png'))
  #
  p <- ggplot(comparison, aes(S1, msd.time, color=S1, fill=S1))
  p <- p + geom_bar(alpha=0.5, position="dodge", stat="identity")
  p <- p + facet_grid(payoff.bus ~ car.level)
  p <- p + ggtitle(title)
  #
  ggsave(paste0(IMGDIR, 'time_', title,  '.png'))
  #
  p <- ggplot(comparison, aes(S1, msd.switch, color=S1, fill=S1))
  p <- p + geom_bar(alpha=0.5, position="dodge", stat="identity")
  p <- p + facet_grid(payoff.bus ~ car.level)
  p <- p + ggtitle(title)
  #
  ggsave(paste0(IMGDIR, 'switch_', title,  '.png'))
}

doPlotsIncrease <- function(P) {
  #
  title <- P #paste0(P, '_phi_', PHI, '_epsilon_', EPSILON)
  #
  thisdata <- data #data[data$phi == PHI & data$epsilon == EPSILON,]
  #
  comparison <- getComparison(thisdata, P)
  #
  p <- ggplot(comparison, aes(increase.shock, msd.bus, color=S1, fill=S1))
  p <- p + geom_bar(alpha=0.5, position="dodge", stat="identity")
  p <- p + facet_grid(payoff.bus ~ car.level)
  p <- p + ggtitle(title)
  #
  ggsave(paste0(IMGDIR, 'bus_', title,  '.png'))
  #
  p <- ggplot(comparison, aes(increase.shock, msd.time, color=S1, fill=S1))
  p <- p + geom_bar(alpha=0.5, position="dodge", stat="identity")
  p <- p + facet_grid(payoff.bus ~ car.level)
  p <- p + ggtitle(title)
  #
  ggsave(paste0(IMGDIR, 'time_', title,  '.png'))
  #
  p <- ggplot(comparison, aes(increase.shock, msd.switch, color=S1, fill=S1))
  p <- p + geom_bar(alpha=0.5, position="dodge", stat="identity")
  p <- p + facet_grid(payoff.bus ~ car.level)
  p <- p + ggtitle(title)
  #
  ggsave(paste0(IMGDIR, 'switch_', title,  '.png'))
}

### LOAD SIMUL DIR


loadSimul <- function(SIM, ALL=FALSE, OVERDIR='/home/stefano/Documents/mypapers/kay_car/matlab') {
  if (!file.exists(OVERDIR)) {
    # We are on the cluster.
    OVERDIR <- '/cluster/home/gess/balistef/matlab/car-sharing-model'
  }
  #
  DATADIR <- paste0(OVERDIR, '/dump/', SIM, '/')
  print(DATADIR)
  setwd(DATADIR)
  IMGDIR <- paste0(DATADIR, "img/")
  # Create IMG dir if not existing
  if (!file.exists(IMGDIR)) {
    dir.create(file.path(IMGDIR))
  }
  #
  if (!ALL) {
    data <- read.table(paste0(DATADIR, 'data_1.csv'), sep=",", header=TRUE)
    filenames <- list.files(DATADIR, pattern="[data_]?.csv")
    nFiles <- length(filenames)
    sprintf("Files found: %i", nFiles)
    for (n in seq(2, nFiles)) {
      tmp <- read.table(paste0(DATADIR, 'data_', n, '.csv'), sep=",", header=TRUE)
      data <- rbind(data, tmp)
    }
  } else {
    data <- read.table(paste0(DATADIR, 'all.csv'), sep=",", header=TRUE)
  }
  data <- decorateData(data)
  return(data)
}

decorateData <- function(data) {
  # Decorate simulation data.
  data$decision <- as.factor(data$decision)
  data$got.car <- as.factor(data$got.car)
  data$car.level <- as.factor(data$car.level)
  data$payoff.bus <- as.factor(data$payoff.bus)
  data$payoff.car <- as.factor(30)
  data$player <- paste0(data$session, '-', data$repetition, '-', data$player)
  #
  # Dep 2
  data$decision <- ifelse(data$decision == 1, "bus", "car")
  data$departure.time.2 <- ifelse(data$decision == "bus", -5, data$departure.time)
  data$bus <- ifelse(data$decision == "bus", 1, 0)
  data$player.short <- data$player
  data$tried.car.got.it <- as.factor(ifelse(data$decision == "car" & data$got.car == 0, 0, 1))
  data$payoff.adjusted <- ifelse(data$decision == "bus", 50, data$payoff)
  data$car.level.num <- ifelse(data$car.level == "25", 0.25, ifelse(data$car.level == "50", 0.5, 0.75))
  data$istimeout.decision <- 0
  sessions <- unique(data$session)
  #
  return(data)
}

loadSimulPar <- function(SIM, ALL=FALSE, OVERDIR='/home/stefano/Documents/mypapers/kay_car/matlab') {
  #
  library(parallel)
  #
  # Calculate the number of cores
  no_cores <- detectCores() - 1
  #
  # Initiate cluster
  cl <- makeCluster(no_cores)
  #
  if (!file.exists(OVERDIR)) {
    # We are on the cluster.
    OVERDIR <- '/cluster/home/gess/balistef/matlab/car-sharing-model'
  }
  #
  DATADIR <- paste0(OVERDIR, '/dump/', SIM, '/')
  print(DATADIR)
  setwd(DATADIR)
  IMGDIR <- paste0(DATADIR, "img/")
  # Create IMG dir if not existing
  if (!file.exists(IMGDIR)) {
    dir.create(file.path(IMGDIR))
  }
  #
  if (!ALL) {
    data <- read.table(paste0(DATADIR, 'data_1.csv'), sep=",", header=TRUE)
    filenames <- list.files(DATADIR, pattern="[data_]?.csv")
    nFiles <- length(filenames)
    print(paste0("Files found: ", nFiles))
    #
    # Exporting data to cluster.
    clusterExport(cl, "data")
    #
    parLapply(cl,
              seq(2,(nFiles-1)),
              function(n) {
                tmp <- read.table(paste0(DATADIR, 'data_', n, '.csv'),
                                  sep=",", header=TRUE)
                data <- rbind(data, tmp)
              })
    stopCluster(cl)
  } else {
    data <- read.table(paste0(DATADIR, 'all.csv'), sep=",", header=TRUE)
  }
  # Decorate simulation data.
  data$decision <- as.factor(data$decision)
  data$got.car <- as.factor(data$got.car)
  data$car.level <- as.factor(data$car.level)
  data$payoff.bus <- as.factor(data$payoff.bus)
  data$payoff.car <- as.factor(30)
  data$player <- paste0(data$session, '-', data$repetition, '-', data$player)
  #
  # Dep 2
  data$decision <- ifelse(data$decision == 1, "bus", "car")
  data$departure.time.2 <- ifelse(data$decision == "bus", -5, data$departure.time)
  data$bus <- ifelse(data$decision == "bus", 1, 0)
  data$player.short <- data$player
  data$tried.car.got.it <- as.factor(ifelse(data$decision == "car" & data$got.car == 0, 0, 1))
  data$payoff.adjusted <- ifelse(data$decision == "bus", 50, data$payoff)
  data$car.level.num <- ifelse(data$car.level == "25", 0.25, ifelse(data$car.level == "50", 0.5, 0.75))
  data$istimeout.decision <- 0
  sessions <- unique(data$session)
  #
  return(data)
}

loadFitsOld <- function(SIM, ALL=FALSE, OVERDIR='/home/stefano/Documents/mypapers/kay_car/matlab') {
  #
  library(parallel)
  #
  # Calculate the number of cores
  no_cores <- detectCores() - 1
  #
  # Initiate cluster
  cl <- makeCluster(no_cores)
  #
  if (!file.exists(OVERDIR)) {
    # We are on the cluster.
    OVERDIR <- '/cluster/home/gess/balistef/matlab/car-sharing-model'
  }
  #
  DATADIR <- paste0(OVERDIR, '/dump/', SIM, '/')
  print(DATADIR)
  setwd(DATADIR)
  IMGDIR <- paste0(DATADIR, "img/")
  # Create IMG dir if not existing
  if (!file.exists(IMGDIR)) {
    dir.create(file.path(IMGDIR))
  }
  #
  if (!ALL) {
    # data <- read.table(paste0(DATADIR, 'data_1.csv'), sep=",", header=TRUE)
    filenames <- list.files(DATADIR, pattern="[data_]?.csv")
    nFiles <- length(filenames)
    print(paste0("Files found: ", nFiles))
    #
    # Exporting data to cluster.
    data <- data.frame()
    fits <- data.frame()
    clusterExport(cl, "data")
    clusterExport(cl, "fits")
    #
    parLapply(cl,
              seq(1,18),
              function(n) {
                tmp <- read.table(paste0(DATADIR, 'data_', n, '.csv'),
                                  sep=",", header=TRUE)
                if (!exists("data")) {
                  data <- tmp
                } else {
                  data <- rbind(data, tmp)
                }
                # Comput
                if (n %% 6 == 0) {
                  myfit <- computeFit(data)
                  if (!exists("fits")) {
                    fits <- myfits
                  } else {
                    fits <- rbind(fits, myfits)
                  }
                  data <- NULL
                }
              })
    stopCluster(cl)
  } else {
    data <- read.table(paste0(DATADIR, 'all.csv'), sep=",", header=TRUE)
  }
  return(fits)
#   # Decorate simulation data.
#   data$decision <- as.factor(data$decision)
#   data$got.car <- as.factor(data$got.car)
#   data$car.level <- as.factor(data$car.level)
#   data$payoff.bus <- as.factor(data$payoff.bus)
#   data$payoff.car <- as.factor(30)
#   data$player <- paste0(data$session, '-', data$repetition, '-', data$player)
#   #
#   # Dep 2
#   data$decision <- ifelse(data$decision == 1, "bus", "car")
#   data$departure.time.2 <- ifelse(data$decision == "bus", -5, data$departure.time)
#   data$bus <- ifelse(data$decision == "bus", 1, 0)
#   data$player.short <- data$player
#   data$tried.car.got.it <- as.factor(ifelse(data$decision == "car" & data$got.car == 0, 0, 1))
#   data$payoff.adjusted <- ifelse(data$decision == "bus", 50, data$payoff)
#   data$car.level.num <- ifelse(data$car.level == "25", 0.25, ifelse(data$car.level == "50", 0.5, 0.75))
#   data$istimeout.decision <- 0
#   sessions <- unique(data$session)
#  #
#  return(data)
}

loadFits <- function(SIM, OVERDIR='/home/stefano/Documents/mypapers/kay_car/matlab') {
  #
  library(parallel)
  #
  # Calculate the number of cores
  no_cores <- detectCores() - 1
  #
  # Initiate cluster
  cl <- makeCluster(no_cores)
  #
  if (!file.exists(OVERDIR)) {
    # We are on the cluster.
    OVERDIR <- '/cluster/home/gess/balistef/matlab/car-sharing-model'
  }
  #
  DATADIR <- paste0(OVERDIR, '/dump/', SIM, '/')
  print(DATADIR)
  setwd(DATADIR)
  IMGDIR <- paste0(DATADIR, "img/")
  # Create IMG dir if not existing
  if (!file.exists(IMGDIR)) {
    dir.create(file.path(IMGDIR))
  }
  #
  clusterExport(cl, "computeFit")
  clusterExport(cl, "stats")
  clusterExport(cl, "summarySE")
  clusterExport(cl, "makeStats")
  clusterExport(cl, "ddply")
  clusterExport(cl, "decorateData")
  clusterExport(cl, "rename")
  filenames <- list.files(DATADIR, pattern="[data_]?.csv")
  nFiles <- length(filenames)
  print(paste0("Files found: ", nFiles))
  #
  start.time <- Sys.time()
  fits <- parLapply(cl,
                    seq(1,6),
                    function(n) {
                      tmp <- read.table(paste0(DATADIR, 'data_', n, '.csv'),
                                        sep=",", header=TRUE)
                      tmp <- decorateData(tmp)
                      myfit <- computeFit(tmp)
                    })
  stopCluster(cl)
  #
  fits <- do.call(rbind.data.frame, fits)
  total.time <- Sys.time() - start.time
  print(paste0("LoadFits Execution time: ", total.time))
  #
  return(fits)  
}

joinMseFiles <- function(SIM, OVERDIR='/home/stefano/Documents/mypapers/kay_car/matlab') {
  #
  if (!file.exists(OVERDIR)) {
    # We are on the cluster.
    OVERDIR <- '/cluster/home/gess/balistef/matlab/car-sharing-model'
  }
  DATADIR <- paste0(OVERDIR, '/dump/', SIM, '/')
  #
  mydata <- data.frame()
  files <- list.files(pattern = "mse_*", path = DATADIR)
  for (f in files) {
    tmp <- read.table(paste0(DATADIR, f), sep=",", header = TRUE)
    if (nrow(mydata) == 0) {
      mydata <- tmp
    } else {
      mydata <- rbind(mydata, tmp)
    }
    # print(f)
  }
  mydata$car.level <- as.factor(mydata$car.level)
  mydata$payoff.bus <- as.factor(mydata$payoff.bus)
  return(mydata)
}
                         

loadFitsSync <- function(SIM, OVERDIR='/home/stefano/Documents/mypapers/kay_car/matlab') {
  #
  if (!file.exists(OVERDIR)) {
    # We are on the cluster.
    OVERDIR <- '/cluster/home/gess/balistef/matlab/car-sharing-model'
  }
  #
  DATADIR <- paste0(OVERDIR, '/dump/', SIM, '/')
  print(DATADIR)
  setwd(DATADIR)
  IMGDIR <- paste0(DATADIR, "img/")
  # Create IMG dir if not existing
  if (!file.exists(IMGDIR)) {
    dir.create(file.path(IMGDIR))
  }
  #
  filenames <- list.files(DATADIR, pattern="[data_]?.csv")
  nFiles <- length(filenames)
  print(paste0("Files found: ", nFiles))
  #

  start.time <- Sys.time()
  # Check: it was only until 6 instead of nFiles
  fits <- lapply(seq(1, nFiles),
                 function(n) {
                   tmp <- read.table(paste0(DATADIR, 'data_', n, '.csv'),
                                     sep=",", header=TRUE)
                   tmp <- decorateData(tmp)
                   myfit <- computeFit(tmp)
                 })
  #
  
  fits <- do.call(rbind.data.frame, fits)
  total.time <- Sys.time() - start.time
  print(paste0("LoadFitsSync Execution time: ", total.time))
  #
  return(fits)
}

loadFitsForLoop <- function(SIM, OVERDIR='/home/stefano/Documents/mypapers/kay_car/matlab') {
  #
  if (!file.exists(OVERDIR)) {
    # We are on the cluster.
    OVERDIR <- '/cluster/home/gess/balistef/matlab/car-sharing-model'
  }
  #
  DATADIR <- paste0(OVERDIR, '/dump/', SIM, '/')
  print(DATADIR)
  setwd(DATADIR)
  IMGDIR <- paste0(DATADIR, "img/")
  # Create IMG dir if not existing
  if (!file.exists(IMGDIR)) {
    dir.create(file.path(IMGDIR))
  }
  #
  filenames <- list.files(DATADIR, pattern="[data_]?.csv")
  nFiles <- length(filenames)
  print(paste0("Files found: ", nFiles))
  #
  start.time <- Sys.time()
  ns <- seq(1,6)
  for (n in ns) {
    tmp <- read.table(paste0(DATADIR, 'data_', n, '.csv'),
                      sep=",", header=TRUE)
    tmp <- decorateData(tmp)
    myfit <- computeFit(tmp)
    if (!exists("tmp.fits")) {
      tmp.fits <- myfit
    } else {
      tmp.fits <- rbind(tmp.fits, myfit)
    }
  }
  #
  total.time <- Sys.time() - start.time
  print(paste0("LoadFitsForLoop Execution time: ", total.time))
  #
  return(tmp.fits)
}

### LOAD DATA

CLUSTER <- 0
OVERDIR <- '/home/stefano/Documents/mypapers/kay_car/'
if (!file.exists(OVERDIR)) {
  # We are on the cluster.
  OVERDIR <- '/cluster/home/gess/balistef/matlab/car-sharing-model/'
  CLUSTER <- 1
}

RDIR <- paste0(OVERDIR, 'R/')
DATADIR <- paste0(OVERDIR, 'data/ALL/')
print(DATADIR)
setwd(DATADIR)
IMGDIR <- paste0(DATADIR, "img/")
# Create IMG dir if not existing
if (!file.exists(IMGDIR)) {
  dir.create(file.path(IMGDIR))
}

WRITE.SUMMARIES <- FALSE

if (WRITE.SUMMARIES) {

  datasummary.bus <- summarySE(data, "bus",
                               c("payoff.bus", "car.level",
                                 "car.level.num", "round"), na.rm=TRUE)

  
  datasummary.deptime <- summarySE(data[data$decision == "car",], "departure.time",
                                   c("payoff.bus", "car.level", "round"), na.rm=TRUE)



  datasummary.payoff.adj <- summarySE(data, "payoff.adjusted",
                                      c("payoff.bus", "car.level", "round"), na.rm=TRUE)
  datasummary.payoff.adj$payoff.bus.num <- ifelse(datasummary.payoff.adj$payoff.bus == "70", 70, 50)


  datasummary.payoff.adj.car <- summarySE(data[data$decision == "car",],
                                          "payoff.adjusted", c("payoff.bus", "car.level", "round"),
                                          na.rm=TRUE)
  datasummary.payoff.adj.car$payoff.bus.num <- ifelse(datasummary.payoff.adj$payoff.bus == "70", 70, 50)

  
  datasummary.switch <- summarySE(data, "decision.switch", c("payoff.bus", "car.level", "round"), na.rm=TRUE)

  # BUS PAYOFF = 50.
  
  # BUS.
  ms50 <- datasummary.bus[datasummary.bus$payoff.bus == 50,]
  ms50$car.level.num <- NULL
  ms5025 <- ms50[ms50$car.level == 25,]
  write.table(ms5025, file=paste0(DATADIR, 'summary_bus_round_50_25.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  ms5050 <- ms50[ms50$car.level == 50,]
  write.table(ms5050, file=paste0(DATADIR, 'summary_bus_round_50_50.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  ms5075 <- ms50[ms50$car.level == 75,]
  write.table(ms5075, file=paste0(DATADIR, 'summary_bus_round_50_75.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")

  # DEPARTURE TIME.
  ms50 <- datasummary.deptime[datasummary.deptime$payoff.bus == 50,]
  ms5025 <- ms50[ms50$car.level == 25,]
  write.table(ms5025, file=paste0(DATADIR, 'summary_deptime_round_50_25.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  ms5050 <- ms50[ms50$car.level == 50,]
  write.table(ms5050, file=paste0(DATADIR, 'summary_deptime_round_50_50.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  ms5075 <- ms50[ms50$car.level == 75,]
  write.table(ms5075, file=paste0(DATADIR, 'summary_deptime_round_50_75.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")

  # SWITCHES.
  ms50 <- datasummary.switch[datasummary.switch$payoff.bus == 50,]
  ms5025 <- ms50[ms50$car.level == 25,]
  write.table(ms5025, file=paste0(DATADIR, 'summary_switch_round_50_25.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  ms5050 <- ms50[ms50$car.level == 50,]
  write.table(ms5050, file=paste0(DATADIR, 'summary_switch_round_50_50.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  ms5075 <- ms50[ms50$car.level == 75,]
  write.table(ms5075, file=paste0(DATADIR, 'summary_switch_round_50_75.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")

  # PAYOFF ADJ.
  ms50 <- datasummary.payoff.adj[datasummary.payoff.adj$payoff.bus == 50,]
  ms5025 <- ms50[ms50$car.level == 25,]
  write.table(ms5025, file=paste0(DATADIR, 'summary_payoff-adj_round_50_25.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  ms5050 <- ms50[ms50$car.level == 50,]
  write.table(ms5050, file=paste0(DATADIR, 'summary_payoff-adj_round_50_50.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  ms5075 <- ms50[ms50$car.level == 75,]
  write.table(ms5075, file=paste0(DATADIR, 'summary_payoff-adj_round_50_75.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")

  # PAYOFF ADJ. CAR.
  ms50 <- datasummary.payoff.adj.car[datasummary.payoff.adj.car$payoff.bus == 50,]
  ms5025 <- ms50[ms50$car.level == 25,]
  write.table(ms5025, file=paste0(DATADIR, 'summary_payoff-adj-car_round_50_25.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  ms5050 <- ms50[ms50$car.level == 50,]
  write.table(ms5050, file=paste0(DATADIR, 'summary_payoff-adj-car_round_50_50.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  ms5075 <- ms50[ms50$car.level == 75,]
  write.table(ms5075, file=paste0(DATADIR, 'summary_payoff-adj-car_round_50_75.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")

  
  # BUS PAYOFF = 70.
  
  # BUS.
  ms70 <- datasummary.bus[datasummary.bus$payoff.bus == 70,]
  ms70$car.level.num <- NULL
  ms7025 <- ms70[ms70$car.level == 25,]
  write.table(ms7025, file=paste0(DATADIR, 'summary_bus_round_70_25.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  ms7050 <- ms70[ms70$car.level == 50,]
  write.table(ms7050, file=paste0(DATADIR, 'summary_bus_round_70_50.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  ms7075 <- ms70[ms70$car.level == 75,]
  write.table(ms7075, file=paste0(DATADIR, 'summary_bus_round_70_75.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")

  # DEPARTURE TIME.
  ms70 <- datasummary.deptime[datasummary.deptime$payoff.bus == 70,]
  ms7025 <- ms70[ms70$car.level == 25,]
  write.table(ms7025, file=paste0(DATADIR, 'summary_deptime_round_70_25.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  ms7050 <- ms70[ms70$car.level == 50,]
  write.table(ms7050, file=paste0(DATADIR, 'summary_deptime_round_70_50.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  ms7075 <- ms70[ms70$car.level == 75,]
  write.table(ms7075, file=paste0(DATADIR, 'summary_deptime_round_70_75.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")

  # SWITCHES.
  ms70 <- datasummary.switch[datasummary.switch$payoff.bus == 70,]
  ms7025 <- ms70[ms70$car.level == 25,]
  write.table(ms7025, file=paste0(DATADIR, 'summary_switch_round_70_25.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  ms7050 <- ms70[ms70$car.level == 50,]
  write.table(ms7050, file=paste0(DATADIR, 'summary_switch_round_70_50.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  ms7075 <- ms70[ms70$car.level == 75,]
  write.table(ms7075, file=paste0(DATADIR, 'summary_switch_round_70_75.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")

  # PAYOFF ADJ.
  ms70 <- datasummary.payoff.adj[datasummary.payoff.adj$payoff.bus == 70,]
  ms7025 <- ms70[ms70$car.level == 25,]
  write.table(ms7025, file=paste0(DATADIR, 'summary_payoff-adj_round_70_25.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  ms7050 <- ms70[ms70$car.level == 50,]
  write.table(ms7050, file=paste0(DATADIR, 'summary_payoff-adj_round_70_50.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  ms7075 <- ms70[ms70$car.level == 75,]
  write.table(ms7075, file=paste0(DATADIR, 'summary_payoff-adj_round_70_75.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")

  # PAYOFF ADJ. CAR.
  ms70 <- datasummary.payoff.adj.car[datasummary.payoff.adj.car$payoff.bus == 70,]
  ms7025 <- ms70[ms70$car.level == 25,]
  write.table(ms7025, file=paste0(DATADIR, 'summary_payoff-adj-car_round_70_25.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  ms7050 <- ms70[ms70$car.level == 50,]
  write.table(ms7050, file=paste0(DATADIR, 'summary_payoff-adj-car_round_70_50.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  ms7075 <- ms70[ms70$car.level == 75,]
  write.table(ms7075, file=paste0(DATADIR, 'summary_payoff-adj-car_round_70_75.csv'),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=",")
  
}
