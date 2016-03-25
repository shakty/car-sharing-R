source("init.R") # /home/stefano/kaycar/R/

# Statistics about the experiment.
stats <- makeStats(data)

datasummary.bus <- summarySE(data, "bus", c("payoff.bus", "car.level",
                                             "car.level.num", "round"), na.rm=TRUE)

datasummary.deptime <- summarySE(data[data$decision == "car",], "departure.time", c("payoff.bus", "car.level", "round"), na.rm=TRUE)


datasummary.payoff.adj <- summarySE(data, "payoff.adjusted", c("payoff.bus", "car.level", "round"), na.rm=TRUE)
datasummary.payoff.adj$payoff.bus.num <- ifelse(datasummary.payoff.adj$payoff.bus == "70", 70, 50)


datasummary.payoff.adj.car <- summarySE(data[data$decision == "car",], "payoff.adjusted", c("payoff.bus", "car.level", "round"), na.rm=TRUE)
datasummary.payoff.adj.car$payoff.bus.num <- ifelse(datasummary.payoff.adj$payoff.bus == "70", 70, 50)

datasummary.switch <- summarySE(data, "decision.switch", c("payoff.bus", "car.level", "round"), na.rm=TRUE)

### LOAD DATA

# OVERDIR <- '/home/stefano/Documents/mypapers/kay_car/'
# RDIR <- paste0(OVERDIR, 'R/')

SIM <- 'b50_c75_rl-2016-3-15-17-29'
SIM <- 'b50_c75_rl-2016-3-15-21-9'
SIM <- 'b50_c75_rl-2016-3-15-21-12'
SIM <- 'b50_c75_rl-2016-3-15-21-16'
SIM <- 'b50_c75_rl-2016-3-15-21-21'

SIM <- 'b50_c75_rl-2016-3-15-21-34'
SIM <- 'b50_c75_rl-2016-3-15-21-39'

SIM <- 'sweep-2016-3-15-22-18'

SIM <- 'sweep-2016-3-16-23-34'

SIM <- 'sweep-2016-3-16-23-41'

SIM <- 'newsweep-2016-3-17-16-15'

SIM <- 'newsweep-2016-3-17-16-22'

SIM <- 'newsweep2-2016-3-17-17-48'

SIM <- 'newsweep2-2016-3-17-17-54'

SIM <- 'newsweep2-2016-3-17-18-2'

# with init
SIM <- 'newsweep3-2016-3-22-21-59'

# without init
SIM <- 'newsweep3-2016-3-22-22-29'

# without init, lower weights (0.3,0.4)
SIM <- 'newsweep3-2016-3-22-22-37'

# without init, higher weights (0.8,0.9)
SIM <- 'newsweep3-2016-3-22-22-43'

# without init, asymmetric weights (0.8,0.2)
SIM <- 'newsweep3-2016-3-22-22-48'

# without init, asymmetric weights, inverted (0.2,0.8)
SIM <- 'newsweep3-2016-3-22-22-55'


# without init, asymmetric weights, inverted (0.2,0.8)
# Rho1 = bus - 10 (was = bus before)
SIM <- 'newsweep3-2016-3-22-23-1'


# without init, asymmetric weights, inverted (0.2,0.8)
# Rho1 = bus + 10
SIM <- 'newsweep3-2016-3-22-23-5'

# small exploration (faster learning!)
SIM <- 'newsweep3-2016-3-23-15-33'

# small exploration (faster learning!), huge rho1 = +100
SIM <- 'newsweep3-2016-3-23-15-44'

# Erev Roth 1-PARAM
SIM <- 'simple-2016-3-23-15-58'

# good_stuff (no init)
SIM <- 'adjusted-2016-3-23-17-39'

# good_stuff (init)
SIM <- 'adjusted-2016-3-23-19-44'


# distribution of car dep time should be right.
SIM <- 'timeright-2016-3-23-22-12'

# SWEEP
SIM <- 'sweep_increase-2016-3-23-23-13'

# decrease and some increase.
SIM <- 'sweep_increase-2016-3-24-14-32'
ALL <- TRUE

FITS.NOW <- TRUE

if (FITS.NOW) {
  ############## LOAD FITS ################
  fits <- loadFitsSync(SIM)
  ###############################################
} else {
  ############## LOAD SIMULATION ################
  simul <- loadSimul(SIM, ALL=ALL)
  ###############################################
  #
  # library(bigmemory)
  # DATADIR <- paste0(OVERDIR, 'matlab/dump/', SIM, '/')
  # simul <- read.big.matrix(paste0(DATADIR, 'all.csv'), sep=",", header=TRUE)
  #
  ############# COMPUTE FIT ######################
  mydata <- simul[simul$increase.shock == 1,]
  fits <- computeFit(mydata)
  for (s in unique(simul$increase.shock)) {
    if (s != 1) {
      mydata <- simul[simul$increase.shock == s,]
      fit <- computeFit(mydata);
      fits <- rbind(fits, fit)
    }
  }
}


# PLOTS variables.

if ("init" %in% colnames(simul)) {
  init = simul[1,]$init
} else {
  init = 'NA'
}
  
paramsInTitle <- paste0('S1=', simul[1,]$S1, ' e=', simul[1,]$epsilon,
                        ' phi=', simul[1,]$phi, ' rho1=', simul[1,]$rho1,
                        '\nw+=', simul[1,]$wPlus, ' w-=', simul[1,]$wMinus,
                        ' t+=', simul[1,]$increase.shock, ' t-=', simul[1,]$decrease.shock,
                        ' i=', simul[1,]$interval, ' I=', init)

paramsInFilename <- paste0('S1=', simul[1,]$S1, '_e=', simul[1,]$epsilon,
                           '_phi=', simul[1,]$phi, '_rho1=', simul[1,]$rho1,
                           '_w+=', simul[1,]$wPlus, '_w-=', simul[1,]$wMinus,
                           '_t+=', simul[1,]$increase.shock, '_t-=', simul[1,]$decrease.shock,
                           '_i=', simul[1,]$interval, '_I=', init)

#######################

#
# fits.melted <- melt(fits, c("S1", "epsilon", "phi",  "rho1",
#                             "wPlus", "wMinus", "upsilon", "increase.shock", "decrease.shock",
#                            "interval", "payoff.bus", "car.level"))

p <- ggplot(fits, aes(decrease.shock, msd.bus, fill=payoff.bus), color="white")
p <- p + geom_bar(stat="identity", position="dodge")
p <- p + facet_grid(. ~ car.level)
p <- p + xlab('Increase time after getting car') + ylab('Mean Squared Error Bus Takers')
p <- p + ggtitle(paramsInTitle)
if (!CLUSTER) {
  p
}

# Saving file.
filepath <- paste0(IMGDIR, 'sweeps/', paramsInFilename, '__sw-bus.jpg')
ggsave(filepath)



p <- ggplot(fits, aes(decrease.shock, msd.time, fill=payoff.bus), color="white")
p <- p + geom_bar(stat="identity", position="dodge")
p <- p + facet_grid(. ~ car.level)
p <- p + xlab('Increase time after getting car') + ylab('Mean Squared Error Departure Time Car')
p <- p + ggtitle(paramsInTitle)
if (!CLUSTER) {
  p
}

# Saving file.
filepath <- paste0(IMGDIR, 'sweeps/', paramsInFilename, '__rl-bus.jpg')
ggsave(filepath)

p <- ggplot(fits, aes(decrease.shock, msd.switch, fill=payoff.bus), color="white")
p <- p + geom_bar(stat="identity", position="dodge")
p <- p + facet_grid(. ~ car.level)
p <- p + xlab('Increase time after getting car') + ylab('Mean Squared Error Switches')
p <- p + ggtitle(paramsInTitle)
if (!CLUSTER) {
  p
}

# Saving file.
filepath <- paste0(IMGDIR, 'sweeps/', paramsInFilename, '__rl-bus.jpg')
ggsave(filepath)


if (FITS.NOW) {
  q()
}

#####################

# Fit
fit <- computeFit(simul); fit

# Decision Car/Bus (in time)
############################

mysummary <- summarySE(simul, "bus", c("payoff.bus", "car.level",
                                      "car.level.num", "round"), na.rm=TRUE)

mysummary$efficiency <- 1 - ((1 - mysummary$car.level.num) - mysummary$bus)

# Payoff distribution by bus payoff and car level.
p <- ggplot(mysummary, aes(round, bus, color=payoff.bus))
p <- p + geom_line(data=datasummary.bus, alpha=0.3, size=1.5)
p <- p + geom_hline(aes(yintercept=(1-car.level.num)))
p <- p + geom_point()
p <- p + geom_line()
#p <- p + geom_errorbar(aes(ymin=bus - ci, ymax=bus + ci))
p <- p + facet_grid(. ~ car.level)
p <- p + xlab('Round') + ylab('Share of Bus Takers')
p <- p + scale_color_discrete(name='Payoff Bus')
p <- p + theme(strip.background = element_blank(),
               strip.text.y = element_blank())
p <- p + ggtitle(paste0(paramsInTitle, '\nBus Takers'))
if (!CLUSTER) {
  p
}

# Saving file.
filepath <- paste0(IMGDIR, 'rl/', paramsInFilename, '__rl-bus.jpg')
ggsave(filepath)



# Car Departure times
#####################


mysummary <- summarySE(simul[simul$decision == "car",], "departure.time", c("payoff.bus", "car.level", "round"), na.rm=TRUE)

# Payoff distribution by bus payoff and car level.
p <- ggplot(mysummary, aes(round, departure.time, color=payoff.bus))
p <- p + geom_line(data=datasummary.deptime, alpha=0.3, size=1.5)
p <- p + geom_point()
p <- p + geom_line()
#p <- p + geom_errorbar(aes(ymin=departure.time - ci, ymax=departure.time + ci))
p <- p + facet_grid(. ~ car.level)
p <- p + xlab('Round') + ylab('Departure times')
p <- p + scale_color_discrete(name='Payoff Bus')
p <- p + theme(strip.background = element_blank(),
               strip.text.y = element_blank())
p <- p + ggtitle(paste0(paramsInTitle, '\nCar Departure Time'))
if (!CLUSTER) {
  p
}


# Saving file.
filepath <-  paste0(IMGDIR, 'rl/', paramsInFilename, '__rl-car_time.jpg')
ggsave(filepath)


# Payoffs
#########

mysummary <- summarySE(simul, "payoff.adjusted", c("payoff.bus", "car.level", "round"), na.rm=TRUE)
mysummary$payoff.bus.num <- ifelse(mysummary$payoff.bus == "70", 70, 50)

# Payoff distribution by bus payoff and car level.
p <- ggplot(mysummary, aes(round, payoff.adjusted, color=payoff.bus))
p <- p + geom_line(data=datasummary.payoff.adj, alpha=0.3, size=1.5)
p <- p + geom_point()
p <- p + geom_line()
#p <- p + geom_errorbar(aes(ymin=payoff.adjusted - ci, ymax=payoff.adjusted + ci))
#p <- p + geom_hline(aes(yintercept=payoff.bus.num))
p <- p + facet_grid(. ~ car.level)
p <- p + xlab('Round') + ylab('Normalized Payoff')
p <- p + scale_color_discrete(name='Condition\nPayoff Bus')
p <- p + theme(strip.background = element_blank(),
               strip.text.y = element_blank())
p <- p + ggtitle(paste0(paramsInTitle, '\nNormalized payoffs by round'))
if (!CLUSTER) {
  p
}

# Saving file.
filepath <- paste0(IMGDIR, 'rl/', paramsInFilename, '__rl-norm_payoff.jpg')
ggsave(filepath)


# Car Payoffs
#############

mysummary <- summarySE(simul[simul$decision == "car",], "payoff.adjusted", c("payoff.bus", "car.level", "round"), na.rm=TRUE)
mysummary$payoff.bus.num <- ifelse(mysummary$payoff.bus == "70", 70, 50)

# Payoff distribution by bus payoff and car level.
p <- ggplot(mysummary, aes(round, payoff.adjusted, color=payoff.bus))
p <- p + geom_line(data=datasummary.payoff.adj.car, alpha=0.3, size=1.5)
p <- p + geom_hline(aes(yintercept=payoff.bus.num))
p <- p + geom_point()
p <- p + geom_line()
#p <- p + geom_errorbar(aes(ymin=payoff.adjusted - ci, ymax=payoff.adjusted + ci))
p <- p + facet_grid(. ~ car.level)
p <- p + xlab('Round') + ylab('Car Payoff')
p <- p + scale_color_discrete(name='Condition\nPayoff Bus')
p <- p + theme(strip.background = element_blank(),
               strip.text.y = element_blank())
p <- p + ggtitle(paste0(paramsInTitle, '\n Car payoffs by round'))
if (!CLUSTER) {
  p
}

# Saving file.
filepath <- paste0(IMGDIR, 'rl/', paramsInFilename, '__rl-payoff-car.jpg')
ggsave(filepath)

# Higher Bus payoff, leads to higher payoffs because helps to create sorting.
# The effect is evident over rounds.


# SWITCHING

library(plm)

# Switching
pdata <- pdata.frame(simul, index=c('player', 'round'))
pdata$decision.lag <- lag(pdata$decision, 1)
pdata$payoff.lag <- lag(pdata$payoff, 1)
pdata$departure.time.lag <- lag(pdata$departure.time, 1)
pdata$got.car.lag <- lag(pdata$got.car, 1)


copy <- as.data.frame(pdata)
copy$round <- as.numeric(copy$round)

copy$decision.switch <- ifelse(copy$decision != copy$decision.lag, 1, 0)

mysummary <- summarySE(copy, "decision.switch", c("payoff.bus", "car.level", "round"), na.rm=TRUE)


# Payoff distribution (hist) by bus payoff and car level. (Distance from ideal level)
p <- ggplot(mysummary, aes(round, decision.switch, group=payoff.bus, color=payoff.bus))
p <- p + geom_line(data=datasummary.switch, alpha=0.3, size=1.5)
p <- p + geom_point()
p <- p + geom_line()
#p <- p + geom_errorbar(aes(ymin=decision.switch - ci, ymax=decision.switch + ci))
p <- p + facet_grid(. ~ car.level)
p <- p + xlab('Round') + ylab('Avg. Switching')
p <- p + scale_color_discrete(name='Condition\nPayoff Bus')
p <- p + theme(strip.background = element_blank(),
               strip.text.y = element_blank())
p <- p + ggtitle(paste0(paramsInTitle, '\nStrategy Switching by round'))
if (!CLUSTER) {
  p
}


# Saving file.
filepath <- paste0(IMGDIR, 'rl/', paramsInFilename, '__rl-switch.jpg')
ggsave(filepath)


# ## S1
# 
# P = "S1"
# 
# PHI = 0
# EPSILON = 0
# doPlots(P, PHI, EPSILON)
# 
# 
# PHI = 0.1
# EPSILON = 0
# doPlots(P, PHI, EPSILON)
# 
# 
# PHI = 0.01
# EPSILON = 0
# doPlots(P, PHI, EPSILON)
# 
# PHI = 0
# EPSILON = 0.4
# doPlots(P, PHI, EPSILON)
# 
# # INCREASE
# 
# P = "increase.shock"
# doPlotsIncrease(P)
