source("init2.R") # /home/stefano/kaycar/R/

### LOAD DATA

SIM <- 'custom-time-init-2016-10-27-18-31'

# FITS.NOW will compute fit measures immediately.
# Otherwise the simulations files are loaded and merged. 
FITS.NOW <- TRUE

# ALL means that there exists a file where all the results are already merged. 
# Otherwise, R will load every single file and merge them.
ALL <- FALSE

if (FITS.NOW) {
  ############## LOAD FITS ################
  # loadFitsSync operations:
  # 1. Checks if we are in the cluster or not (adjusts paths).
  # 2. List all files in SIM directory, open them and join them.
  # 3. Decorate data (i.e., add factors, and other useful variables)
  # 4. Compute fits as follows.
  #    A) Calls makeStats on simulated data to get macro statistics per round:
  #       number of cars, departure time of cars, switching (TODO: check how).
  #    B) Merges experiment's stats with simulated stats in the same data frame.
  #    C) Computes differences in average values per round, squares them,
  #       and multiplies * 100 (MSE).
  #    D) Returns the subsetted dataset.
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
  mydata <- simul[simul$increase.shock == 1,] # check this.
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

if ("init" %in% colnames(fits)) {
  init = fits[1,]$init
} else {
  init = 'NA'
}
#
paramsInTitle <- paste0('S1=', fits[1,]$S1, ' e=', fits[1,]$epsilon,
                        ' phi=', fits[1,]$phi, ' rho1=', fits[1,]$rho1,
                        '\nw+=', fits[1,]$wPlus, ' w-=', fits[1,]$wMinus,
                        ' t+=', fits[1,]$increase.shock, ' t-=', fits[1,]$decrease.shock,
                        ' i=', fits[1,]$interval, ' I=', init)
#
paramsInFilename <- paste0('S1=', fits[1,]$S1, '_e=', fits[1,]$epsilon,
                           '_phi=', fits[1,]$phi, '_rho1=', fits[1,]$rho1,
                           '_w+=', fits[1,]$wPlus, '_w-=', fits[1,]$wMinus,
                           '_t+=', fits[1,]$increase.shock, '_t-=', fits[1,]$decrease.shock,
                           '_i=', fits[1,]$interval, '_I=', init)

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
filepath <- paste0(IMGDIR, 'sweeps/', paramsInFilename, '__msd-bus.jpg')
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
filepath <- paste0(IMGDIR, 'sweeps/', paramsInFilename, '__msd-time.jpg')
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
filepath <- paste0(IMGDIR, 'sweeps/', paramsInFilename, '__msd-switch.jpg')
ggsave(filepath)


if (FITS.NOW) {
  print('FITS.NOW: not doing more plots...')
  q()
}

#####################

SIM <- 'hetero-high-ex-2016-4-6-18-39'
SIM <- 'hetero-high-ex-2016-4-6-19-24'

SIM <- 'custom-time-init-2016-10-28-16-37-a'

SIM <- 'new-deal-2016-10-28-23-57'

simul <- loadSimul(SIM, ALL=ALL)

# Fit
#fit <- computeFit(simul); fit




# Decision Car/Bus (in time)
############################

mysummary <- summarySE(simul, "bus", c("payoff.bus", "car.level",
                                      "car.level.num", "round"), na.rm=TRUE)
#
mysummary$efficiency <- 1 - ((1 - mysummary$car.level.num) - mysummary$bus)
#
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

