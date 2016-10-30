source("init2.R") # /home/stefano/kaycar/R/

### LOAD DATA

SIM <- 'custom-time-init-2016-10-27-18-31'

# ACTION:
#
# FITS.ONLY will compute fit measures immediately.
#
# SIMUL.ONLY will load only the simulations.
#
# FITS.AND.SIMUL will load the simulations and then compute fits (both objects available)

ACTION <- "FITS.AND.SIMUL"

# ALL means that there exists a file where all the results are already merged. 
# Otherwise, R will load every single file and merge them.
ALL <- FALSE

if (ACTION == "FITS.ONLY") {
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
  # Use the big memory (in case we have an all.csv file).
  # library(bigmemory)
  # DATADIR <- paste0(OVERDIR, 'matlab/dump/', SIM, '/')
  # simul <- read.big.matrix(paste0(DATADIR, 'all.csv'), sep=",", header=TRUE)
  #
  ############# COMPUTE FIT ######################
  if (ACTION == "FITS.AND.SIMUL") {
    fits <- computeFit(simul)
  }
}

# Create main directory, if not found.
MAINDIR <- 'newdeal'
IMGDIR <- paste0(IMGDIR, MAINDIR, '/')
if (!file.exists(IMGDIR)) {
  dir.create(file.path(IMGDIR))
}

# In case we need to subset.
myfits <- fits

## Find out which parameters have changed.

# Write params.
paramNames <- c("S1", "epsilon", "phi", "rho1", "wPlus",
                "wMinus", "increase.shock", "decrease.shock",
                "interval", "init", "hetero", "reward.car")

count <- 1
countChanged <- 1
paramString <- ''
for (param in paramNames) {
  #
  if (param %in% colnames(myfits)) {
    paramValue <- unique(myfits[, param])
  } else {
    paramValue <- 'NA'
  }
  #
  if (length(paramValue) > 1) {
    assign(paste0('param', countChanged), param)
    countChanged <- countChanged + 1
  } else if (length(paramValue) != 1) {
    print(paste0('FIT2: param not found: ', param))
    q()
  }  
  #
  if (count != 1) {
    paramString <- paste0(paramString, '\n')
  }
  paramString <- paste0(paramString, param, ' = ', paramValue)
  count <- count + 1
  # Create the variable.
  assign(param, paramValue)
}

if (countChanged == 2) {
  param2 <- 'NONE'
} else if (countChanged > 3) {
  print(paste0('FIT2: more than two params changed: ', countChanged))
  q()
} else if (countChanged == 0) {
  print(paste0('FIT2: no param changed: ', countChanged))
  q()
}

# File name prefix.
fileNamePrefix <- paste0(param1)
if (param2 != "NONE") {
  IMGDIRSIM <- paste0(fileNamePrefix, '-', param2)
}
# Create IMG DIR for the sweep.
IMGDIRSIM <- paste0(IMGDIR, fileNamePrefix, '/')
if (!file.exists(IMGDIRSIM)) {
  dir.create(file.path(IMGDIRSIM))
}

# Write all params combinations to file.
write(paramString, file=paste0(IMGDIRSIM, 'params.txt'))

# Fig.
# Mean Square Deviation SHARE BUS TAKERS.
############################################
p <- ggplot(myfits, aes_string(x = param1, y = "msd.bus", fill="payoff.bus"), color="white")
p <- p + geom_bar(stat="identity", position="dodge")
#
if (param2 != "NONE") {
  p <- p + facet_grid(reformulate(param2, "car.level"))
} else {
  p <- p + facet_grid(. ~ car.level)
}
#
p <- p + xlab('Increase time after getting car') + ylab('Mean Squared Error Bus Takers')
#p <- p + ggtitle(paramsInTitle)
if (!CLUSTER) {
  p
}

# Saving file.
filepath <- paste0(IMGDIRSIM, fileNamePrefix, '__msd-bus.jpg')
ggsave(filepath)

# Fig.
# Mean Square Deviation DEPARTURE TIME.
############################################
p <- ggplot(myfits, aes_string(x = param1, y = 'msd.time', fill='payoff.bus'), color="white")
p <- p + geom_bar(stat="identity", position="dodge")
#
if (param2 != "NONE") {
  p <- p + facet_grid(reformulate(param2, "car.level"))
} else {
  p <- p + facet_grid(. ~ car.level)
}
#
p <- p + xlab('Increase time after getting car') + ylab('Mean Squared Error Departure Time Car')
#p <- p + ggtitle(paramsInTitle)
if (!CLUSTER) {
  p
}

# Saving file.
filepath <- paste0(IMGDIRSIM, fileNamePrefix, '__msd-time.jpg')
ggsave(filepath)

# Fig.
# Mean Square Deviation STRATEGY SWITCHES.
############################################
p <- ggplot(myfits, aes_string(x = param1, 'msd.switch', fill='payoff.bus'), color="white")
p <- p + geom_bar(stat="identity", position="dodge")
#
if (param2 != "NONE") {
  p <- p + facet_grid(reformulate(param2, "car.level"))
} else {
  p <- p + facet_grid(. ~ car.level)
}
p <- p + xlab('Increase time after getting car') + ylab('Mean Squared Error Switches')
#p <- p + ggtitle(paramsInTitle)
if (!CLUSTER) {
  p
}

# Saving file.
filepath <- paste0(IMGDIRSIM, fileNamePrefix,'__msd-switch.jpg')
ggsave(filepath)

if (FITS.ONLY) {
  print('FITS.ONLY: not doing more plots...')
  q()
}


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


  
  
}
