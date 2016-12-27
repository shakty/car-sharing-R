source("init3.R") # /home/stefano/kaycar/R/

### LOAD DATA

# SIM <- 'new-deal-2016-10-30-15-49'
# To be executed on cluster.
source(paste0(OVERDIR, 'simName.R'))

fits <- joinMseFiles(SIM)

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
  print(paramValue)
  # Create the variable.
  assign(param, paramValue)
}
#
if (countChanged == 2) {
  param2 <- 'NONE'
} else if (countChanged > 3) {
  print(paste0('FIT2: more than two params changed: ', countChanged))
  q()
} else if (countChanged < 2) {
  print(paste0('FIT2: no param changed: ', countChanged))
  q()
}

# File name prefix.
fileNamePrefix <- paste0(param1)
if (param2 != "NONE") {
  fileNamePrefix <- paste0(fileNamePrefix, '-', param2)
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
##########################################
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
p <- p + ggtitle(fileNamePrefix)
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
p <- p + ggtitle(fileNamePrefix)
if (!CLUSTER) {
  p
}

# Saving file.
filepath <- paste0(IMGDIRSIM, fileNamePrefix, '__msd-time.jpg')
ggsave(filepath)

# Fig.
# Mean Square Deviation STRATEGY SWITCHES.
############################################
p <- ggplot(myfits, aes_string(x = param1, y = 'msd.switch', fill='payoff.bus'), color="white")
p <- p + geom_bar(stat="identity", position="dodge")
#
if (param2 != "NONE") {
  p <- p + facet_grid(reformulate(param2, "car.level"))
} else {
  p <- p + facet_grid(. ~ car.level)
}
p <- p + xlab('Increase time after getting car') + ylab('Mean Squared Error Switches')
p <- p + ggtitle(fileNamePrefix)
if (!CLUSTER) {
  p
}

# Saving file.
filepath <- paste0(IMGDIRSIM, fileNamePrefix, '__msd-switch.jpg')
ggsave(filepath)

if (TRUE) {
  print('FITS.ONLY: not doing more plots...')
  q()
}

#### GENERAL PLOTS ########
###########################


# Decision Car/Bus (in time)
############################

mysummary <- read.csv('./summary_bushare.csv')
mysummary$payoff.bus <- as.factor(mysummary$payoff.bus)
mysummary$car.level <- as.factor(mysummary$car.level)


# Payoff distribution by bus payoff and car level.
# [mysummary$epsilon == 0.05,]
p <- ggplot(mysummary, aes(round, bus, color=payoff.bus))
p <- p + geom_line(data=datasummary.bus, alpha=0.3, size=1.5)
p <- p + geom_hline(aes(yintercept=(1-car.level.num)))
p <- p + geom_point()
p <- p + geom_line()
#p <- p + geom_errorbar(aes(ymin=bus - ci, ymax=bus + ci))
p <- p + facet_wrap(epsilon ~ car.level)
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


mysummary <- read.csv('./summary_deptime.csv')
mysummary$payoff.bus <- as.factor(mysummary$payoff.bus)
mysummary$car.level <- as.factor(mysummary$car.level)

# Payoff distribution by bus payoff and car level.
p <- ggplot(mysummary[mysummary$epsilon == 0.1,], aes(round, departure.time, color=payoff.bus))
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
