# CAR-SHARING
source("/home/stefano/kaycar/R/init.R")

# Statistics about the experiment.


stats <- makeStats(data)

# Initial Values

mydata <- data[data$round == 1 & data$istimeout.decision == 0 & data$decision=="car",]

summarySE(mydata, "departure.time", c("payoff.bus"), na.rm = TRUE)

t.test(departure.time ~ payoff.bus, data=mydata)


p <- ggplot(mydata, aes(departure.time,fill=payoff.bus))
p <- p + geom_density(alpha=0.2)
p


p <- ggplot(mydata, aes(departure.time,fill=payoff.bus, group=payoff.bus))
p <- p + geom_bar(position="dodge", stat="bin")
p <- p + facet_grid(~payoff.bus)
p

# Not significantly different.
ks.test(mydata[mydata$payoff.bus == 50,]$departure.time, mydata[mydata$payoff.bus == 70,]$departure.time)

mydata <- data[data$round == 1 & data$istimeout.decision == 0,]

summarySE(mydata, "bus", c("payoff.bus"), na.rm = TRUE)

p <- ggplot(mydata, aes(bus,fill=payoff.bus, group=payoff.bus))
p <- p + geom_bar(position="dodge", stat="bin")
p <- p + facet_grid(~payoff.bus)
p


# 1 DEPARTURE TIME

# 1.1 DEPARTURE TIME vs GOT-CAR.

dep.time.car.diff2 <- summarySE(data, "departure.time.car.diff", c("payoff.bus",
                                                                   "car.level",
                                                                   "got.car.lag"),na.rm = TRUE)
dep.time.car.diff2 <- na.omit(dep.time.car.diff2)

p <- ggplot(dep.time.car.diff2, aes(payoff.bus,
                                    departure.time.car.diff,
                                    color=payoff.bus,
                                    fill=payoff.bus))
p <- p + geom_bar(stat="identity", position="dodge")
p <- p + geom_errorbar(aes(ymin=departure.time.car.diff - ci, ymax = departure.time.car.diff + ci), width=0.2, color="black")
p <- p + geom_hline(aes(yintercept=(0)))
p <- p + facet_grid(. ~ car.level)
p <- p + ylab('Avg. Departure Time Difference Between Rounds') + xlab('Payoff Bus and Car Share')
#p <- p + ggtitle('Negative values for missing\n a car in previous round')
p

ggsave(paste0(IMGDIR, 'time_diff_got_car.jpg'))

# Their average response is consistent across conditions, with slight variations in size.
# However, the average might cancel out some important learning and adjustgoing on across rounds"

# 1.1.1 DEPARTURE TIME vs GOT-CAR vs ROUND.

dep.time.car.diff2 <- summarySE(data, "departure.time.car.diff",
                                c("round.cut",
                                  "payoff.bus",
                                  "car.level",
                                  "got.car.lag"), na.rm = TRUE)
dep.time.car.diff2 <- na.omit(dep.time.car.diff2)

p <- ggplot(dep.time.car.diff2, aes(round.cut,
                                    departure.time.car.diff,
                                    color=payoff.bus,
                                    fill=payoff.bus))
p <- p + geom_bar(stat="identity", position="dodge")
p <- p + geom_errorbar(aes(ymin=departure.time.car.diff - ci, ymax = departure.time.car.diff + ci),
                       position="dodge", color="black")
p <- p + geom_hline(aes(yintercept=(0)))
p <- p + facet_grid(. ~ car.level)
p <- p + ylab('Avg. Departure Time Difference Between Rounds') + xlab('Payoff Bus and Car Share')
#p <- p + ggtitle('Negative values for missing\n a car in previous round')
p


ggsave(paste0(IMGDIR, 'time_diff_got_car_round-cut.jpg'), width=14)


# The effect of getting or missing a car on the increase/decrease in departure
# time is much bigger in the first rounds. The effect of missing a car is
# generally stronger than that of getting a car, and its decrease over rounds
# is more limited. Interestingly, for car level = 0.75 getting a car on
# round > 10 has almost no effect.

# 1.1.2 DEPARTURE TIME vs GOT-CAR vs ROUND (ALL).

dep.time.car.diff2 <- summarySE(data, "departure.time.car.diff",
                                c("round",
                                  "payoff.bus",
                                  "car.level",
                                  "got.car.lag"), na.rm = TRUE)
dep.time.car.diff2 <- na.omit(dep.time.car.diff2)

p <- ggplot(dep.time.car.diff2, aes(round,
                                    departure.time.car.diff,
                                    color=payoff.bus,
                                    fill=payoff.bus))
p <- p + geom_bar(stat="identity", position="dodge")
p <- p + geom_errorbar(aes(ymin=departure.time.car.diff - ci, ymax = departure.time.car.diff + ci),
                       position="dodge", color="black")
p <- p + geom_hline(aes(yintercept=(0)))
p <- p + facet_grid(. ~ car.level)
p <- p + ylab('Avg. Departure Time Difference ') + xlab('Payoff Bus and Car Share')
p <- p + ggtitle('Negative values for missing\n a car in previous round')
p

# If we look at all rounds (not bins of 5) the effect is the same.


# 1.2 DEPARTURE TIME vs PAYOFF.

dep.time.car.diff2 <- summarySE(data, "departure.time.car.diff",
                                c("payoff.bus",
                                  "car.level",
                                  "payoff.lag"), na.rm = TRUE)
dep.time.car.diff2 <- na.omit(dep.time.car.diff2)

p <- ggplot(dep.time.car.diff2, aes(payoff.lag,
                                    departure.time.car.diff,
                                    color=payoff.bus,
                                    fill=payoff.bus))
p <- p + geom_bar(stat="identity", position="dodge")
#p <- p + geom_errorbar(aes(ymin=departure.time.car.diff - ci, ymax = departure.time.car.diff + ci),
#                       position="dodge", color="black")
p <- p + geom_hline(aes(yintercept=(0)))
p <- p + facet_grid(. ~ car.level)
p <- p + ylab('Avg. Departure Time Difference Between Rounds') + xlab('Payoff Received and Car Share')
#p <- p + ggtitle('Negative values for missing a car in previous round')
p


ggsave(paste0(IMGDIR, 'time_diff_payoff.jpg'), width=14)


# Generally, payoffs < 20-22 leads to a decrease in departure time.
# Larger increases are for payoffs between 25-30 - 50-55 (makes sense, given
# the upper bound).
# The larger the share of cars the larger the gap between low and high payoff
#   (there is a hole) due to the fact that departure times are increasing.
# Condition bus.payoff=50 has lower payoff, and people are also more reactive
# to low payoff
# CI are often too large and removed, payoff could be binned.


# 1.2.1 DEPARTURE TIME vs PAYOFF vs ROUND.


dep.time.car.diff2 <- summarySE(data, "departure.time.car.diff",
                                c("round.cut",
                                  "payoff.bus",
                                  "car.level",
                                  "payoff.lag"), na.rm = TRUE)
dep.time.car.diff2 <- na.omit(dep.time.car.diff2)

p <- ggplot(dep.time.car.diff2, aes(payoff.lag,
                                    departure.time.car.diff,
                                    color=payoff.bus,
                                    fill=payoff.bus))
p <- p + geom_bar(stat="identity", position="dodge")
#p <- p + geom_errorbar(aes(ymin=departure.time.car.diff - ci, ymax = departure.time.car.diff + ci),
#                       position="dodge", color="black")
p <- p + geom_hline(aes(yintercept=(0)))
p <- p + facet_grid(round.cut ~ car.level)
p <- p + ylab('Avg. Departure Time Difference Between Rounds') + xlab('Payoff Bus and Car Share')
#p <- p + ggtitle('Negative values for missing a car in previous round')
p


ggsave(paste0(IMGDIR, 'time_diff_payoff_round.jpg'), width=14, height=16)

# Generally, adjustments are stronger in the first rounds.


# 1.2.2 DEPARTURE TIME vs PAYOFF BINNED.


dep.time.car.diff2 <- summarySE(data, "departure.time.car.diff",
                                c("payoff.bus",
                                  "car.level",
                                  "payoff.lag.cut"), na.rm = TRUE)
dep.time.car.diff2 <- na.omit(dep.time.car.diff2)

p <- ggplot(dep.time.car.diff2, aes(payoff.lag.cut,
                                    departure.time.car.diff,
                                    color=payoff.bus,
                                    fill=payoff.bus))
p <- p + geom_bar(stat="identity", position="dodge")
p <- p + geom_errorbar(aes(ymin=departure.time.car.diff - ci, ymax = departure.time.car.diff + ci),
                       position="dodge", color="black")
p <- p + geom_hline(aes(yintercept=(0)))
p <- p + facet_grid(. ~ car.level)
p <- p + ylab('Avg. Departure Time Difference Between Rounds') + xlab('Payoff Received and Car Share')
p


ggsave(paste0(IMGDIR, 'time_diff_payoff_binned.jpg'), width=14)

# Payoff < 20 generally lead to a decrease in departure time.
# Difference between payoff.bus = 50 or 70 seems not significant

# 1.2.2 DEPARTURE TIME vs PAYOFF BINNED vs ROUND.


dep.time.car.diff2 <- summarySE(data, "departure.time.car.diff",
                                c("payoff.bus",
                                  "car.level",
                                  "payoff.lag.cut",
                                  "round.cut"), na.rm = TRUE)
dep.time.car.diff2 <- na.omit(dep.time.car.diff2)

p <- ggplot(dep.time.car.diff2, aes(payoff.lag.cut,
                                    departure.time.car.diff,
                                    color=payoff.bus,
                                    fill=payoff.bus))
p <- p + geom_bar(stat="identity", position="dodge")
#p <- p + geom_errorbar(aes(ymin=departure.time.car.diff - ci, ymax = departure.time.car.diff + ci),
#                       position="dodge", color="black")
p <- p + geom_hline(aes(yintercept=(0)))
p <- p + facet_grid(round.cut ~ car.level)
p <- p + ylab('Avg. Departure Time Difference Between Rounds') + xlab('Payoff Received and Car Share')
p


ggsave(paste0(IMGDIR, 'time_diff_payoff_binned_round.jpg'), width=14)

# Not different from not-binned payoff plot.


# 1.3 DEPARTURE TIME vs PAYOFF vs GOT-CAR.

# Did they get a low payoff because they missed the car, or because they
# departed too early??

dep.time.car.diff2 <- summarySE(data, "departure.time.car.diff",
                                c("payoff.bus",
                                  "car.level",
                                  "payoff.lag",
                                  "got.car.lag"), na.rm = TRUE)
dep.time.car.diff2 <- na.omit(dep.time.car.diff2)

p <- ggplot(dep.time.car.diff2, aes(payoff.lag,
                                    departure.time.car.diff,
                                    color=payoff.bus,
                                    fill=payoff.bus))
p <- p + geom_bar(stat="identity", position="dodge")
#p <- p + geom_errorbar(aes(ymin=departure.time.car.diff - ci, ymax = departure.time.car.diff + ci),
#                       position="dodge", color="black")
p <- p + geom_hline(aes(yintercept=(0)))
p <- p + facet_grid(got.car.lag ~ car.level)
p <- p + ylab('Avg. Departure Time Difference Between Rounds') + xlab('Payoff Received and Car Share')
p


ggsave(paste0(IMGDIR, 'time_diff_payoff_got-car.jpg'), width=14)

# Only in condition car.level=0.25 there are some people going under equilibrium
#    level for departure time (e.g. they would get more by chosing the bus).
#    This leads to an increase, mainly for payoff > 20.

# 1.3.1 DEPARTURE TIME vs PAYOFF BINNED vs GOT-CAR.


# Did they get a low payoff because they missed the car, or because they
# departed too early??

dep.time.car.diff2 <- summarySE(data, "departure.time.car.diff",
                                c("payoff.bus",
                                  "car.level",
                                  "payoff.lag.cut",
                                  "got.car.lag"), na.rm = TRUE)
dep.time.car.diff2 <- na.omit(dep.time.car.diff2)

p <- ggplot(dep.time.car.diff2, aes(payoff.lag.cut,
                                    departure.time.car.diff,
                                    color=payoff.bus,
                                    fill=payoff.bus))
p <- p + geom_bar(stat="identity", position="dodge")
p <- p + geom_errorbar(aes(ymin=departure.time.car.diff - ci, ymax = departure.time.car.diff + ci),
                       position="dodge", color="black")
p <- p + geom_hline(aes(yintercept=(0)))
p <- p + facet_grid(got.car.lag ~ car.level)
p <- p + ylab('Avg. Departure Time Difference Between Rounds') + xlab('Payoff Received and Car Share')
p


ggsave(paste0(IMGDIR, 'time_diff_payoff_binned_got-car.jpg'), width=14)

# Only in condition car.level=0.25 there are some people going under equilibrium
#    level for departure time (e.g. they would get more by chosing the bus).
#    However, even so, it leads to an increase in departure time (might not be
#    statistically significant).


########################

# 2 SWITCHING TO BUS FROM CAR

# 2.1 CAR-2-BUS VS PAYOFF LAG

mysummary <- summarySE(data, "car2bus", c("payoff.lag", "payoff.bus", "car.level"), na.rm = TRUE)

p <- ggplot(mysummary, aes(payoff.lag, car2bus, color=payoff.bus))
p <- p + geom_point(size=4)
p <- p + geom_smooth(method="lm")
p <- p + facet_grid(payoff.bus~car.level)
p <- p + xlab('Payoff in previous round') + ylab('Fraction of people who switched to bus')
p <- p + ggtitle('')
p


ggsave(paste0(IMGDIR, 'car2bus_payoff.jpg'), height=12, width=14)

p <- ggplot(mysummary[mysummary$N > 1,], aes(payoff.lag, car2bus, color=payoff.bus))
p <- p + geom_point(size=4)
p <- p + geom_smooth(method="lm")
p <- p + facet_grid(payoff.bus~car.level)
p <- p + xlab('Payoff in previous round') + ylab('Fraction of people who switched to bus')
p

p <- ggplot(mysummary[mysummary$N > 5,], aes(payoff.lag, car2bus, color=payoff.bus))
p <- p + geom_point(size=4)
p <- p + geom_smooth(method="lm")
p <- p + facet_grid(payoff.bus~car.level)
p <- p + xlab('Payoff in previous round') + ylab('Fraction of people who switched to bus')
p

ggsave(paste0(IMGDIR, 'car2bus_payoff_5obs.jpg'), height=12, width=14)

p <- ggplot(mysummary[mysummary$N > 10,], aes(payoff.lag, car2bus, color=payoff.bus))
p <- p + geom_point(size=4)
p <- p + geom_smooth(method="lm")
p <- p + facet_grid(payoff.bus~car.level)
p <- p + xlab('Payoff in previous round') + ylab('Fraction of people who switched to bus')
p

# As expected, high payoffs in the previous round decrease the likelihood of switching to bus
# in the current round. However, there are a few occasional "tries", people
# who get a very high payoff and then go back to bus.


mysummary <- summarySE(data[data$payoff.lag > 30,], "car2bus", c("payoff.lag", "payoff.bus", "car.level"), na.rm = TRUE)

p <- ggplot(mysummary[mysummary$N > 1,], aes(payoff.lag, car2bus, color=payoff.bus))
p <- p + geom_point(size=4)
p <- p + geom_smooth(method="lm")
p <- p + facet_grid(payoff.bus~car.level)
p

# If we consider only "medium-high" payoffs (>30 = car payoff), the slope is steeper.
# In fact, there could be a curvilinear relationship as well (for all payoffs).


# 2.2 CAR-2-BUS VS GOT-CAR

mysummary <- summarySE(data, "car2bus", c("got.car.chose.car.lag",
                                          "payoff.bus",
                                          "car.level"), na.rm = TRUE)

p <- ggplot(na.omit(mysummary), aes(payoff.bus, car2bus, fill=payoff.bus, group=as.factor(got.car.chose.car.lag)))
p <- p + geom_bar(stat="identity", position="dodge", color="white")
p <- p + geom_errorbar(aes(ymin=car2bus -ci, ymax=car2bus + ci),
                       position="dodge", color="black")
p <- p + facet_grid(.~car.level)
p <- p + xlab('Got-Car (or not) in previous round and Bus Payoff') +  ylab('Fraction of people who switched to bus')
p

ggsave(paste0(IMGDIR, 'car2bus_got-car.jpg'))

# In general, missing the car leads more people to switching to the bus.
# In general, the higher bus payoff leads even more people to switch to bus.
# Difference are significant (only car.level=0.25 might not be so).

# 2.3 CAR-2-BUS VS GOT-CAR vs ROUND

mysummary <- summarySE(data, "car2bus", c("got.car.chose.car.lag",
                                          "payoff.bus",
                                          "car.level",
                                          "round.cut"), na.rm = TRUE)

p <- ggplot(na.omit(mysummary), aes(as.factor(got.car.chose.car.lag), car2bus, fill=round.cut))
p <- p + geom_bar(stat="identity", position="dodge", color="white")
p <- p + geom_errorbar(aes(ymin=car2bus -ci, ymax=car2bus + ci),
                       position="dodge", color="black")
p <- p + facet_grid(payoff.bus~car.level)
p <- p + xlab('Got-Car (or not) in previous round') +  ylab('Fraction of people who switched to bus')
p


ggsave(paste0(IMGDIR, 'car2bus_got-car_round.jpg'))

# In general, non-getting a car has the same effect in all rounds. Only in car.level = 0.75,
#   and bus.payoff=50 in the first 5 rounds it has a bigger effect.
# Conversely, the effect of getting a car decreases over rounds, and only in car.level = 0.25
#   and bus.payoff=50 is always the same.


# 3. ACTUAL CAR PAYOFF

# 3.1 ACTUAL CAR PAYOFF vs ROUND

mysummary <- summarySE(data, "payoff", c("payoff.bus.num",
                                         "payoff.bus",
                                         "round.cut",
                                         "car.level"), na.rm = TRUE)

p <- ggplot(na.omit(mysummary), aes(round.cut, payoff, fill=payoff.bus))
p <- p + geom_bar(stat="identity", position="dodge")
p <- p + geom_errorbar(aes(ymin=payoff -ci, ymax=payoff + ci),
                       position="dodge", color="black")
p <- p + geom_hline(aes(yintercept=payoff.bus.num))
p <- p + facet_grid(payoff.bus~car.level)
p


ggsave(paste0(IMGDIR, 'carpayoff_round.jpg'), width=12)

# Car payoffs are a function of bus payoff, higher bus payoff the higher car payoff.
# Car payoffs increase over rounds, and the increase is higher for car.level=0.25
# Avg. Car payoffs are always below BUS payoff for car.level=0.25
# Avg. Car payoffs are always above BUS payoff for car.level=0.75
# Avg. Car payoffs are close to BUS payoff for car.level=0.5



mysummary <- summarySE(data, "payoff", c("payoff.bus",
                                         "round",
                                         "car.level"), na.rm = TRUE)

p <- ggplot(na.omit(mysummary), aes(round, payoff, fill=payoff.bus))
p <- p + geom_bar(stat="identity", position="dodge")
p <- p + geom_errorbar(aes(ymin=payoff -ci, ymax=payoff + ci),
                       position="dodge", color="black")
p <- p + facet_grid(payoff.bus~car.level)
p

# 4. SWITCHING TO CAR FROM BUS


# 4.1 BUS-2-CAR VS NUMBER OF STRIKES

mysummary <- summarySE(data, "bus2car", c("decision.strike", "payoff.bus", "car.level"), na.rm = TRUE)

p <- ggplot(mysummary, aes(decision.strike, bus2car, color=payoff.bus))
p <- p + geom_bar(stat="identity", position="dodge")
p <- p + geom_errorbar(aes(ymin=bus2car -ci, ymax=bus2car + ci),
                       position="dodge", color="black")
p <- p + facet_grid(payoff.bus~car.level)
p <- p + xlab('Number of consecutive bus choices') + ylab('Fraction of people who switched to car')
p <- p + ggtitle('')
p

ggsave(paste0(IMGDIR, 'bus2car_strike.jpg'), width=12)

# 4.2 BUS-2-CAR VS NUMBER OF STRIKES VS ROUND

mydata <- data[data$istimeout.decision == 0,]
mysummary <- summarySE(mydata, "bus2car", c("decision.strike", "payoff.bus", "car.level", "round.cut"), na.rm = TRUE)

p <- ggplot(mysummary, aes(decision.strike, bus2car, fill=payoff.bus))
p <- p + geom_bar(stat="identity", position="dodge")
#p <- p + geom_errorbar(aes(ymin=bus2car -ci, ymax=bus2car + ci),
#                       position="dodge", color="black")
p <- p + facet_grid(round.cut~car.level)
p <- p + xlab('Number of consecutive bus choices') + ylab('Fraction of people who switched to car')
p <- p + ggtitle('')
p

ggsave(paste0(IMGDIR, 'bus2car_strike_rounds.jpg'), width=12, height=14)


# For all rounds is the same
