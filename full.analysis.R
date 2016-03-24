# CAR-SHARING
source("/home/stefano/kaycar/R/init.R")

## General Stats"

nrow(data)
summary(data)

table(data$car.level, data$payoff.bus)

#       50   70
#  25 5540 5539
#  50 5508 5465
#  75 5558 5986

tmp <- data[!duplicated(data$session),]
table(tmp$condition)

# noinfo_car25_p5030 noinfo_car25_p7030 noinfo_car50_p5030 noinfo_car50_p7030 
#                 10                 10                 10                 10 
# noinfo_car75_p5030 noinfo_car75_p7030 
#                 10                 11 

# 0. Initial Propensities

mysummary <- summarySE(data[data$round == 1,], "bus", c("payoff.bus", "car.level"), na.rm=TRUE)



# 1. Payoffs (adjusted)

# Payoff distribution by bus payoff and car level.
p <- ggplot(data, aes(payoff.adjusted, color=payoff.bus, fill=payoff.bus))
p <- p + geom_density(alpha=0.5)
p <- p + facet_grid(. ~ car.level)
p <- p + xlab('Normalized Payoffs') + ylab('Density')
p <- p + scale_fill_discrete(name='Payoff Bus')
p <- p + scale_color_discrete(name='Payoff Bus')
p <- p + theme(strip.background = element_blank(),
               strip.text.y = element_blank())
p

# Payoff distribution (hist) by bus payoff and car level.
p <- ggplot(data, aes(payoff.adjusted, color=payoff.bus, fill=payoff.bus))
p <- p + geom_bar(alpha=0.5, position="dodge")
p <- p + facet_grid(. ~ car.level)
p <- p + xlab('Normalized Payoffs') + ylab('Frequency')
p <- p + scale_fill_discrete(name='Payoff Bus')
p <- p + scale_color_discrete(name='Payoff Bus')
p <- p + theme(strip.background = element_blank(),
               strip.text.y = element_blank())
p


# Payoff distribution (hist) by bus payoff.
p <- ggplot(data, aes(payoff.adjusted, color=payoff.bus, fill=payoff.bus))
p <- p + geom_bar(alpha=0.5, position="dodge")
#p <- p + facet_grid(. ~ car.level)
p <- p + xlab('Normalized Payoffs') + ylab('Frequency')
p <- p + scale_fill_discrete(name='Payoff Bus')
p <- p + scale_color_discrete(name='Payoff Bus')
p <- p + theme(strip.background = element_blank(),
               strip.text.y = element_blank())
p

# Higher Bus payoff, leads to higher payoffs because reduces
# information uncertainty, and helps to create sorting. Effect more evident for car = 0.5.

# 2. Payoffs (in time)
mysummary <- summarySE(data, "payoff.adjusted", c("payoff.bus", "car.level", "round"), na.rm=TRUE)

# Payoff distribution by bus payoff and car level.
p <- ggplot(mysummary, aes(round, payoff.adjusted, color=payoff.bus))
p <- p + geom_point()
p <- p + geom_line()
p <- p + geom_errorbar(aes(ymin=payoff.adjusted - ci, ymax=payoff.adjusted + ci))
p <- p + facet_grid(. ~ car.level)
p <- p + xlab('Round') + ylab('Normalized Payoff')
p <- p + scale_color_discrete(name='Condition\nPayoff Bus')
p <- p + theme(strip.background = element_blank(),
               strip.text.y = element_blank())
p <- p + ggtitle('Normalized payoffs by round')
p

# Saving file.
# filepath <- paste0(IMGDIR, 'norm_payoff_by_round_by_condition.jpg')
# ggsave(filepath)

# Higher Bus payoff, leads to higher payoffs because helps to create sorting.
# The effect is evident over rounds.

# 3. Payoffs (adjusted)

# Payoff distribution by bus payoff and car level.
p <- ggplot(data, aes(departure.time, color=payoff.bus, fill=payoff.bus))
p <- p + geom_density(alpha=0.5)
p <- p + facet_grid(. ~ car.level)
p <- p + xlab('Normalized Payoffs') + ylab('Density')
p <- p + scale_fill_discrete(name='Payoff Bus')
p <- p + scale_color_discrete(name='Payoff Bus')
p <- p + theme(strip.background = element_blank(),
               strip.text.y = element_blank())
p

# Payoff distribution (hist) by bus payoff and car level.
p <- ggplot(data, aes(departure.time, color=payoff.bus, fill=payoff.bus))
p <- p + geom_bar(alpha=0.5, position="dodge")
p <- p + facet_grid(. ~ car.level)
p <- p + xlab('Normalized Payoffs') + ylab('Frequency')
p <- p + scale_fill_discrete(name='Payoff Bus')
p <- p + scale_color_discrete(name='Payoff Bus')
p <- p + theme(strip.background = element_blank(),
               strip.text.y = element_blank())
p


# Payoff distribution (hist) by bus payoff.
p <- ggplot(data, aes(departure.time, color=payoff.bus, fill=payoff.bus))
p <- p + geom_bar(alpha=0.5, position="dodge")
#p <- p + facet_grid(. ~ car.level)
p <- p + xlab('Normalized Payoffs') + ylab('Frequency')
p <- p + scale_fill_discrete(name='Payoff Bus')
p <- p + scale_color_discrete(name='Payoff Bus')
p <- p + theme(strip.background = element_blank(),
               strip.text.y = element_blank())
p


# 4. Departure times (in time)

mysummary <- summarySE(data, "departure.time", c("payoff.bus", "car.level", "round"), na.rm=TRUE)

# Payoff distribution by bus payoff and car level.
p <- ggplot(mysummary, aes(round, departure.time, color=payoff.bus))
p <- p + geom_point()
p <- p + geom_line()
p <- p + geom_errorbar(aes(ymin=departure.time - ci, ymax=departure.time + ci))
p <- p + facet_grid(. ~ car.level)
p <- p + xlab('Round') + ylab('Departure times')
p <- p + scale_color_discrete(name='Payoff Bus')
p <- p + theme(strip.background = element_blank(),
               strip.text.y = element_blank())
p


# cars only.
mysummary <- summarySE(data[data$decision == "car",], "departure.time", c("payoff.bus", "car.level", "round"), na.rm=TRUE)

# Payoff distribution by bus payoff and car level.
p <- ggplot(mysummary, aes(round, departure.time, color=payoff.bus))
p <- p + geom_point()
p <- p + geom_line()
p <- p + geom_errorbar(aes(ymin=departure.time - ci, ymax=departure.time + ci))
p <- p + facet_grid(. ~ car.level)
p <- p + xlab('Round') + ylab('Departure times')
p <- p + scale_color_discrete(name='Payoff Bus')
p <- p + theme(strip.background = element_blank(),
               strip.text.y = element_blank())
p


# Saving file.
filepath <- paste0(IMGDIR, 'time_car_by_condition.jpg')
ggsave(filepath)

# Round 1, cars only.

mydata <- data[data$decision == "car" & data$round == 1,]

p <- ggplot(mydata, aes(departure.time, color=payoff.bus))
p <- p + geom_density()
p <- p + xlab('Round') + ylab('Departure times')
p <- p + scale_color_discrete(name='Payoff Bus')
p <- p + theme(strip.background = element_blank(),
               strip.text.y = element_blank())
p


a <- hist(mydata$departure.time,63)
b <- summarySE(mydata, "payoff", "departure.time", na.rm=TRUE)

propensities <- b$N

# 5. Decision (Car vs Bus)


mysummary <- summarySE(data, "bus", c("payoff.bus", "car.level", "car.level.num"), na.rm=TRUE)
mysummary$efficiency <- 1 - ((1 - mysummary$car.level.num) - mysummary$bus)


# Only round 30.
mysummary <- summarySE(data[data$round == 30,], "bus", c("payoff.bus", "car.level", "car.level.num"), na.rm=TRUE)
mysummary$efficiency <- 1 - ((1 - mysummary$car.level.num) - mysummary$bus)

# Payoff distribution (hist) by bus payoff and car level.
p <- ggplot(mysummary, aes(x=payoff.bus,y=bus, color=payoff.bus, fill=payoff.bus))
p <- p + geom_bar(alpha=0.5, position="dodge", stat="identity")
p <- p + geom_errorbar(aes(ymin=bus - ci, ymax=bus + ci), color="black", width=0.3)
p <- p + geom_hline(aes(yintercept=(1-car.level.num)))
p <- p + facet_grid(. ~ car.level)
p <- p + xlab('') + ylab('Frequency Bus Takers')
p <- p + scale_fill_discrete(name='Payoff Bus')
p <- p + scale_color_discrete(name='Payoff Bus')
p <- p + theme(strip.background = element_blank(),
               strip.text.y = element_blank())
p <- p + ggtitle('Frequency Bus takers')
p

# Saving file.
filepath <- paste0(IMGDIR, 'freq_bus_takers_by_condition.jpg')
ggsave(filepath)



# Payoff distribution (hist) by bus payoff and car level. (Distance from ideal level)
p <- ggplot(mysummary, aes(x=payoff.bus, y=efficiency, color=payoff.bus, fill=payoff.bus))
p <- p + geom_bar(alpha=0.5, position="dodge", stat="identity")
#p <- p + geom_errorbar(aes(ymin=bus - ci, ymax=bus + ci), color="black", width=0.3)
p <- p + facet_grid(. ~ car.level)
p <- p + xlab('') + ylab('Efficiency')
p <- p + scale_fill_discrete(name='Payoff Bus')
p <- p + scale_color_discrete(name='Payoff Bus')
p <- p + theme(strip.background = element_blank(),
               strip.text.y = element_blank())
p <- p + ggtitle('Efficiency = ratio max / actual Payoff')
p

# Saving file.
filepath <- paste0(IMGDIR, 'efficiency_by_condition.jpg')
ggsave(filepath)

# 5. Decision Car/Bus (in time)

mysummary <- summarySE(data, "bus", c("payoff.bus", "car.level",
                                      "car.level.num", "round"), na.rm=TRUE)

mysummary$efficiency <- 1 - ((1 - mysummary$car.level.num) - mysummary$bus)

# Payoff distribution by bus payoff and car level.
p <- ggplot(mysummary, aes(round, bus, color=payoff.bus))
p <- p + geom_hline(aes(yintercept=(1-car.level.num)))
p <- p + geom_point()
p <- p + geom_line()
p <- p + geom_errorbar(aes(ymin=bus - ci, ymax=bus + ci))
p <- p + facet_grid(. ~ car.level)
p <- p + xlab('Round') + ylab('Share of Bus Takers')
p <- p + scale_color_discrete(name='Payoff Bus')
p <- p + theme(strip.background = element_blank(),
               strip.text.y = element_blank())
p

# Saving file.
filepath <- paste0(IMGDIR, 'freq_bus_takers_by_round_by_condition.jpg')
ggsave(filepath)

# 6. Decision times


# Payoff distribution by bus payoff and car level.
p <- ggplot(data, aes(time.decision, color=payoff.bus, fill=payoff.bus))
p <- p + geom_density(alpha=0.5)
p <- p + facet_grid(. ~ car.level)
p <- p + xlab('Normalized Payoffs') + ylab('Density')
p <- p + scale_fill_discrete(name='Payoff Bus')
p <- p + scale_color_discrete(name='Payoff Bus')
p <- p + theme(strip.background = element_blank(),
               strip.text.y = element_blank())
p

mysummary <- summarySE(data, "time.decision", c("payoff.bus", "car.level",
                                                "round"), na.rm=TRUE)


# Payoff distribution by bus payoff and car level.
p <- ggplot(mysummary, aes(round, time.decision, color=payoff.bus))
p <- p + geom_point()
p <- p + geom_line()
p <- p + geom_errorbar(aes(ymin=time.decision - ci, ymax=time.decision + ci))
p <- p + facet_grid(. ~ car.level)
p <- p + xlab('Round') + ylab('Number of Bus Decisions')
p <- p + scale_color_discrete(name='Payoff Bus')
p <- p + theme(strip.background = element_blank(),
               strip.text.y = element_blank())
p

mysummary <- summarySE(data, "time.results", c("payoff.bus", "car.level",
                                                "round"), na.rm=TRUE)
# Payoff distribution by bus payoff and car level.
p <- ggplot(mysummary, aes(round, time.results, color=payoff.bus))
p <- p + geom_point()
p <- p + geom_line()
p <- p + geom_errorbar(aes(ymin=time.results - ci, ymax=time.results + ci))
p <- p + facet_grid(. ~ car.level)
p <- p + xlab('Round') + ylab('Number of Bus Decisions')
p <- p + scale_color_discrete(name='Payoff Bus')
p <- p + theme(strip.background = element_blank(),
               strip.text.y = element_blank())
p

# No differences across conditions for the time to make a decision
# or look a the results


# Decision times (car vs bus)

mydata <- data[complete.cases(data, data$decision),]
mydata <- data[data$decision != "",]
mysummary <- summarySE(mydata, "time.decision", c("payoff.bus", "car.level",
                                                  "round", "decision"),
                       na.rm=TRUE)


# Payoff distribution by bus payoff and car level.
p <- ggplot(mysummary, aes(round, time.decision, color=decision))
p <- p + geom_point()
p <- p + geom_line()
p <- p + geom_errorbar(aes(ymin=time.decision - ci, ymax=time.decision + ci))
p <- p + facet_grid(payoff.bus ~ car.level)
p <- p + xlab('Round') + ylab('Number of Bus Decisions')
p <- p + scale_color_discrete(name='Decision')
p <- p + theme(strip.background = element_blank(),
               strip.text.y = element_blank())
p

mysummary <- summarySE(mydata, "time.results", c("payoff.bus", "car.level",
                                                  "round", "decision"),
                       na.rm=TRUE)


# Payoff distribution by bus payoff and car level.
p <- ggplot(mysummary, aes(round, time.results, color=decision))
p <- p + geom_point()
p <- p + geom_line()
p <- p + geom_errorbar(aes(ymin=time.results - ci, ymax=time.results + ci))
p <- p + facet_grid(payoff.bus ~ car.level)
p <- p + xlab('Round') + ylab('Number of Bus Decisions')
p <- p + scale_color_discrete(name='Decision')
p <- p + theme(strip.background = element_blank(),
               strip.text.y = element_blank())
p


mysummary <- summarySE(mydata, "time.decision", c("payoff.bus", "car.level",
                                                  "decision"), na.rm=TRUE)



# Payoff distribution (hist) by bus payoff and car level. (Distance from ideal level)
p <- ggplot(mysummary, aes(decision, time.decision, group=decision, fill=decision))
p <- p + geom_bar(position="dodge", stat="identity")
p <- p + geom_errorbar(aes(ymin=time.decision - ci, ymax=time.decision + ci), color="black", width=0.3)
p <- p + facet_grid(payoff.bus ~ car.level)
p <- p + xlab('') + ylab('Time to Decision')
p <- p + scale_fill_discrete(name='Decision')
p <- p + scale_color_discrete(name='Payoff Bus')
p <- p + theme(strip.background = element_blank())
p


p


# 7. Switching
##############

library(plm)

# Switching
pdata <- pdata.frame(data, index=c('player','round'))
pdata$decision.lag <- lag(pdata$decision, 1)
pdata$payoff.lag <- lag(pdata$payoff, 1)
pdata$departure.time.lag <- lag(pdata$departure.time, 1)
pdata$istimeout.decision.lag <- lag(pdata$istimeout.decision, 1)
pdata$got.car.lag <- lag(pdata$got.car, 1)

data <- as.data.frame(pdata)
data$round <- as.numeric(data$round)

data$decision.switch <- ifelse(data$decision != data$decision.lag, 1, 0)


mysummary <- summarySE(data, "decision.switch", c("payoff.bus", "car.level", "round"), na.rm=TRUE)


# Payoff distribution (hist) by bus payoff and car level. (Distance from ideal level)
p <- ggplot(mysummary, aes(round, decision.switch, group=payoff.bus, color=payoff.bus))
p <- p + geom_point()
p <- p + geom_line()
p <- p + geom_errorbar(aes(ymin=decision.switch - ci, ymax=decision.switch + ci))
p <- p + facet_grid(. ~ car.level)
p <- p + xlab('Round') + ylab('Avg. Switching')
p <- p + scale_color_discrete(name='Condition\nPayoff Bus')
p <- p + theme(strip.background = element_blank(),
               strip.text.y = element_blank())
p <- p + ggtitle('Strategy Switching by round')
p

# Saving file.
filepath <- paste0(IMGDIR, 'strategy_switching_by_round_by_condition.jpg')
ggsave(filepath)





# First 5 rounds.
mysummary <- summarySE(data[data$round < 6,], "decision.switch", c("payoff.bus", "car.level"), na.rm=TRUE)

# Last 5 rounds.
mysummary <- summarySE(data[data$round > 25,], "decision.switch", c("payoff.bus", "car.level"), na.rm=TRUE)

# 7. Car Departure Adjustment 
##############

mysummary <- summarySE(data[data$round < 6 & data$departure.time.car.diff != 0,], "departure.time.car.diff", c("payoff.bus", "car.level", "round"), na.rm=TRUE)


# Payoff distribution (hist) by bus payoff and car level. (Distance from ideal level)
p <- ggplot(mysummary, aes(round, departure.time.car.diff, group=payoff.bus, color=payoff.bus))
p <- p + geom_point()
p <- p + geom_line()
p <- p + geom_errorbar(aes(ymin=departure.time.car.diff - ci, ymax=departure.time.car.diff + ci))
p <- p + facet_grid(. ~ car.level)
p <- p + xlab('Round') + ylab('Avg. Adjustment')
p <- p + scale_color_discrete(name='Condition\nPayoff Bus')
p <- p + theme(strip.background = element_blank(),
               strip.text.y = element_blank())
#p <- p + ggtitle('Car  by round')
p


# Saving file.
filepath <- paste0(IMGDIR, 'car-departure-time_adjustment_by_round_by_condition.jpg')
ggsave(filepath)

## Analysis by Session

SAVE <- TRUE

for (s in sessions) {
  mydata <- data[data$session == s,]
  mydata <- mydata[complete.cases(mydata, mydata$decision),]
  # Condition
  c <- mydata$condition[1]
  # SUBIMGDIR
  SUBIMGDIR <- paste0(IMGDIR, s, "-", c, "/")                         
  if (!file.exists(SUBIMGDIR)) {
    dir.create(file.path(SUBIMGDIR))
  }
  if (SAVE == TRUE) {
    filepath <- paste0(SUBIMGDIR, 'player_decisions_by_round.jpg')
  } else {
    filepath <- FALSE
  }
  plotPlayerDecisionsByRound(mydata, filepath)  
}



summary <- summarySE(data, "departure.time", c("payoff.bus", "car.level"), na.rm=TRUE)
                                        
p <- ggplot(summary, aes(payoff.bus, departure.time, fill=payoff.bus))
p <- p + geom_bar(position="dodge",stat="identity")
p <- p + geom_errorbar(aes(ymax=departure.time + se, ymin = departure.time - se), width=0.3)
p <- p + facet_grid(~car.level)
p

summary <- summarySE(data, "bus", c("payoff.bus", "car.level"), na.rm=TRUE)
                                        
p <- ggplot(summary, aes(payoff.bus, bus, fill=payoff.bus))
p <- p + geom_bar(position="dodge",stat="identity")
p <- p + geom_errorbar(aes(ymax=bus + se, ymin = bus - se), width=0.3)
p <- p + facet_grid(~car.level)
p


# Alternative viz.
# # Tiles
# p <- ggplot(mydata, aes(round, player.short))
# p <- p + geom_tile(aes(fill = departure.time), color="white", size=3)
# p <- p + geom_tile(data = bus, fill="tomato4", color="white", size=3)
# p <- p + scale_fill_gradient(limits=c(0, 60), low="red", high="white")
# p
# # Bars
# p <- ggplot(mydata, aes(round, departure.time, group=player.short))
# p <- p + geom_bar(aes(color = got.car), position="dodge", stat="identity")
# p <- p + geom_bar(data = bus, aes(fill = departure.time, color="tomato4"), position="dodge", stat="identity")
# p <- p + facet_grid(player.short~.)
# p
