# CAR-SHARING
library(ggplot2)
library(reshape2)

OVERDIR <- '/home/stefano/Documents/mypapers/kay_car/'
RDIR <- paste0(OVERDIR, 'R/')
source(paste0(RDIR, "init.R"))

DIR <- 'jul_16'
DIR <- 'jul_23'
DIR <- 'jul_27'
DIR <- 'jul_31'
DIR <- 'aug_19'
DIR <- 'aug_20'
# DIR <- 'test'

DATADIR <- paste0(OVERDIR, 'data/', DIR, '/')
setwd(DATADIR)


IMGDIR <- paste0(DATADIR, "img/")
# Create IMG dir if not existing
if (!file.exists(IMGDIR)) {
  dir.create(file.path(IMGDIR))
}

sessions <- seq(29,40)

data <- data.frame()
for (s in sessions) {
  tmp <- read.table(paste0('carsharing', s, '.csv'), sep=",", header = TRUE)
  tmp$car.level <- as.numeric(substring(tmp$condition[1], 11, 12))
  tmp$payoff.bus <- as.numeric(substring(tmp$condition[1], 15,16))
  tmp$payoff.car <- as.numeric(substring(tmp$condition[1], 17,18))
  tmp$decision <- as.factor(tmp$decision)
  tmp$got.car <- as.factor(tmp$got.car)
  if (nrow(data) == 0) {
    data <- tmp
  } else {
    data <- rbind(data, tmp)
  }
}

#mydata <- data

for (s in unique(data$session)) {
  # Subset data.
  mydata <- data[data$session == s,]
  # Condition
  c <- mydata$condition[1]
  # SUBIMGDIR
  SUBIMGDIR <- paste0(IMGDIR, s, "-", c, "/")                         
  if (!file.exists(SUBIMGDIR)) {
    dir.create(file.path(SUBIMGDIR))
  }
  # Overall distribution.
  p <- ggplot(mydata, aes(decision, color=decision, fill=decision), position="dodge")
  p <- p + geom_bar(stat="bin")
  p <- p + facet_wrap(~round)
  p
  #
  ggsave(paste0(SUBIMGDIR, 'decision_round_barplots.png'))
  #
  # By round.
  a <- dcast(mydata, round ~ decision) #a <- table(mydata$decision, mydata$round)
  class(a)
  #
  p <- ggplot(a, aes(round, bus))
  p <- p + geom_point(color="red")
  p <- p + geom_line(color="red")
  p <- p + geom_point(aes(round, car), color="blue")
  p <- p + geom_line(aes(round, car), color="blue")
  p
  #
  ggsave(paste0(SUBIMGDIR, 'decision_round_timeline.png'))
  #
  # And departure time.
  p <- ggplot(mydata, aes(round, departure.time))
  p <- p + geom_point(aes(color = decision, shape=got.car), size=5)
  p <- p + geom_line()
  p <- p + facet_grid(player ~ .)
  p
  #
  ggsave(paste0(SUBIMGDIR, 'decision_round_player.png'))
  #
  # Summaries
  summary <- summarySE(mydata, "departure.time", c("round", "decision"), na.rm=TRUE)
  summary2 <- summarySE(mydata, "departure.time", c("round"), na.rm=TRUE)
  #
  p <- ggplot(summary2, aes(round, departure.time))
  p <- p + geom_point()
  p <- p + geom_line()
  p <- p + geom_point(data = summary, aes(color = "car"))
  p <- p + geom_line(data = summary, aes(color = as.factor(decision), group=decision))
  p
  #
  ggsave(paste0(SUBIMGDIR, 'departure_time_round.png'))
  #
  # Time to decision.
  summary <- summarySE(mydata, "time.decision", c("round", "decision"), na.rm=TRUE)
  #
  p <- ggplot(summary, aes(round, time.decision, group=decision, color=decision))
  p <- p + geom_point()
  p <- p + geom_line()
  p
  #
  ggsave(paste0(SUBIMGDIR, 'time_decision.png'))
  #
  summary <- summarySE(mydata, "time.results", c("round", "decision"), na.rm=TRUE)
  p <- ggplot(summary, aes(round, time.results, group=decision, color=decision))
  p <- p + geom_point()
  p <- p + geom_line()
  p
  #
  ggsave(paste0(SUBIMGDIR, 'time_decision.png'))
}
