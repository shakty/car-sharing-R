# CAR-SHARING
library(ggplot2)
library(reshape2)

OVERDIR <- '/home/stefano/Documents/mypapers/kay_car/'
RDIR <- paste0(OVERDIR, 'R/')
source(paste0(RDIR, "init.R"))

OLD <- c('jul_16',
         'jul_23',
         'jul_27',
         'jul_31')

# First sessions.
DIRS <-c('aug_19',
         'aug_20',
         'aug_21',
         'aug_27',
         'sep_01'
         )

# Additional sessions.
DIRS <-c('aug_19',
         'aug_20',
         'aug_21',
         'aug_27',
         'sep_01',
         'nov_02',
         'nov_05'
         )



DATADIR <- paste0(OVERDIR, 'data/')
setwd(DATADIR)
OUTDIR <-  paste0(DATADIR, 'ALL/')
IMGDIR <- paste0(OUTDIR, "img/")
# Create IMG dir if not existing
if (!file.exists(IMGDIR)) {
  dir.create(file.path(IMGDIR))
}



data <- data.frame()
for (d in DIRS) {
  DIR <- paste0(DATADIR, d, '/')
  files <- list.files(pattern = "\\.csv$", path = DIR)
  for (f in files) {
    tmp <- read.table(paste0(DIR, f), sep=",", header = TRUE)
    tmp$car.level <- as.numeric(substring(tmp$condition[1], 11, 12))
    tmp$payoff.bus <- as.numeric(substring(tmp$condition[1], 15,16))
    tmp$payoff.car <- as.numeric(substring(tmp$condition[1], 17,18))
    if (nrow(data) == 0) {
      data <- tmp
    } else {
      data <- rbind(data, tmp)
    }
  }
}


data$decision <- as.factor(data$decision)
data$got.car <- as.factor(data$got.car)
data$istimeout.decision <- ifelse(data$istimeout.decision == "false", 0, 1)

write.table(data, paste0(OUTDIR, 'carsharing_all.csv'), sep=",", row.names=FALSE)
