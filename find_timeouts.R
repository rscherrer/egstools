# This script was used to find what simulations timed out in a given folder
# 03-02-2020
# Raphaël Scherrer

rm(list = ls())

dir <- "/media/raphael/bigass/simulations_epistatic_large_unbiased"
files <- c("EI.dat", "SI.dat", "RI.dat")

setwd(dir)

folders <- list.files()[grep("sim_", list.files())]
timeouts <- rep(FALSE, length(folders))

for (i in seq_along(folders)) {

  folder <- folders[i]
  print(paste0(i, " out of ", length(folders)))

  setwd(folder)

  log <- read.delim(list.files()[grep("slurm-", list.files())])

  timeout <- FALSE
  for (j in seq_len(nrow(log))) {
    if (length(grep("TIMEOUT", log[j, ])) > 0) {
      timeout <- TRUE
    }
  }

  timeouts[i] <- timeout

  setwd(dir)

}

length(which(timeouts)) / length(timeouts)
relaunch.df <- as.data.frame(folders[which(timeouts)])
write.csv(relaunch.df, "relaunch.txt")

