# install packages
install.packages("dplyr")
install.packages("ggplot2")

# load packages
library(dplyr)
library(ggplot2)

# 2018 SVI data
svi <- read.csv(file = "/users/danny/documents/capstone/SVI2018_US.csv")
View(svi)

# Crisis Cleanup data
ccd <- read.csv(file = "/users/danny/documents/capstone/ccd.csv")
View(ccd)

