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
unique(ccd$name)

# incident type
it <- data.frame(ccd$incident_type)
it <- data.frame(table(unlist(it)))
it <- it[order(-it$Freq), ]
it <- subset(it, Freq >= 4000)
View(it)
ggplot(it, aes(x = Var1)) +
  geom_bar()

# work type
wt <- data.frame(ccd$work_type_key)
wt <- data.frame(table(unlist(wt)))
wt <- wt[order(-wt$Freq), ]
wt <- subset(wt, Freq >= 20000)
View(wt)
ggplot(wt, aes(x = Var1)) + 
  geom_bar()

# incident type work type
itwt <- data.frame(ccd$incident_type, ccd$work_type_key)

View(itwt)

# Hurricane Sandy
sandy <- subset(ccd[, c("name", "created_at", "svi")], name == "Hurricane Sandy Recovery")
sandy <- sandy[order(sandy$created_at), ]
View(sandy)

ggplot(sandy, aes(x = created_at, y = svi)) +
  geom_point()

# Hurricane Michael
michael <- subset(ccd[, c("name", "created_at", "svi")], name == "Hurricane Michael")
michael <- michael[order(michael$created_at), ]
View(michael)

ggplot(michael, aes(x = created_at, y = svi)) + 
  geom_point()
