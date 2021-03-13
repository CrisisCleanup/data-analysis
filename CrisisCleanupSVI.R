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

# hurricane
hurricane <- subset(ccd, incident_type == "hurricane")
hurricane <- data.frame(hurricane$work_type_key)
hurricane <- data.frame(table(unlist(hurricane)))
hurricane <- hurricane[order(-hurricane$Freq), ]
hurricane <- subset(hurricane, Freq >= 10000)
View(hurricane)

# flood
flood <- subset(ccd, incident_type == "flood")
flood <- data.frame(flood$work_type_key)
flood <- data.frame(table(unlist(flood)))
flood <- flood[order(-flood$Freq), ]
flood <- subset(flood, Freq >= 2000)
View(flood)

# tropical storm
ts <- subset(ccd, incident_type == "tropical_storm")
ts <- data.frame(ts$work_type_key)
ts <- data.frame(table(unlist(ts)))
ts <- ts[order(-ts$Freq), ]
ts <- subset(ts, Freq >= 70)
View(ts)

# tornado
tornado <- subset(ccd, incident_type == "tornado")
tornado <- data.frame(tornado$work_type_key)
tornado <- data.frame(table(unlist(tornado)))
tornado <- tornado[order(-tornado$Freq), ]
tornado <- subset(tornado, Freq >= 400)
View(tornado)

# wind
wind <- subset(ccd, incident_type == "wind")
wind <- data.frame(wind$work_type_key)
wind <- data.frame(table(unlist(wind)))
wind <- wind[order(-wind$Freq), ]
wind <- subset(wind, Freq >= 70)
View(wind)

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
