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
it <- data.frame(ccd$incident_type, ccd$name)
it <- unique(it)
it <- data.frame(it$ccd.incident_type)
it <- data.frame(table(unlist(it)))
it <- subset(it, it$Freq > 1)
View(it)
ggplot(it, aes(x = reorder(it$Var1, -it$Freq), y = it$Freq)) +
  geom_bar(stat = "identity")

# work type
wt <- data.frame(ccd$work_type_key)
wt <- data.frame(table(unlist(wt)))
wt <- subset(wt, wt$Freq >= 20000)
wt <- wt[order(-wt$Freq), ]
View(wt)
ggplot(wt, aes(x = reorder(Var1, -Freq), y = Freq)) + 
  geom_bar(stat = "identity")

# work type by incident type
wtit <- data.frame(ccd$incident_type, ccd$work_type_key)
View(wtit)

# hurricane
hurricane <- subset(ccd, incident_type == "hurricane")
hurricane <- data.frame(hurricane$work_type_key)
hurricane <- data.frame(table(unlist(hurricane)))
hurricane <- hurricane[order(-hurricane$Freq), ]
hurricane <- subset(hurricane, Freq >= 10000)
View(hurricane)
ggplot(hurricane, aes(x = reorder(Var1, -Freq), y = Freq)) + 
  geom_bar(stat = "identity")

# flood
flood <- subset(ccd, incident_type == "flood")
flood <- data.frame(flood$work_type_key)
flood <- data.frame(table(unlist(flood)))
flood <- flood[order(-flood$Freq), ]
flood <- subset(flood, Freq >= 2000)
View(flood)
ggplot(flood, aes(x = Var1)) + 
  geom_bar()

# tropical storm
ts <- subset(ccd, incident_type == "tropical_storm")
ts <- data.frame(ts$work_type_key)
ts <- data.frame(table(unlist(ts)))
ts <- ts[order(-ts$Freq), ]
ts <- subset(ts, Freq >= 70)
View(ts)
ggplot(ts, aes(x = Var1)) + 
  geom_bar()

# tornado
tornado <- subset(ccd, incident_type == "tornado")
tornado <- data.frame(tornado$work_type_key)
tornado <- data.frame(table(unlist(tornado)))
tornado <- tornado[order(-tornado$Freq), ]
tornado <- subset(tornado, Freq >= 400)
View(tornado)
ggplot(tornado, aes(x = Var1)) + 
  geom_bar()

# wind
wind <- subset(ccd, incident_type == "wind")
wind <- data.frame(wind$work_type_key)
wind <- data.frame(table(unlist(wind)))
wind <- wind[order(-wind$Freq), ]
wind <- subset(wind, Freq >= 70)
View(wind)
ggplot(wind, aes(x = Var1)) + 
  geom_bar()

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
