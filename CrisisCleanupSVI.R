# install packages
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("pivottabler")
install.packages("tidyverse")

# load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(pivottabler)

# US map
us_states <- map_data("state")
c <- ggplot(data = us_states, mapping = aes(x = long, y = lat, group = group, fill = region))
c + geom_polygon() + guides(fill = FALSE)

# 2018 SVI data
svi <- read.csv(file = "/users/danny/documents/capstone/SVI2018_US.csv")
View(svi)
svilocation <- data.frame(svi$STATE, svi$COUNTY)
View(svilocation)

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
ggplot(it, aes(x = reorder(it$Var1, -it$Freq), y = it$Freq)) +
  geom_bar(stat = "identity")

# work type
wt <- data.frame(ccd$work_type_key)
wt <- data.frame(table(unlist(wt)))
wt <- subset(wt, wt$Freq >= 20000)
wt <- wt[order(-wt$Freq), ]
ggplot(wt, aes(x = reorder(Var1, -Freq), y = Freq)) + 
  geom_bar(stat = "identity")

# pivot table
itwt <- data.frame(ccd$incident_type, ccd$work_type_key)
itwt <- subset(itwt, (ccd.incident_type == "flood" | ccd.incident_type == "hurricane"| ccd.incident_type == "tornado" | 
                        ccd.incident_type == "wind") & 
                 (ccd.work_type_key == "tarp" | ccd.work_type_key == "muck_out" | ccd.work_type_key == "trees" | 
                    ccd.work_type_key == "debris" | ccd.work_type_key == "mold_remediation"))
pt <- PivotTable$new()
pt$addData(itwt)
pt$addColumnDataGroups("ccd.incident_type")
pt$addRowDataGroups("ccd.work_type_key")
pt$defineCalculation(calculationName = "TotalIncidents", summariseExpression = "n()")
pt$renderPivot()

# test pivot table
floodpt <- subset(ccd, incident_type == "flood" & (work_type_key == "debris" | work_type_key == "mold_remediation" | 
                                                     work_type_key == "muck_out" | work_type_key == "tarp" | 
                                                     work_type_key == "trees"))
hurricanept <- subset(ccd, incident_type == "hurricane" & (work_type_key == "debris" | work_type_key == "mold_remediation" | 
                                                     work_type_key == "muck_out" | work_type_key == "tarp" | 
                                                     work_type_key == "trees"))
tornadopt <- subset(ccd, incident_type == "tornado" & (work_type_key == "debris" | work_type_key == "mold_remediation" | 
                                                     work_type_key == "muck_out" | work_type_key == "tarp" | 
                                                     work_type_key == "trees"))
windpt <- subset(ccd, incident_type == "wind" & (work_type_key == "debris" | work_type_key == "mold_remediation" | 
                                                     work_type_key == "muck_out" | work_type_key == "tarp" | 
                                                     work_type_key == "trees"))

# flood
flood <- subset(ccd, incident_type == "flood")
flood <- data.frame(flood$work_type_key)
flood <- data.frame(table(unlist(flood)))
flood <- flood[order(-flood$Freq), ]
flood <- subset(flood, Freq >= 2000)
View(flood)
ggplot(flood, aes(x = Var1)) + 
  geom_bar()

# hurricane
hurricane <- subset(ccd, incident_type == "hurricane")
hurricane <- data.frame(hurricane$work_type_key)
hurricane <- data.frame(table(unlist(hurricane)))
hurricane <- hurricane[order(-hurricane$Freq), ]
hurricane <- subset(hurricane, Freq >= 10000)
View(hurricane)
ggplot(hurricane, aes(x = reorder(Var1, -Freq), y = Freq)) + 
  geom_bar(stat = "identity")

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
