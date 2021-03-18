# install packages
install.packages("ggplot2")
install.packages("pivottabler")
install.packages("lubridate")

# load packages
library(ggplot2)
library(pivottabler)
library(lubridate)

# Crisis Cleanup data
ccd <- read.csv(file = "/users/danny/documents/capstone/ccd.csv")
ccd$created_at <- as.Date(ccd$created_at)

# incident type
it <- data.frame(ccd$incident_type, ccd$name)
it <- unique(it)
it <- data.frame(it$ccd.incident_type)
it <- data.frame(table(unlist(it)))
it <- subset(it, it$Freq > 1)
ggplot(it, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "identity") + 
  labs(x = "incident type", y = "incident count")

# work type
wt <- data.frame(ccd$work_type_key)
wt <- data.frame(table(unlist(wt)))
wt <- subset(wt, wt$Freq >= 20000)
wt <- wt[order(-wt$Freq), ]
ggplot(wt, aes(x = reorder(Var1, -Freq), y = Freq)) + 
  geom_bar(stat = "identity") + 
  labs(x = "work type", y = "work type count")

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

# 2018 SVI data
svi <- read.csv(file = "/users/danny/documents/capstone/SVI2018_US.csv")

# SVI US
svius <- subset(svi, RPL_THEMES != "-999")
svius <- data.frame(svius$ST_ABBR, svius$RPL_THEMES)
ggplot(svius, aes(x = svius.ST_ABBR, y = svius.RPL_THEMES, fill = svius.ST_ABBR)) + 
  geom_boxplot() + 
  theme(legend.position = "none")
  labs(x = "state", y = "svi")

# SVI Utah
sviutah <- subset(svi, STATE == "UTAH" & RPL_THEMES != "-999")
sviutah <- data.frame(sviutah$STATE, sviutah$RPL_THEMES)
ggplot(sviutah, aes(x = sviutah.STATE, y = sviutah.RPL_THEMES)) + 
  geom_boxplot() + 
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) + 
  theme(legend.position = "none") + 
  xlab("") + 
  labs(y = "svi")

# flood
flood <- subset(ccd, incident_type == "flood" & name == "Michigan Floods, May 2020")
View(flood)
ggplot(flood, aes(x = created_at, y = svi)) + 
  geom_point() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Michigan Floods 2020") + 
  labs(x = "time", y = "svi")

# hurricane
hurricane <- subset(ccd, incident_type == "hurricane" & name == "Hurricane Zeta")
View(hurricane)
ggplot(hurricane, aes(x = created_at, y = svi)) + 
  geom_point() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Hurricane Zeta 2020") + 
  labs(x = "time", y = "svi")
  
# tornado
tornado <- subset(ccd, incident_type == "tornado" & name == "Easter/April 2020 Tornadoes")
View(tornado)
ggplot(tornado, aes(x = created_at, y = svi)) + 
  geom_point() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Easter Tornadoes 2020") + 
  labs(x = "time", y = "svi")

# wind
wind <- subset(ccd, incident_type == "wind" & name == "Midwest Derecho, Aug 2020")
ggplot(wind, aes(x = created_at, y = svi)) + 
  geom_point() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Midwest Derecho 2020") + 
  labs(x = "time", y = "svi")
