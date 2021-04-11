# install packages
install.packages("ggplot2")
install.packages("pivottabler")
install.packages("lubridate")

# load packages
library(ggplot2)
library(pivottabler)
library(lubridate)

# CC data
ccd <- read.csv(file = "/users/danny/documents/capstone/ccd_v2.csv")
ccd$created_at <- as.Date(ccd$created_at)

# CC incident type
it <- data.frame(ccd$incident_type, ccd$name)
it <- unique(it)
it <- data.frame(it$ccd.incident_type)
it <- data.frame(table(unlist(it)))
it <- subset(it, it$Freq > 1)
ggplot(it, aes(x = reorder(Var1, -Freq), y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none") + 
  labs(x = "incident type", y = "incident count")

# CC work type
wt <- data.frame(ccd$work_type_key)
wt <- data.frame(table(unlist(wt)))
wt <- subset(wt, wt$Freq >= 20000)
wt <- wt[order(-wt$Freq), ]
ggplot(wt, aes(x = reorder(Var1, -Freq), y = Freq, fill = Var1)) + 
  geom_bar(stat = "identity") + 
  theme(legend.position = "none") + 
  labs(x = "work type", y = "work type count")

# CC pivot table
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

# SVI data
svi <- read.csv(file = "/users/danny/documents/capstone/SVI2018_US.csv")

# SVI US
svius <- subset(svi, RPL_THEMES != "-999")
svius <- data.frame(svius$ST_ABBR, svius$RPL_THEMES)
ggplot(svius, aes(x = reorder(svius.ST_ABBR, svius.RPL_THEMES), y = svius.RPL_THEMES, fill = svius.ST_ABBR))  + 
  geom_boxplot() + 
  theme(legend.position = "none") + 
  labs(x = "state", y = "svi")

# SVI Michigan Floods, May 2020
sviflood <- subset(svi, STATE == "MICHIGAN" & RPL_THEMES != "-999" & 
                     (COUNTY == "Midland" | COUNTY == "Gladwin" | COUNTY == "Clare" | 
                      COUNTY == "Saginaw" | COUNTY == "Arenac" | COUNTY == "Gratiot" | 
                      COUNTY == "Bay" | COUNTY == "Lenawee" | COUNTY == "Iosco" | 
                      COUNTY == "Isabella" | COUNTY == "Wayne" | COUNTY == "Muskegon" | 
                      COUNTY == "Gladwin" | COUNTY == "Charlevoix" | COUNTY == "Berrien" | 
                      COUNTY == "Washtenaw" | COUNTY == "Macomb" | COUNTY == "Clinton" | 
                      COUNTY == "Kent" | COUNTY == "St. Clair" | COUNTY == "Livingston"))
svifloodlocation <- data.frame(sviflood$LOCATION, sviflood$RPL_THEMES)
  # Average SVI
mean(svifloodlocation$sviflood.RPL_THEMES)
  # Number of SVI locations
nrow(svifloodlocation)
sviflood <- data.frame(sviflood$COUNTY, sviflood$ST_ABBR, sviflood$RPL_THEMES)
sviflood$ctyst <- paste(sviflood$sviflood.COUNTY, sviflood$sviflood.ST_ABBR, sep = ", ")
  # Chart
ggplot(sviflood, aes(x = reorder(ctyst, sviflood.RPL_THEMES), y = sviflood.RPL_THEMES, fill = ctyst)) + 
  geom_boxplot() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.text = element_text(angle = 90)) + 
#  ggtitle("Michigan Floods 2020") + 
  labs(x = "county", y = "svi")

# CC Michigan Floods, May 2020
ccdflood <- subset(ccd, incident_type == "flood" & name == "Michigan Floods, May 2020" & state == "Michigan")
  # Number of worksites
ccdfloodloct <- data.frame(ccdflood$location)
worksites <- nrow(ccdflood)
  # Number of residences
ccdfloodlocu <- data.frame(unique(ccdfloodloct$ccdflood.location))
residences <- nrow(ccdfloodlocu)
  # Crisis Cleanup total impact (residences / approved FEMA applications)
residences / 2754
  # Chart
ggplot(ccdflood, aes(x = created_at, y = svi)) + 
  geom_point() + 
#  theme(plot.title = element_text(hjust = 0.5)) + 
#  ggtitle("Michigan Floods 2020") + 
  labs(x = "time", y = "svi")
