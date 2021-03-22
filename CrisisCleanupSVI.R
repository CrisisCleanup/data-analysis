# install packages
install.packages("ggplot2")
install.packages("pivottabler")
install.packages("lubridate")

# load packages
library(ggplot2)
library(pivottabler)
library(lubridate)

# Crisis Cleanup data
ccd <- read.csv(file = "/users/danny/documents/capstone/ccd_v2.csv")
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
  theme(legend.position = "none") + 
  labs(x = "state", y = "svi")

# SVI Utah
sviutah <- subset(svi, STATE == "UTAH" & RPL_THEMES != "-999")
sviutah <- data.frame(sviutah$STATE, sviutah$RPL_THEMES)
ggplot(sviutah, aes(x = sviutah.STATE, y = sviutah.RPL_THEMES)) + 
  geom_boxplot() + 
  geom_jitter() + 
  theme(legend.position = "none") + 
  xlab("") + 
  labs(y = "svi")

# SVI Utah Count
sviutcounty <- subset(svi, STATE == "UTAH" & RPL_THEMES != "-999")
sviutcounty <- data.frame(svicounty$COUNTY, svicounty$RPL_THEMES)
ggplot(svicounty, aes(x = svicounty.COUNTY, y = svicounty.RPL_THEMES, fill = svicounty.COUNTY)) + 
  geom_boxplot() + 
  theme(legend.position = "none", axis.text.x = element_text(angle = 90)) +
  xlab("") +
  labs(y = "svi")

# SVI flood
sviflood <- subset(svi, STATE == "MICHIGAN" & RPL_THEMES != "-999" & 
                     (COUNTY == "Midland" | COUNTY == "Gladwin" | COUNTY == "Clare" | 
                      COUNTY == "Saginaw" | COUNTY == "Arenac" | COUNTY == "Gratiot" | 
                      COUNTY == "Bay" | COUNTY == "Lenawee" | COUNTY == "Iosco" | 
                      COUNTY == "Isabella" | COUNTY == "Wayne" | COUNTY == "Muskegon" | 
                      COUNTY == "Gladwin" | COUNTY == "Charlevoix" | COUNTY == "Berrien" | 
                      COUNTY == "Washtenaw" | COUNTY == "Macomb" | COUNTY == "Clinton" | 
                      COUNTY == "Kent" | COUNTY == "St. Clair" | COUNTY == "Livingston"))
sviflood <- data.frame(sviflood$COUNTY, sviflood$ST_ABBR, sviflood$RPL_THEMES)
sviflood$ctyst <- paste(sviflood$sviflood.COUNTY, sviflood$sviflood.ST_ABBR, sep = " County, ")
ggplot(sviflood, aes(x = ctyst, y = sviflood.RPL_THEMES, fill = ctyst)) + 
  geom_boxplot() + 
  theme(legend.position = "none", axis.text = element_text(angle = 90)) + 
  xlab("") + 
  labs(y = "svi")

# flood
flood <- subset(ccd, incident_type == "flood" & name == "Michigan Floods, May 2020" & state == "Michigan")
ggplot(flood, aes(x = created_at, y = svi)) + 
  geom_point() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Michigan Floods 2020") + 
  labs(x = "time", y = "svi")

# SVI hurricane
svihurricane <- subset(svi, RPL_THEMES != "-999" & (STATE == "LOUISIANA" | STATE == "ALABAMA" | STATE == "MISSISSIPPI" | 
                                                      STATE == "NORTH CAROLINA" | STATE == "GEORGIA" | STATE == "FLORIDA") & 
                         (COUNTY == "St. Tammy" | COUNTY == "Jefferson" | COUNTY == "Orleans" | COUNTY == "Mobile" | 
                            COUNTY == "Barldwin" | COUNTY == "Forrest" | COUNTY == "Lamar" | COUNTY == "St. Bernard" | 
                            COUNTY == "Pearl River" | COUNTY == "Clarke" | COUNTY == "Harrison" | COUNTY == "St. John the Baptist" | 
                            COUNTY == "Lafourche" | COUNTY == "Hancock" | COUNTY == "Plaquemines" | COUNTY == "Mecklenburg" | 
                            COUNTY == "Fulton" | COUNTY == "Ascension" | COUNTY == "Wilcox" | COUNTY == "Jackson" | 
                            COUNTY == "Autauga" | COUNTY == "Stone" | COUNTY == "East Baton Rouge" | COUNTY == "Washington" | 
                            COUNTY == "George" | COUNTY == "Douglas" | COUNTY == "Dallas" | COUNTY == "Lumpkin" | 
                            COUNTY == "Escambia" | COUNTY == "Union" | COUNTY == "Dekalb" | COUNTY == "Bartow" | 
                            COUNTY == "St. Charles" | COUNTY == "Gwinnett" | COUNTY == "Marengo" | COUNTY == "Marshall" | 
                            COUNTY == "Cobb" | COUNTY == "Paulding" | COUNTY == "Tallapoosa" | COUNTY == "Talladega" | 
                            COUNTY == "White" | COUNTY == "Terrebonne" | COUNTY == "Shelby" | "Greene" | COUNTY == "Bibb" | 
                            COUNTY == "Chilton" | COUNTY == "Perry" | COUNTY == "Montgomery"))

unique(svi$COUNTY)

# hurricane
hurricane <- subset(ccd, incident_type == "hurricane" & name == "Hurricane Zeta")
ggplot(hurricane, aes(x = created_at, y = svi)) + 
  geom_point() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Hurricane Zeta 2020") + 
  labs(x = "time", y = "svi")
  
colnames(hurricane)
unique(hurricane$state)
unique(hurricane$county)

# tornado
tornado <- subset(ccd, incident_type == "tornado" & name == "Easter/April 2020 Tornadoes")
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
