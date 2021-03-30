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
ggplot(it, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "identity") + 
  labs(x = "incident type", y = "incident count")

# CC work type
wt <- data.frame(ccd$work_type_key)
wt <- data.frame(table(unlist(wt)))
wt <- subset(wt, wt$Freq >= 20000)
wt <- wt[order(-wt$Freq), ]
ggplot(wt, aes(x = reorder(Var1, -Freq), y = Freq)) + 
  geom_bar(stat = "identity") + 
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
sviflood <- data.frame(sviflood$COUNTY, sviflood$ST_ABBR, sviflood$RPL_THEMES)
sviflood$ctyst <- paste(sviflood$sviflood.COUNTY, sviflood$sviflood.ST_ABBR, sep = ", ")
ggplot(sviflood, aes(x = reorder(ctyst, sviflood.RPL_THEMES), y = sviflood.RPL_THEMES, fill = ctyst)) + 
  geom_boxplot() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.text = element_text(angle = 90)) + 
  ggtitle("Michigan Floods 2020") + 
  labs(x = "county", y = "svi")

# CC Michigan Floods, May 2020
flood <- subset(ccd, incident_type == "flood" & name == "Michigan Floods, May 2020" & state == "Michigan")
ggplot(flood, aes(x = created_at, y = svi)) + 
  geom_point() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Michigan Floods 2020") + 
  labs(x = "time", y = "svi")

# SVI Hurricane Zeta
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
                            COUNTY == "White" | COUNTY == "Terrebonne" | COUNTY == "Shelby" | COUNTY == "Greene" | 
                            COUNTY == "Bibb" | COUNTY == "Chilton" | COUNTY == "Perry" | COUNTY == "Montgomery"))
svihurricane <- data.frame(svihurricane$COUNTY, svihurricane$ST_ABBR, svihurricane$RPL_THEMES)
svihurricane$ctyst <- paste(svihurricane$svihurricane.COUNTY, svihurricane$svihurricane.ST_ABBR, sep = ", ")
ggplot(svihurricane, aes(x = reorder(ctyst, svihurricane.RPL_THEMES), y = svihurricane.RPL_THEMES, fill = ctyst)) + 
  geom_boxplot() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.text = element_text(angle = 90)) + 
  ggtitle("Hurricane Zeta 2020") + 
  labs( x = "county", y = "svi")

# CC Hurricane Zeta
hurricane <- subset(ccd, incident_type == "hurricane" & name == "Hurricane Zeta")
ggplot(hurricane, aes(x = created_at, y = svi)) + 
  geom_point() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Hurricane Zeta 2020") + 
  labs(x = "time", y = "svi")

# SVI Easter/April 2020 Tornadoes
svitornado <- subset(svi, RPL_THEMES != "-999" & (STATE == "SOUTH CAROLINA" | STATE == "ALABAMA" | STATE == "MISSISSIPPI" | 
                                                    STATE == "LOUISIANA" | STATE == "TENNESSEE" | STATE == "GEORGIA" | 
                                                    STATE == "NORTH CAROLINA" | STATE == "FLORIDA" | STATE == "OKLAHOMA" | 
                                                    STATE == "TEXAS" | STATE == "CONNECTICUT" | STATE == "OHIO" | 
                                                    STATE == "PENNSYLVANIA" | STATE == "ARKANSAS" | STATE == "MASSACHUSETTS" | 
                                                    STATE == "MAINE" | STATE == "VIRGINIA" | STATE == "KENTUCKY") & 
                       (COUNTY == "Colleton" | COUNTY == "York" | COUNTY == "Etowah" | COUNTY == "Covington" | COUNTY == "Jefferson Davis" | 
                          COUNTY == "Jones" | COUNTY == "Ouachita" | COUNTY == "Hampton" | COUNTY == "Oconee" | COUNTY == "Hamilton" | 
                          COUNTY == "Jasper" | COUNTY == "Clarke" | COUNTY == "Catoosa" | COUNTY == "Greenville" | COUNTY == "Walker" | 
                          COUNTY == "Walthall" | COUNTY == "Pickens" | COUNTY == "Hoke" | COUNTY == "Bradley" | COUNTY == "Screven" | 
                          COUNTY == "Murray" | COUNTY == "Forrest" | COUNTY == "Clay" | COUNTY == "Stewart" | COUNTY == "Santa Rosa" | 
                          COUNTY == "Lamar" | COUNTY == "Marion" | COUNTY == "Calhoun" | COUNTY == "Lauderdale" | COUNTY == "Lee" | 
                          COUNTY == "Dooly" | COUNTY == "Tallapoosa" | COUNTY == "Marion" | COUNTY == "Rankin" | COUNTY == "Rogers" | 
                          COUNTY == "Polk" | COUNTY == "Richland" | COUNTY == "Harrison" | COUNTY == "Fairfield" | COUNTY == "Montgomery" | 
                          COUNTY == "Oklahoma" | COUNTY == "Pender" | COUNTY == "Brunswick" | COUNTY == "Washington" | COUNTY == "Lawrence" | 
                          COUNTY == "Knox" | COUNTY == "Whitfield" | COUNTY == "Banks" | COUNTY == "Coahoma" | COUNTY == "Henry" | 
                          COUNTY == "Jefferson" | COUNTY == "Bladen" | COUNTY == "Upson" | COUNTY == "Tangipahoa" | COUNTY == "Cherokee" | 
                          COUNTY == "Kaufman" | COUNTY == "Sevier" | COUNTY == "Davidson" | COUNTY == "Wilson" | COUNTY == "Maury" | 
                          COUNTY == "Essex" | COUNTY == "Coffee" | COUNTY == "Robertson" | COUNTY == "Rutherford" | COUNTY == "Henderson" | 
                          COUNTY == "Crittenden" | COUNTY == "Ohio" | COUNTY == "Calcasieu" | COUNTY == "Orange" | COUNTY == "Vernon"))
svitornado <- data.frame(svitornado$COUNTY, svitornado$ST_ABBR, svitornado$RPL_THEMES)
svitornado$ctyst <- paste(svitornado$svitornado.COUNTY, svitornado$svitornado.ST_ABBR, sep = ", ")
ggplot(svitornado, aes(x = reorder(ctyst, svitornado.RPL_THEMES), y = svitornado.RPL_THEMES, fill = ctyst)) + 
  geom_boxplot() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.text.x = element_blank()) + 
  ggtitle("Easter/April 2020 Tornadoes") + 
  labs(x = "Crisis affecting the following states: AL, AR, CT, FL, GA, KT, LA, MA, ME, MI, NC, OH, OK, PA, SC, TN, TX, VA", y = "svi")

# CC Easter/April 2020 Tornadoes
tornado <- subset(ccd, incident_type == "tornado" & name == "Easter/April 2020 Tornadoes")
ggplot(tornado, aes(x = created_at, y = svi)) + 
  geom_point() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Easter Tornadoes 2020") + 
  labs(x = "time", y = "svi")

# SVI Midwest Derecho, Aug 2020
sviwind <- subset(svi, RPL_THEMES != "-999" & (STATE == "INDIANA" | STATE == "MISSOURI" | STATE == "IOWA" | STATE == "ILLINOIS" | 
                                                 STATE == "KANSAS" | STATE == "MICHIGAN" | STATE == "MINNESOTA") & 
                    (COUNTY == "Fayette" | COUNTY == "Boone" | COUNTY == "Jasper" | COUNTY == "Cedar" | COUNTY == "St. Clair" | 
                    COUNTY == "Geary" | COUNTY == "Linn" | COUNTY == "Clinton" | COUNTY == "Polk" | COUNTY == "Marshall" | 
                    COUNTY == "Tama" | COUNTY == "Whiteside" | COUNTY == "Benton" | COUNTY == "Dallas" | COUNTY == "Hardin" | 
                    COUNTY == "Story" | COUNTY == "Macomb" | COUNTY == "Scott" | COUNTY == "Iowa" | COUNTY == "Poweshiek" | 
                    COUNTY == "Jones" | COUNTY == "Cook" | COUNTY == "Greene" | COUNTY == "Warren" | COUNTY == "Johnson" | 
                    COUNTY == "Monroe" | COUNTY == "Marion" | COUNTY == "Cass" | COUNTY == "Coles"))
sviwind <- data.frame(sviwind$COUNTY, sviwind$ST_ABBR, sviwind$RPL_THEMES)
sviwind$ctyst <- paste(sviwind$sviwind.COUNTY, sviwind$sviwind.ST_ABBR, sep = ", ")
ggplot(sviwind, aes(x = reorder(ctyst, sviwind.RPL_THEMES), y = sviwind.RPL_THEMES, fill = ctyst)) + 
  geom_boxplot() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.text = element_text(angle = 90)) + 
  ggtitle("Easter/April 2020 Tornadoes") + 
  labs(x = "county", y = "svi")

# CC Midwest Derecho, Aug 2020
wind <- subset(ccd, incident_type == "wind" & name == "Midwest Derecho, Aug 2020")
ggplot(wind, aes(x = created_at, y = svi)) + 
  geom_point() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Midwest Derecho 2020") + 
  labs(x = "time", y = "svi")
