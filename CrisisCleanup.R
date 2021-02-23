# packages
library(ggplot2)

# svi data
svi <- read.csv(file = "/Users/danny/Documents/Capstone/SVI2018_US.csv")

# incident count by incident type
icit <- read.csv(file = "/Users/danny/Documents/Capstone/IncidentCountByIncidentType.csv")
sum(icit$incident_count)

icit$incident_type <- factor(icit$incident_type, levels = c("hurricane", "flood", "tornado", "tropical_storm", "virus",
                                                            "wind", "flood_tstorm", "fire", "snow", "volcano",
                                                            "rebuild", "mudslide"))
icit$incident_count <- factor(icit$incident_count, levels = icit$incident_count[order(icit$incident_count, decreasing = FALSE)])

ggplot(data = icit, aes(x = incident_type, y = incident_count)) + 
  geom_bar(stat = "identity")

# incident count by incident name
icin <- read.csv(file = "/Users/danny/Documents/Capstone/IncidentCountByIncidentName.csv")

# flood
flood <- subset(icin, incident_type == "flood")
View(flood)
ggplot(data = flood, aes(x = incident_name, y = incident_count)) +
  geom_bar(stat = "identity")

# hurricane
hurricane <- subset(icin, incident_type == "hurricane" & incident_name != "Hurricane Dorian (Bahamas)")
View(hurricane)

# tornado
tornado <- subset(icin, incident_type == "tornado")
View(tornado)

# tropical_storm
tropical_storm <- subset(icin, incident_type == "tropical_storm")

# virus
virus <- subset(icin, incident_type == "virus")

# wind
wind <- subset(icin, incident_type == "wind")
