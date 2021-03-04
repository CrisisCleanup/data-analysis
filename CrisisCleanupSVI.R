# install packages
install.packages("ggplot2")
install.packages("leaflet")

# packages
library(ggplot2)
library(leaflet)

# notes
# histogram with colored tails

# svi data
svi <- read.csv(file = "/Users/danny/Documents/Capstone/SVI2018_US.csv")
View(svi)

# incident count by incident type
icit <- read.csv(file = "/Users/danny/Documents/Capstone/IncidentCountByIncidentType.csv")
View(icit)

hist(icit$incident_count, xlab = "Incident Type", ylab = "Incident Count")

icit$incident_type <- factor(icit$incident_type, levels = c("hurricane", "flood", "tornado", "tropical_storm", "virus",
                                                            "wind", "flood_tstorm", "fire", "snow", "volcano",
                                                            "rebuild", "mudslide"))
icit$incident_count <- factor(icit$incident_count, levels = icit$incident_count[order(icit$incident_count, decreasing = FALSE)])

ggplot(data = icit, aes(x = incident_type, y = incident_count)) + 
  geom_bar(stat = "identity")

# incident count by incident name
icin <- read.csv(file = "/Users/danny/Documents/Capstone/IncidentCountByIncidentName.csv")
View(icin)

# flood
flood <- subset(icin, incident_type == "flood")
View(flood)
ggplot(data = flood, aes(x = incident_name, y = incident_count)) +
  geom_bar(stat = "identity")

# hurricane
hurricane <- subset(icin, incident_type == "hurricane" & incident_name != "Hurricane Dorian (Bahamas)")
View(hurricane)
my_hist <- hist(hurricane$incident_count, breaks = 100, plot = F)
my_color <- ifelse(my_hist$breaks < -10, rgb(0.2,0.8,0.5,0.5), ifelse(my_hist$breaks >= 10, "purple", rgb(0.2,0.2,0.2,0.2)))
plot(my_hist, col = my_color, border = F, main = "", xlab = "value of the variable", xlim = c(0, 100))

# tornado
tornado <- subset(icin, incident_type == "tornado")
View(tornado)

# tropical_storm
tropical_storm <- subset(icin, incident_type == "tropical_storm")

# virus
virus <- subset(icin, incident_type == "virus")

# wind
wind <- subset(icin, incident_type == "wind")
