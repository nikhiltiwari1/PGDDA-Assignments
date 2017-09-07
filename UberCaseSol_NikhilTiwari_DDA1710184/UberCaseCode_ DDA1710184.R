# creating & setting working directory

dir.create(file.path("/Users/Nikhil/Desktop", "UberCase"), showWarnings = FALSE)
setwd(file.path("/Users/Nikhil/Desktop", "UberCase"))

# Downloading Uber Request Data
fileUrl <- "https://cdn.upgrad.com/UpGrad/temp/76b3b6a4-d87d-4e82-b1c3-3f6e10b9c076/Uber%20Request%20Data.csv"
download.file(fileUrl, destfile = "Uber Request Data.csv", method = "curl")

# loading relevant packages
library(readr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(grid)
library(gridExtra)


# Loading Data 
uber_req <- read.csv("Uber Request Data.csv", stringsAsFactors = FALSE)

# looking at data
str(uber_req)
head(uber_req)

# data Cleaning 
uber_req$Pickup.point <- as.factor(uber_req$Pickup.point)
uber_req$Status <- as.factor(uber_req$Status)
uber_req$Driver.id <- as.factor(uber_req$Driver.id)

# Data Cleaning -- Date formats
uber_req$Request.timestamp <- parse_date_time(x = uber_req$Request.timestamp, orders = c("%d %m %Y %H%M","%d %m %Y %H:%M:%S"), locale = "en_US")
uber_req$Drop.timestamp <- parse_date_time(x = uber_req$Drop.timestamp, orders = c("%d %m %Y %H%M","%d %m %Y %H:%M:%S"), locale = "en_US")

# Sorting Data based on Driver.id and Request Time
uber_req <- uber_req[order(uber_req$Driver.id, uber_req$Request.timestamp),]

# calculating the Triptime 
uber_req$triptime<-as.numeric(uber_req$Drop.timestamp-uber_req$Request.timestamp)
Average_trip_time <- mean(!is.na(uber_req$triptime))*60

# Culling out hour of day from Request time and Drop time
uber_req1 <- separate(data = uber_req, col = "Request.timestamp", into = c("req.date","req.time"), sep = " ")
uber_req2 <- separate(data = uber_req1, col = "Drop.timestamp", into = c("drop.date","drop.time"), sep = " ")
uber_req2$Req.hrs <- as.factor(substring(uber_req2$req.time,0,2))
uber_req2$drop.hrs <- as.factor(substring(uber_req2$drop.time,0,2))

# demand and supply at airport and city
a_demand <- ggplot(subset(uber_req2, uber_req2$Pickup.point == "Airport"), aes(Req.hrs))+
            geom_bar(fill = "blue") + labs(title ="Demand at Airport(Requests)")+
            theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)
a_supply <- ggplot(subset(subset(uber_req2, uber_req2$Pickup.point == "Airport"),!is.na(drop.hrs)), aes(drop.hrs))+
            geom_bar(fill = "red") + labs(title ="Supply at Airport(hours)")+
            theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)

c_demand <- ggplot(subset(uber_req2, uber_req2$Pickup.point == "City"), aes(Req.hrs))+
            geom_bar(fill = "blue") + labs(title ="Demand at City(Requests)")+
            theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)
c_supply <- ggplot(subset(subset(uber_req2, uber_req2$Pickup.point == "City"),!is.na(drop.hrs)), aes(drop.hrs))+
            geom_bar(fill = "red") + labs(title ="Supply at City(Drops)")+
            theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)
                  
# Seeing overall demand and supply trend
grid.arrange(a_demand, a_supply, c_demand, c_supply, nrow = 2, ncol = 2)


# Demand & Supply together at Airport
g_DS_air <- ggplot(subset(uber_req2, uber_req2$Pickup.point == "Airport")) +
            geom_bar(aes(Req.hrs), fill = c("blue"))+ geom_bar(aes(drop.hrs),fill = c("red"),position = "dodge")+
            ylim(0,500) + labs(title = "From Airport (Blue = Request for Cab, Red = Availability (after Drops))") + 
             theme(plot.title = element_text(hjust = 0.5)) + xlab("Hrs of the day")

# Demand & Supply together at City
g_DS_city <- ggplot(subset(uber_req2, uber_req2$Pickup.point == "City")) +
              geom_bar(aes(Req.hrs), fill = c("blue"))+ geom_bar(aes(drop.hrs),fill = c("red"),position = "dodge")+
              ylim(0,500) + labs(title = "From City (Blue = Request for Cab, Red = Availability (after Drops))") + 
               theme(plot.title = element_text(hjust = 0.5)) + xlab("Hrs of the day")

# Demand and Supply together for Comparision
grid.arrange(g_DS_air, g_DS_city, ncol = 2)

# Plotting the Data to see the trends in request time vs Status based on pickup point

# when pick up is at Airport
g_req_air <- ggplot(subset(uber_req2, uber_req2$Pickup.point == "Airport"), aes(Req.hrs, fill = Status))
g_req_air <- g_req_air + geom_bar() + ylim(0,500) + labs(title = "From Airport") + theme(plot.title = element_text(hjust = 0.5))


# when pick up at the city
g_req_city <- ggplot(subset(uber_req2, uber_req2$Pickup.point == "City"), aes(Req.hrs, fill = Status))
g_req_city <- g_req_city + geom_bar() + ylim(0,500) + labs(title = "From City") + theme(plot.title = element_text(hjust = 0.5))

# combining Plots side by side 
grid_arrange_with_shared_legend <- function(..., nrow = 1, ncol = length(list(...)), position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, nrow = nrow, ncol = ncol)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  
}

# combined plot
grid_arrange_with_shared_legend(g_req_air, g_req_city)

# Cancelled and No car available Scenarios at Airport
g_cancel_air <- ggplot(subset(uber_req2, uber_req2$Pickup.point == "Airport" & uber_req2$Status == "Cancelled"), aes(Req.hrs))+
  geom_bar(fill = "blue") + labs(title = "Cancel Status at Airport")+ theme(plot.title = element_text(hjust = 0.5))

g_nocar_air <- ggplot(subset(uber_req2, uber_req2$Pickup.point == "Airport" & uber_req2$Status == "No Cars Available"), aes(Req.hrs))+
  geom_bar(fill = "red") + labs(title = "No Cars Available at Airport")+ theme(plot.title = element_text(hjust = 0.5))


grid.arrange(g_cancel_air, g_nocar_air, ncol = 2)

# Cancelled and No car available Scenarios at city
g_cancel_city <- ggplot(subset(uber_req2, uber_req2$Pickup.point == "City" & uber_req2$Status == "Cancelled"), aes(Req.hrs))+
  geom_bar(fill = "blue") + labs(title = "Cancel Status at City")+ theme(plot.title = element_text(hjust = 0.5))
g_nocar_city <- ggplot(subset(uber_req2, uber_req2$Pickup.point == "City" & uber_req2$Status == "No Cars Available"), aes(Req.hrs))+
  geom_bar(fill = "red") + labs(title = "No Cars Available at City")+ theme(plot.title = element_text(hjust = 0.5))

grid.arrange(g_cancel_city, g_nocar_city, ncol = 2)

# Exporting clean data frame for operations in tableau
write.csv(uber_req2, "Uber Request Clean Data.csv", na = "")
