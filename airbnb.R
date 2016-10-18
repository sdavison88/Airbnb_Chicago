library(ggmap)
library(ggthemes)
library(reshape2)
listings <- read.csv("~/Desktop/Career/Airbnb/listings.csv", comment.char="#")

chicago <- get_map(location = "Chicago", maptype = "satellite", zoom = 12, scale = c(1280,600))

ggmap(chicago, extent = "device") +
  geom_point(aes(x = longitude, y = latitude), color="#fd5c63", data = listings[which(listings$Year==2008),]) +
  scale_fill_gradient(low = "green", high = "red", 
                      guide = FALSE) +
  ggtitle("2008") +
  theme_fivethirtyeight() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        plot.title = element_text(size = 30, face = "bold"))

ggmap(chicago, extent = "device") +
  geom_point(aes(x = longitude, y = latitude), color="#fd5c63", data = listings[which(listings$Year<=2009),]) +
  scale_fill_gradient(low = "green", high = "red", 
                      guide = FALSE) +
  ggtitle("2009") +
  theme_fivethirtyeight() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        plot.title = element_text(size = 30, face = "bold"))

ggmap(chicago, extent = "device") +
  geom_point(aes(x = longitude, y = latitude), color="#fd5c63", data = listings[which(listings$Year<=2010),]) +
  scale_fill_gradient(low = "green", high = "red", 
                      guide = FALSE) +
  ggtitle("2010") +
  theme_fivethirtyeight() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        plot.title = element_text(size = 30, face = "bold"))

ggmap(chicago, extent = "device") +
  geom_point(aes(x = longitude, y = latitude), color="#fd5c63", data = listings[which(listings$Year<=2011),]) +
  scale_fill_gradient(low = "green", high = "red", 
                      guide = FALSE) +
  ggtitle("2011") +
  theme_fivethirtyeight() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        plot.title = element_text(size = 30, face = "bold"))

ggmap(chicago, extent = "device") +
  geom_point(aes(x = longitude, y = latitude), color="#fd5c63", data = listings[which(listings$Year<=2012),]) +
  scale_fill_gradient(low = "green", high = "red", 
                      guide = FALSE) +
  ggtitle("2012") +
  theme_fivethirtyeight() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        plot.title = element_text(size = 15, face = "bold"))

ggmap(chicago, extent = "device") +
  geom_point(aes(x = longitude, y = latitude),color="#fd5c63", data = listings[which(listings$Year<=2013),]) +
  scale_fill_gradient(low = "green", high = "red", 
                      guide = FALSE) +
  ggtitle("2013") +
  theme_fivethirtyeight() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        plot.title = element_text(size = 30, face = "bold"))

ggmap(chicago, extent = "device") +
  geom_point(aes(x = longitude, y = latitude), color="#fd5c63", data = listings[which(listings$Year<=2014),]) +
  scale_fill_gradient(low = "green", high = "red", 
                      guide = FALSE) +
  ggtitle("2014") +
  theme_fivethirtyeight() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        plot.title = element_text(size = 30, face = "bold"))

ggmap(chicago, extent = "device") +
  geom_point(aes(x = longitude, y = latitude), color="#fd5c63", data = listings[which(listings$Year<=2015),]) +
  scale_fill_gradient(low = "green", high = "red", 
                      guide = FALSE) +
  ggtitle("2015") +
  theme_fivethirtyeight() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        plot.title = element_text(size = 30, face = "bold"))

ggmap(chicago, extent = "device") + 
  geom_density2d(data = listings[which(listings$Year<=2015),], 
                 aes(x = longitude, y = latitude))+
  geom_point(aes(x = longitude, y = latitude), alpha = 0.1, color="#fd5c63", data = listings[which(listings$Year<=2015),]) +
  scale_fill_gradient(low = "green", high = "#fd5c63", 
                      guide = FALSE) +
  scale_alpha(range = c(0, 0.3), guide = FALSE) +
  stat_density2d(data = listings[which(listings$Year<=2015),], 
                 aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon")+
  ggtitle("2015 Density of Airbnb Rentals") +
  theme_fivethirtyeight() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        plot.title = element_text(size = 24, face = "bold"))

listings_neighbors <- aggregate(id ~ neighbourhood_cleansed, data=listings, FUN=length)
listings_neighbors$freq <- listings_neighbors$id / sum(listings_neighbors$)

ggplot(listings_neighbors[which(listings_neighbors$freq > .03),],aes(reorder(neighbourhood_cleansed,-id),id))+
  geom_bar(stat = "identity") +
  theme_fivethirtyeight()

listings_zip <- aggregate(id ~ zipcode, data=listings, FUN=length)
listings_zip <- merge(listings_zip,df_zip_demographics,by.x="zipcode", by.y="region")

colnames(listings_zip)[2:10] <- c("# of Listings","Population","% White", "% Black","% Asian", "% Hispanic", "Per Cap. Income","Median Rent", "Median Age")

qplot(x=Var1, y=Var2, data=melt(cor(listings_zip[2:10])), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1)) +
  theme_fivethirtyeight() +
  theme(legend.title=element_blank()) +
  theme(axis.text=element_text(size=12))

facet_zip <- melt(listings_zip,id.vars = "zipcode")

ggplot(listings_zip,aes(id,per_capita_income),addRe) +
  geom_point()+
  facet_wrap(~variable, scales = "free_y") +
  ggtitle("Demographics and Number of Airbnb Listings") +
  theme_fivethirtyeight()

ggplot2.scatterplot(data=listings_zip, xName='percent_white',yName='percent_black',
                    addRegLine=TRUE, regLineColor="#353535",
                    addConfidenceInterval=TRUE, smoothingMethod="loess")+
  theme_fivethirtyeight()

install_github("easyGgplot2", "kassambara")
library(devtools)
install_github("easyGgplot2", "kassambara")

Airports_LatLong <- read.csv("~/Desktop/Career/Airports/Airports_LatLong.csv")

plotScatterFunc <- function(x, na.rm = TRUE, ...) {
  nm <- names(x)
  for (i in seq_along(nm)) {
    plots <-ggplot(x,aes_string(x = nm[i])) + geom_histogram(alpha = .5,fill = "dodgerblue")
    ggsave(plots,filename=paste("myplot",nm[i],".png",sep=""))
  }
}

for (value in Airports_LatLong$Timezone) {
  plots <- ggplot(Airports_LatLong[which(Airports_LatLong$Timezone<=value),], aes(x = Long, y = Lat)) + geom_point(color="#353535",alpha=.5, stroke=0, size=1.5) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(legend.title=element_blank(),legend.position="none")
  ggsave(plots,filename=paste("myplot",Airports_LatLong$Timezone[value],".png",sep=""))}

for (i in seq_along(nm)) {
  print(ggplot(x,aes_string(x = nm[i])) + geom_histogram(alpha = .5,fill = "mediumseagreen")) }
}

install.packages("animation")
devtools::install_github("dgrtwo/gganimate")
library(gganimate)
Airports_LatLong$row <- as.numeric(factor(Airports_LatLong$country))
Airports_LatLong$row[which(Airports_LatLong$city=="Chicago")] <- "241"

p <- ggplot(Airports_LatLong, aes(x = Long, y = Lat, frame=row)) + geom_point(aes(frame=row, cumulative=TRUE),color="#353535",alpha=.5, stroke=0, size=1.5) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(legend.title=element_blank(),legend.position="none")

gg_animate(p, interval=.001, ani.width=1280, ani.height=900, "output3.gif")

Airports_LatLong$color <- "No"
Airports_LatLong$color[which(Airports_LatLong$city=="Chicago")] <- "CH"


ggplot(Airports_LatLong, aes(x = Long, y = Lat)) + 
  geom_point(aes(color=color),alpha=.5, stroke=0, size=1.5) +
  theme_bw() +
  scale_color_manual(values=c("#05C8F5","#353535"))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(legend.title=element_blank(),legend.position="none")




row(ai)