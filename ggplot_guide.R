#Mapping Tornadoes by latitude and longitude:

r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)
  
#Packages/libraries: ggplot2, maps, mapdata
#my_tornado_data <- 1950_2022_all_tornadoes.csv
install.packages("tidyverse")
install.packages("maps")
install.packages("mapdata")
library(tidyverse)
library(ggplot2)
library(maps)
library(mapdata)
my_tornado_data <- read_csv("https://www.spc.noaa.gov/wcm/data/1950-2022_all_tornadoes.csv")


#get map of usa data

usa <- map_data("state")

#plot a basic map of the usa

ggplot(usa, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "gray") 

#localize data to Midwest states in 2011

midwest <- c("OK","NE","KS")

sites_midwest_2011 <-
  my_tornado_data |>
  filter(st %in% midwest & yr == "2011")

#create a data.frame of the Midwest latitude and longitude

sitesdf <- data.frame(long = sites_midwest_2011$slon,
                      lat = sites_midwest_2011$slat)

#map lat and long points onto usa map
#separate the data into the two geoms (polygon and point)

ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group),
               fill = "white", color = "grey") +
  geom_point(data = sitesdf, aes(x = long, y = lat))

#map as line segments showing tornado length
#create new data.frame to include both start (slon, slat) and end (elon, elat) points

sitesdf2 <- data.frame(slon = sites_midwest_2011$slon,
                       slat = sites_midwest_2011$slat,
                       elon = sites_midwest_2011$elon,
                       elat = sites_midwest_2011$elat)


#add data frame into geom_segment() code

ggplot() +
  geom_polygon(data = usa, 
               aes(x = long, y = lat, group = group),
               fill = "white", color = "gray") +
  geom_segment(data = sitesdf2, mapping = aes(x = slon,
                                              y = slat,
                                              xend = elon,
                                              yend = elat))  

#zoom in using coord_cartesian()

ggplot() +
  geom_polygon(data = usa, 
               aes(x = long, y = lat, group = group),
               fill = "white", color = "grey") +
  geom_segment(data = sitesdf2, mapping = aes(x = slon,
                                              y = slat,
                                              xend = elon,
                                              yend = elat)) +
  coord_cartesian(xlim = c(-103, -93), ylim = c(33, 43))

# add magnitude to data.frames to plot by size
# first with the point chart
# I added Missouri (“MO”) to data set in my code for these examples 

midwest2 <- c("OK","NE","KS", "MO")

sites_midwest2_2011 <-
  my_tornado_data |>
  filter(st %in% midwest2 & yr == "2011")

sitesdf3 <- data.frame(long = sites_midwest2_2011$slon,
                       lat = sites_midwest2_2011$slat,
                       mag = sites_midwest2_2011$mag)

ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group),
               fill = "white", color = "gray") +
  geom_point(data = sitesdf3, aes(x = long,
                                  y = lat,
                                  size = mag))

# try with line segments

sitesdf4 <- data.frame(slon = sites_midwest2_2011$slon,
                       slat = sites_midwest2_2011$slat,
                       elon = sites_midwest2_2011$elon,
                       elat = sites_midwest2_2011$elat,
                       mag = sites_midwest2_2011$mag)

ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group),
               fill = "white", color = "gray") +
  geom_segment(data = sitesdf4, mapping = aes(x = slon,
                                              y = slat,
                                              xend = elon,
                                              yend = elat,
                                              size = mag)) +
  coord_cartesian(xlim = c(-103, -90), ylim = c(34, 43))

# other cool stuff!
# visualize direction with arrow = arrow() in geom_segment() code
# removed size = mag for more clear visualization

ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group),
               fill = "white", color = "gray") +
  geom_segment(data = sitesdf4, mapping = aes(x = slon,
                                              y = slat,
                                              xend = elon,
                                              yend = elat),
               arrow = arrow()) +
  coord_cartesian(xlim = c(-103, -90), ylim = c(34, 43))

# you can change the transparency of geoms with alpha
# alpha defaults to a 0-1 scale so to map transparency by magnitude divide $mag by 10
# I filtered my data set to show only mag >= 1 tornadoes for these examples

sites_midwest2_2011 <- filter(sites_midwest2_2011, mag >= 1)

sitesdf5 <- data.frame(slon = sites_midwest2_2011$slon,
                       slat = sites_midwest2_2011$slat,
                       elon = sites_midwest2_2011$elon,
                       elat = sites_midwest2_2011$elat,
                       mag = sites_midwest2_2011$mag,
                       mag_alpha = (sites_midwest2_2011$mag/10))

ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group),
               fill = "white", color = "gray") +
  geom_segment(data = sitesdf5, mapping = aes(x = slon,
                                              y = slat,
                                              xend = elon,
                                              yend = elat,
                                              size = mag,
                                              alpha = mag_alpha)) +
  coord_cartesian(xlim = c(-103, -90), ylim = c(34, 43))

# add titles with labs()

ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group),
               fill = "white", color = "gray") +
  geom_segment(data = sitesdf5, mapping = aes(x = slon,
                                              y = slat,
                                              xend = elon,
                                              yend = elat,
                                              size = mag,
                                              alpha = mag_alpha)) +
  labs(title = "Midwest Tornadoes By F Scale",
       subtitle = "2011") +
  xlab("NEW LONG") + ylab("NEW LAT") +
  coord_cartesian(xlim = c(-103, -90), ylim = c(34, 43))

# let’s add some scaled colors with scale_color_continuous()
# lots of ways to add color to ggplots but this is the easiest way to do via scaling

ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group),
               fill = "white", color = "gray") +
  geom_segment(data = sitesdf5, mapping = aes(x = slon,
                                              y = slat,
                                              xend = elon,
                                              yend = elat,
                                              size = mag,
                                              alpha = mag_alpha,
                                              color = mag)) +
  scale_color_continuous(name = "F Scale",
                         low = "pink",
                         high = "maroon",
                         limits = c(1,5)) +
  labs(title = "Midwest Tornadoes By F Scale",
       subtitle = "2011") +
  xlab("NEW LONG") + ylab("NEW LAT") +
  coord_cartesian(xlim = c(-103, -90), ylim = c(34, 43))

# plots only with low = “lightblue”, high = “darkblue” in scale_color_continuous mapping

ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group),
               fill = "white", color = "gray") +
  geom_point(data = sitesdf5, mapping = aes(x = slon,
                                            y = slat,
                                            size = mag,
                                            alpha = mag_alpha,
                                            color = mag)) +
  scale_color_continuous(name = "F Scale",
                         low = "lightblue",
                         high = "darkblue",
                         limits = c(1,5)) +
  labs(title = "Midwest Tornadoes By F Scale",
       subtitle = "2011") +
  xlab("NEW LONG") + ylab("NEW LAT") +
  coord_cartesian(xlim = c(-103, -90), ylim = c(34, 43))


# let’s add every midwest tornado to the above plot and animate it as a .gif by year
# create new data set for all midwest tornadoes
# there are some 0 coord tornadoes that need to be filtered out. 
# add the year to a new data.frame

install.packages("gganimate")
library(gganimate)

midwest_sites <-
  my_tornado_data |>
  filter(st %in% midwest2 & mag >= 1 & slon != 0)

sitesdf6 <- data.frame(slon = midwest_sites$slon,
                       slat = midwest_sites$slat,
                       elon = midwest_sites$elon,
                       elat = midwest_sites$elat,
                       mag = midwest_sites$mag,
                       mag_alpha = (midwest_sites$mag/10),
                       year = midwest_sites$yr,
                       state = midwest_sites$st)

# save your static plot as its own variable

p <- ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group),
               fill = "white", color = "gray") +
  geom_point(data = sitesdf6, aes(x = slon, 
                                  y = slat, 
                                  size = mag, 
                                  color = mag)) +
  scale_color_continuous(name = "F Scale",
                         low = "lightblue",
                         high = "darkblue",
                         limits = c(1,5))
p

# add plot into transition_time() from gganimate library
# add title with labs to show progress

p + transition_time(year) +
  labs(title = "Year: {frame_time}")

# use facet_wrap to visualize individual states
# add the vector state = midwest_sites$st to sitesdf4 and update p to include new sitesdf4 data.frame 

p + facet_wrap(~state) +
  transition_time(year) +
  labs(title = "Year: {frame_time}") +
  coord_cartesian(xlim = c(-103, -90), ylim = c(34, 43))

# let’s find each states mean magnitude per year and chart it as a line graph then animate it to show #progress by year
#first get your summary table with correct groupings

midwest_by_st <-
  sitesdf6 |>
  group_by(state, year) |>
  summarize(mag = mean(mag))

midwest_by_st

#save static plot

m <- ggplot(midwest_by_st, aes(x = year, 
                               y = mag,
                               group = state, 
                               color = factor(state))) +
  geom_line() +
  theme_minimal()

m

# use transition_reveal() to animate

m + transition_reveal(year)

# now that we have a good grouping of data, let’s try some other groupings and hunt for some signals
# first lets look at the number of tornadoes by Midwest state and see what has happened since 1950

midwest_n <-
  sitesdf6 |>
  group_by(state, year) |>
  summarize(n = n())

midwest_n

n <- ggplot(midwest_n, aes(x = year,
                      y = n,
                      group = state,
                      color = factor(state))) +
  geom_line() +
  theme_minimal()

n

# this is really colorful but doesn’t clearly tell us much
# add the function stat_smooth() and use method = “lm” to show a time series regression trendline

ggplot(midwest_n, aes(x = year,
                      y = n,
                      group = state,
                      color = factor(state))) +
  geom_line() +
  stat_smooth(method = "lm") +
  theme_minimal()

# now we have a more clear picture!
# clear downward trends in the number of torndadoes in KS, NE, OK; while a slight upward trend in MO
# another question: are tornado occurrences moving eastward over time?
# to examine further let’s mean the slong by state and by year and plot the above line graph

midwest_slong <-
  sitesdf6 |>
  group_by(state, year) |>
  summarize(slong = mean(slon))


l <- ggplot(midwest_slong, aes(x = year,
                          y = slong, 
                          group = state,
                          color = factor(state))) +
  geom_line() +
  stat_smooth(method = "lm") +
  theme_minimal() 

l

# positive slong trend in ¾ of states is compelling evidence of eastern movement trend in Midwest 
# add Texas to further strengthen the case!

midwest3 <- c(midwest2, "TX")

midwest_sites2 <-
  my_tornado_data |>
  filter(st %in% midwest3 & mag >= 1 & slon != 0)

sitesdf7 <- data.frame(slon = midwest_sites2$slon,
                       slat = midwest_sites2$slat,
                       elon = midwest_sites2$elon,
                       elat = midwest_sites2$elat,
                       mag = midwest_sites2$mag,
                       mag_alpha = (midwest_sites2$mag/10),
                       year = midwest_sites2$yr,
                       state = midwest_sites2$st)

midwest_slong2 <-
  sitesdf7 |>
  group_by(state, year) |>
  summarize(slong = mean(slon))

o <- ggplot(midwest_slong2, aes(x = year,
                          y = slong, 
                          group = state,
                          color = factor(state))) +
  geom_line() +
  stat_smooth(method = "lm") +
  theme_minimal() 

o
