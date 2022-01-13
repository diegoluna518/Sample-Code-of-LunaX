
library(ggplot2)
qplot(data = diamonds, x = price, bins = 30)+
  facet_wrap(~diamonds$cut)

qplot(data = diamonds, x = price, bins = 30,
      xlim = c(5000,10000))+
  facet_wrap(~diamonds$cut)
by(diamonds$price, diamonds$cut, summary)

qplot(x = price, data = diamonds) + 
  facet_wrap(~cut, scales = "free")

qplot(x = price, data = diamonds, binwidth = .1) +
  scale_x_log10()+ 
  facet_wrap(~cut, scales = "free")

ggplot(diamonds, aes(x = price, y = color)) +
  geom_boxplot()
by(diamonds$price, diamonds$color, summary)

by(diamonds$price, diamonds$color, IQR)

# Investigate the price per carat of diamonds across
# the different colors of diamonds using boxplots.

ggplot(aes(x = color, y = price/carat), data = diamonds,
       binwidth = 25)+
  scale_y_continuous(breaks = seq(2000,6000,100))+
  coord_cartesian(ylim = c(2000,6000))+
  geom_boxplot(fill = c("gainsboro","gold","goldenrod","khaki", "lightseagreen","lightslateblue","lightslategrey"))+
  labs(title = "Price by carat by color")+
  theme(plot.title = element_text(size = rel(2)))+
  theme(plot.title = element_text(face = "bold"))
library(gridExtra)
library(grid)

p1 <- qplot(x = cut, y= price, data = diamonds, geom = "boxplot", fill = cut) +
  coord_cartesian(ylim = c(0,7000))
p2 <- qplot(x = color, y= price, data = diamonds, geom = "boxplot"
            , fill = color) +
  coord_cartesian(ylim = c(0,8000))
p3 <- qplot(x = clarity, y= price, data = diamonds, geom = "boxplot", fill = clarity) +
  coord_cartesian(ylim = c(0,6500))
grid <- grid.arrange(p1, p2, p3, ncol = 1)

#Investigate frequency polygon
ggplot( data = diamonds, aes(x = carat), binwidth = 10) +
  scale_y_continuous(breaks = 2000,15000,100) +
  geom_freqpoly()
qplot(x = carat,
      data = diamonds,
      binwidth = 0.1,
      xlab = 'Carats',
      ylab = 'Count',
      geom = 'freqpoly') +
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, 0.1)) +
  scale_y_continuous(breaks = seq(0,12000,1000))

qplot(x = carat, data = diamonds, binwidth = 0.1,
      geom = "freqpoly")+
  scale_x_continuous(limits=c(0,5), breaks = seq(0,5,0.1))+
  scale_y_continuous(breaks = seq(0,12000,1000))
table(diamonds$carat)

#Comapny X Data
population <- read.csv("population_total.csv",
                  header = TRUE, sep = "\t",
                  check.names = FALSE)
str(population)
colnames(total)[1] <- 'country'

#Prepare population data
population <- population %>% 
  gather(year, total_population, `2008`:`2020`)
population$year = as.integer(population$year)
population = population[!is.na(population$total_population)]
str()

class(total_gathered)

list(total_gathered)
names(grid)
names(total_gathered)

#Attempt to fit population data into working plot
ggplot(aes(x = country, y = total), data = subset(total_gathered, country == "United States")) + 
  geom_col()


library(tidyr)
library(dplyr)
hdi <- read.csv("hdi.csv")
hdi
hdi_gathered <- gather(hdi, "year", "hdi", 2:27)
names(hdi_gathered)

for(row in 1:nrow(hdi_gathered)) {
  year <- hdi_gathered[row, "year"]
  year <- substr(year, start = 2, stop = 5)
  hdi_gathered[row, "year"] <- year
}
names(hdi_gathered)
ggplot(aes(x = year, y = hdi, group = 1),
       data = subset(hdi_gathered, country = "China")) +
  geom_point() +
  ggtitle("China HDI - 1990 to 2015") +
  theme(axis.text.x = element_text(angle = 90))

GDP<- read.csv("gdp_per_capita_yearly_growth.csv")
NETPER<- read.csv("internet_users.csv")
Perscomp<- read.csv("personal_computers_per_100_people.csv")
GDP
NETPER
Perscomp

#Changing column names for ease of use
names(GDP)[[1]] <- "Country"
names(NETPER)[[1]] <- "Country"
names(Perscomp)[[1]] <- "Country"

names(GDP)
#Subsetting data to specific years
GDP <- subset(GDP, select = c(Country, X2008:X2019))
NETPER <- subset(NETPER, select = c(Country, X2008:X2019))
Perscomp <- subset(Perscomp, select = c(Country, X2008:X2019))

#Cleaning up data and removing X's
GDP.2 <- gather(GDP, "year", "GDP", 2:17)
GDP.2$year <- gsub("X", "", GDP.2$year)
NETPER.2 <- gather(NETPER, "year", "Internet_Per_100", 2:17)
NETPER.2$year <- gsub("X", "", NETPER.2$year)
Perscomp.2 <- gather(Perscomp, "year", "Computers_Per_100", 2:17)
Perscomp.2$year <- gsub("X", "", Perscomp.2$year)

#Joining data and removing NA values
GDPwNet <- right_join(GDP.2, NETPER.2, by = c("Country", "year"))
GDPwComp <- right_join(GDP.2, Perscomp.2, by = c("Country", "year"))
GDPwNet <- filter(GDPwNet, !is.na(GDPwNet$Internet_Per_100))
GDPwComp <-filter(GDPwComp, !is.na(GDPwComp$Computers_Per_100))
GDPwComp <-filter(GDPwComp, !is.na(GDPwComp$GDP))