# Install the required pacakges
install.packages("lubridate")
install.packages("dplyr")

# Loading the required pacages
library(lubridate)
library(dplyr)

library()

# Practise date questions
setwd("C:/Users/AH0667765/Desktop")

Escalation_data <- read.csv("Escalation_test.csv")

View(Escalation_data)

X <- data.frame(A = c(1,2,3), B = c("P","V","R"))
X

Y <- data.frame(D = c(1,2, 3), B = c("A","N", "S"))
Y

XY <- cbind(X,Y)
XY

YZ <- rbind(X,Y)
YZ

# Merge
NameAdd <- merge(Y, X, by = c("A", "D"))
NameAdd <- merge(X, Y, by.x = "A" , by.y = "D")
NameAdd

# Merge

library(dplyr)
NameAddDplyr <- dplyr::left_join(X, Y, by = c("A" = "D"))

d1 <- data_frame(
  x = letters[1:3],
  y = LETTERS[1:3],
  a = rnorm(3)
)

d2 <- data_frame(
  x2 = letters[3:1],
  y2 = LETTERS[3:1],
  b = rnorm(3)
)

left_join(d1, d2, by = c("x" = "x2", "y" = "y2"))

# Date manipuation

dput(colnames(Escalation_data))




# Select Colunns
Escalation_sub <- select(Escalation_data, "Date.Time.Opened", "Request.Type", "Internal.Only", "Request.Record.Type", 
                         "Request.Status", "Request.Status.Detail", "Request.Number", 
                         "Original.Due.Date", "Last.Modified.Date", "Date.Time.Request.Reassigned", 
                         "Request.Due.Date", "Time.in..In.Progress.", "Date.Time.Closed", 
                         "Request.Resolution.Type")


View(Escalation_sub)

betterDates <- as.Date(dates,
                       origin = "1899-12-30")




betterDates <- function(dates) {
  as.Date(dates,
          origin = "1899-12-30")
}


betterDates(dates)


Escalation_sub[,"Original.Due.Date"] <- betterDates(Escalation_sub[,"Original.Due.Date"]) 
Escalation_sub[,"Original.Due.Date"] <- betterDates(Escalation_sub[,"Original.Due.Date"]) 
Escalation_sub[,"Original.Due.Date"] <- betterDates(Escalation_sub[,"Original.Due.Date"]) 
Escalation_sub[,"Original.Due.Date"] <- betterDates(Escalation_sub[,"Original.Due.Date"]) 


View(Escalation_sub)


dates <- c("84/27/05", "07/07/05")
betterDates <- as.Date(dates,
                       format = "%y/%d/%m")
betterDates


dates <- c("May 27 1984", "July 7 2005")
betterDates <- as.Date(dates,
                       format = "%B %d %Y")
betterDates

format(betterDates,
       "%a %b %d")



dates <- c("05/27/84", "07/07/05", "08/17/20")
betterDates <- as.Date(dates, "%m/%d/%y")
betterDates

correctCentury <- as.Date(ifelse(betterDates > Sys.Date(),
                                 format(betterDates, "19%y-%m-%d"),
                                 format(betterDates)))

correctCentury <- as.Date(ifelse(betterDates > Sys.Date(), 
                                 format(betterDates, "19%y-%m-%d"),
                                 format(betterDates)))




########################### New Data ###########################################


mydata <- mtcars
myirisdata <- iris


mynewdata <- tbl_df(mydata)

filter(mynewdata, cyl > 4, gear > 4)
filter(mynewdata, cyl > 4 & gear > 4)
filter(myirisdata, Species %in% c('setosa', 'virginica'))


select(mynewdata, cyl,mpg,hp)
select(mynewdata, -cyl, -mpg )

select(mynewdata, cyl:gear)


# Subset
mynewdata %>%
  select(cyl, wt, gear)%>%
  filter(wt > 2)

mynewdata %>%
  select(cyl, wt, gear)%>%
  arrange(wt)

mynewdata%>%
  select(cyl, wt, gear)%>%
  arrange(desc(wt))



mynewdata %>%
  select(mpg, cyl)%>%
  mutate(newvariable = mpg*cyl)


mynewdata %>%
  select(mpg, cyl) %>%
  mutate(newvariable = mpg*cyl)



myirisdata%>%
  group_by(Species)%>%
  summarise(Average = mean(Sepal.Length, na.rm = TRUE))


myirisdata%>%
  group_by(Species) %>%
  summarise(Average = mean(Sepal.Length, na.rm = TRUE))


myirisdata%>%
  group_by(Species)%>%
  summarise_each(funs(mean, n()), Sepal.Length, Sepal.Width)


mynewdata %>% rename(miles = mpg)


## Data table package


data("airquality")
mydata <- airquality
head(airquality,6)


library(data.table)


mydata1 <- data.table(mydata)


## GGPLOT 2

library(ggplot2)
bp <- ggplot(diamonds, aes(clarity, fill = cut)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5))
bp

sp <- ggplot(mpg, aes(x = cty, y = hwy, color = factor(cyl)))+geom_point(size = 2.5)
sp

#compare two plots
plot_grid(sp, bp, labels = c("A","B"), ncol = 2, nrow = 1)


ggplot(diamonds, aes(x = carat)) + 
  geom_histogram(binwidth = 0.25, fill = 'steelblue')+
  scale_x_continuous(breaks=seq(0,3, by=0.5))

# Reshape data

ID <- c(1,2,3,4,5)
Names <- c('Joseph','Matrin','Joseph','James','Matrin')
DateofBirth <- c(1993,1992,1993,1994,1992)
Subject<- c('Maths','Biology','Science','Psycology','Physics')
thisdata <- data.frame(ID, Names, DateofBirth, Subject)
data.table(thisdata)


install.packages('reshape2')
library(reshape2)

# MELT
mt <- melt(thisdata, id=(c('ID','Names')))

mt

#cast
mcast <- dcast(mt, DateofBirth + Subject ~ variable)
mcast

# Lubridate

now()

n_time <- now()
n_update <- update(n_time, year = 2013, month = 10)
n_update

#add days, months, year, seconds
d_time <- now()
d_time + ddays(1)
d_time + dweeks(2)
d_time + dyears(3)
d_time + dhours(2)
d_time + dminutes(50)
d_time + dseconds(60)

now()
n_time$hour <- hour(now())
n_time$minute <- minute(now())
n_time$second <- second(now())
n_time$month <- month(now())
n_time$year <- year(now())




# TidyR


library(tidyr)

names <- c('A','B','C','D','E','A','B')
weight <- c(55,49,76,71,65,44,34)
age <- c(21,20,25,29,33,32,38)
Class <- c('Maths','Science','Social','Physics','Biology','Economics','Accounts')

tdata <- data.frame(names, age, weight, Class)
tdata

long_t <- tdata %>% gather(Key, Value, weight:Class)


#create a data set
Humidity <- c(37.79, 42.34, 52.16, 44.57, 43.83, 44.59)
Rain <- c(0.971360441, 1.10969716, 1.064475853, 0.953183435, 0.98878849, 0.939676146)
Time <- c("27/01/2015 15:44","23/02/2015 23:24", "31/03/2015 19:15", "20/01/2015 20:52", "23/02/2
          015 07:46", "31/01/2015 01:55")


d_set <- data.frame(Humidity, Rain, Time)

separate_d <- d_set %>% separate(Time, c('Date', 'Month','Year'))
unite_d <- separate_d%>% unite(Time, c(Date, Month, Year), sep = "/")

unite_d

wide_t <- long_t %>% spread(Key, Value)





















