

## GGPLot 2


install.packages("gcookbook")
library(gcookbook)

install.packages("ggplot2")
library(ggplot2)

# Create a scatter plot

plot(mtcars$wt, mtcars$mpg)


# With ggplot package
qplot(wt, mpg, data = mtcars)

ggplot(mtcars, aes(x= wt, y= mpg)) + geom_point()

## Line Graph

plot(pressure$temperature, pressure$pressure, type="l")
points(pressure$temperature, pressure$pressure)

lines(pressure$temperature, pressure$pressure/2, col="red")
points(pressure$temperature, pressure$pressure/2, col="red")



qplot(pressure$temperature, pressure$pressure, geom="line")

# This is equivalent to:
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line()

# Lines and point together
qplot(temperature, pressure, data=pressure, geom=c("line", "point"))

# Equivalent to:
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line() + geom_point()


## Bar Graph

table(mtcars$cyl)

qplot(mtcars$cyl)


# Treat cyl as discrete
qplot(factor(mtcars$cyl))

# Bar graph of counts
qplot(factor(cyl), data=mtcars)

# This is equivalent to:
ggplot(mtcars, aes(x=factor(cyl))) + geom_bar()


## Hisogram

hist(mtcars$mpg)

# Specify approximate number of bins with breaks
hist(mtcars$mpg, breaks=10)

# 

qplot(mtcars$mpg)

qplot(mpg, data=mtcars, binwidth=4)

# This is equivalent to:
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth=4)



##Boxplot 2

plot(ToothGrowth$supp, ToothGrowth$len)

# Formula syntax
boxplot(len ~ supp, data = ToothGrowth)

# Put interaction of two variables on x-axis
boxplot(len ~ supp + dose, data = ToothGrowth)


qplot(ToothGrowth$supp, ToothGrowth$len, geom="boxplot")

qplot(supp, len, data=ToothGrowth, geom="boxplot")

# This is equivalent to:
ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()


# Using three separate vectors
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len,
      geom="boxplot")


qplot(interaction(supp, dose), len, data=ToothGrowth, geom="boxplot")

# This is equivalent to:
ggplot(ToothGrowth, aes(x=interaction(supp, dose), y=len)) + geom_boxplot()


## Functional Curve

curve(x^3 - 5*x, from=-4, to=4)

# This sets the x range from 0 to 20
myfun <- function(xvar) {
  1/(1 + exp(-xvar + 10))
}

qplot(c(0,20), fun=myfun, stat="function", geom="line")

# This is equivalent to:
ggplot(data.frame(x=c(0, 20)), aes(x=x)) + stat_function(fun=myfun, geom="line")



################## Bar Graph ######################################


#There's an important distinction you should be aware of when making bar graphs:
#sometimes the bar heights represent counts of cases in the data set, and sometimes they
#represent values in the data set.

# When you want the heights of the bar to represent vaulues in the data then use stat = "Identity"

ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity")


# 
BOD
str(BOD)

ggplot(BOD, aes(x=Time, y=demand)) + geom_bar(stat="identity")


# Convert Time to a discrete (categorical) variable with factor()

ggplot(BOD, aes(x=factor(Time), y=demand)) + geom_bar(stat="identity")


ggplot(pg_mean, aes(x=group, y=weight)) +
  geom_bar(stat="identity", fill="lightblue", colour="black")


### Grouping Bar together

# Stacked Bar Graph
ggplot(cabbage_exp, aes(x= Date, y=Weight, fill=Cultivar)) + 
  geom_bar(stat = "identity") 


# 
ggplot(cabbage_exp, aes(x= Date, y=Weight, fill=Cultivar)) + 
  geom_bar(stat = "identity", position="dodge") 


ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat = "identity", position="dodge", colour="black") +
  scale_fill_brewer(palette="Pastel1")

# Making a Bar Graph of Counts

ggplot(diamonds, aes(x=cut)) + geom_bar()

ggplot(diamonds, aes(x=carat)) + geom_bar()

diamonds$carat


# Using Colors in a Bar Graph

upc <- subset(uspopchange, rank(Change)>40)
upc

ggplot(upc, aes(x=Abb, y=Change, fill=Region)) + geom_bar(stat="identity")

# The default colors aren't very appealing, so you may want to set them, using
# scale_fill_brewer() or scale_fill_manual().

ggplot(upc, aes(x=reorder(Abb, Change), y=Change, fill=Region)) +
  geom_bar(stat="identity", colour="black") +
  scale_fill_manual(values=c("#669933", "#FFCC66")) +
  xlab("State")
## For more about using the reorder() function to reorder the levels of a factor based on
##the values of another variable

# Coloring Negative and Positive Bars Differently

## Subsetting the data where source is Berkeley and year is greater than 1900

csub <- subset(climate, Source=="Berkeley" & Year >= 1900)

# Create a pos variables were Anomaly10y >=0

csub$pos <- csub$Anomaly10y >= 0

## Plot the ggplot

ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) +
  geom_bar(stat="identity", position="identity")

# Notice that we use position="identity" with the bars. 
# This will prevent a warning message about stacking not being well defined for negative numbers

ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) +
  geom_bar(stat="identity", position="identity", colour="black", size=0.25) +
  scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE)

# remove the legend with guide=FALSE


## Adjusting Bar Width and Spacing

# To make the bars narrower or wider, set width in geom_bar()


# The default value is 0.9; 
#larger values make the bars wider, 
# and smaller values make the bars narrower


ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity")

ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity", width=0.5)

ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity", width=1)



# For a grouped bar graph with narrow bars:
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
geom_bar(stat="identity", width=0.5, position="dodge")


# And with some space between the bars:
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
geom_bar(stat="identity", width=0.5, position=position_dodge(0.7))


# The first graph used position="dodge", and the second graph used position=posi
#tion_dodge(). This is because position="dodge" is simply shorthand for position=po
#sition_dodge() with the default value of 0.9, but when we want to set a specific value,
#we need to use the more verbose command.

# All of these will have the same result:

geom_bar(position="dodge")
geom_bar(width=0.9, position=position_dodge())
geom_bar(position=position_dodge(0.9))
geom_bar(width=0.9, position=position_dodge(width=0.9))


library(gcookbook) # For the data set

# Making a Stacked Bar Graph


ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity")


ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity") +
  guides(fill=guide_legend(reverse=TRUE))

## you can reverse the order of items
## in the legend by using guides() and specifying the aesthetic for which the legend should
### be reversed

# If you'd like to reverse the stacking order specify order=desc() in the aesthetic mapping:
  
library(plyr)
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar, order=desc(Cultivar))) +
  geom_bar(stat="identity")


# For a more polished graph, we'll keep the reversed legend order, use 
## scale_fill_brewer() to get a different color palette, and use colour="black" to get a black outline


ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", colour="black") +
  guides(fill=guide_legend(reverse=TRUE)) +
  scale_fill_brewer(palette="Pastel1")

## Making a Proportional Stacked Bar Graph

#You want to make a stacked bar graph that shows proportions (also called a 100% stacked
#                                                             bar graph).

library(gcookbook) # For the data set
library(plyr)
# Do a group-wise transform(), splitting on "Date"

ce <- ddply(cabbage_exp, "Date", transform,
            percent_weight = Weight / sum(Weight) * 100)

ggplot(ce, aes(x=Date, y=percent_weight, fill=Cultivar)) +
  geom_bar(stat="identity")

cabbage_exp

ddply(cabbage_exp, "Date", transform,
      percent_weight = Weight / sum(Weight) * 100)

# As with regular stacked bar graphs, it makes sense to reverse the legend order, change
# the color palette, and add an outline.


ggplot(ce, aes(x=Date, y=percent_weight, fill=Cultivar)) +
  geom_bar(stat="identity", colour="black") +
  guides(fill=guide_legend(reverse=TRUE)) +
  scale_fill_brewer(palette="Pastel1")

# Adding Labels to a Bar Graph


# Add geom_text() to your graph. It requires a mapping for x, y, and the text itself. By
# setting vjust (the vertical justification), it is possible to move the text above or below
# the tops of the bars,


library(gcookbook) # For the data set
# Below the top
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Weight), vjust=1.5, colour="white")

# Above the top
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Weight), vjust=-0.2)

# Adjust y limits to be a little higher
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Weight), vjust=-0.2) +
  ylim(0, max(cabbage_exp$Weight) * 1.05)

# Map y positions slightly above bar top - y range of plot will auto-adjust
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(y=Weight+0.1, label=Weight))


ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=Weight), vjust=1.5, colour="white",
            position=position_dodge(.9), size=3)


# Putting labels on stacked bar graphs requires finding the cumulative sum for each stack.
# To do this, first make sure the data is sorted properly-if it isn't, the cumulative sum
# might be calculated in the wrong order. We'll use the arrange() function from the plyr
# package, which automatically gets loaded with ggplot2:


library(plyr)
# Sort by the day and sex columns
ce <- arrange(cabbage_exp, Date, Cultivar)

# Get the cumulative sum
ce <- ddply(ce, "Date", transform, label_y=cumsum(Weight))
ce

ggplot(ce, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity") +
  geom_text(aes(y=label_y, label=Weight), vjust=6.5, colour="white")


ce <- arrange(cabbage_exp, Date, Cultivar)
# Calculate y position, placing it in the middle

ce <- ddply(ce, "Date", transform, label_y=cumsum(Weight)-0.5*Weight)
ggplot(ce, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity") +
  geom_text(aes(y=label_y, label=Weight), colour="white")


ggplot(ce, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", colour="black") +
  geom_text(aes(y=label_y, label=paste(format(Weight, nsmall=2), "kg")),size=4) +
  guides(fill=guide_legend(reverse=TRUE)) +
  scale_fill_brewer(palette="Pastel1")


# Making a Cleveland Dot Plot

#Cleveland dot plots are sometimes used instead of bar graphs because they reduce visual
# clutter and are easier to read.

library(gcookbook) # For the data set
tophit <- tophitters2001[1:25, ] # Take the top 25 from the tophitters data set

ggplot(tophit, aes(x=avg, y=name)) + geom_point()



ggplot(tophit, aes(x=avg, y=reorder(name, avg))) +
  geom_point(size=3) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))


ggplot(tophit, aes(x=reorder(name, avg), y=avg)) +
  geom_point(size=3) + # Use a larger dot
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour="grey60", linetype="dashed"))


# Get the names, sorted first by lg, then by avg
nameorder <- tophit$name[order(tophit$lg, tophit$avg)]

# Turn name into a factor, with levels in the order of nameorder
tophit$name <- factor(tophit$name, levels=nameorder)


ggplot(tophit, aes(x=avg, y=name)) +
  geom_segment(aes(yend=name), xend=0, colour="grey50") +
  geom_point(size=3, aes(colour=lg)) +
  scale_colour_brewer(palette="Set1", limits=c("NL","AL")) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(), # No horizontal grid lines
        legend.position=c(1, 0.55), # Put legend inside plot area
        legend.justification=c(1, 0.5))


# Another way to separate the two groups is to use facets,


ggplot(tophit, aes(x=avg, y=name)) +
  geom_segment(aes(yend=name), xend=0, colour="grey50") +
  geom_point(size=3, aes(colour=lg)) +
scale_colour_brewer(palette="Set1", limits=c("NL","AL"), guide=FALSE) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(lg ~ ., scales="free_y", space="free_y")




############################### Line Graph ##############################################

# Line graphs are typically used for visualizing how one continuous variable, on the yaxis,
# changes in relation to another continuous variable, on the x-axis.

# Use ggplot() with geom_line(), and specify what variables you mapped to x and y
ggplot(BOD, aes(x=Time, y=demand)) + geom_line()


# Let's convert the demand into factor

BOD1 <- BOD # Make a copy of the data
BOD1$Time <- factor(BOD1$Time)
ggplot(BOD1, aes(x=Time, y=demand, group=1)) + geom_line()


# With ggplot2, the default y range of a line graph is just enough to include the y values
# in the data. For some kinds of data, it's better to have the y range start from zero.

ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + ylim(0, max(BOD$demand))

# Another way
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + expand_limits(y=0)


# Adding Points to a Line Graph

ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + geom_point()



library(gcookbook) # For the data set
ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point()


# Same with a log y-axis
ggplot(worldpop, aes(x=Year, y=Population)) + 
  geom_line() + 
  geom_point() + 
  scale_y_log10()


# Making a Line Graph with Multiple Lines


# Load plyr so we can use ddply() to create the example data set
library(plyr)
# Summarize the ToothGrowth data
tg <- ddply(ToothGrowth, c("supp", "dose"), summarise, length=mean(len))

# Map supp to colour
ggplot(tg, aes(x=dose, y=length, colour=supp)) + geom_line()

# Map supp to linetype
ggplot(tg, aes(x=dose, y=length, linetype=supp)) + geom_line()


ggplot(tg, aes(x=factor(dose), y=length, colour=supp)) + geom_line()

## Error : geom_path: Each group consists of only one observation. Do you need to adjust the group aesthetic?

ggplot(tg, aes(x=dose, y=length)) + geom_line()



# If your plot has points along with the lines, you can also map variables to properties of
# the points, such as shape and fill

ggplot(tg, aes(x=dose, y=length, shape=supp)) + geom_line() +
  geom_point(size=4) # Make the points a little larger

ggplot(tg, aes(x=dose, y=length, fill=supp)) + geom_line() +
  geom_point(size=4, shape=21) # Also use a point with a color fill


ggplot(tg, aes(x=dose, y=length, shape=supp)) +
  geom_line(position=position_dodge(0.2)) + # Dodge lines by 0.2
  geom_point(position=position_dodge(0.2), size=4) # Dodge points by 0.2


# Changing the Appearance of Lines

# The type of line (solid, dashed, dotted, etc.) is set with linetype, the thickness (in mm)
# with size, and the color of the line with colour.


ggplot(BOD, aes(x=Time, y=demand)) +
  geom_line(linetype="dashed", size=1, colour="blue")

## The default colors aren't the most appealing, so you may want to use a different palette,
## by using scale_col our_brewer() or scale_colour_manual():
  
# Load plyr so we can use ddply() to create the example data set
library(plyr)

# Summarize the ToothGrowth data
tg <- ddply(ToothGrowth, c("supp", "dose"), summarise, length=mean(len))

ggplot(tg, aes(x=dose, y=length, colour=supp)) +
  geom_line() +
  scale_colour_brewer(palette="Set1")


# If both lines have the same properties, you need to specify a variable to
# use for grouping
ggplot(tg, aes(x=dose, y=length, group=supp)) +
  geom_line(colour="darkgreen", size=1.5)

# Since supp is mapped to colour, it will automatically be used for grouping
ggplot(tg, aes(x=dose, y=length, colour=supp)) +
  geom_line(linetype="dashed") +
  geom_point(shape=22, size=3, fill="white")

# Changing the Appearance of Points

ggplot(BOD, aes(x=Time, y=demand)) +
  geom_line() +
  geom_point(size=4, shape=22, colour="darkred", fill="blue")



ggplot(BOD, aes(x=Time, y=demand)) +
  geom_line() +
  geom_point(size=4, shape=21, fill="white")


# Load plyr so we can use ddply() to create the example data set
library(plyr)
# Summarize the ToothGrowth data
tg <- ddply(ToothGrowth, c("supp", "dose"), summarise, length=mean(len))

# Save the position_dodge specification because we'll use it multiple times

ggplot(tg, aes(x=dose, y=length, fill=supp)) +
  geom_line(position=position_dodge(0.2)) +
  geom_point(shape=21, size=3, position=position_dodge(0.2)) +
  scale_fill_manual(values=c("black","white"))


## Making a Graph with a Shaded Area

# Convert the sunspot.year data set into a data frame for this example
sunspotyear <- data.frame(
  Year = as.numeric(time(sunspot.year)),
  Sunspots = as.numeric(sunspot.year)
)
ggplot(sunspotyear, aes(x=Year, y=Sunspots)) + geom_area()

ggplot(sunspotyear, aes(x=Year, y=Sunspots)) +
  geom_area(colour="black", fill="blue", alpha=.2)

ggplot(sunspotyear, aes(x=Year, y=Sunspots)) +
  geom_area(fill="blue", alpha=.2) +
  geom_line()

# we'll also make it 80% transparent by setting alpha to 0.2

## Making a Stacked Area Graph

library(gcookbook) # For the data set

ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area()

# The default order of legend items is the opposite of the stacking order


ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Blues", breaks=rev(levels(uspopage$AgeGroup)))


# To reverse the stacking order, we'll put order=desc(AgeGroup)

ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup, order=desc(AgeGroup))) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Blues")


ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup, order=desc(AgeGroup))) +
  geom_area(colour=NA, alpha=.4) +
  scale_fill_brewer(palette="Blues") +
  geom_line(position="stack", size=.2)


# Making a Proportional Stacked Area Graph

uspopage_prop <- ddply(uspopage, "Year", transform,
                       Percent = Thousands / sum(Thousands) * 100)

ggplot(uspopage_prop, aes(x=Year, y=Percent, fill=AgeGroup)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Blues", breaks=rev(levels(uspopage$AgeGroup)))

# Let's look it closer

uspopage

uspopage_prop <- ddply(uspopage, "Year", transform,
                       Percent = Thousands / sum(Thousands) * 100)


# Adding a Confidence Region

# Grab a subset of the climate data
clim <- subset(climate, Source == "Berkeley",
               select=c("Year", "Anomaly10y", "Unc10y"))

# Use geom_ribbon() and map values to ymin and ymax.

# Shaded region
ggplot(clim, aes(x=Year, y=Anomaly10y)) +
  geom_ribbon(aes(ymin=Anomaly10y-Unc10y, ymax=Anomaly10y+Unc10y),
              alpha=0.2) + geom_line()


# Notice that the geom_ribbon() is before geom_line(), so that the line is drawn on top
# of the shaded region.


# With a dotted line for upper and lower bounds
ggplot(clim, aes(x=Year, y=Anomaly10y)) +
  geom_line(aes(y=Anomaly10y-Unc10y), colour="grey50", linetype="dotted") +
  geom_line(aes(y=Anomaly10y+Unc10y), colour="grey50", linetype="dotted") +
  geom_line()

#Shaded regions can represent things other than confidence regions, such as the difference
#between two values

############# Scatter Plot ################