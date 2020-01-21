library(tidyverse)

data = mpg
ggplot2::mpg

library(ggplot2)
geom_point() # Produces scatterplots
geom_bar() # Bar plots
geom_boxplot() # boxplots

install.packages("gapminder")
library(gapminder)
library(ggplot2)
gapminder
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point()

#Exercise 1
plot1 <- ggplot(data = mpg,
                mapping = aes(x=displ, y=hwy))
plot1 + geom_point()

#The data shows with larger engines comes worse rates 
#of gas mileage, which is intuitively correct
plot1 <- ggplot(data = mpg,
                mapping = aes(x=class, y=drv))
plot1 + geom_point()
#This graph will not work because the classifications of 
#drv are classes that each car type is assigned to, so plotting 
#them this way would not make sense

#Exercise 1b
plot1 <- ggplot(data = mpg,
                mapping = aes(x=displ, y=hwy, color = class))
plot1 + geom_point()
#We can conclude that suv's, minivans, and pickups have worse gas
#mileage with bigger engines, which intuitively makes sense when 
#compared with smaller, lighter cars

p + geom_point()
p + geom_smooth()

?geom_smooth
p + geom_point() + geom_smooth(method = "loess") + geom_smooth(method = "lm") + geom_smooth(method = "auto")
p + geom_point() + geom_smooth() + geom_smooth(method = "glm") + geom_smooth(method = "gam", color = "red")

p + geom_point() + geom_smooth(method = "lm") + scale_x_log10()

#Try it
#The scale log 10 code takes the log base 10 of
#the numbers and scales it down so that the data 
#can be distributed more evenly to find the trend.
#Without this code, we see a graph of cluttered 
#data towards the bottom right, but the log scale 
#moves the points to their log base 10 so that 
#they are manageable points for a graph and 
#trend line.

library(scales)
p + geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10(labels = scales::dollar)
p + geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10(labels = scales::...)
#dollar() call changes the x label to dollar values
?dollar()

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp, color = 'yellow'))
p + geom_point() + scale_x_log10()

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp, color = "yellow"))
p + geom_point(color = "yellow") + scale_x_log10()
#In my own words, color = "yellow" is just an argument inside of a  plotting function

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point() + geom_smooth(color = "orange", se = FALSE, size = 8, method = "lm") + scale_x_log10()

#color = orange makes the line orange
#se = false sets the displays confidence interval
#size = 8 changes the size of the line
#method = lm is one of 5 possible arguments that changes the way the plot is presented

p + geom_point(alpha = 0.3) +
  geom_smooth(method = "gam") +
  scale_x_log10(labels = scales::dollar) +
  labs(x = "GDP Per Capita", y = "Life Expectancy in Years",
       title = "Economic Growth and Life Expectancy",
       subtitle = "Data Points are country-years",
       caption = "Source: Gapminder")

library(scales)
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp, color = continent, fill = continent))
p + geom_point()
p + geom_point() + scale_x_log10(labels = dollar)
p + geom_point() + scale_x_log10(labels = dollar) + geom_smooth()

#fill = continent fills missing values in the selected columns by using previous data entries
#the matching colors for the lines and error bands helps track each line individually

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point(mapping = aes(color = continent)) + geom_smooth() + scale_x_log10()
#this is different because the mapping is done to one line 
#instead of the whole plot individually for each continent

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point(mapping = aes(color = continent)) +
  geom_smooth(mapping = aes(color = continent, fill = continent)) +
  scale_x_log10() +
  geom_smooth(mapping = aes(color = continent), method = "gam")
#This code is bad because of the cluttering of data
#Setting aesthetics at the top is useful because it will apply to
#the whole graph of data to make it fit together, while doing
#the data trend individually can lead to clutter when put together

#Exercise 2
#bank 4 shows balance and y column of yes/no enrollment
bank4 <- bank[, c(6,15)]

#balance of accounts that said yes
bank5 <- bank4[bank4$y == "yes" & bank4$balance < 10000, c(1:2)]

p4 <- ggplot(data = bank5,
            mapping = aes(x = y, y = balance, fill = y))
p4 + geom_boxplot()

#balance of accounts that said no
bank6 <- bank4[bank4$y == "no" & bank$balance < 10000, c(1:2)]
p5 <- ggplot(data = bank6,
             mapping = aes(x = y, y = balance, fill = y))
p5 + geom_boxplot()







