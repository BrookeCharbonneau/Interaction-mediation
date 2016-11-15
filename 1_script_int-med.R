library(tidyverse)
library(apaTables)

#load data
my_data <- read_csv("lectureData6060.csv")
glimpse(my_data)

analytic.data <- my_data


##Do anxiety and preparation interact to predict exam scores?

#Center variables
analytic.data <- analytic.data %>% mutate(x.centered=as.numeric(scale(Anxiety, center=T, scale=F)))
analytic.data <- analytic.data %>% mutate(z.centered=as.numeric(scale(Preparation, center=T, scale=F)))
glimpse (analytic.data)

#Compute regression including interaction
interaction.regression <- lm(Exam ~ x.centered + z.centered + I(x.centered*z.centered), 
                             data=analytic.data, na.action=na.exclude)
#na.action=na.exclude = if any row has a missing value, drop the row (can't make the interaction product)

#regression table
apa.reg.table(interaction.regression)
#or can use block approach
block1 <- lm(Exam ~ x.centered + z.centered,data=analytic.data, na.action=na.exclude)
block2 <- lm(Exam ~ x.centered + z.centered + I(x.centered*z.centered), data=analytic.data, na.action=na.exclude)
apa.reg.table(block1, block2)
#then look at delta R2 - will give same value as reg table approach
#although R2 is significant, its CI overlaps with 0 - inconsistent messaging = small effect


##make the graph - getting the lines on the surface (+1 SD)

#get sd of z, then mutate a bit
sd.z <- sd(analytic.data$z.centered, na.rm=TRUE)
analytic.data <- analytic.data %>% mutate(z.centered.at.plus.1SD = z.centered - sd.z)
#This may seem counter intuitive, but we lower the scores to increase the zero point to +1

#get formula
simple.slope.plus.1SD <- lm(Exam ~ x.centered + z.centered.at.plus.1SD + I(x.centered*z.centered.at.plus.1SD),
                            data=analytic.data, na.action=na.exclude) 
summary(simple.slope.plus.1SD)

#make regression table
apa.reg.table(simple.slope.plus.1SD)
#drop last 2 lines - look only at x.centered and intercept lines


##make the graph - getting the lines on the surface (-1 SD)

#get sd of z, then mutate a bit
analytic.data <- analytic.data %>% mutate(z.centered.at.minus.1SD = z.centered + sd.z)
#This may seem counter intuitive, but we increase the scores to decrease the zero point to -1

#get formula
simple.slope.minus.1SD <- lm(Exam ~ x.centered + z.centered.at.minus.1SD + I(x.centered*z.centered.at.minus.1SD),
                            data=analytic.data, na.action=na.exclude) 
summary(simple.slope.minus.1SD)

#make regression table
apa.reg.table(simple.slope.minus.1SD)
#drop last 2 lines - look only at x.centered and intercept lines


##make the graph - plot 
#continue from packages Brooke



##Mediation - does anxiety explain the relationship between preparation and exam scores?

#create new data frame to make commands easier
mediation.data <- analytic.data %>% select(Exam, Preparation, Anxiety)
# select in order of y, x, mediator

#numbers relate to the column number in the data set
psych::mediate(y=1, x=2, m=3, data=mediation.data)
#look at ab - overall relation (with lower and upper bounds)
#Here, no mediator because ab is 0 AND because ab CI overlaps with 0

