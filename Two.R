# Analyse

# T-Test

install.packages("gapminder")
library(gapminder)
View(gapminder)

library(ggplot2)
library(patchwork)

gapminder %>%
  filter(continent %in% c("Africa","Europe")) %>%
  t.test(lifeExp ~ continent, data= ., 
         alternative="two.sided",
         paired=FALSE)

data()
View(gapminder)
plot(gapminder)

#density plot of Africa
gapminder %>% ggplot(aes(x= lifeExp, fill = continent)) +
  geom_density(colour = 4,
               alpha = 0.3) +
  facet_wrap(~continent) +
  theme_bw() 

# ANOVA

ANOVA_plot

gapminder %>%
  filter (year==2007) %>%
  filter(continent %in% c("Americas", "Europe", "Asia")) %>%
  aov(lifeExp ~ continent, data= .)%>%
  summary()

# Chi-Squared
chi_plot

#chi-squared gooodness fit test (used for categorical variable)
head(iris)
#making new dataframe
flowers <- iris %>%
  mutate(size = cut(Sepal.Length,
                    breaks = 3,
                    labels = c("small", "Medium", "Large")))
head(flowers)

flowers <- flowers %>%
  select(Species , size)

table(flowers)

#question: is there a significant difference in the proportion of flowers that are small, medium, large (alpha/p/significance = 0.05)

#chi-squared gooodness fit test (used for categorical variable)

# H0: the proportion of flowers that are small, medium, and large are equal
# H1: the proportion of flowers that are small, medium, and large are not equal

table(flowers$size)


chi_size <- flowers %>%
  select(size) %>%
  table() %>%
  chisq.test()
attributes(chi_size)
head(chi_size)
View(chi_size)

#as the p < 0.05
#we reject the null hypothesis(that is proportion are not equal)

#chi-squared test for independence

flowers %>% table() %>%
  chisq.test() 

#fischer exact test if >20% of expected values are <5
#or all are if any values of < 5 in a 2x2


flowers %>% table() %>%
  chisq.test() %>%
  .$expected


#####################advanced and Neat methods
library(tidyverse)

#to get summary statistics (visualising given data)
library(summarytools)

#to get publication ready tables (contingency table)
library(gtsummary)

#for visualising data and running statistical tests 
library(ggstatsplot)

#get explporatory results for our data
summ <- dfSummary(flowers)
view(summ)

#generate cross tabs using tbl_cross of gtsummary package
crosstabs_flowers <- flowers %>%
  tbl_cross(row = Species,
            col = size,
            percent = "row")

crosstabs_flowers

#chi-square tests
##values in cells should be > 5 if <5 we use fischer exacts test
chisq.test(flowers$Species, flowers$size)
#or
flowers %>%
  table()%>%
  chisq.test()

#plotting a bar plot with chi-square test results incorporated
#( using ggbarstats function of ggstatplots package)

plot_bar <- ggbarstats(data = file_LT,
                       x = `Limited training0`,
                       y = `Limited training1`)
plot_bar

#plotting a pie chart
plot_pie <- ggpiestats(flowers,
                       x = "Limited training0",
                       y = "Limited training1")
plot_pie

