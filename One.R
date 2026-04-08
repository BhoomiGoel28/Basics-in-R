#Installing and using packages

install.packages("tidyverse")
library(tidyverse)

starwars %>%
  filter (height>150 & mass<200) %>%
  mutate (height_in_metres = height/100) %>%
  select (height_in_metres, mass) %>%
  arrange(mass) %>%
  plot()


library(ggplot2)

# Bar Plot

ggplot(data=starwars,
       mapping=aes(x=gender)) + geom_bar()


# Histograms

starwars%>%
  drop_na(height)%>%
  ggplot(aes(height))+
  geom_histogram()


# Box Plot

#sub-1
starwars%>%
  drop_na(height)%>%
  ggplot(aes(height))+
  geom_boxplot(fill="steelblue")+
  theme_bw()

#sub-2
starwars%>%
  drop_na(height)%>%
  ggplot(aes(height))+
  geom_boxplot(fill="steelblue")+
  theme_bw()+
    labs(title="Boxplot of height",x="Height of characters")


# Example 1

ggplot(data = BOD,
       mapping = aes(x = Time,
                     y = demand)) +
  geom_point(size = 5) +
  geom_line(colour="red")


# Example 2

data()
view(CO2)
names(CO2)
CO2 %>% 
  ggplot(aes(conc, uptake, colour = Treatment))+
  geom_point(size=3,alpha = 0.5)+
  geom_smooth(method=lm, se= F)+
  facet_wrap(~Type)+
  theme_bw()+
  labs(title= "Concentration of CO2")

#for giving labels we use = labs
#for transparency of colors we use  = alpha


# Example 3

view(CO2)
CO2 %>%
  ggplot(aes(Treatment, uptake))+
  geom_boxplot()+
  geom_point(alpha=0.5,
             aes(size=conc,
                 colour= Plant))+
  facet_wrap(~Type)+
  coord_flip()+
  theme_bw()+
  labs(title= "chilled vs non_chilled")


# Example 4

CO2 %>%
  filter(uptake < 40) %>%
  ggplot(aes(Treatment, uptake))+
  geom_boxplot(aes(colour= Treatment))+
  geom_point(alpha=0.5,
             aes(size=conc,
                 colour= Plant))+
  facet_wrap(~Type)+
  coord_flip()+
  theme_bw()+
  labs(x= "shocked",
       y= "are you",
       title= "chilled vs non_chilled")


# Density Plot

view(msleep)
msleep %>%
  drop_na(vore) %>%
  ggplot(aes(sleep_total, fill = vore))+
  geom_density(alpha=0.2)+
  theme_bw()

msleep %>%
  drop_na(vore) %>%
  ggplot(aes(sleep_total))+
  geom_density()+
  facet_wrap(~vore)+
  theme_bw()

## using filter

msleep %>%
  drop_na(vore) %>%
  filter(vore == "herbi" | vore == "carni") %>%
  ggplot(aes(sleep_total, fill = vore))+
  geom_density(alpha=0.2)+
  theme_bw()


msleep %>%
  drop_na(vore) %>%
  filter(vore %in% c("carni", "herbi")) %>%
  ggplot(aes(sleep_total, fill = vore))+
  geom_density(alpha=0.2)+
  theme_bw()