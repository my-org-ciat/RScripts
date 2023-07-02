library(tidyverse)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

#grouping by color using class variable
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

#grouping by size using class variable
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))
#> Warning: Using size for a discrete variable is not advised.

# Left
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# ggplot2 will only use six shapes at a time. By default, additional groups will 
#go unplotted when you use the shape aesthetic
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# all the same color
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

#facets - particularly useful for categorical variables, is to split your plot 
#into facets, subplots that each display one subset of the data.
#The variable that you pass to facet_wrap() should be discrete
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

#To facet your plot on the combination of two variables, add facet_grid()
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

#mohammed data
ggplot(data = data) + 
  geom_point(mapping = aes(x = BYton, y = GYton), color = "blue", 
             alpha = .5,
             size = 2) + 
  facet_grid(Woreda ~ advisory)

ggplot(data = data) + 
  geom_point(mapping = aes(x = BYton, y = GYton), color = "blue", 
             alpha = .5,
             size = 2) +
  geom_smooth(mapping = aes(x = BYton, y = GYton), color = "blue", 
              alpha = .5,
              size = 2)+
  facet_grid(Woreda ~ advisory)

# smooth with color category
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()

#smooth line displays just a subset of the mpg dataset, the subcompact cars.
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class))+ 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)
  
#mame data
ggplot(data = data, mapping = aes(x = BYton, y = GYton)) + 
  geom_point() +
  geom_smooth(data = filter(data, Woreda == c(unique(data$Woreda))), se = FALSE)+
  facet_grid(Woreda ~ advisory)
