library(dplyr)
library(ggplot2)

##practice plotting w the dataset
ggplot(data=bestsellers_data, aes(x=Reviews, y=User.Rating)) +
  labs (title = "title") +
  geom_violin()


##practice subsetting your data
##use a combination of filter, select, mutate, arrange, summarise, group_by, 
##sample, and/or slice
names(bestsellers_data)

bestsellers_data_noMissing <- bestsellers_data%>%
  filter(Reviews >= 1000)

ggplot(bestsellers_data_noMissing, aes(x=Price, y=User.Rating, color = as.numeric(Year)))+
  geom_point() +
  labs(title = "user rating and price of bestsellers")
  


##filtering observations of your dataset, create a smaller dataset w a subset of
#variables, add 2 new columns using mutate(), create a data frame of grouped summaries
#on at least one numeric variable by at least one categorical var, create at
#least one new visualization using updated dataset
bestsellers_trust <- bestsellers_data%>%
  filter(Reviews >= 1000)

average_rating <- mean(bestsellers_trust$User.Rating)
ratings <- bestsellers_trust$User.Rating
prices <- bestsellers_trust$Price

bestsellers_trust1 <- mutate(bestsellers_trust, z.score.rating = (User.Rating-average_rating)/sd(ratings))
bestsellers_trust2 <- mutate(bestsellers_trust1, percentile.price = pnorm((Price-mean(bestsellers_trust$Price))/sd(prices)))
summarise(group_by(bestsellers_trust2,Genre), mean_rating = mean (User.Rating, na.rm = T))
View(bestsellers_trust2)

ggplot(bestsellers_trust2, aes(x=User.Rating, y=percentile.price, color = as.numeric(Year)))+
  geom_point() +
  labs(title = "user rating and percentile of price")
