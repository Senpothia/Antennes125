library(ggplot2)

#create data frame
df <- data.frame(x=c(1, 2, 2, 3, 5, 6, 8, 8, 9, 9, 10, 11, 12, 15, 15),
                 y=c(2, 3, 3, 4, 5, 5, 6, 7, 8, 8, 9, 10, 16, 19, 28))

#create plot with three fitted regression models
print(
  ggplot(df, aes(x, y)) +
  geom_point() +
  geom_smooth(se=FALSE, aes(color='Linear')) +
  geom_smooth(formula=y~poly(x, 2), se=FALSE, aes(color='Quadratic')) +
  geom_smooth(formula=y~poly(x, 3), se=FALSE, aes(color='Cubic')) +
  scale_color_manual(name='Regression Model',
                     breaks=c('Linear', 'Quadratic', 'Cubic'),
                     values=c('Cubic'='pink', 'Quadratic'='blue', 'Linear'='purple'))
  
)