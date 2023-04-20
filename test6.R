x <- seq(-pi, pi, len = 1000)
val <- data.frame(x = x, y = sin(x))
p<-(
  ggplot(data = val, aes(x = x, y = y)) + 
  geom_line() + 
  scale_y_continuous(name = "sin(x)")
)
print(p)