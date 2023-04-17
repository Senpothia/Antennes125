library(sf)
a = data.frame(x=rnorm(20000, 10, 1.9), y=rnorm(20000, 10, 1.2) )
b = data.frame(x=rnorm(20000, 14.5, 1.9), y=rnorm(20000, 14.5, 1.9) )
c = data.frame(x=rnorm(20000, 9.5, 1.9), y=rnorm(20000, 15.5, 1.9) )
data = rbind(a,b,c)

#Lines
pp = ggplot(data, aes(x=x, y=y)) +
  geom_hex(bins = 20, size = 0.5, color = "black") +
  scale_fill_viridis_c(option = "C")
plot_gg(pp, width = 4, height = 4, scale = 300, multicore = TRUE)

#No lines
pp_nolines = ggplot(data, aes(x=x, y=y)) +
  geom_hex(bins = 20, size = 0) +
  scale_fill_viridis_c(option = "C")
plot_gg(pp_nolines, width = 4, height = 4, scale = 300, multicore = TRUE)
