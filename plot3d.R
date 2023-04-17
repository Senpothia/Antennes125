x = c(1,2,3,4,5)

y = c(1,2,3,4,5)

z = rbind(
  
  c(0, 1, 0, 1, 0),
  
  c(1, 0, 1, 0, 1),
  
  c(0, 1, 0, 1, 0),
  
  c(1, 0, 1, 0, 1),
  
  c(0, 1, 0, 1, 0))



library(plotly)

fig <- plot_ly(
  
  type = 'surface',
  
  contours = list(
    
    x = list(show = TRUE, start = 1.5, end = 2, size = 0.04, color = 'white'),
    
    z = list(show = TRUE, start = 0.5, end = 0.8, size = 0.05)),
  
  x = ~x,
  
  y = ~y,
  
  z = ~z)

fig <- fig %>% layout(
  
  scene = list(
    
    xaxis = list(nticks = 20),
    
    zaxis = list(nticks = 4),
    
    camera = list(eye = list(x = 0, y = -1, z = 0.5)),
    
    aspectratio = list(x = .9, y = .8, z = 0.2)))


print(fig)