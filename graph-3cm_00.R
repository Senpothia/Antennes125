library(plotly)

# volcano is a numeric matrix that ships with R

#fig <- plot_ly(z = ~volcano)

#fig <- fig %>% add_surface()


# print(fig)


champ3<-x<- matrix(
  
  c(
    
    0.270, 0.135, 0.017, 0.003, 0.006,
    0.320, 0.180, 0.066, 0.026, 0.007, 
    0.376, 0.255, 0.132, 0.033, 0.007, 
    0.410, 0.284, 0.166, 0.045, 0.008, 
    0.420, 0.291, 0.220, 0.046, 0.017
  
    
    
  ), nrow = 5, ncol=5, byrow=TRUE,
  dimnames = list(c("row1", "row2", "row3", "row4", "row5"), c("C.1", "C.2", "C.3", "C.4", "C.5")))

fig <- plot_ly(z = ~champ3)

fig <- fig %>% add_surface()

print(fig)