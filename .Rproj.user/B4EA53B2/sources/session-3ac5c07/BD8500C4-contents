library(plotly)

# volcano is a numeric matrix that ships with R

#fig <- plot_ly(z = ~volcano)

#fig <- fig %>% add_surface()


# print(fig)


champ1<-x<- matrix(
  
  c(
    
    0.985, 0.660, 0.168, 0.016, 0.009,
    1.030, 0.788, 0.450, 0.018, 0.044,
    1.016, 0.644, 0.310, 0.017, 0.045,
    0.970, 0.715, 0.348, 0.014, 0.050,
    1.01 , 0.660, 0.370, 0.190, 0.054 
  ), nrow = 5, ncol=5, byrow=TRUE,
                   dimnames = list(c("row1", "row2", "row3", "row4", "row5"), c("C.1", "C.2", "C.3", "C.4", "C.5")))

fig <- plot_ly(z = ~champ1)

fig <- fig %>% add_surface()

print(fig)