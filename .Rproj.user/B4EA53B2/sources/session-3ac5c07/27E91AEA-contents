library(plotly)

# volcano is a numeric matrix that ships with R

#fig <- plot_ly(z = ~volcano)

#fig <- fig %>% add_surface()


# print(fig)


champ2<-x<- matrix(
  
  c(
    
    0.896, 0.620, 0.320, 0.030, 0.012,
    0.715, 0.492, 0.240, 0.040, 0.012,
    0.712, 0.492, 0.217, 0.023, 0.017,
    0.757, 0.488, 0.242, 0.023, 0.021,
    0.540 , 0.382, 0.063, 0.029, 0.020 
  ), nrow = 5, ncol=5, byrow=TRUE,
                   dimnames = list(c("row1", "row2", "row3", "row4", "row5"), c("C.1", "C.2", "C.3", "C.4", "C.5")))

fig <- plot_ly(z = ~champ2)

fig <- fig %>% add_surface()

print(fig)