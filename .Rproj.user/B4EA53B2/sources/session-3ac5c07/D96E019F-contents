library(plotly)

# volcano is a numeric matrix that ships with R

#fig <- plot_ly(z = ~volcano)

#fig <- fig %>% add_surface()


# print(fig)


champ1<-x<- matrix(c(1.01,0.310,0.054, 0.970,0.348,0.045,1.016,0.370, 0.044,1.030,0.450,0.050), nrow = 4, ncol=3, byrow=TRUE,
                   dimnames = list(c("row1", "row2", "row3", "row4"), c("C.1", "C.2", "C.3")))

fig <- plot_ly(z = ~champ1)

fig <- fig %>% add_surface()

print(fig)