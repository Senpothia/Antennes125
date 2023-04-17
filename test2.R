# Create some plots
library(ggplot2)
myplot1 <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) + 
  geom_point()
myplot2 <- ggplot(iris, aes(Species, Sepal.Length)) + 
  geom_boxplot()

# Print plots to a pdf file
png("ggplot.png")
print(myplot1)     # Plot 1 --> in the first page of PDF
print(myplot2)     # Plot 2 ---> in the second page of the PDF
dev.off() 