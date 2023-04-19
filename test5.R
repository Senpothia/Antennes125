x<-seq(1:10)
y<-x+2
plot(x,y)
X11()


savePlot(filename = "saved",
         type = "png",
         device = dev.cur(),
         restoreConsole = TRUE)
dev.off()