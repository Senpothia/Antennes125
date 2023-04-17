Vcc<-5
Vss<-0
Rser<-30
Rad<-9

Iant<-function(Rant){(4/pi)*(Vcc-Vss)/(Rant+Rser+2*Rad)}

curve(Iant, 50, 300, xname = "I")