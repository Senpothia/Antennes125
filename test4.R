a <- 17
b <- a/2
flag <- 0
i <- 2

Iantenne <- readline(prompt="Entrée la valeur du courant d'antenne en mA: ")
repeat
{
  if ((Iantenne)== "0")
  {
    flag <- 1
    break
  }else{
    
    Iantenne <- readline(prompt="Entrée la valeur du courant d'antenne en mA: ")
    print(Iantenne)
    
  }
}


  print("FIN")

