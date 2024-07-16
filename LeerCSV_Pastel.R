ldatos <- read.csv("/home/charles/R Studio/Datos.csv", header = FALSE)

vdatos <- c()

for (i in 1:length(ldatos[[1]])) 
{
  vdatos[i] <- ldatos[i,1]
}

n <- length(vdatos)
rango <- max(vdatos)-min(vdatos)
k <- sqrt(n)
ac <- rango/k

if(ac<1)
{
  ac <- 1
} else {
  ac <- round(ac)
  if(ac%%2 == 0)
  {
    ac <- ac +1
  }
}


frec <- c()
clases <- c()
min <- min(vdatos)
for (i in 1:n)
{
  contar <- 0
  for (j in 1:n) 
  {
    if(vdatos[j]>=min && vdatos[j]<min+ac)
    {
      contar <- contar + 1
    }
  }
  #print( paste(min,"-",min+ac ) )
  clases[i] <- paste(min,"-",min+ac )
  min <- min+ac
  frec[i] <- contar
  if(min>max(vdatos))
  {
    break
  }
}

frecAcum <- c()
Acum <- 0
for (i in 1:length(frec) )
{
  Acum <- Acum + frec[i]
  frecAcum[i] <- Acum;
}

frecRel <- c()
porciento <- c()

for (i in 1:length(frec) )
{
  frecRel[i] <- frec[i]/n;
  porciento[i] <- frecRel[i] * 100
}

porcientoE <- paste0(porciento, "%")

pie(porciento, labels = porcientoE, main = "Porcentajes %", col = c("red","blue","green","orange"))
legend("topleft",clases, fill = c("red","blue","green","orange"))

frecAcumRel <- c()
Acum <- 0
for (i in 1:length(frec) )
{
  Acum <- Acum + frecRel[i]
  frecAcumRel[i] <- Acum;
}

tabla <- data.frame(Clases=clases,Frecuencia=frec,Frecuencia_Acumulada=FrecAcum, Frecuencia_Relativa=frecRel,Frecuencia_Relativa_Acumulada=frecAcumRel,Porcentaje=porciento)
View(tabla)

