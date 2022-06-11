library(ggplot2)
numrows <- 250
matrix.sir <- matrix(nrow = numrows, ncol = 4)

matrix.sir[,1] <- 1:numrows

poblacion <- 1e4 + 1
matrix.sir[1,2:4] <- c(poblacion-1, 1, 0)

alpha = 1/9
beta = 0.253

for(i in 2:numrows){
  matrix.sir[i,2] <- matrix.sir[(i-1),2] - beta*matrix.sir[(i-1),2]*matrix.sir[(i-1),3]/poblacion
  matrix.sir[i,3] <- matrix.sir[(i-1),3] + beta*matrix.sir[(i-1),2]*matrix.sir[(i-1),3]/poblacion - alpha*matrix.sir[(i-1),3]
  matrix.sir[i,4] <- matrix.sir[(i-1),4] + alpha*matrix.sir[(i-1),3]
}

df.sir <- data.frame(matrix.sir)
colnames(df.sir) <- c('t','s','i','r')

p <- ggplot(df.sir, aes(x=t)) +
  geom_line(aes(y=s,colour="Susceptibles")) +
  geom_line(aes(y=i,colour="Infectados")) +
  geom_line(aes(y=r,colour="Recuperados")) +
  xlab("Tiempo")+
  ylab("Población")
p
