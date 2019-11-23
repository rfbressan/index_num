# Exporte a série Econômica do preço do Milho 60kg de Jul/2005 a Jul/2012

milho<-read.csv("C:\\Users\\Aishameriane\\Documents\\Rotinas em R\\aisha\\Milho.csv",sep=";", dec=",")

head(milho)
colnames(milho)[1] <- "Data_r"
colnames(milho)[2] <- "Data"
colnames(milho)[4] <- "Mes"

head(milho)
tail(milho)
milho$Datas=seq(as.Date("2005-07-01"), as.Date("2012-07-01"), by="1 mon")

attach(milho)

# Análises descritivas do banco de dados

summary(Preco)
boxplot(Preco~Ano, las = 2, par(mar = c(4, 5, 4, 2)+ 0.1), at=c(1,2,4,5,7,8,10,11), ylab ="Preço (base 2005 - em R$)", xlab ="Ano", main = "Preços reais (em R$), por ano, da saca de 60kg de milho")

# Faça um gráfico do preço ao longo do tempo e interprete-o

plot(Datas, Preco, type = "l",xaxt="n", ylab="Preço*(R$)", xlab="Mês", main="Preços do Milho 60kg em Reais", sub="*Valores nominais de jul/05 a jul/12")
axis.Date(1, at=seq(min(Datas), max(Datas), by="6 mon"), format="%m/%y")

# Transforme a série em índice com base fixa no período julho 2005
# Transformação de Base: Dividir o preço do ano corrente pelo ano base

Base_2005<-rep(0,length(Preco))

for (i in 1:length(Preco)){
  Base_2005[i]<-Preco[i]/Preco[1]
}
Base_2005

# Faça um gráfico desta variável em relação ao tempo. O que mudou?

plot(Datas, Base_2005, type = "l",xaxt="n", ylab="Relativo de preço*", xlab="Mês", main="Evolução do relativo de preços (base fixa) do Milho 60kg", sub="*Preço base = Jul/2015")
axis.Date(1, at=seq(min(Datas), max(Datas), by="6 mon"), format="%m/%y")

boxplot(Base_2005~Ano, las = 2, par(mar = c(4, 5, 4, 2)+ 0.1), at=c(1,2,4,5,7,8,10,11), ylab ="Relativo de Preço (base 2005)", xlab ="Ano", main = "Preços reais (em R$), por ano, da saca de 60kg de milho")


# Tentando plotar os dois gráficos juntos

par(mar=c(5, 8, 4, 4) + 0.1)

plot(Datas, Preco, axes=F, type = "l",xaxt="n", ylab="", xlab="", main="Preços (em R$) e relativo de base fixa (2005) da saca 60kg \n de milho - Jul/05 a Jul/12")
axis(2, ylim=c(0,max(Preco)), col="black", lwd=2)
axis.Date(1, at=seq(min(Datas), max(Datas), by="6 mon"), format="%m/%y")
mtext(2,text="Preço (R$)", line=2)

par(new=T)
plot(Datas, Base_2005, axes=F, type = "l",col="red",xaxt="n", ylab="", xlab="Data", main="", lty=2,lwd=2)
axis(2, ylim=c(0,max(Base_2005)), lwd=2, line=3.5)
mtext(2,text="Relativo base fixa 2005", line=5.5)

legend(x=1,y=1.2,legend=c("Preços","Relativo base 2005"), lty=c(1,2))

plot(Base_2005,Preco)
cor(Base_2005,Preco)

head(Base_2005)
head(Preco)

# Transforme a série em índice de base relativa

Base_movel<-rep(0, length(Preco))

for (i in 2:length(Preco)){
  Base_movel[i]<-Preco[i]/Preco[i-1]
}

plot(Datas[-1], Base_movel[-1], type = "l",xaxt="n", ylab="Relativo de preço*", xlab="Mês", main="Evolução do relativo de preços (base móvel) \n do Milho 60kg")
axis.Date(1, at=seq(min(Datas[-1]), max(Datas), by="6 mon"), format="%m/%y")


# Gráfico das três quantidades juntas

par(mar=c(5, 12, 4, 4) + 0.1)

plot(Datas, Preco, axes=F, type = "l",xaxt="n", ylab="", xlab="", main="Preços (em R$), relativo de base fixa (2005) \n e base móvel da saca 60kg de milho", sub="Jul/05 a Jul/12")
axis(2, ylim=c(0,max(Preco)), col="black", lwd=2)
axis.Date(1, at=seq(min(Datas), max(Datas), by="6 mon"), format="%m/%y")
mtext(2,text="Preço (R$)", line=2)

par(new=T)
plot(Datas, Base_2005, axes=F, type = "l",col="red",xaxt="n", ylab="", xlab="Data", main="", lty=2,lwd=2)
axis(2, ylim=c(0,max(Base_2005)), lwd=2, line=3.5)
mtext(2,text="Relativo base fixa 2005", line=5.5)

par(new=T)
plot(Datas[-1], Base_movel[-1], axes=F, type = "l",col="green",xaxt="n", ylab="", xlab="Data", main="", lty=3,lwd=2)
axis(2, ylim=c(0,max(Base_movel)), lwd=2, line=7)
mtext(2,text="Relativo base movel", line=9.5)

legend("topleft", c("Preço","Base fixa 2005", "Base móvel"), cex=.5, col = c("black", "red", "green"), lty = c(1,2,3))

# Transforme a Base relativa em base fixa (julho de 2005)

Base_fixa<-rep(0, length(Preco))
Base_fixa[1]<-1

for (i in 2:length(Preco)){
  Base_fixa[i]<-Base_fixa[i-1]*Base_movel[i]
}

head(Base_fixa)
head(Base_2005)

par(mar=c(5,5,4,3))
plot(Base_fixa ~ Base_2005, xlab="Base fixa 2005",  ylab="Base fixa", main="Gráfico de dispersão do relativo de base fixa (base 2005) \n e do relativo de base fixa (a partir do relativo de base móvel)")

# Calcular a Variação mês a mês (a partir da série original)

Variacao = rep(0, length(Preco))

for (i in 2:length(Preco)){
  Variacao[i]<-(Preco[i]/Preco[i-1] - 1)*100
}

plot(Datas[-1], Variacao[-1], type = "l",xaxt="n", ylab="Variação de preços*", xlab="Mês", main="Variação de preços \n da saca de Milho 60kg")
axis.Date(1, at=seq(min(Datas[-1]), max(Datas), by="6 mon"), format="%m/%y")


# Comparando com o relativo de base móvel

par(mar=c(5, 8, 4, 4) + 0.1)

plot(Datas[-1], Variacao[-1], axes=F, type = "l",xaxt="n", ylab="", xlab="", main="Preços (em R$), relativo de base fixa (2005) \n e base móvel da saca 60kg de milho", sub="Jul/05 a Jul/12")
axis(2, ylim=c(0,max(Variacao)), col="black", lwd=2)
axis.Date(1, at=seq(min(Datas), max(Datas), by="6 mon"), format="%m/%y")
mtext(2,text="Variação de preços", line=2)

par(new=T)
plot(Datas[-1], Base_movel[-1], axes=F, type = "l",col="green",xaxt="n", ylab="", xlab="Data", main="", lty=3,lwd=2)
axis(2, ylim=c(0,max(Base_movel)), lwd=2, line=3.5)
mtext(2,text="Relativo base móvel", line=5.5)

legend("topleft", c("Variação", "Base móvel"), cex=.5, col = c("black", "green"), lty = c(1,3))

# Essa variação é igual a qual tipo de base de número índice?

# Mudar a base de 2005.07 para 2006.1

match("2006_01",Data)

Preco[7]
milho[7,]


Base_2006<-rep(0,length(Preco))

for (i in 1:length(Preco)){
  Base_2006[i]<-Preco[i]/Preco[7]
}

par(mar=c(5, 8, 4, 4) + 0.1)

plot(Datas, Base_2006, axes=F, type = "l",xaxt="n", ylab="", xlab="", main="Relativos de base fixa (2005 e 2006) \n  da saca 60kg de milho", sub="Jul/05 a Jul/12")
axis(2, ylim=c(0,max(Base_2006)), col="black", lwd=2)
axis.Date(1, at=seq(min(Datas), max(Datas), by="6 mon"), format="%m/%y")
mtext(2,text="Relativo de base fixa 2006", line=2)

par(new=T)
plot(Datas, Base_2005, axes=F, type = "l",col="green",xaxt="n", ylab="", xlab="Data", main="", lty=3,lwd=2)
axis(2, ylim=c(0,max(Base_2005)), lwd=2, line=3.5)
mtext(2,text="Relativo base fixa 2005", line=5.5)

legend("topleft", c("Base fixa 2006", "Base fixa 2005"), cex=.5, col = c("black", "green"), lty = c(1,3))