datanba <- read.csv("nba.csv")
attach(datanba)
View(datanba)
library(dplyr)

#Buscar ausentes
any(is.na(datanba))
sum(is.na(datanba))
apply(is.na(datanba), 2, which)

nba <- select(datanba, -'Player', -'NBA_Country', -'Tm')
delete.na <- function(nba, n=0) {
  nba[rowSums(is.na(nba)) <= n,]
}
nba1 <- delete.na(nba)



lModel <- lm (formula = Salary ~ NBA_DraftNumber + Age + G + MP + PER + TS. + X3PAr + FTr + ORB. + DRB. + TRB. + AST. + STL. + BLK. + TOV. + USG. + OWS + DWS + WS + WS.48 + OBPM + DBPM + BPM + VORP, data = nba1)
summary(lModel)
