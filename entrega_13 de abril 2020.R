rm(list = ls())
setwd("~/Desktop/COVIDServer")
dir()
sim <- read.csv('sim_365dias.csv', stringsAsFactors = FALSE)
sim.control <- read.csv('sim_365dias_control.csv', stringsAsFactors = FALSE)


I1_1_i <- 194:225
I1_2_i <- 226:257
I1_3_i <- 258:289
I1_4_i <- 290:321

I2_1_i <-322:353
I2_2_i <-354:385
I2_3_i <-386:417
I2_4_i <-418:449

I3_1_i <-450:481
I3_2_i <-482:513
I3_3_i <-514:545
I3_4_i <-546:577

D_i <- 578:609
R_i <- 610:641

pdf(file =paste0("Comparación escenario controlado vs no controlado.pdf"))

matplot(sim[,1],rowSums(sim[,c(I1_1_i,I2_1_i,I3_1_i)]), t="l", col='red',xlab="Dias",ylab="Poblacion", main="Inf. Asintomaticos ")
lines(sim.control[,1],rowSums(sim.control[,c(I1_1_i,I2_1_i,I3_1_i)]), t="l",
        col='blue', xlab="Dias",ylab="Poblacion", main="Inf. Asintomaticos ")

matplot(sim[,1],rowSums(sim[,c(I1_2_i,I2_2_i,I3_2_i)]),t="l", col='red',xlab="Dias",ylab="Poblacion", main="Inf. Leves ")
lines(sim.control[,1], rowSums(sim.control[,c(I1_2_i,I2_2_i,I3_2_i)]),t="l", col='blue', xlab="Dias",ylab="Poblacion", main="Inf. Leves ")


matplot(sim[,1],rowSums(sim[,c(I1_3_i,I2_3_i,I3_3_i)]),t="l", col='red',xlab="Dias",ylab="Poblacion", main="Inf. Hosp")
lines(sim.control[,1],rowSums(sim.control[,c(I1_3_i,I2_3_i,I3_3_i)]),t="l", col='blue',xlab="Dias",ylab="Poblacion", main="Inf. Hosp")


matplot(sim[,1],rowSums(sim[,c(I1_4_i,I2_4_i,I3_4_i)]),t="l", col='red',
        xlab="Dias",ylab="Poblacion", main="Inf. UCI")
lines(sim.control[,1],rowSums(sim.control[,c(I1_4_i,I2_4_i,I3_4_i)]),
      t="l", col='blue',
        xlab="Dias",ylab="Poblacion", main="Inf. UCI")

matplot(sim[,1],rowSums(sim[,sort(c(I1_1_i,I2_1_i,I3_1_i,I1_2_i,I2_2_i,I3_2_i,I1_3_i,I2_3_i,I3_3_i, I1_4_i, I2_4_i,I3_4_i))]),t="l", col='red',
        xlab="Dias", ylab="Poblacion", main="Total de Infeccioso")
lines(sim.control[,1],rowSums(sim.control[,sort(c(I1_1_i,I2_1_i,I3_1_i,I1_2_i,I2_2_i,I3_2_i,I1_3_i,I2_3_i,I3_3_i, I1_4_i, I2_4_i,I3_4_i))]),t="l", col='blue',
        xlab="Dias", ylab="Poblacion", main="Total de Infeccioso")


matplot(sim[,1],rowSums(sim[,D_i]),t="l",
        xlab="Dias",ylab="Poblacion", col='red',
        main="Defunciones")
lines(sim.control[,1],rowSums(sim.control[,D_i]),t="l",
      xlab="Dias",ylab="Poblacion", col='blue',
        main="Defunciones")


matplot(sim[,1],rowSums(sim[,R_i]),t="l", xlab="Dias",ylab="Poblacion",
        main="Removidos", col='red')

lines(sim.control[,1],rowSums(sim.control[,R_i]),t="l",
      xlab="Dias",ylab="Poblacion", col='blue',
        main="Removidos")
dev.off()


no.pares <- seq(1, nrow(sim), by = 2)
sim.copy <- sim[ no.pares, ]

sim.copy$Inf.Asintomaticos <- rowSums(sim.copy[, c(I1_1_i,I2_1_i,I3_1_i)])
sim.copy$Inf.Leves <- rowSums(sim.copy[, c(I1_2_i,I2_2_i,I3_2_i)])
sim.copy$Inf.Hosp <- rowSums(sim.copy[ , c(I1_3_i,I2_3_i,I3_3_i)])
sim.copy$Inf.UCI   <- rowSums(sim.copy[,c(I1_4_i,I2_4_i,I3_4_i)])
sim.copy$Total.infecciosos <- rowSums(sim.copy[,sort(c(I1_1_i,I2_1_i,I3_1_i,I1_2_i,I2_2_i,I3_2_i,I1_3_i,I2_3_i,I3_3_i, I1_4_i, I2_4_i,I3_4_i))])
sim.copy$Defunciones <- rowSums(sim.copy[,D_i])
sim.copy$Removidos <- rowSums(sim.copy[,R_i])

entre1.sin.control <- sim.copy[, c('time', 'Inf.Asintomaticos',
                                   'Inf.Leves', 'Inf.Hosp', 'Inf.UCI',
                                   'Total.infecciosos', 'Defunciones',
                                   'Removidos')]
entre1.sin.control$Defunciones_por_dia <-
    c(0,diff(entre1.sin.control$Defunciones))
entre1.sin.control$Removidos_por_dia <-
    c(0,diff(entre1.sin.control$Removidos))
write.csv( entre1.sin.control, file='tabla_datos_por_dia_sin_control.csv',
           row.names = FALSE  )
# defunciones y recuperados sí diferencias
sim.copy <- sim.control

sim.copy$Inf.Asintomaticos <- rowSums(sim.copy[, c(I1_1_i,I2_1_i,I3_1_i)])
sim.copy$Inf.Leves <- rowSums(sim.copy[, c(I1_2_i,I2_2_i,I3_2_i)])
sim.copy$Inf.Hosp <- rowSums(sim.copy[ , c(I1_3_i,I2_3_i,I3_3_i)])
sim.copy$Inf.UCI   <- rowSums(sim.copy[,c(I1_4_i,I2_4_i,I3_4_i)])
sim.copy$Total.infecciosos <- rowSums(sim.copy[,sort(c(I1_1_i,I2_1_i,I3_1_i,I1_2_i,I2_2_i,I3_2_i,I1_3_i,I2_3_i,I3_3_i, I1_4_i, I2_4_i,I3_4_i))])
sim.copy$Defunciones <- rowSums(sim.copy[,D_i])
sim.copy$Removidos <- rowSums(sim.copy[,R_i])

entre1.con.control <- sim.copy[, c('time', 'Inf.Asintomaticos',
                                   'Inf.Leves', 'Inf.Hosp', 'Inf.UCI',
                                   'Total.infecciosos', 'Defunciones',
                                   'Removidos')]
entre1.con.control$Defunciones_por_dia <-
    c(0,diff(entre1.con.control$Defunciones))
entre1.con.control$Removidos_por_dia <-
    c(0,diff(entre1.con.control$Removidos))
write.csv( entre1.con.control, file='tabla_datos_por_dia_con_control.csv',
           row.names = FALSE  )
getwd()
