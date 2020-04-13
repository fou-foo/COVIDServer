#Este es un ejemplo para obtener las curvas basadas en el modelo epidemiologgico en ModeloMigracion.R
library(deSolve)
print("Corriendo en server CIMAT")
#path <- "C:/Users/fou-f/Desktop/CIMAT"
path <- "/home/josegarcia/COVIDServer/Ejecucion300nuevosrceros/"
setwd(path)
print(getwd())
#Todas las tasas en el modelo se consideran diarias.

# PARAMETROS  ####################################
R0s <- read.csv("R0Estados11abril_actualizado.csv")

R01 <- R0s[,6]  #sin control
R02 <- R0s[,7]  #con control

plot(R01,R02)
#abline(0,1)

sigma1 <- 1  #Carlos considera que practiamente E tiene periodo 0
sigma2 <- 1  #Carlos considera que practiamente E tiene periodo 0
sigma3 <- 1  #Carlos considera que practiamente E tiene periodo 0

alfa <- matrix(c(0.3,0.55,0.1,0.05,0.3,0.55,0.1,0.05,0.3,0.55,0.1,0.05),4,3) #carlos
colSums(alfa)  #tienen que ser un vector de 1's
alfa<-as.vector(alfa)
names(alfa) <- paste0("alfa",rep(1:3,each=4), "_", rep(1:4, 3))

gama1<-1/14  #Carlos
gama2<-1/21  #Carlos
gama3<-1/25  #Carlos
gama4<-1/25  #Carlos

delta1<-0     #Carlos
delta2<-0     #Carlos
delta3<-0.15  #Carlos
delta4<-0.5   #Carlos

omega<-1

bet<-R01*(gama1+gama1+gama3+gama4)/4
bet2<-R02*(gama1+gama1+gama3+gama4)/4
#if(length(R0)==0) {bet<-rep(bet,32)}
names(bet)<-paste0("bet", 1:32)


movilidad<-read.csv("movilidad_nacional.csv",head=TRUE)  #La información que nos pasó Pablo
Ji<-matrix(0,32,32)
Ji[cbind(movilidad$entidad_origen, movilidad$entidad_destino)]<-movilidad$tasa_movilidad
ji<-as.vector(Ji)

names(ji)<- paste0("ji",rep(1:32, 32), "_", rep(1:32, each=32))

theta <- c(sigma1=sigma1
           , sigma2=sigma2
           , sigma3=sigma3
           , alfa
           , gama1=gama1
           , gama2=gama2
           , gama3=gama3
           , gama4=gama4
           , delta1=delta1
           , delta2=delta2
           , delta3=delta3
           , delta4=delta4
           , omega=omega
           , bet
           , ji)


# ESTADO INICIAL  ################################
datos <- read.csv("Data-2020-04-09.csv")  #hay que checar que los estados tengan el mismo orden que parametreos.csv y valores_ini.csv
#utilizo los datos confimados hasta el 8 de marzo

N_subnet<- read.csv("grupo_pob_nacional_entidad.csv")
N_subnet_grupo<-cbind(N_subnet[,4], N_subnet[,8]-N_subnet[,4]-N_subnet[,7],N_subnet[,7])
# Los grupos de edad son [0,4],[5,59] y [60,+]

dd<-as.Date(datos$fecha_corte,format="%Y-%m-%d")
GrupoEdad<-cut(datos$edad,breaks=c(0,4,59,120), include.lowes=TRUE, label=FALSE)
tt<-table(factor(datos$ent[dd<="2020-03-08"],levels=1:32),factor(GrupoEdad[dd<="2020-03-08"],levels=1:3))
(NumCasos<-as.matrix(tt))


NumCasos_No_Obs<-0.85/.15*NumCasos # Carlos  (considerando los asintomáticos y los leves no se observan)
NumCasos_Total<-NumCasos+NumCasos_No_Obs #todos

m<-32  #el numero de subpoblaciones
S_val <- (N_subnet_grupo-NumCasos_Total)
E_val <- NumCasos_Total #0*S_val#rep(0,m)
I_1_val <- (NumCasos_No_Obs)/2
I_2_val <- (NumCasos_No_Obs)/2
I_3_val <- (NumCasos)/2
I_4_val <- (NumCasos)/2
D_val <-  rep(0,m)
R_val <-  rep(0,m)
Y_val <-  I_2_val+I_3_val+I_4_val   #los de cualquier grupo de edad pero en severidad _2, _3 y _4
                                    #(por ahora supongo quela obs es perfecta)
N_val <-  N_subnet_grupo
X_ini <- c(as.matrix(S_val)
        , as.matrix(E_val)
        , I_1_val[,1],I_2_val[,1],I_3_val[,1],I_4_val[,1]   #grupo edad 1
        , I_1_val[,2],I_2_val[,2],I_3_val[,2],I_4_val[,2]   #grupo edad 2
        , I_1_val[,3],I_2_val[,3],I_3_val[,1],I_4_val[,3]   #grupo edad 3
        , D_val
        , R_val
        , as.matrix(Y_val)
        , as.matrix(N_val))


n_S <- paste0("S", rep(1:3,each=m),"_", 1:m)
n_E <- paste0("E", rep(1:3,each=m),"_", 1:m)
n_I <- paste0("I", rep(1:3,each=m*4),"_",rep(rep(1:4,each=m),3),"_",1:m)
n_D <- paste0("D", 1:m)
n_R <- paste0("R", 1:m)
n_Y <- paste0("Y", rep(1:3,each=m),"_",1:m)
n_N <- paste0("N", rep(1:3,each=m),"_", 1:m)
np <- c(n_S, n_E, n_I, n_D,n_R, n_Y, n_N)

names(X_ini) <- np  #El orden es super importante
#sum(X_ini[-grep("y",np)])  #tiene que ser igual a 1
#X_ini<-X_ini*100

# TIEMPO  ########################################

#days<-9
#tiempos <- seq(0, days ,length = days+1)


# CORRIENDO Y GRAFICANDO  ########################
source("ModeloMigracion_M6.R")
days<-9; tiempos <- seq(0, days ,length = days+1)
sim1<-X_theta(theta, tiempos, X_ini)


###############################
days<-6; tiempos <- seq(0, days ,length = days+1)
#modificamos los valores de beta
nt<-names(theta)
theta[grep('bet',names(theta))]<-bet2
names(theta)<-nt  #solo para asegurarme
sim2<-X_theta(theta, tiempos, X_ini=sim1[dim(sim1)[1],-1])

#################################
days <- 10
tiempos <- seq(0, days ,length = days+1)
#modificamos la conectividad
theta[grep('ji',names(theta))]<-0.7*theta[grep('ji',names(theta))]

print('days *************************')
print(days)
t1 <- Sys.time()
sim3<-X_theta(theta, tiempos, X_ini=sim2[dim(sim2)[1],-1])
t2 <- Sys.time()
print("Tiempo ------------------------")
print(t2 - t1)

sim<-rbind(sim1,cbind(sim2[,1]+9,sim2[,-1]),cbind(sim3[,1]+15,sim3[,-1]))


### Graficas
#indices para cada compatimento
S1_i <-2:33
S2_i <-34:65
S3_i <-66:97
E1_i <-98:129
E2_i <-130:161
E3_i <-162:193

I1_1_i <-194:225
I1_2_i <-226:257
I1_3_i <-258:289
I1_4_i <-290:321

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
Y1_i <- 642:673
Y2_i <- 674:705
Y3_i <- 706:737
N1_i <- 738:769
N2_i <- 770:801
N3_i <- 802:833

# Total de cada compartimento
pdf(file =paste0("Total_Compartimento_controlado", days, ".pdf"))

matplot(sim[,1],rowSums(sim[,S1_i]),t="l", col=1, xlab="Dias",ylab="Poblacion", main="Susceptibles, GE 1")
matplot(sim[,1],rowSums(sim[,S2_i]),t="l", col=1, xlab="Dias",ylab="Poblacion", main="Susceptibles, GE 2")
matplot(sim[,1],rowSums(sim[,S3_i]),t="l", col=1, xlab="Dias",ylab="Poblacion", main="Susceptibles, GE 3")
matplot(sim[,1],rowSums(sim[,c(S1_i,S2_i,S3_i)]),t="l", col=1, xlab="Dias",ylab="Poblacion", main="Susceptibles")

matplot(sim[,1],rowSums(sim[,E1_i]),t="l", col=1, xlab="Dias",ylab="Poblacion", main="Expuestos, GE 1")
matplot(sim[,1],rowSums(sim[,E2_i]),t="l", col=1, xlab="Dias",ylab="Poblacion", main="Expuestos, GE 2")
matplot(sim[,1],rowSums(sim[,E3_i]),t="l", col=1, xlab="Dias",ylab="Poblacion", main="Expuestos, GE 3")
matplot(sim[,1],rowSums(sim[,c(E1_i,E2_i,E3_i)]),t="l", col=1, xlab="Dias",ylab="Poblacion", main="Expuestos")
matplot(sim[,1],rowSums(sim[,c(I1_1_i,I2_1_i,I3_1_i)]),t="l", col=3,xlab="Dias",ylab="Poblacion", main="Inf. Asintomaticos")
matplot(sim[,1],rowSums(sim[,c(I1_2_i,I2_2_i,I3_2_i)]),t="l", col=3,xlab="Dias",ylab="Poblacion", main="Inf. Leves")
matplot(sim[,1],rowSums(sim[,c(I1_3_i,I2_3_i,I3_3_i)]),t="l", col=3,xlab="Dias",ylab="Poblacion", main="Inf. Hosp")
matplot(sim[,1],rowSums(sim[,c(I1_4_i,I2_4_i,I3_4_i)]),t="l", col=3,xlab="Dias",ylab="Poblacion", main="Inf. UCI")
matplot(sim[,1],rowSums(sim[,sort(c(I1_1_i,I2_1_i,I3_1_i,I1_2_i,I2_2_i,I3_2_i,I1_3_i,I2_3_i,I3_3_i, I1_4_i, I2_4_i,I3_4_i))]),t="l", col=3,xlab="Dias", ylab="Poblacion", main="Total de Infeccioso")
matplot(sim[,1],rowSums(sim[,D_i]),t="l", xlab="Dias",ylab="Poblacion",
        main="Defunciones")
matplot(sim[,1],rowSums(sim[,R_i]),t="l", xlab="Dias",ylab="Poblacion",
        main="Removidos")
matplot(sim[,1],rowSums(sim[,Y1_i]),t="l", xlab="Dias",ylab="Poblacion",
        main="Acumulado de I_3+I_4s, GE 1")
matplot(sim[,1],rowSums(sim[,Y2_i]),t="l", xlab="Dias",ylab="Poblacion",
        main="Acumulado de I_3+I_4, GE 2")
matplot(sim[,1],rowSums(sim[,Y3_i]),t="l", xlab="Dias",ylab="Poblacion",
        main="Acumulado de I_3+I_4, GE 3")
matplot(sim[,1],rowSums(sim[,c(Y1_i,Y2_i,Y3_i)]),t="l", col=6,xlab="Dias",ylab="Poblacion",
        main="Acumulado de I_3+I_4")
dev.off()



# Compartimento por cada estado

pdf(file = paste0("Compartimento_Estado_controldado", days, ".pdf"))

matplot(sim[,1],sim[,S1_i]+sim[,S2_i]+sim[,S3_i],t="l",xlab="Dias",ylab="Poblacion", main="Susceptibles")
matplot(sim[,1],sim[,E1_i]+sim[,E2_i]+sim[,E3_i],t="l",xlab="Dias",ylab="Poblacion", main="Expuestos")
matplot(sim[,1],sim[,I1_1_i]+sim[,I2_1_i]+sim[,I3_1_i],t="l",xlab="Dias",ylab="Poblacion", main="Infecciosos Asintomaticos")
matplot(sim[,1],sim[,I1_2_i]+sim[,I2_2_i]+sim[,I3_2_i],t="l",xlab="Dias",ylab="Poblacion", main= "Infecciosos Leves")
matplot(sim[,1],sim[,I1_3_i]+sim[,I2_3_i]+sim[,I3_3_i],t="l",xlab="Dias",ylab="Poblacion", main= "Infecciosos Hop")
matplot(sim[,1],sim[,I1_4_i]+sim[,I2_4_i]+sim[,I3_4_i],t="l",xlab="Dias",ylab="Poblacion", main= "Infecciosos UCI")
matplot(sim[,1],sim[,D_i],t="l",xlab="Dias",ylab="Poblacion", main="Defunciones")
matplot(sim[,1],sim[,R_i],t="l",xlab="Dias",ylab="Poblacion", main="Removidos")
matplot(sim[,1],sim[,Y1_i]+sim[,Y2_i]+sim[,Y3_i],t="l",xlab="Dias",ylab="Poblacion", main="Acumulado de I_3+I_4")
dev.off()
