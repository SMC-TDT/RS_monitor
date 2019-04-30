############################                                                                  #
# Programa de lectura de radiosondatge                                                        #
# Va a llegir el fitxer .log i en treu variables que ens interessen.                          #
# Creació FRA 25/03/2019                                                                      # 
# modificació: FRA 29 març 2019
# modificació: FRA 17 abril 2019 s'han posat els errors en un únic fitxer general de sortida
# modificació: FRA 2x abril 2019 s'ha integrat la funció al programa principal. Només un únic fitxer de partida. Més detalls al fitxer "descripció_nou_escript(curt).odt"
############################                                                                  #

# obrir i llegir fitxer .log

any <- substr(Sys.Date(),1,4)
mes <- substr(Sys.Date(),6,7)
dia <- substr(as.character(Sys.Date()),9,10)

file1 <- paste("/mnt/Robotsonde/",any,"/",mes,"/",substr(any,3,4),mes,dia,"00.log",sep="")

  if(!file.exists(file1)){
    file1 <- paste("/mnt/Robotsonde/",any,"/",mes,"/",substr(any,3,4),mes,dia,"12.log",sep="")
      
      if(!file.exists(file1)){
        file1 <- NA
        error <- paste("El fitxer .LOG del dia",any,mes,dia ,"no existeix", sep=" ")
        print(error)
        write.table(error,file="/home/becatdt/Francesc_Roura/RADIOSONDATGE/Renovació/radiosonda_errors_v2.txt", append = TRUE,sep="\t", row.names=F, col.names=F)
        q(save="no", status = 0, runLast = TRUE)
      }
  }

file <- readLines(file1)

# partir en dia/nit
missatge_inici <- "Launch planned"
missatge_fi <- "Radiosounding end"

inici <- grep(missatge_inici,file,value=F)
fi <- grep(missatge_fi,file,value=F)

log11 <- file[inici[1]:fi[1]]
log23 <- file[inici[2]:fi[2]]

for(i in 1:2){
  if(i==1){
    logx <- log11
    sessio <- "11:00"
  }else{
    logx <- log23
    sessio <- "23:00"
  }

#####################################################################  
#####################################################################  
#####################################################################  
  
# declarar i inicialitzar variables
    
v_rell <- numeric(0) #rellançaments
v_s_act <- numeric(0) #sondes activades
v_fall <- numeric(0) #rellançaments fallits
v_vol <- numeric(0) #sondes voladores (han abandonat el mòdul)
v_real <- 0 #radiosondatge realitzat? (1/0)
    
SONDE <- grep("##### SONDE",logx,value=F)
DEPART <- grep("DEPART DETECTE",logx,value=F)
SONDAGE <- grep("SONDAGE OK",logx,value=F)
ERROR <- c(grep("ERROR",logx,value=F),grep("CAUSE",logx,value=F))
    
# sondes activades
v_s_act <- length(SONDE)
    
# sondes voladores  
v_vol <- length(DEPART)
    
# realitzat?  
v_real <- length(SONDAGE)
    
# llançaments fallits  
v_fall <- v_s_act-v_real
    
# rellançaments
v_rell <- v_s_act-1
    
#
variables <- logx[SONDAGE]
str_depart <- logx[DEPART]
str_sonde <- logx[SONDE]
str_error <- logx[ERROR]
    
output <- list(v_rell,v_s_act,v_fall,v_vol,v_real,variables,str_depart,str_sonde,str_error)
names(output) <- c("rellançaments","activades","fallides","voladores","realitzat","variables","str_depart_detecte","str_sonde","errors") 
    
###################################################################################
###################################################################################
###################################################################################
radio<-output
###################################################################################

## preparar variables per fitxer de sortida

#data
  data <- paste(any,mes,as.character(as.numeric(dia)-2),sep="-")

#realitzat?
  realitzat <- radio$realitzat

#hora
  hora <- substr(radio$variables,1,8)
  if(radio[6]=="character(0)"){
    hora <- NA
  }
#durada en hores
  durada <- strptime(substr(radio$variables,1,8),"%H:%M:%OS")-strptime(substr(radio$str_depart_detecte,1,8),"%H:%M:%OS")
  
if(!is.na(durada)){
  if(abs(durada)>25){durada <- durada/60} #distingir entre hores i minuts
  if(as.numeric(durada)<0){
    durada=durada+24
  }
# durada en minuts
  durada <- substr(as.numeric(durada*60),0,5)
}
  
# altitud en metres
  altitud <- substr(radio$variables,36,40)
  if(radio$variables=="character(0)"){
    altitud <- NA
  }

# pressio 
  pressio <- substr(radio$variables, 42, 46)
  if (length(pressio) != 0) {
    if (substring(pressio, 5, 5) == "h") {
      pressio <- as.numeric(gsub("h", "", pressio))
    } else {
      pressio <- as.numeric(pressio)
    }
  }

#fallida
  fallida <- radio$fallides

#identificador sonda
  id <- substr(radio$str_sonde,41,50)

#fitxer
  fitxer <- substr(file1,25,40)
  
#errors
sonde_error <- list()

for(i in 1:length(SONDE)){
  sonde_error[[i]]<-0
}

if(length(ERROR)!=0){
  for(i in 1:length(ERROR)){
  
    for(k in 1:(length(SONDE)-1)){
      if(ERROR[i]>SONDE[k]&ERROR[i]<SONDE[k+1]){
        sonde_error[[k]] <- append(sonde_error[[k]],ERROR[i])
      }
      if(ERROR[length(ERROR)]>SONDE[length(SONDE)]){
        sonde_error[[length(ERROR)]]<-append(sonde_error[[length(ERROR)]],ERROR[length(ERROR)])
      }
    }
  }
}

#vàlvula fallida
valv_fall <- paste("B",substr(radio$str_sonde,37,38),sep="")

#fitxer de sortida
  final <- list(data,fitxer,sessio,id,realitzat,hora,durada,altitud,as.numeric(pressio),radio$rellançaments,fallida,radio$activades,radio$voladores,NA,NA)
  names(final) <- c("data","fitxer","sessió","id_sond","realit","hora","dur(min)","alti(m)","press(hPa)","rellanç","llanç.fall","activ","sond_voladores","errors","valv.fall")
  final <- as.data.frame(final)

#si tenim més d'una sonda activada, senyal que hi ha hagut algun error. Els introduïm al fitxer de sortida  
  if(radio$activades>1){
    for(i in 1:(radio$activades-1)){
      final[i,] <- list(data,fitxer,sessio,id[i],0,NA,NA,NA,NA,NA,fallida,radio$activades,radio$voladores,paste(substr(logx[as.vector(sonde_error[[i]][2:length(sonde_error[[i]])])],14,80),collapse=" , "),valv_fall[i])
    }
  }

#format data.frame
  final <- as.data.frame(final)
  
#Escriure fitxer de sortida
  write.table(final,file="/home/becatdt/Francesc_Roura/RADIOSONDATGE/Renovació/radiosonda_v2.txt", append = TRUE,sep="\t", row.names=F, col.names=F)
}