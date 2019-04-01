############################                                                                  #
# Programa de radiosondatge                                                                   #
# Va a llegir el fitxer .log i en treu variables que ens interessen.                          #
# FRA 25/03/2019                                                                              # 
# modificació: 29 març 2019                                                                   #
#
#
#
#
############################                                                                  #


# obrir i llegir fitxer .log

# file <- "/home/becatdt/Francesc_Roura/RADIOSONDATGE/Renovació/log/19030312.log" #.log per provar amb errors

# data
# any <- "2018"
# mes <- "12"
# dia <- "23"

# això s'ha de descomentar quan es posa en proves
any <- substr(Sys.Date(),1,4)
mes <- substr(Sys.Date(),6,7)
dia <- substr(as.character(Sys.Date()-1),9,10)

file <- readLines(paste("/mnt/Robotsonde/",any,"/",mes,"/",substr(any,3,4),mes,dia,"12.log",sep=""))

# partir en dia/nit
missatge_inici <- "Launch planned"
missatge_fi <- "Radiosounding end"

inici <- grep(missatge_inici,file,value=F)
fi <- grep(missatge_fi,file,value=F)

log_mati <- file[inici[1]:fi[1]]
log_nit <- file[inici[2]:fi[2]]

if(!exists("fn_radiosondatge", mode="function")){
   source("/home/becatdt/Francesc_Roura/RADIOSONDATGE/RScripts/fn_radiosondatge.R")
}

for(i in 1:2){
  if(i==1){
    logx <- log_mati
    sessio <- "mati"
  }else{
    logx <- log_nit
    sessio <- "nit"
  }
  
radio <- fn_radiosondatge(logx,v_rell,v_s_act,v_fall,v_vol,v_real)
  
# preparar variables per fitxer de sortida

#data
  data <- paste(any,mes,dia,sep="-")

#realitzat?
  if(radio$realitzat==1){
    realitzat <- "Si"
  }else{
    realitzat <- "No"
  }  

#hora
  hora <- substr(radio$variables,1,8)
  if(radio[6]=="character(0)"){
    hora <- NA
  }
#durada en hores
  durada <- strptime(substr(radio$variables,1,8),"%H:%M:%OS")-strptime(substr(radio$str_depart_detecte,1,8),"%H:%M:%OS")
  
if(!is.na(durada)){
  if(as.numeric(durada)<0){
    durada=durada+24
  }
# durada en minuts
  durada <- as.numeric(durada*60)
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
  if(radio$fallides==0){
    fallida <- "Correcte"
  }else{
    fallida <- c()
    for(j in 1:length(radio$str_sonde[[1]])){
      fallida[j] <- substr(radio$str_sonde[[1]][j],37,38)
    }
    fallida <- paste("B",fallida,sep="")
  }

# final <- c(data,sessio,realitzat,hora,durada,altitud,pressio,rellançaments,fallida,sondes_act)

  final <- list(data,sessio,realitzat,hora,durada,altitud,pressio,radio$rellançaments,fallida,radio$activades)
  names(final) <- c("data","sessió","hora","durada","altitud","pressió","rellançaments","fallida","activades")
  final <- as.data.frame(final)
  write.table(final,file="/home/becatdt/Francesc_Roura/RADIOSONDATGE/Renovació/radiosonda_v2.txt", append = TRUE,sep="\t", row.names=F, col.names=F)

# errors::
 
# if(exists("fn_errors_radiosondatge", mode="function")==F){
#     source("/home/becatdt/Francesc_Roura/RADIOSONDATGE/RScripts/fn__erros_radiosondatge.R")
# }

error <- grep("ERROR",logx,value=F)
cause <- grep("cause",logx,value=F)
  
  if(length(error)!=0){
    str_error <- substr(logx[error],14,50)
    str_hora_error <- substr(logx[error],1,8)
    errors <- data.frame(data,str_hora_error,str_error) 
    write.table(errors,file="/home/becatdt/Francesc_Roura/RADIOSONDATGE/Renovació/radiosonda_errors_v2.txt", append = TRUE,sep="\t", row.names=F, col.names=F)
  }
  
  if(length(cause)!=0){
    str_cause <- logx[cause]
    str_hora_cause <- substr(logx[cause],1,8)
    causes <- data.frame(data,str_hora_cause,str_cause)
    write.table(causes,file="/home/becatdt/Francesc_Roura/RADIOSONDATGE/Renovació/radiosonda_errors_v2.txt", append = TRUE,sep="\t", row.names=F, col.names=F)
  }
  
}

