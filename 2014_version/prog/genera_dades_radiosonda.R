
## genera_dades_radiosonda.R ######################################################################

## Created : Carles Cayuela 2014
## Last modified: Francesc Roura 04-12-2018 

## Descripció:
## Consulta el LOG de l'últim radiosondatge i genera dos fitxers TXT amb les dades 
## i els errors de cada LOG.

options(warn=-1)

## SETTINGS #######################################################################################

path_log <- "../../log_data"
path_out <- "../output"

fname_out <- "radiosonda.txt"
fname_err <- "radiosonda_errors.txt"

###################################################################################################

# LOAD TABLE 
file_out <- paste(path_out, fname_out, sep="/")
file_err <- paste(path_out, fname_err, sep="/")

radios <- read.table(file=file_out, sep="\t", header=T)

# Ultim log
any <- format(Sys.Date(), "%Y")
mes <- format(Sys.Date(), "%m")
dia <- format(Sys.Date(), "%d")

lsdir <- list.dirs(path_log,recursive=F)
path_log <- paste(path_log, any, mes, sep="/")
dirs <- paste(path_log, list.files(path_log, pattern="*00.log"), sep="/")
file <- max(dirs)

error <- 0
if (file!=paste("/mnt/Robotsonde",any,mes,paste(substr(any,3,4),mes,dia,"00.log",sep=""),sep="/")){
 if (file.exists(paste("/mnt/Robotsonde",substr(as.Date(strptime((substr(file,25,30)), "%y%m%d")),1,4),
                       substr(as.Date(strptime((substr(file,25,30)), "%y%m%d")),6,7),
                       paste(substr(as.Date(strptime((substr(file,25,30)), "%y%m%d")),3,4),
                             substr(as.Date(strptime((substr(file,25,30)), "%y%m%d")),6,7),
                             substr(as.Date(strptime((substr(file,25,30)), "%y%m%d")),9,10),"12.log",sep=""),sep="/"))){
   file <- paste("/mnt/Robotsonde",substr(as.Date(strptime((substr(file,25,30)), "%y%m%d")),1,4),
                 substr(as.Date(strptime((substr(file,25,30)), "%y%m%d")),6,7),
                 paste(substr(as.Date(strptime((substr(file,25,30)), "%y%m%d")),3,4),
                       substr(as.Date(strptime((substr(file,25,30)), "%y%m%d")),6,7),
                       substr(as.Date(strptime((substr(file,25,30)), "%y%m%d")),9,10),"12.log",sep=""),sep="/")
   error <- 1
 }
}

## DATA 

data <- as.Date(strptime((substr(file,25,30)), "%y%m%d"))-2
if (error == 1){
 data <- as.Date(strptime((substr(file,25,30)), "%y%m%d"))-1
}

# Existeix o no?
existeix <- data%in%radios$Data
if (existeix == F) {
 file <- file
} else stop ("Aquest fitxer ja existeix a la base de dades")

## LOG 

log <- readLines(file)
require(chron)
log_hores <- chron(times=substring(log,1,8))

log_mati <- log[which(log_hores >= "07:00:00" & log_hores <= "16:00:00")]
log_nit <- log[which(log_hores >= "19:00:00" | log_hores <= "04:00:00")]

## ESTAT DEL RADIOSONDATGE: Si (realitzat) / No (no realitzat)

# Mati
estat_mati <- NULL
estat_mati <- grep("SONDAGE OK", log_mati, value=F)
if (length(estat_mati != 0)) {
 estat_mati <- "Si"
} else {
 estat_mati <- "No"
}

# Nit
estat_nit <- NULL
estat_nit <- grep("SONDAGE OK", log_nit, value=F)
if (length(estat_nit != 0)) {
 estat_nit <- "Si"
} else {
 estat_nit <- "No"
}

## FINALITZACIO

# Mati
hora_mati <- NULL
hora_mati <- substr(grep("SONDAGE OK", log_mati, value=T), 1, 8)
if (length(hora_mati != 0)) {
 hora_mati <- hora_mati
} else {
 hora_mati <- NA
}

# Nit
hora_nit <- NULL
hora_nit <- substr(grep("SONDAGE OK", log_nit, value=T), 1, 8)
if (length(hora_nit != 0)) {
 hora_nit <- hora_nit
} else {
 hora_nit <- NA
}

## ALTITUT

# Mati
altitud_mati <- NULL
altitud_mati <- substr(grep("SONDAGE OK", log_mati, value=T), 36,40)
if (length(altitud_mati != 0)) {
 if (substring(altitud_mati, 5, 5) == "m") {
   altitud_mati <- as.numeric(gsub("m", "", altitud_mati))
 } else {
   altitud_mati <- as.numeric(altitud_mati)}
} else {
 altitud_mati <- NA
}

# Nit
altitud_nit <- NULL
altitud_nit <- substr(grep("SONDAGE OK", log_nit, value=T), 36,40)
if (length(altitud_nit != 0)) {
 if (substring(altitud_nit, 5, 5) == "m") {
   altitud_nit <- as.numeric(gsub("m", "", altitud_nit))
 } else {
   altitud_nit <- as.numeric(altitud_nit)}
} else {
 altitud_nit <- NA
}

as.numeric(altitud_mati)

## PRESSIO

# Mati
pressio_mati <- NULL
pressio_mati <- substr(grep("SONDAGE OK", log_mati, value=T), 42, 47)
if (length(pressio_mati != 0)) {
 if (substring(pressio_mati, 6, 6) == "h") {
   pressio_mati <- as.numeric(gsub("h", "", pressio_mati))
 } else {
   pressio_mati <- as.numeric(pressio_mati)}
} else {
 pressio_mati <- NA
}

# Nit
pressio_nit <- NULL
pressio_nit <- substr(grep("SONDAGE OK", log_nit, value=T), 42, 47)
if (length(pressio_nit != 0)) {
 if (substring(pressio_nit, 6, 6) == "h") {
   pressio_nit <- as.numeric(gsub("h", "", pressio_nit))
 } else {
   pressio_nit <- as.numeric(pressio_nit)}
} else {
 pressio_nit <- NA
}

## DURADA

# Mati
durada_mati <- NULL
durada_mati <- (chron(times=substr(grep("SONDAGE", log_mati, value=T), 1, 8)))-(chron(times=substr(grep("DEPART DETECTE", log_mati, value=T), 1, 8)))
durada_mati <- durada_mati[length(durada_mati)]
durada_mati <- hours(durada_mati)*60 + minutes(durada_mati)
if (length(durada_mati != 0) & estat_mati == "Si") {
 durada_mati <- durada_mati
} else {
 durada_mati <- NA
}
# Nit
durada_nit <- NULL
durada_nit <- (chron(times=substr(grep("SONDAGE", log_nit, value=T), 1, 8)))-(chron(times=substr(grep("DEPART DETECTE", log_nit, value=T), 1, 8)))
durada_nit <- durada_nit[length(durada_nit)]
durada_nit <- hours(durada_nit)*60 + minutes(durada_nit)
if (length(durada_nit != 0) & estat_nit == "Si") {
 durada_nit <- durada_nit
} else {
 durada_nit <- NA
}

## NOMBRE DE RELLANÇAMENTS

# ATENCIO!!!!!
# A partir del 05/01/2016 (a partir del log 16010700.log) es va canviar l'idioma d'entrada del
# frances a l'angles, canviant el terme "Gonflage" per "Inflation". Per a fer-ho servir per a
# dates anterior a aquest, canviar "SONDE ON" per "Gonflage" en els dos seguents greps.

# Mati
sondes_mati <- NULL
# sondes_mati <- grep("SONDE ON", log_mati) # vàlid fins a 21/11/2018, canvi de missatge sonde on -> ####...SONDE
sondes_mati <- grep("############## SONDE", log_mati)
if (length(sondes_mati != 0)) {
 sondes_mati <- length(sondes_mati)-1
} else {
 sondes_mati <- NA
}

# Nit
sondes_nit <- NULL
# sondes_nit <- grep("SONDE ON", log_nit) # vàlid fins a 21/11/2018, canvi de missatge sonde on -> ####...SONDE
sondes_nit <- grep("############## SONDE", log_nit)
if (length(sondes_nit != 0)) {
 sondes_nit <- length(sondes_nit)-1
} else {
 sondes_nit <- NA
}

## SONDES FALLIDES

# ATENCIO!!!!!
# A partir del 05/01/2016 (a partir del log 16010700.log) es va canviar l'idioma d'entrada del
# frances a l'angles, canviant el terme "Gonflage" per "Inflation". Per a fer-ho servir per a
# dates anterior a aquest, canviar "SONDE ON" per "Gonflage" en els dos seguents greps.
# A mes a mes, es canviaran els valors dels dos greps següents de "23, 25" a "28, 30".

# Mati
fallida_mati<-NULL
fallida_mati <- substr(grep("SONDE ON", log_mati, value=T), 28, 30)
if (length(fallida_mati) != 0) {
 for (j in 1:length(fallida_mati)) {
   if (substring(fallida_mati[j], 3, 3) == ",") {
     fallida_mati[j] <- gsub(",", "", fallida_mati[j])
   } else {
     fallida_mati[j] <- fallida_mati[j]
   }
 }
 for (num_llanc in 1:length(fallida_mati)){
   if (substring(fallida_mati[num_llanc],3,3)==" "){
     fallida_mati[num_llanc]<-substring(fallida_mati[num_llanc],1,2)
   }
 }
} else {
 fallida_mati <- NA
}

if (estat_mati == "Si" & sondes_mati == 0) {
 fallida_mati <- "Correcte"
} else if (estat_mati == "Si" & sondes_mati >= 1) {
 length(fallida_mati)<-length(fallida_mati)-1
 fallida_mati<-paste(fallida_mati,collapse = " ")
} else {
 fallida_mati<-paste(fallida_mati,collapse = " ")
}

# Nit
fallida_nit<-NULL
fallida_nit <- substr(grep("SONDE ON", log_nit, value=T), 28, 30)
if (length(fallida_nit) != 0) {
 for (j in 1:length(fallida_nit)) {
   if (substring(fallida_nit[j], 3, 3) == ",") {
     fallida_nit[j] <- gsub(",", "", fallida_nit[j])
   } else {
     fallida_nit[j] <- fallida_nit[j]
   }
 }
 for (num_llanc in 1:length(fallida_nit)){
   if (substring(fallida_nit[num_llanc],3,3)==" "){
     fallida_nit[num_llanc]<-substring(fallida_nit[num_llanc],1,2)
   }
 }
} else {
 fallida_nit <- NA
}

if (estat_nit == "Si" & sondes_nit == 0) {
 fallida_nit <- "Correcte"
} else if (estat_nit == "Si" & sondes_nit >= 1){
 length(fallida_nit)<-length(fallida_nit)-1
 fallida_nit<-paste(fallida_nit,collapse = " ")
} else {
 fallida_nit<-paste(fallida_nit,collapse = " ")
}

## NOMBRE DE SONDES

# ATENCIO!!!!!
# A partir del 11/11/2015 (a partir del log 15111300.log) es va modificar la linia que
# indicava quantes sondes s'havien fet servir, que començava per "Rotation". A partir de
# llavors, aquesta línia va ser eliminada, per lo que s'ha fet un canvi al script per
# detectar el terme "SONDE ON".

# Mati
utilit_mati<-NULL
# utilit_mati <- grep("Rotation", log_mati) # Comentari per més nous a 15111300.log
# utilit_mati <- grep("SONDE ON", log_mati) # Comentari per més antics a 15111300.log # vàlid fins a 21/11/2018
utilit_mati <- grep("############## SONDE", log_mati)
if (length(utilit_mati) != 0) {
 utilit_mati <- length(utilit_mati)
} else {
 utilit_mati <- NA
}

# Nit
utilit_nit<-NULL
# utilit_nit <- grep("Rotation", log_nit) # Comentari per més nous a 15111300.log
# utilit_nit <- grep("SONDE ON", log_nit) # Comentari per més antics a 15111300.log # vàlid fins a 21/11/2018
utilit_nit <- grep("############## SONDE", log_nit)
if (length(utilit_nit) != 0) {
 utilit_nit <- length(utilit_nit)
} else {
 utilit_nit <- NA
}

## ERRORS

errors<-NULL
errors_2<-NULL
errors_3<-NULL
errors <- substring(grep("CAUSE", log, value=T), 0, 100)
errors_2 <- substring(grep("No start detection by IR2010",log,value=T),0,100)
errors_3 <- substring(grep("Valve ejected, sonde switched",log,value=T),0,100)
errors_4 <- substring(grep("Balloon disappearance, ejection",log,value=T),0,100)
errors_5 <- substring(grep("Balloon blocked",log,value=T),0,100)
errors_6 <- substring(grep("No gas =",log,value=T),0,100)
errors <- c(errors,errors_2,errors_3,errors_4,errors_5,errors_6)
if (length(errors != 0)) {
 hora <- substr(errors, 0, 8)
 errors_cause <- errors
 for (num_error in 1:length(errors_cause)){
   if (substring(errors_cause[num_error], 54,71)=="SONDE NOT RECEIVED"){
     errors_cause[num_error] <- substring(errors_cause[num_error], 54,71)
   } else{
     if (substring(errors_cause[num_error], 51,68)=="Humidity sensor KO"){
       errors_cause[num_error] <- substring(errors_cause[num_error], 51,68)
     } else{
       if (substring(errors_cause[num_error],14,41)=="No start detection by IR2010"){
         errors_cause[num_error] <- substring(errors_cause[num_error], 14,41)
       } else {
         if (substring(errors_cause[num_error],54,59)=="No GPS"){
           errors_cause[num_error] <- substring(errors_cause[num_error], 54,59)
         } else {
           if (substring(errors_cause[num_error],14,42)=="Valve ejected, sonde switched"){
             errors_cause[num_error] <- substring(errors_cause[num_error], 14,42)
           } else {
             if (substring(errors_cause[num_error],51,63)=="Sonde unknown"){
               errors_cause[num_error] <- substring(errors_cause[num_error], 51,63)
             } else {
               if (substring(errors_cause[num_error],51,72)=="Batteries too low 4.6V"){
                 errors_cause[num_error] <- substring(errors_cause[num_error], 51,72)
               } else {
                 if (substring(errors_cause[num_error],14,44)=="Balloon disappearance, ejection"){
                   errors_cause[num_error] <- substring(errors_cause[num_error], 14,44)
                 } else {
                   if (substring(errors_cause[num_error],14,28)=="Balloon blocked"){
                     errors_cause[num_error] <- substring(errors_cause[num_error], 14,28)
                   } else {
                     if (substring(errors_cause[num_error],47,52)=="No gas"){
                       errors_cause[num_error] <- substring(errors_cause[num_error], 47,52)
                     } else {
                       errors_cause[num_error] <- substr(errors_cause[num_error], 43, 100)
                     }
                   }
                 }
               }
             }
           }
         }
       }
     }
   }
 }
 df_errors <- data.frame(row.names=NULL, Data=data, Hora=hora, Error=errors_cause)
} else {
 df_errors <- NULL
 errors_cause <- NULL
 num_error<-0
}

if (num_error>1){
 for (num_error in 1:(length(errors_cause)-1)){
   if (num_error<=length(errors_cause)){
     if ((errors_cause[num_error]=="No GPS")|(errors_cause[num_error]=="SONDE NOT RECEIVED")){
       if (errors_cause[num_error]==errors_cause[num_error+1]){
         codi_sonda_1<-substring(errors[num_error],37,45)
         codi_sonda_2<-substring(errors[num_error+1],37,45)
         if (codi_sonda_1==codi_sonda_2){
           for (num_error_2 in num_error:length(errors_cause)){
             errors_cause[num_error_2]<-errors_cause[num_error_2+1]
             errors[num_error_2]<-errors[num_error_2+1]
           }
           length(errors_cause)<-length(errors_cause)-1
           length(errors)<-length(errors)-1
           num_error<-num_error
         }
       }
     }
   }
 }
 hora <- substr(errors, 0, 8)
 df_errors <- data.frame(row.names=NULL, Data=data, Hora=hora, Error=errors_cause)
} else {
 if (num_error==1){
   if ((errors_cause[num_error]=="No GPS")|(errors_cause[num_error]=="SONDE NOT RECEIVED")){
     errors_cause<-NULL
     df_errors <- NULL
   }
 }
}

# Recomptatge de sondes "utilitzades" segons els errors

if (length(errors_cause)!=0){
 for (num_error in 1:length(errors_cause)){
   if ((errors_cause[num_error]=="Humidity sensor KO")|
       (errors_cause[num_error]=="SONDE NOT RECEIVED")|
       (errors_cause[num_error]=="No GPS")|
       (errors_cause[num_error]=="Valve ejected, sonde switched")|
       (errors_cause[num_error]=="Sonde unknown")|
       (errors_cause[num_error]=="Batteries too low 4.6V")){
     if ((substring(hora[num_error],1,2)=="10")|(substring(hora[num_error],1,2)=="11")|
         (substring(hora[num_error],1,2)=="12")|(substring(hora[num_error],1,2)=="13")|
         (substring(hora[num_error],1,2)=="14")|(substring(hora[num_error],1,2)=="15")){
       utilit_mati<-utilit_mati-1
     }
     if ((substring(hora[num_error],1,2)=="22")|(substring(hora[num_error],1,2)=="23")|
         (substring(hora[num_error],1,2)=="00")|(substring(hora[num_error],1,2)=="01")|
         (substring(hora[num_error],1,2)=="02")|(substring(hora[num_error],1,2)=="03")){
       utilit_nit<-utilit_nit-1
     }
   }
 }
}

if ((utilit_mati==0)&(estat_mati=="Si")){
 utilit_mati <- 1
}
if ((utilit_nit==0)&(estat_nit=="Si")){
 utilit_nit <- 1
}

# Base de dades
df <- data.frame(row.names=NULL, Data=data, Sessio=c("mati", "nit"), Realitzat=c(estat_mati, estat_nit),
                Hora=c(hora_mati, hora_nit), Durada=c(durada_mati, durada_nit),
                Altitud=c(altitud_mati, altitud_nit), Pressio=c(pressio_mati, pressio_nit),
                Rellancaments=c(sondes_mati, sondes_nit), Fallida=c(fallida_mati, fallida_nit),
                Sondes=c(utilit_mati, utilit_nit), stringsAsFactors=TRUE)

write.table(df, file=file_out, append=T, sep="\t", row.names=F, col.names=F)
write.table(df_errors, file=file_err, append=T, sep="\t", row.names=F, col.names=F)
