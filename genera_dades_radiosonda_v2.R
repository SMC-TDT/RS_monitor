## genera_dades_radiosonda.R ######################################################################

## Created : Francesc Roura 25/03/2019
## Last modified (28/05/2019): 

## Passem a extreure només les 9 variables del log que més endavant ens serviran per trobar-ne la resta. 
## Log separat en sondes. Tenim una sortida per a cada sonda activada.

## Description : Programa de lectura de radiosondatge. Va a llegir el fitxer .log i en treu 
## variables que ens interessen.                          

## LIBRARIES ######################################################################################

library(stringi)

## FUNCTIONS ######################################################################################

extract_pattern <- function(str, patt){
  # -----------------------------------------------------------------------------------------------
  # Extracts a pattern from a string 
  #
  # PARAMETERS
  # **********
  # str : str
  #   String
  # patt : reg expr
  #   Pattern string, regular expression
  #
  # RETURNS
  # *******
  # extr : str
  #   Substring that matches the pattern
  # -----------------------------------------------------------------------------------------------
  
  extr <- regmatches(str, regexpr(patt, str))
  return(extr)
}


extract_data <- function(log_x,missatges){
  # -----------------------------------------------------------------------------------------------
  #
  # PARAMETERS
  # **********
  # log_x : str
  #   Part of the sondage log
  # missatges : list
  #   List with key messages 
  #
  # RETURNS
  # *******
  # sondage_data : list(session, position_into_the_carousel, ID_sonde, start_time, end_time, altitude, pressure, errors)
  #   List containing extracted data
  # -----------------------------------------------------------------------------------------------
  
  patt1_1 <- paste0("\\d*\\m\\s((\\d*)|(\\d*\\.\\d*))\\hPa")
  match <- extract_pattern(log_x, patt1_1)
  sondage_data[["altitude"]] <- substr(match,1,5)
  
  patt1_2 <- paste0("\\d*\\m\\s((\\d*)|(\\d*\\.\\d*))\\hPa")
  match <- extract_pattern(log_x, patt1_2)
  sondage_data[["pressure"]] <- as.numeric(substr(match,7,10))
  
  # log_sonde[[1]]
  
  patt2 <- paste0("\\s\\d{2}:\\d{2}\\s")
  match <- extract_pattern(log_x, patt2)
  sondage_data[["session"]] <- substr(match,2,6)
  
  patt3 <- paste0("\\d{2}:\\d{2}:\\d{2}\\s\\d{3}\\s\\IR2010\\s\\-->\\s\\", missatges$sortida) 
  match <- extract_pattern(log_x, patt3)
  sondage_data[["start_time"]] <- substr(match,1,8)
  
  patt4 <- paste0("\\d{2}:\\d{2}:\\d{2}\\s\\d{3}\\s\\IR2010\\s\\-->\\s\\", missatges$realitzacio)
  match <- extract_pattern(log_x, patt4)
  sondage_data[["end_time"]] <- substr(match,1,8)
  
  patt5 <- paste0("\\s\\d{9}\\s\\s\\d*")
  match <- extract_pattern(log_x, patt5)
  sondage_data[["ID_sonde"]] <- substr(match,2,10)
  
  patt6 <- paste0("\\SONDE\\s\\[\\s\\d*\\s\\]\\s\\d*")
  match <- extract_pattern(log_x, patt6)
  sondage_data[["position_carousel"]] <- as.numeric(substr(match,8,10))
  
  patt7_1 <- grep(missatges$errors,log_x,value=T)
  patt7_2 <- grep(missatges$causa,log_x, value=T)
  
  sondage_data[["errors"]] <- substr(patt7_1,14,80)
  sondage_data[["cause"]] <- substr(patt7_2,14,80)
  
  return(sondage_data)
}


split_log <- function(logi,missatges){
  # -----------------------------------------------------------------------------------------------
  #
  # PARAMETERS
  # **********
  # logi : str
  #   Part of the sondage log
  # missatges : str
  #   List with key messages
  #
  # RETURNS
  # *******
  # log_sonde : str
  #   List with ordered sonde messages
  # -----------------------------------------------------------------------------------------------
  
  sondei <- c(grep(missatges$activacio, logi, value=F))
  
  # Abans d'activar-se la primera sonda
  log_sonde[[1]] <- logi[1:(sondei[1]-1)] 
  
  # Si tenim dues sondes o més
  if(sondei[1]!=sondei[length(sondei)]){ 
    for(j in 2:length(sondei)){
      log_sonde[[j]] <- logi[sondei[j-1]:(sondei[j]-1)]
    }
  }
  
  # Sonda final
  log_sonde[[length(sondei)+1]] <- logi[sondei[length(sondei)]:length(logi)]
  
  return(log_sonde)
}

## SETTINGS #######################################################################################

date_rs <- "2019-05-06" # RS launch date
path_in <- "/mnt/Robotsonde/2019/05"
path_ou <- "/home/becatdt/Francesc_Roura/RADIOSONDATGE/Renovació/"

## DEFAULT METADATA ###############################################################################

# lag: delay in days between RS launch date and log filename date
launches <- list("1"=list(time="00", lag=2), "2"=list(time="12", lag=1)) 

# Llista de missatges importants en format llista
missatges <- list(activacio="##### SONDE", 
                  sortida="DEPART DETECTE", 
                  realitzacio="SONDAGE OK", 
                  errors="ERROR", 
                  causa="CAUSE",
                  inici="Launch planned", 
                  final="Radiosounding end")

###################################################################################################

# Initialize list

log_sonde <- list()
sondage_data <- list()

# Choose file

if (is.null(date_rs)){
  date_rs <- format(Sys.time()-2*86400, "%Y-%m-%d")
}
date_rs <- as.Date(date_rs)

# Fitxers log d'entrada
files_in <- sapply(launches, function(x) 
{paste(path_in, paste0(format(date_rs+x$lag, "%y%m%d"), x$time, ".log"), sep="/")})

# Seleccionar un dels fitxers que es trobin
indx_f <- which(file.exists(files_in))[1]
if(is.na(indx_f)){
  stop(paste("No log files found for the selected date:", date_rs))
}

file_in <- files_in[[as.character(indx_f)]]
# date_f <- date_rs+launches[[as.character(indx_f)]]$lag

# Load log info
conn <- file(file_in, "r")
info <- readLines(conn, encoding = 'UTF-8')
close(conn)

# Identificar inici/final radiosondatge
start_end <- list("inici"=grep(missatges$inici, info, value=F),"final"=grep(missatges$final, info, value=F))

for(i in 1:length(start_end$inici)){
  
  # Separar sessions(11:00 / 23:00)
  logi <- info[start_end$inici[i]:start_end$final[i]]
  
  # Separar
  log_sonde <- split_log(logi,missatges)



  for(m in 2:length(log_sonde)){ # el primer element no correspon a cap sonda

    # Extraiem les variables (sessió[11:00/23:00], posició carrussel[BXX], ID sonda[9 dígits], hora inici[hh:mm:ss], hora final[hh:mm:ss], altitud[m], pressió[hPa])
  
    out <- extract_data(log_sonde[[m]],missatges)

    if(length(out$errors)>0){
      for( k in 1:length(out$errors)){
        out$errors[k] <- trimws(out$errors[k]) # delete spaces
      }
    }
  
    if(length(out$cause)>0){
      for( k in 1:length(out$cause)){
        out$cause[k] <- trimws(out$cause[k]) # delete spaces
      }
    }
  
    if(length(out$errors) > 1){ # merge "error" messages in an unique string
      out$errors <- stri_paste(out$errors[1:length(out$errors)],collapse=" || ")
    }
  
  
  
    if(length(out$cause) > 1){ # ídem amb "cause"
      out$cause <- stri_paste(out$cause[1:length(out$cause)],collapse=" || ")
    }
  
    # print(out)
    for(n in names(out)){
       if(length(out[[n]]) == 0){
         out[[n]] <- NA
      }
    }

    # session
    if(is.na(out$session)){
      out$session <- extract_data(log_sonde[[1]], missatges)$session
    }

    # print(out$session)
    
    df <- as.data.frame(out)
    write.table(df, paste(path_ou,"out_prova.txt",sep="") ,sep="\t", append = T, col.names = F, row.names = F)
  }                              
}


