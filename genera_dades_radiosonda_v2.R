## genera_dades_radiosonda.R ######################################################################

## Created : Francesc Roura 25/03/2019
## Last modified (13/06/2019): 

## Queden comerts més casos particulars, com per exemple un llançament manual ("Launch triggered")

## Description : Programa de lectura de radiosondatge. Va a llegir el fitxer .log i en treu 
## variables que ens interessen.                          

## LIBRARIES ######################################################################################

library(stringi)
library(stringr)

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
  match <- regexpr(patt, str)
  extr <-  NA
  
  if (match[1]!=-1){
    extr <- regmatches(str, regexpr(patt, str)) 
  }
  return(extr)
}

extract_time <- function(str){
  
  time <- extract_pattern(str, patt="\\d{2}:\\d{2}:\\d{2}")
  return(time)
  
}

find_rs_chunks <- function(log, msg_st, msg_st2, msg_end){
  
  
  chunks <- NA
  st <- grep(msg_st, log, value=F)
  if (length(st)!=0){
    end <- grep(msg_end, log, value=F)
    end <- end[end>min(st)]
    
    st_end <- sort(c(st, end))
    chunks <- split(st_end, ceiling(seq_along(st_end)/2))
  
  trig <- grep(msg_st2,log)
  end_trig <- 0
  j <- 1
  
  if(length(trig)!=0){
    while(end_trig==0){
      if(length(grep(msg_end,log[j]))==0){
        j <- j+1
      }else{
        end_trig <- j
      }
    }
    chunks[[as.character(length(chunks)+1)]] <- c(trig,end_trig)  
  }  
    
  }
  return(chunks)
}

sonde_data <- function(log, msg, grep_lst){
  # -----------------------------------------------------------------------------------------------
  # Extracts altitude, pressure and speed data from completed sondage string
  #
  # PARAMETERS
  # **********
  # sonde_str : str
  #   Completed sondage string from log file
  #
  # RETURNS
  # *******
  # sondage_data : list(altitude, pressure, speed)
  #   List containing extracted data
  # -----------------------------------------------------------------------------------------------
  
  data_s <- as.list(sapply(names(grep_lst), function(x) NA))
  str <- log[grep(msg, log)[1]]
  
  if (!is.na(str)){
    for (u in names(grep_lst)){
      
      match <- extract_pattern(str, grep_lst[[u]])
      data_s[[u]] <- as.numeric(gsub("\\D", "", match))
      
    }
  }
  
  return(data_s)
}

sonde_times <- function(log, msg_lst){
  
  times <- as.list(sapply(names(msg_lst), function(x) NA))
  
  for (pn in names(msg_lst)){
    
    log_str <- log[grep(msg_lst[[pn]], log)[1]]
    
    if (!is.na(log_str)){
      times[[pn]] <- extract_time(log_str) 
    }
    
  }
  
  return(times)
}

flying_sond <- function(log, msg){
  
  vola <- length(grep(msg, log, value = FALSE))
  
  return(vola)
}

extract_error <- function(log, msg_err){

  error <- c(NULL)
  for(n_err in names(msg_err)){
    extr <- unique(regmatches(log, regexpr(msg_err[[n_err]], log)))
    if(length(extr)>0){
      error <- append(error,n_err)
    }
  } 
  
  if(is.null(error)){
    error <- "0"
  }else{
    error <- stri_paste(error[1:length(error)],collapse=" ") 
  }
  error_out <- error
  
  return(error_out)
}

circular_diff <- function(tf, ti, mod=24){
  # -----------------------------------------------------------------------------------------------
  # Calculate minimum difference between two circular quantities
  #
  # PARAMETERS
  # **********
  # tf : float
  #   "Final" angle (deg)
  # ti : float
  #   "Initial" angle (deg)
  #
  # OTHER PARAMETERS
  # ****************
  # mod : int
  #   Module, 24 hours by default
  #
  # RETURNS
  # *******
  # diff : float
  #   Minimum difference tf-ti
  # -----------------------------------------------------------------------------------------------
  
  diff_a <- (tf-ti)%%mod
  diff_b <- (ti-tf)%%mod
  
  diff_a[diff_b<diff_a] <- -diff_b[diff_b<diff_a]
  
  return(diff_a)
}

time2dec <- function(timestr){
  # -----------------------------------------------------------------------------------------------
  # Converts time string (%H:%M:%S) into decimal time
  #
  # PARAMETERS
  # **********
  # timestr : str
  #   Time string (%H:%M:%S)
  #
  # RETURNS
  # *******
  # t_dec : float
  #   Date split into components: 'year_l' (%Y), 'year_s' (%y), 'month' (%m), 'day' (%d)
  # -----------------------------------------------------------------------------------------------
  
  t_dec <- NA
  if ((!is.na(timestr))&(!is.null(timestr))){
    
    tt <- lapply(strsplit(timestr, split = ":"), as.integer)[[1]]
    if (length(tt)==2){
      tt <- c(tt, 0)
    }
    
    tt <- as.list(tt)
    names(tt) <- c("H", "M", "S")
    
    tt$M <- tt$M/60
    tt$S <- tt$S/3600
    
    t_dec <- with(tt, H+M+S)
    
  }
  return(t_dec)
}

try_date <- function(date_str){
  # -----------------------------------------------------------------------------------------------
  # Finds out the existence of a given date
  #
  # PARAMETERS
  # **********
  # timestr : date_str
  #   Date string (%Y-%M-%D)
  #
  # RETURNS
  # *******
  # d : try-error
  #   If the given does not exist, the program is stopped.  
  # -----------------------------------------------------------------------------------------------
  d <- try(as.Date(date_str), silent = TRUE)
  if (class(d)=="try-error"){
    stop(paste("The following date does not exist:",date_str))
  }else{
    return(d)
  }
}

write_df <- function(df, file, path, hdr=NULL, create=TRUE, append=FALSE){
  # -----------------------------------------------------------------------------------------------
  # Write data frame to output file
  #
  # PARAMETERS
  # **********
  # df : data frame
  #   Data frame to be written to output
  # file : str
  #   Output file name
  # path : str
  #   Output path where output file is stored
  #
  # OTHER PARAMETERS
  # ****************
  # hdr : c(str) or NULL
  #   Output file header(s)
  # create : bool
  #   Whether to create output path if it does not exist
  # append : bool
  #   Whether to append data to existing file (only if hdr=NULL) 
  # -----------------------------------------------------------------------------------------------
  
  file_out <- paste(path, file, sep="/")
  
  cols <- FALSE
  if (!file.exists(file_out)){
    append <- FALSE
  }
  
  if (!append){
    cols <- TRUE
    if (!is.null(hdr)){
      write(hdr, file=file_out, ncolumns=length(hdr), sep="\n", append=append)
      append <- TRUE
    }
  }
  
  # Write data
  suppressWarnings(write.table(df, file=file_out, sep="\t", quote=FALSE, 
                               col.names=cols, row.names=FALSE, append=append))
}

## SETTINGS #######################################################################################
# "2019-01-14"
# "2019-01-15"
# "2019-02-03"
# "2019-02-06"
# "2019-02-07"
# "2019-03-02"
# "2019-01-14"
# "2019-03-02"
# "2019-04-07"
# "2019-05-13"

date_rs <- "2019-04-31" # RS launch date
path_inp <- paste("/mnt/Robotsonde/2019/", substr(date_rs,6,7),sep="")
path_out <- "/home/becatdt/Francesc_Roura/RADIOSONDATGE/Renovació/"
file_out <- "monitorRS.txt"

## DEFAULT METADATA ###############################################################################

# lag: delay in days between RS launch date and log filename date
launches <- list("1"=list(time="00", lag=2), "2"=list(time="12", lag=1)) 

#llista d'errors
msg_err <- list("1"="No start detection by IR2010",
                "2"="Valve ejected, sonde switched",
                "3"="Balloon disappearance",
                "4"="Balloon blocked",
                "5"="No gas",
                "6"="SONDE NOT RECEIVED",
                "7"="Humidity sensor KO",
                "8"="Temperature failure",
                "9"="No GPS",
                "10"="Sonde unknown",
                "11"="Batteries too low",
                "12"="No ICAR response",
                "13"="End before",
                "14"="analysis aborted (<2points)",
                "15"="ROOF BLOCKED",
                "16"="Inflation failure",
                "17"="TOO LATE FOR RELAUNCH",
                "18"="OPERATOR INTERVENTION",
                "19"="NO SONDE AVAILABLE",
                "20"="Inflation tube not empty",
                "21"="IR2010 not ready",
                "22"="onde stayed ON",
                "23"="witch OFF the sonde",
                "24"="essage not generated",
                "25"="STOP button")

# Llista de missatges importants en format llista
messages <- list(rs_start="= Launch planned", 
                 rs_forced="= Launch triggered",
                 rs_end="= Radiosounding end",
                 sonde_start="# SONDE",
                 sonde_launch="# LAUNCH ",
                 sonde_depart="DEPART DETECTE",
                 sonde_ok="SONDAGE OK", 
                 errors="ERROR", 
                 causa="CAUSE")

grep_sonde_ok <- list("altitude"="\\s((\\d*)|(\\d*\\.\\d*))m", 
                      "pressure"="\\s((\\d*)|(\\d*\\.\\d*))hPa")

grep_sonde_act <- list("ID"="\\s\\d{9}\\s\\s", 
                       "position"="\\[\\s\\d*\\s\\]")

grep_sonde_times <- list("launch"=messages$sonde_launch, 
                         "depart"=messages$sonde_depart, 
                         "end"=messages$sonde_ok)

###################################################################################################

# Mirar si el dia existeix
try_date(date_rs)

# Choose file
if (is.null(date_rs)){
  date_rs <- format(Sys.time()-2*86400, "%Y-%m-%d")
}

date_rs <- as.Date(date_rs)

# Fitxers log d'entrada
files_inp <- sapply(launches, function(x){
  file_name <- paste0(format(date_rs+x$lag, "%y%m%d"), x$time, ".log")
  if(substr(file_name,3,4)!=format(as.POSIXct(date_rs), "%m")){ #si últim dia del mes
    str_sub(path_inp,-2,-1) <- substr(file_name,3,4)
  }
  paste(path_inp, file_name, sep="/")
  
  }
  )



# Seleccionar un dels fitxers que es trobin
indx_f <- which(file.exists(files_inp))[1]
if(is.na(indx_f)){
  stop(paste("No log files found for the selected date:", date_rs))
}

file_inp <- files_inp[[as.character(indx_f)]]

# Load log info
conn <- file(file_inp, "r")
info <- readLines(conn, encoding = 'UTF-8')
close(conn)

# Identificar inici/final radiosondatge

rs_chunks <- find_rs_chunks(log=info, msg_st=messages$rs_start, messages$rs_forced, msg_end=messages$rs_end)
if (is.na(rs_chunks[1])){
  stop(paste("No RS data found in input file:", file_inp))
}

data_out <- data.frame()
for(rsc in rs_chunks){
  
  time_session <- gsub("\\s*", "", extract_pattern(info[rsc[1]], "\\s\\d{2}:\\d{2}\\s"))
  if(is.na(time_session)){
    time_session <- "not_planned"
  }
  
  if(is.na(rsc[2])){
    stop(paste("Uncomplete log file found for the selected date:", date_rs))
  }
  
  log_rs <- info[rsc[1]:rsc[2]]
  
  sonde_st <- grep(messages$sonde_start, log_rs, value=F)
  sonde_seq <- seq_along(sonde_st)
  sonde_st <- c(sonde_st, length(log_rs))
  
  # print(log_rs)
  
  for (s in sonde_seq){
    
    log_sonde <- log_rs[sonde_st[s]:(sonde_st[s+1])]
    
    # Sonde ID and position
    sonde_data_act <- sonde_data(log=log_sonde, msg=messages$sonde_start, grep_lst=grep_sonde_act)
    
    # Start and end times
    times_sonde <- sonde_times(log=log_sonde, msg_lst= grep_sonde_times)
    times_dec <- lapply(times_sonde, time2dec)
    flight_duration <- 3600*circular_diff(tf=times_dec$end, ti=times_dec$depart)
    
    #
    sonde_data_ok <- sonde_data(log=log_sonde, msg=messages$sonde_ok, grep_lst=grep_sonde_ok)
    realitzat <- 1
    
    if(is.na(sonde_data_ok$altitude)){
      realitzat <- 0 # FALSE
    }
    
    voladora <- flying_sond(log_sonde, messages$sonde_depart)
    
    error <- extract_error(log_sonde, msg_err)
    
    data_out_s <- data.frame(date=date_rs, session=time_session,
                             sonde_id=sonde_data_act$ID, sonde_position=sonde_data_act$position,
                             time_launch=times_sonde$launch, time_depart=times_sonde$depart,
                             time_end=times_sonde$end, duration=sprintf("%.1f", flight_duration),
                             altitude=sonde_data_ok$altitude, pressure=sonde_data_ok$pressure, 
                             done=realitzat, flying_sonde=voladora, sonde_error=error)
    
    data_out <- rbind(data_out, data_out_s)
    
  }
  
}

write_df(df=data_out, file=file_out, path=path_out, append=TRUE)
