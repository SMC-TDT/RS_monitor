#!/usr/bin/env Rscript

## RS_readLOG_main.R ##############################################################################

## Created : Francesc Roura Adserias, 25-03-2019
## Last modified: Patricia Altube, 22-10-2019 

## Description: 
## Reads the ".log" file from the radiosounding process and extracts the performance parameters of
## all activated sondes.

## FUNCTIONS ######################################################################################

script_path <- function(){
  # -----------------------------------------------------------------------------------------------
  # Get the path of the current script
  #
  # PARAMETERS
  # **********
  #
  # RETURNS
  # *******
  # script_dir : str
  #   Path where the script is located
  # -----------------------------------------------------------------------------------------------
  
  args <- commandArgs(trailingOnly = FALSE)
  script_dir <- NULL
  
  if (any(grep("--slave", args))){
    
    file <- gsub("--file=", "", args[grep("--file=", args)])
    script_dir <- regmatches(file, regexpr("(\\/[[:graph:]]*\\/)*", file))
    
    if (script_dir==""){
      script_dir <- "." 
    }
  }else if (any(grep("--interactive", args))){
    try(script_dir <- dirname(sys.frame(1)$ofile))
  }
  
  return(script_dir)
}

## INITIALIZATION #################################################################################

# Current script path
path_scr <- script_path()

# Load auxiliary files (functions and configuration parameters)
source(paste(path_scr,"RS_readLOG_funs.R", sep="/"))
source(paste(path_scr,"RS_readLOG_config.R", sep="/"))

# Command line input; radiosounding date 
date_rs <- cl_date()

###################################################################################################

date_rs <- try_date(date_rs, date_min=date_min, day_lag=day_lag)

# INPUT FILES
files_in <- sapply(launches, build_input_files, date=date_rs, path=path_in)

# Select an existing file
files_in <- files_in[which(file.exists(files_in))]
if(length(files_in)==0){
  stop(paste("No log files found for the selected date:", date_rs, "(ERROR)"))
}

cat("Input file(s):\n", paste(files_in, collapse = "\n"), "\n")
data_out <- data.frame()

for (f_in in files_in){
  
  # Load log info
  conn <- file(f_in, "r")
  log_all <- readLines(conn, encoding = 'UTF-8')
  close(conn)
  
  # Separate log info into chunks corresponding to different radiosounding sessions
  rs_chunks <- find_rs_sessions(log=log_all, msg_auto=messages$rs_start, 
                              msg_trig=messages$rs_trig, msg_end=messages$rs_end)
  if (is.null(rs_chunks)){
    stop("No RS data found in input (ERROR)")
  }
  
  for(rsc in rs_chunks){
    
    time_session <- gsub("\\s*", "", extract_pattern(log_all[rsc$st], "\\s\\d{2}:\\d{2}\\s"))
    
    # Chunk of info of single radiosounding session 
    log_rs <- log_all[rsc$st:rsc$end]
    
    # Check given date with radiosounding date inside file 
    date_file <- find_date(log=log_rs, date_msg=messages$date, patt=grep_date, format=date_format)
    
    if (!is.na(date_file)){
      if (date_rs!=date_file){
        cat("WARNING: Required date ", date_rs, " does not match radiosounding date in input file ", date_file, "\n")
      }
    } else{
      date_file <- date_rs
    }
    
    # Indexes of sonde activation messages
    sonde_st <- c(grep(messages$sonde_start, log_rs, value=F), length(log_rs))
    
    for (s in seq_along(sonde_st[-1])){
      
      log_sonde <- log_rs[sonde_st[s]:(sonde_st[s+1])]
      
      # Sonde ID and position
      sonde_data_act <- sonde_data(log=log_sonde, msg=messages$sonde_start, patt_lst=grep_sonde_act)
      
      # Sonde start, launch and end times
      sonde_times_lst <- list("launch"=messages$sonde_launch, 
                              "depart"=messages$sonde_depart, 
                              "end"=messages$sonde_ok)
      times_sonde <- sonde_times(log=log_sonde, msg_lst= sonde_times_lst)
      times_dec <- lapply(times_sonde, time2dec)
      flight_duration <- 3600*circular_diff(tf=times_dec$end, ti=times_dec$depart)
      
      # Sonde OK end data 
      sonde_data_ok <- sonde_data(log=log_sonde, msg=messages$sonde_ok, patt_lst=grep_sonde_ok)
      
      realitzat <- 1
      if(is.na(sonde_data_ok$altitude)){
        realitzat <- 0 # FALSE
      }
      
      depart <- length(grep(messages$sonde_depart, log_sonde, value = FALSE))
      error <- extract_error(log_sonde, msg_err)
      
      data_out_s <- data.frame(date=date_file, type=rsc$type, session=time_session,
                               sonde_id=sonde_data_act$ID, sonde_position=sonde_data_act$position,
                               time_launch=times_sonde$launch, time_depart=times_sonde$depart,
                               time_end=times_sonde$end, duration=sprintf("%.1f", flight_duration),
                               altitude=sonde_data_ok$altitude, pressure=sonde_data_ok$pressure,
                               sonde_depart=depart, sonde_OK=realitzat, errors=error)
      
      data_out <- rbind(data_out, data_out_s)
      
    }
    
  }
  
}

data_out <- data_out[!duplicated(data_out), ]
write_df(df=data_out, file=file_out, path=path_out, append=TRUE)