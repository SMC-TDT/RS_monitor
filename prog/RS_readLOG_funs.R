
## RS_readLOG_funs.R ##############################################################################

## Created : Francesc Roura Adserias 25-03-2019
## Last modified: Patricia Altube 22-10-2019 

## Description : 
## Contains the functions used in RS_LOGparse_main.R. 

###################################################################################################

# build_input_files
# circular_diff
# cl_input
# extract_error
# extract_pattern
# extract_time
# find_rs_sessions
# sonde_data
# sonde_times
# time2dec
# try_date
# write_df

library(optparse, quietly=TRUE, warn.conflicts=FALSE) # parse command line arguments

###################################################################################################

build_input_files <- function(lst, date, path){
  # -----------------------------------------------------------------------------------------------
  # Returns expected .log file names with their complete paths (/path/YYYY/MM) for the given date 
  #
  # PARAMETERS
  # **********
  # lst : list(time=, lag=)
  #   List with radiosounding time and lag in days according to the input file names
  # date : Date
  #   Radiosounding date
  # path : str
  #   Root path
  #
  # RETURNS
  # *******
  # file : c(str)
  #   Log file names with their complete paths (/path/YYYY/MM)
  # -----------------------------------------------------------------------------------------------
  
  date_f <- date+lst$lag # date in file name
  # build input path:
  path_inp <- paste(path, format(date_f, "%Y"), format(date_f, "%m"), sep="/")
  # Input file(s)
  file_name <- paste0(format(date_f, "%y%m%d"), lst$time, ".log")
  file <- paste(path_inp, file_name, sep="/")
  
  return(file)
}

circular_diff <- function(tf, ti, mod=24){
  # -----------------------------------------------------------------------------------------------
  # Calculate minimum difference between two circular quantities
  #
  # PARAMETERS
  # **********
  # tf : float
  #   "Final" quantity
  # ti : float
  #   "Initial" quantity
  #
  # OTHER PARAMETERS
  # ****************
  # mod : int
  #   Module, 24 (hours) by default
  #
  # RETURNS
  # *******
  # diff : float
  #   Minimum circular difference tf-ti
  # -----------------------------------------------------------------------------------------------
  
  diff <- (tf-ti)%%mod
  diff_b <- (ti-tf)%%mod
  
  diff[diff_b<diff] <- -diff_b[diff_b<diff]
  
  return(diff)
}

cl_date <- function(){
  # -----------------------------------------------------------------------------------------------
  # Retrieve date in "yyyy-mm-dd" format from command-line:
  #   Date: -d, --date
  #
  # RETURNS
  # *******
  # date : str
  #   Command line date input in format "%Y-%m-%d"
  # -----------------------------------------------------------------------------------------------
  
  option_list = list(make_option(c("-d", "--date"), type="character", default="",
                                 help="Radiosounding date; if not specified, current date is taken", metavar="character"))
  
  opt_parser <- OptionParser(option_list=option_list);
  opt <- parse_args(opt_parser);
  date <- opt$date
  
  return(date)
}

extract_error <- function(log, err_lst){
  # -----------------------------------------------------------------------------------------------
  # Returns the ID of the error messages found in the log data.
  #
  # PARAMETERS
  # **********
  # log : str 
  #   Input log data, in character format
  # err_lst : list
  #   Error list where all possible errors are stored
  #
  # RETURNS
  # *******
  # err_id : str
  #   String containing the error IDs
  # -----------------------------------------------------------------------------------------------
  
  err_id <- NA
  
  matches <- lapply(err_lst, grep, x=log, value=FALSE)
  err <- matches[sapply(matches, function(x) length(x)!=0)]
  
  if (length(err)!=0){
    err_id <- paste(names(err), collapse=",") 
  }
  
  return(err_id)
}

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
  # -----------------------------------------------------------------------------------------------
  # Extracts time in HH:MM:SS format from a string.
  #
  # PARAMETERS
  # **********
  # str : str
  #   Input string
  #
  # RETURNS
  # *******
  # time : str
  #   Extracted time in HH:MM:SS format
  # -----------------------------------------------------------------------------------------------
  
  time <- extract_pattern(str, patt="\\d{2}:\\d{2}:\\d{2}")
  return(time)
  
}

find_date <- function(log, date_msg, patt="\\d{8}", format="%Y%m%d"){
  # -----------------------------------------------------------------------------------------------
  # Finds the line that contains the date and extracts the date.
  #
  # PARAMETERS
  # **********
  # log : str 
  #   Input log data, in character format
  # date_msg : str
  #   Message that characterises the line with the date
  #
  # OTHER PARAMETERS
  # ****************
  # patt : reg expr
  #   Pattern string for the date, regular expression
  # format : format str
  #   Format in which date is given
  #
  # RETURNS
  # *******
  # date : Date
  #   Extracted date
  # -----------------------------------------------------------------------------------------------
  
  date <- NA
  line <- grep(date_msg, log, value=TRUE)[1]
  
  if (!is.na(line)){
    date_str <- extract_pattern(str=line, patt="\\d{8}")
    date <- as.Date(date_str, format=format)
  }else{
    cat("WARNING: No date info found in input file; nominal date will be used \n")
  }

  return(date)
  
}

find_rs_sessions <- function(log, msg_auto, msg_trig, msg_end){
  # -----------------------------------------------------------------------------------------------
  # This function splits the .log file in launches/chunks
  #
  # PARAMETERS
  # **********
  # log : str 
  #   Input log data, in character format
  # msg_auto : str
  #   Message that characterises the line that indicates an automatic launch
  # msg_trig : str
  #   Message that characterises the line that indicates a triggered launch
  # msg_end : str
  #   Message that characterises the line that indicates the launch end
  #
  # RETURNS
  # *******
  # chunks_lst : list(list(st=, end=)) 
  #   List of lists that specify each of the launches/chunks with the start and end indexes with 
  #   respect to the input log data
  # -----------------------------------------------------------------------------------------------
  
  chunks_lst <- NULL
  
  # find launch starting line indexes and label each case (auto/trig)
  auto <- grep(msg_auto, log, value=F)
  trig <- grep(msg_trig, log, value=F)
  
  df_st <- data.frame(st=c(auto, trig), 
                      type=c(rep("auto", length(auto)), rep("trig", length(trig))))
  
  # Find launch ending line indexes
  if (nrow(df_st)!=0){
    df_st <- df_st[order(df_st$st),]
    
    end <- grep(msg_end, log, value=F)
    end <- end[end>min(df_st$st)]
    
    # If end line number does not coincide with start line number, use next start indexes as previous launch end lines
    if (length(end)!=length(df_st$st)){
      end <- c(df_st$st[-1], length(log))
    }
    
    # Sort start and end indexes and match them into launch chunks
    line_all <- sort(c(df_st$st, end))
    pair_lst <- split(line_all, ceiling(seq_along(line_all)/2))
    df_end <- t(data.frame(pair_lst))
    colnames(df_end) <- c("st", "end")
    
    chunks_df <- merge(df_st, df_end, by="st")
    chunks_lst <- split(chunks_df, seq(nrow(chunks_df)))
  }
  
  return(chunks_lst)
}

sonde_data <- function(log, msg, patt_lst){
  # -----------------------------------------------------------------------------------------------
  # Finds the line with the specified message and extracts the data that matches the patterns 
  # given.
  #
  # PARAMETERS
  # **********
  # log : str 
  #   Input log data, in character format
  # msg : str
  #   Message that characterises the line that contains the desired data
  # patt_lst: list
  #   List of name=pattern pairs 
  #
  # RETURNS
  # *******
  # data_s : list
  #   List containing extracted data, the names correspond to the ones given in patt_lst
  # -----------------------------------------------------------------------------------------------
  
  data_s <- as.list(sapply(names(patt_lst), function(x) NA))
  str <- log[grep(msg, log)[1]]
  
  if (!is.na(str)){
    for (u in names(patt_lst)){
      
      match <- extract_pattern(str, patt_lst[[u]])
      data_s[[u]] <- as.numeric(gsub("\\D", "", match))
      
    }
  }
  
  return(data_s)
}

sonde_times <- function(log, msg_lst){
  # -----------------------------------------------------------------------------------------------
  # Extracts the times in HH:MM:DD format corresponding to lines characterised by the given messages
  #
  # PARAMETERS
  # **********
  # log : str 
  #   Input log data, in character format
  # msg_lst : list
  #   List of name=message pairs that characterise the target lines
  #
  # RETURNS
  # *******
  # times : list
  #   List containing extracted times in HH:MM:DD format, the names correspond to the ones given in msg_lst
  # -----------------------------------------------------------------------------------------------
  
  times <- as.list(sapply(names(msg_lst), function(x) NA))
  
  for (pn in names(msg_lst)){
    
    log_str <- log[grep(msg_lst[[pn]], log)[1]]
    
    if (!is.na(log_str)){
      times[[pn]] <- extract_time(log_str) 
    }
    
  }
  
  return(times)
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

try_date <- function(date_str, date_min="2018-01-01", day_lag=2){
  # -----------------------------------------------------------------------------------------------
  # Finds out if the given date exists and is after a given minimum date.
  # If the date input is empty it returns the date corresponding to the current date 
  # plus the specified day lag.
  #
  # PARAMETERS
  # **********
  # date_str : str (format "%Y-%m-%d")
  #   Date under testing
  # date_min : str (format "%Y-%m-%d")
  #   Minimum date allowed
  # day_lag : int
  #   Number of days from RS performance date to log file reception date (file name date)
  #
  # RETURNS
  # *******
  # date : Date
  #   The given date. If it does not exist or is before the minimum date, 
  #   the program stops and issues an error.
  # -----------------------------------------------------------------------------------------------
  
  date_min <- as.Date(date_min)
  secs_day  <- 24*60*60

  if (date_str==""){
    date <- try(as.Date(Sys.time()-day_lag*secs_day), silent = TRUE)
  }else{
    date <- try(as.Date(date_str), silent = TRUE)
  }

  if (class(date)=="try-error"){
    stop(paste("Could not find the given date:", date_str, "(ERROR)"))
    
  }else if(date<=date_min){
    stop(paste("This script cannot handle log files for dates before", date_min, "(ERROR)"))
    
  }else{
    return(date)
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
  suppressWarnings(write.table(df, file=file_out, sep=" ", quote=FALSE, 
                               col.names=cols, row.names=FALSE, append=append))
}
