
## USER SETTINGS ########################################################################
## Set the values of the variable (right hand side of the <- sign) in this section for 
## customization.  Do not change the variable names (the left hand side of the <- sign). 

# Working directory
path_in <- "../log_data"

# Output directory
path_out <- ".."

# Output file name
file_out <- "monitorRS.txt"

## DEFAULT METADATA #####################################################################
## Do not modify this section unless you know what you are doing!

# Number of days that the file name lags from the RS log data contained
day_lag <- 2

# Reference date (log format change date); this script is applicable from this date on
date_min <- as.Date("2018-11-22")

# lag: delay in days between RS launch date and log filename date.
# This list is used to build the expected input file names.
launches <- list("1"=list(time="00", lag=2), "2"=list(time="12", lag=1)) 

# List of key-messages in LOG file
messages <- list(date="FICHIERS CREES=",
                 rs_start="= Launch planned", 
                 rs_trig="= Launch triggered",
                 rs_end="= Radiosounding end",
                 sonde_start="# SONDE",
                 sonde_launch="# LAUNCH ",
                 sonde_depart="DEPART DETECTE",
                 sonde_ok="SONDAGE OK", 
                 errors="ERROR", 
                 cause="CAUSE")

# List of possible errors
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
                "25"="STOP button",
                "26"="Roof blocked (middle)",
                "27"="Sonde ascent not detected")

# Regular expression for finding radiosounding date
grep_date <- "BA\\d{8}"
date_format <- "%Y%m%d"

# Regular expression for finding sonde altitude and pressure
grep_sonde_ok <- list("altitude"="\\s((\\d*)|(\\d*\\.\\d*))m", 
                      "pressure"="\\s((\\d*)|(\\d*\\.\\d*))hPa")

# Regular expression for finding sonde ID and its carousel position
grep_sonde_act <- list("ID"="\\s\\d{9}\\s\\s", 
                       "position"="\\[\\s\\d*\\s\\]")
