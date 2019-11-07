Radiosounding monitoring: 2019 version
======================================

This version, developed in 2019, improves the previous in robustness and flexibility but only implements the radiosounding log file processing.

The user input for the main script (*RS_readLOG_main.R*) is the radiosounding date. The script searches for the log file(s) corresponding to the specified date in the input folder and parses them. First, it splits the information into the different sonde processes. It outputs a file containing a sonde-based information table with the relevant data about the sonde characteristics and performance. 

The scripts required can be found in the *./prog/* folder. The output file is stored in the *./output/* folder. The *./docs/* folder contains a flowchart of the process carried out by the main script *RS_readLOG_main.R*.

# Required R libraries

- [optparse](https://cran.r-project.org/web/packages/optparse/index.html)

# Configuration

The following parameters may be modified in the *USER SETTINGS* section of the *RS_readLOG_config.R* file:

- *path_in*: input path where log files are stored; the storage must follow the structure: path_in/YYYY/MM/
- *path_out*: output path where output file will be stored
- *fname_out*: output file; all sonde data entries are accumulated in this file.

The *DEFAULT METADATA* section of the *RS_readLOG_config.R* file includes variables and parameters specific to the parsing process and the particular structure of the log files. Do not modify it unless strictly necessary due to a major change in the log files (name, structure, content). It can also be modified if the error list needs to be updated (*msg_err* variable list).

# Execution

Command line execution requires date input:
```
$ RScript /path_to_script/RS_readLOG_main.R -d "YYYY-MM-DD"
```

# Processing several dates

The repository includes a simple shell script for processing multiple dates: *man_RS_readLOG.sh*.

Open the script, modify *STARTdate* and *ENDdate* variables at convenience and execute:
```
$ ./man_RS_readLOG.sh
```
# Output

**monitorRS.txt:** 

Contains a table with the following data columns (space separated):

- *date*: radiosounding session date, YYYY-MM-DD
- *type*: type of session; automatic (auto) or manually triggered (trig)
- *session*: radiosounding session time HH:MM
- *sonde_id*: sonde identifier
- *sonde_position*: sonde position in the carousel
- *time_launch*: sonde launch time, HH:MM:SS
- *time_depart*: sonde departure time, HH:MM:SS
- *time_end*: sonde flight end time, HH:MM:SS
- *duration*: sonde flight duration in seconds (time_end - time_depart)
- *altitude*: altitude reached by the sonde in meters
- *pressure*: pressure reading [hPa]
- *sonde_depart*: whether the sonde has departed or not (1/0 booleans)
- *sonde_OK*: whether the sonde has reached the required level (1/0 booleans)
- *errors*: IDs of the encountered errors
