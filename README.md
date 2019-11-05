Monitorització del radiosondatge
================================

This repository includes the R code for processing the log files output in the automatic radiosounding process carried out at the Meteorological Service of Catalonia.

The user input is the radiosounding date amd the script searches for the corresponding log file(s) in the input folder. The script parses the log file and divides it first into the different launches and then each launch into its different (if more than one) sonde activation processes. It outputs a sonde-based information table, containing the relevant data about the sonde performance. 

## Required libraries

- [optparse](https://cran.r-project.org/web/packages/optparse/index.html)

## Configuration (RS_readLOG_config.R)

The following parameters may be modified in the *USER SETTINGS* section of the *RS_readLOG_config.R* file:

- path_in: input path where log files are stored; the storage must follow the structure: path_in/YYYY/MM/
- path_out: output path where output file will be stored
- file:out: output file; all entries are accumulated in this single file.

The *DEFAULT METADATA* section of the *RS_readLOG_config.R* file includes variables and parameters specific to the parsing process and the particular structure of the log files. Do not modify it unless strictly necessary due to a major change in the log files (name, structure, content). It can also be modified if the error list needs to be updated (*msg_err* variable list).

## Execution

Command line execution requires date input
```
$ RScript /path_to_script/RS_readLOG_main.R -d "YYYY-MM-DD"
```

## Processing several dates

The repository includes a simple shell script for processing multiple dates: *man_RS_readLOG.sh*.

Open the script, modify STARTdate and ENDdate variables at convenience and execute:
```
$ ./man_RS_readLOG.sh
```

## Output

- date: radiosounding session date YYYY-MM-DD
- type: type of session; automatic (auto) or manually triggered (trig)
- session: radiosounding session time HH:MM
- sonde_id: sonde identifier
- sonde_position: sonde position in the caroussel
- time_launch: sonde launch time  HH:MM:SS
- time_depart: sonde departure time  HH:MM:SS
- time_end: sonde flight end time HH:MM:SS
- duration: sonde flight duration in seconds (time_end - time_depart)
- altitude: altitude reached by the sonde in meters
- pressure: pressure reading [hPa]
- sonde_depart: wheter the sonde has departed or not (1/0 boolean)
- sonde_OK: whether the sonde has reached the required level (1/0 boolean)
- errors: IDs of the encountered errors
