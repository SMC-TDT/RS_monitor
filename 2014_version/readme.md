Radiosounding monitoring: 2014 version
======================================

This version, operative since 2014, includes log file processing and interactive result visualization.

The main script (*genera_dades_radiosonda.R*) searches the latest log file in the current month's input folder and parses it. It splits the information into the two different launches. It outputs a file containing a launch-based information table with the relevant data about the launch process (this output is written in Catalan language). It also outputs a second file containing a table with the encountered error data. 

The main script can be found in the *./prog/* folder. The output files are stored in the *./output/* folder. 

The *./brew/* folder contains the *radiomonit.rhtml* file (to be run by [RApache](http://rapache.net/manual.html)) that allows the visualization of the output data in a web server. The *./docs/* folder contains a manual that describes the operation of this script and the installation of RApache for Unix (in Catalan).

# Required R libraries

The following libraries are required by the web-visualization script:

- [reshape2](https://cran.r-project.org/web/packages/reshape2/index.html)
- [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html)
- [stats](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/00Index.html)
- [brew](https://cran.r-project.org/web/packages/brew/index.html)
- [scales](https://cran.r-project.org/web/packages/scales/index.html)
- [chron](https://cran.r-project.org/web/packages/chron/index.html)
- [xtable](https://cran.r-project.org/web/packages/xtable/index.html)

# Configuration

The user should configure the process by specifying the following variable values in the *SETTINGS* section at the beginning of the scripts.

**genera_dades_radiosonda.R**:

- path_log: input path where log files are stored; the storage must follow the structure: path_in/YYYY/MM/
- path_out: output path where output files will be stored
- fname_out: output file; all launch data entries are accumulated in this file.
- fname_err: output file; all error data entries are accumulated in this file.

**radiomonit.rhtml**

- path_inp: input path where files output by the previous process are stored; see *path_out* variable above
- fname_inp: input file name (without path); see *fname_out* variable above
- fname_err: input file name (without path); see *fname_err* variable above
- path_out: output path where output figures will be stored; commonly a subfolder in */var/ww/html/*
- path_lib: path where required libraries are installed or *NULL* 

# Execution

The main script is designed for automatic date search, so execution is direct:
```
$ RScript /path_to_script/genera_dades_radiosonda.R
```

# Output

**radiosonda.txt:**

Contains a table with the following radiosounding session data columns (space separated):

- Data: radiosounding session date, YYYY-MM-DD
- Sessio: radiosounding session type; either "mati" (morning) or "nit" (evening)
- Realitzat: whether the radiosounding was successful (reach nominal height); either "Si" or "No"
- Hora: sonde flight end time, HH:MM:SS
- Durada: sonde flight duration in minutes
- Altitut: altitude reached by the sonde in meters
- Pressio: pressure reading [hPa]
- Rellançaments: number of failed launches
- Fallida: carousel position(s) of failed sonde(s)
- Sondes: number of sondes used

**radiosonda_errors.txt:**

Contains a table with the following error data columns (space separated):

- Data: error date, YYYY-MM-DD
- Hora: error time, HH:MM:SS
- Error: error message

# Visualization
