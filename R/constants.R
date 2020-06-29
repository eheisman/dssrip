######
# constants.R
# Constants used by dssrip to fill in metadata
#
######

TSC_TYPES = c("INST-VAL", "INST-CUM", "PER-AVER", "PER-CUM")
minutes = c(1,2,3,4,5,6,10,12,15,20,30)
hours = c(1,2,3,4,6,8,12)
TSC_INTERVALS = c(minutes, 60*hours, 60*24*c(1,7,10,15,30,365), rep(0,5))
## Irregular appears to have interval of 0, not -1
names(TSC_INTERVALS) = c(paste0(minutes, "MIN"),
                         paste0(hours, "HOUR"),
                         "1DAY","1WEEK","TRI-MONTH","SEMI-MONTH", "1MON","1YEAR",
                         paste0("IR-",c("DAY","MON","YEAR","DECADE","CENTURY")))
