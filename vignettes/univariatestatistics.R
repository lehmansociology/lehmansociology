## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(ggplot2)

## ----startupcode---------------------------------------------------------

library(lehmansociology)


## ----descriptive---------------------------------------------------------

    # This gives us the number of observations.
    length(poverty.states$PCTPOVALL_2013)
    max(poverty.states$PCTPOVALL_2013)
    min(poverty.states$PCTPOVALL_2013)
    mean(poverty.states$PCTPOVALL_2013)
    median(poverty.states$PCTPOVALL_2013)
    sd(poverty.states$PCTPOVALL_2013)
    var(poverty.states$PCTPOVALL_2013)
    range(poverty.states$PCTPOVALL_2013)
    sum(poverty.states$PCTPOVALL_2013)
    #summary(poverty.states$PCTPOVALL_2013)
    fivenum(poverty.states$PCTPOVALL_2013)
    ### Calculates the numbers associated to defined percentiles
    quantile(poverty.states$PCTPOVALL_2013, c(.25, .5, .75, 1))



## ----narmtrue------------------------------------------------------------

# This gives us the number of observations.

    max(poverty.states$PCTPOVALL_2013, na.rm = TRUE)
    min(poverty.states$PCTPOVALL_2013, na.rm = TRUE)
    mean(poverty.states$PCTPOVALL_2013, na.rm = TRUE)
    median(poverty.states$PCTPOVALL_2013, na.rm = TRUE)
    sd(poverty.states$PCTPOVALL_2013, na.rm = TRUE)
    var(poverty.states$PCTPOVALL_2013, na.rm = TRUE)
    range(poverty.states$PCTPOVALL_2013, na.rm = TRUE)
    sum(poverty.states$PCTPOVALL_2013, na.rm = TRUE)
    #summary(poverty.states$PCTPOVALL_2013, na.rm = TRUE)
    fivenum(poverty.states$PCTPOVALL_2013, na.rm = TRUE)
 ### Calculates the numbers associated to defined percentiles
    quantile(poverty.states$PCTPOVALL_2013, c(.25, .5, .75, 1))
 


## ------------------------------------------------------------------------

    mean(poverty.states$PCTPOVALL_2013)


## ------------------------------------------------------------------------
    temp <- poverty.states$PCTPOVALL_2013
    temp[50]<- NA

## ------------------------------------------------------------------------
    mean(temp)

## ------------------------------------------------------------------------

   mean(temp, na.rm = TRUE)
    

