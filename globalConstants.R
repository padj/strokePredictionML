# globalConstants
#             Constants for consistent look and feel. Things like plotting
#             size, colours, resolution.
###############################################################################
library(HEOR)


RUN <- TRUE
if(RUN){
  GC <- new.env()

  with(GC,
       {
##### R Version #####
         #specify at least one R version used for analysis
         R.VERSION <- c("4.0.2")

##### DATA #####
         # How to read and store working data
         READ.FST <- TRUE # Will fall back to csv if not
         READ.CSV <- !READ.FST # Mutually exclusive with reading fst
         WRITE.FST <- READ.FST || TRUE # Has to be true if reading fst
         WRITE.CSV <- READ.CSV || TRUE

##### SEEED #####
         #Specify random seed for random number generators
         SEED <- 19280903

###### BOOTSTRAP #####
         # Number of threads for parallel clusters. NULL will use detectCores(logical = FALSE)
         THREADS <- NULL
         # Specify if parallel computing should be used
         PARALLEL <- TRUE

##### PLOTTING #####
         # Specify file extension for plots
         PLOT.EXTENSION <- "png" # Default

##### SURVIVAL #####
         # Specify type of CI for Kaplan-Meier Plots
         KM.CONF.TYPE <- "log-log" # Default confidence intervals - here for parity
         # with SAS

##### IDs #####
         INDEX.ID.COL <- "uid"
         COMPARATOR.ID.COL <- "uid"

##### DATE DETAILS #####
         #DY2MN <- 1/30.4375
         DY2MN <- 1/30
         DY2YR <- 1/360

##### FURTHER DETAILS #####
         #specify further variables here

  })

}
