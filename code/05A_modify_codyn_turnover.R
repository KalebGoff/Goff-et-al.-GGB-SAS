# Title: 05A_modify_codyn_turnover
# Author: Kaleb Goff
# Date: May 2024

##############################################################################################################
#This script makes one small modification to the source code of codyn (Hallet et al. 2013), so that gains and losses are not proportional to the total species richness of a given replicate.

##############################################################################################################

turnover_MOD <- function(df, time.var, 
                         species.var, 
                         abundance.var, 
                         replicate.var=NA, 
                         metric="total") {
  
  if(is.na(replicate.var)){
    
    # check there unique species x time combinations
    check_single_onerep(df, time.var, species.var)
    
    # calculate turnover
    output <- turnover_allyears_MOD(df, time.var, species.var, abundance.var, metric)
    
    
  } else {
    
    # remove unused levels if replicate.var is a factor
    df[replicate.var] <- if(is.factor(df[[replicate.var]])) {
      factor(df[[replicate.var]])
    } else {
      df[replicate.var]
    }
    
    
    # sort and apply turnover to all replicates
    df <- df[order(df[[replicate.var]]),]
    X <- split(df, df[replicate.var])
    out <- lapply(X, FUN=turnover_allyears_MOD, time.var, species.var, abundance.var, metric)
    ID <- unique(names(out))
    out <- mapply(function(x, y) "[<-"(x, replicate.var, value = y) ,
                  out, ID, SIMPLIFY = FALSE)
    output <- do.call("rbind", out)
  }
  
  # results
  row.names(output) <- NULL
  return(as.data.frame(output))
}

# A function to calculate species turnover between years

turnover_allyears_MOD <- function(df, 
                                  time.var, 
                                  species.var, 
                                  abundance.var, 
                                  metric=c("total", "disappearance","appearance")) {
  
  # allows partial argument matching
  metric = match.arg(metric) 
  
  # sort and remove 0s
  df <- df[order(df[[time.var]]),]
  df <- df[which(df[[abundance.var]]>0),]
  
  ## split data by year
  templist <- split(df, df[[time.var]])
  
  ## create consecutive pairs of time points
  t1 <- templist[-length(templist)]
  t2 <- templist[-1]
  
  ## calculate turnover for across all time points
  out <- Map(turnover_twoyears_MOD, t1, t2, species.var, metric)
  output <- as.data.frame(unlist(out))
  names(output)[1] = metric
  
  ## add time variable column
  alltemp <- unique(df[[time.var]])
  output[time.var] =  alltemp[2:length(alltemp)]
  
  # results
  return(output)
}

# A function to calculate species turnover between two years 

turnover_twoyears_MOD <- function(d1, d2, 
                                  species.var, 
                                  metric=c("total", "disappearance","appearance")){
  
  # allows partial argument matching
  metric = match.arg(metric)
  
  # create character vectors of unique species from each df
  d1spp <- as.character(unique(d1[[species.var]]))
  d2spp <- as.character(unique(d2[[species.var]]))
  
  # ID shared species
  commspp <- intersect(d1spp, d2spp)
  
  # count number not present in d2
  disappear <- length(d1spp)-length(commspp)
  
  # count number that appear in d2
  appear <- length(d2spp)-length(commspp)
  
  # calculate total richness
  totrich <- sum(disappear, appear, length(commspp))
  
  # output based on metric 
  if(metric == "total"){
    output <- (totrich)
  } else {
    if(metric == "appearance"){
      output <- appear
    } else {
      if(metric == "disappearance"){
        output <- disappear
      }
    }
  }
  
  # results
  return(output)
}

#################################################################################################
############################################################################
#Modify codyn::turnover to export total richness values:

calc_totrich <- function(df, time.var, 
                         species.var, 
                         abundance.var, 
                         replicate.var=NA, 
                         metric="total") {
  
  if(is.na(replicate.var)){
    
    # check there unique species x time combinations
    check_single_onerep(df, time.var, species.var)
    
    # calculate turnover
    output <- turnover_allyears(df, time.var, species.var, abundance.var, metric)
    
    
  } else {
    
    # remove unused levels if replicate.var is a factor
    df[replicate.var] <- if(is.factor(df[[replicate.var]])) {
      factor(df[[replicate.var]])
    } else {
      df[replicate.var]
    }
    
    # sort and apply turnover to all replicates
    df <- df[order(df[[replicate.var]]),]
    X <- split(df, df[replicate.var])
    out <- lapply(X, FUN=turnover_allyears, time.var, species.var, abundance.var, metric)
    ID <- unique(names(out))
    out <- mapply(function(x, y) "[<-"(x, replicate.var, value = y) ,
                  out, ID, SIMPLIFY = FALSE)
    output <- do.call("rbind", out)
  }
  
  # results
  row.names(output) <- NULL
  return(as.data.frame(output))
}

turnover_allyears <- function(df, 
                              time.var, 
                              species.var, 
                              abundance.var, 
                              metric=c("total", "disappearance","appearance")) {
  
  # allows partial argument matching
  metric = match.arg(metric) 
  
  # sort and remove 0s
  df <- df[order(df[[time.var]]),]
  df <- df[which(df[[abundance.var]]>0),]
  
  ## split data by year
  templist <- split(df, df[[time.var]])
  
  ## create consecutive pairs of time points
  t1 <- templist[-length(templist)]
  t2 <- templist[-1]
  
  ## calculate turnover for across all time points
  out <- Map(turnover_twoyears, t1, t2, species.var, metric)
  output <- as.data.frame(unlist(out))
  names(output)[1] = metric
  
  ## add time variable column
  alltemp <- unique(df[[time.var]])
  output[time.var] =  alltemp[2:length(alltemp)]
  
  # results
  return(output)
}

turnover_twoyears <- function(d1, d2, 
                              species.var, 
                              metric=c("total", "disappearance","appearance")){
  
  # allows partial argument matching
  metric = match.arg(metric)
  
  # create character vectors of unique species from each df
  d1spp <- as.character(unique(d1[[species.var]]))
  d2spp <- as.character(unique(d2[[species.var]]))
  
  # ID shared species
  commspp <- intersect(d1spp, d2spp)
  
  # count number not present in d2
  disappear <- length(d1spp)-length(commspp)
  
  # count number that appear in d2
  appear <- length(d2spp)-length(commspp)
  
  # calculate total richness
  totrich <- sum(disappear, appear, length(commspp))
  
  # output based on metric 
  if(metric == "total"){
    output <- ((appear+disappear)/totrich)
  } else {
    if(metric == "appearance"){
      output <- appear/totrich
    } else {
      if(metric == "disappearance"){
        output <- disappear/totrich
      }
    }
  }
  
  # results
  return(totrich)
}
