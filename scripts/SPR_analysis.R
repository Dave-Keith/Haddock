

library(fishmethods)
library(tidyverse)


# First bring in the raw data

dat.num <- readxl::read_xlsx("D:/Github/Haddock/Data/data_for_SPR.xlsx",sheet = "Num",col_types = c(rep('numeric',17),rep("guess",3)))
dat.num <- as.data.frame(dat.num)
dat.wa <- readxl::read_xlsx("D:/Github/Haddock/Data/data_for_SPR.xlsx",sheet = "WA",col_types = c(rep('numeric',17),rep("guess",3)))
dat.wa <- as.data.frame(dat.wa)
dat.m <- readxl::read_xlsx("D:/Github/Haddock/Data/data_for_SPR.xlsx",sheet = "M",col_types = c(rep('numeric',17),rep("guess",3)))
dat.m <- as.data.frame(dat.m)
dat.mat <- readxl::read_xlsx("D:/Github/Haddock/Data/data_for_SPR.xlsx",sheet = "maturity",col_types = c(rep('numeric',17),rep("guess",3)))
dat.mat <- as.data.frame(dat.mat)


# Now get the Recent/Pre-1993 in these...
dat.num$Period <- "Recent"
dat.wa$Period <- "Recent"
dat.m$Period <- "Recent"
dat.mat$Period <- "Recent"
# And everything before 1993...
dat.num$Period[dat.num$Year < 1993] <- "Pre 1993"
dat.wa$Period[dat.wa$Year < 1993] <- "Pre 1993"
dat.m$Period[dat.m$Year < 1993] <- "Pre 1993"
dat.mat$Period[dat.mat$Year < 1993] <- "Pre 1993"

# Now I want to take the above and get means of the time series for each Period and for each stock, the last bit replaces NaN's with NA's, noice!
num.period <- dat.num %>% dplyr::group_by(Location,Species,Period) %>% dplyr::summarise(A0 = mean(A0,na.rm=T),
                                                                                        A1 = mean(A1,na.rm=T),
                                                                                        A2 = mean(A2,na.rm=T),
                                                                                        A3 = mean(A3,na.rm=T),
                                                                                        A4 = mean(A4,na.rm=T),
                                                                                        A5 = mean(A5,na.rm=T),
                                                                                        A6 = mean(A6,na.rm=T),
                                                                                        A7 = mean(A7,na.rm=T),
                                                                                        A8 = mean(A8,na.rm=T),
                                                                                        A9 = mean(A9,na.rm=T),
                                                                                        A10 = mean(A10,na.rm=T),
                                                                                        A11= mean(A11,na.rm=T),
                                                                                        A12= mean(A12,na.rm=T),
                                                                                        A13 = mean(A13,na.rm=T),
                                                                                        A14 = mean(A14,na.rm=T),
                                                                                        A15 = mean(A15,na.rm=T))  %>% dplyr::mutate_all(~replace(., is.nan(.), NA))

mat.period <- dat.mat %>% dplyr::group_by(Location,Species,Period) %>% dplyr::summarise(A0 = mean(A0,na.rm=T),
                                                                                        A1 = mean(A1,na.rm=T),
                                                                                        A2 = mean(A2,na.rm=T),
                                                                                        A3 = mean(A3,na.rm=T),
                                                                                        A4 = mean(A4,na.rm=T),
                                                                                        A5 = mean(A5,na.rm=T),
                                                                                        A6 = mean(A6,na.rm=T),
                                                                                        A7 = mean(A7,na.rm=T),
                                                                                        A8 = mean(A8,na.rm=T),
                                                                                        A9 = mean(A9,na.rm=T),
                                                                                        A10 = mean(A10,na.rm=T),
                                                                                        A11= mean(A11,na.rm=T),
                                                                                        A12= mean(A12,na.rm=T),
                                                                                        A13 = mean(A13,na.rm=T),
                                                                                        A14 = mean(A14,na.rm=T),
                                                                                        A15 = mean(A15,na.rm=T))  %>% dplyr::mutate_all(~replace(., is.nan(.), NA))

m.period <- dat.m %>% dplyr::group_by(Location,Species,Period) %>% dplyr::summarise(A0 = mean(A0,na.rm=T),
                                                                                        A1 = mean(A1,na.rm=T),
                                                                                        A2 = mean(A2,na.rm=T),
                                                                                        A3 = mean(A3,na.rm=T),
                                                                                        A4 = mean(A4,na.rm=T),
                                                                                        A5 = mean(A5,na.rm=T),
                                                                                        A6 = mean(A6,na.rm=T),
                                                                                        A7 = mean(A7,na.rm=T),
                                                                                        A8 = mean(A8,na.rm=T),
                                                                                        A9 = mean(A9,na.rm=T),
                                                                                        A10 = mean(A10,na.rm=T),
                                                                                        A11= mean(A11,na.rm=T),
                                                                                        A12= mean(A12,na.rm=T),
                                                                                        A13 = mean(A13,na.rm=T),
                                                                                        A14 = mean(A14,na.rm=T),
                                                                                        A15 = mean(A15,na.rm=T))  %>% dplyr::mutate_all(~replace(., is.nan(.), NA))

wa.period <- dat.wa %>% dplyr::group_by(Location,Species,Period) %>% dplyr::summarise(A0 = mean(A0,na.rm=T),
                                                                                    A1 = mean(A1,na.rm=T),
                                                                                    A2 = mean(A2,na.rm=T),
                                                                                    A3 = mean(A3,na.rm=T),
                                                                                    A4 = mean(A4,na.rm=T),
                                                                                    A5 = mean(A5,na.rm=T),
                                                                                    A6 = mean(A6,na.rm=T),
                                                                                    A7 = mean(A7,na.rm=T),
                                                                                    A8 = mean(A8,na.rm=T),
                                                                                    A9 = mean(A9,na.rm=T),
                                                                                    A10 = mean(A10,na.rm=T),
                                                                                    A11= mean(A11,na.rm=T),
                                                                                    A12= mean(A12,na.rm=T),
                                                                                    A13 = mean(A13,na.rm=T),
                                                                                    A14 = mean(A14,na.rm=T),
                                                                                    A15 = mean(A15,na.rm=T))  %>% dplyr::mutate_all(~replace(., is.nan(.), NA))

species <- unique(num.period$Species)
n.species <- length(species)
locations <- unique(num.period$Location)
n.locations <- length(locations)
periods <- unique(num.period$Period)
n.periods <- length(periods)
count <- 0

res <- data.frame(Species = rep(NA,32),Location = rep(NA,32),Period = rep(NA,32),SBPR = rep(NA,32),M.avg = rep(NA,32))
for(i in 1:n.species)
{
  for(j in 1:n.locations)
  {
    for(k in 1:n.periods)
    {
      count = count+1
      if(locations[j] != "Irish Sea")
      {
      # Massage our data how we need it.
      num.tmp <- num.period %>% dplyr::filter(Species == species[i],Location == locations[j],Period == periods[k])
      num.tmp <- num.tmp[as.logical(!is.na(num.tmp))][-(1:3)] %>% as.numeric()
      wa.tmp <- wa.period %>% dplyr::filter(Species == species[i],Location == locations[j],Period == periods[k]) 
      # Grab the ages before I strip them off
      ages <- substr(names(wa.tmp[as.logical(!is.na(wa.tmp))][-(1:3) ]),2,3) %>% as.numeric() 
      wa.tmp <- wa.tmp[as.logical(!is.na(wa.tmp))][-(1:3) ]%>% as.numeric()
      m.tmp <- m.period %>% dplyr::filter(Species == species[i],Location == locations[j],Period == periods[k]) 
      m.tmp <- m.tmp[as.logical(!is.na(m.tmp))][-(1:3)]%>% as.numeric()
      mat.tmp <- mat.period %>% dplyr::filter(Species == species[i],Location == locations[j],Period == periods[k]) 
      mat.tmp <- mat.tmp[as.logical(!is.na(mat.tmp))][-(1:3)]%>% as.numeric()
      
      #Do the first step of the calculation
     
      tst <- sbpr(age=ages,
                  ssbwgt=wa.tmp,
                  partial=rep(1,length(ages)),
                  pmat=mat.tmp,
                  M=m.tmp, # This can just be one value if m assumed fixed.
                  pF=0, pM=0.5,MSP=30,plus=T,oldest =max(ages), maxF=1, incrF=0.1)	
      
      
      if(locations[j] != "Eastern Georges Bank")
      {
        if(locations[j] == "Icelandic" & species[i] == "Haddock")
        {
          M.avg <- 0.2
        } else {M.avg <- sum(num.tmp*m.tmp)/sum(num.tmp)}
      } 
      
        if(locations[j] == "Eastern Georges Bank") { M.avg <- 0.2 }
        spr <- tst$F_vs_SPR$SPR[1]
        res$SBPR[count]      <- spr
        res$M.avg[count]     <- 1-exp(-M.avg)
      }
      res$Species[count]  <- species[i]
      res$Location[count] <- locations[j]
      res$Period[count]   <- periods[k]
    }
  }
}
# Something screwy with logicals above for Icelandic Haddock, so I'll be lazy...
res$M.avg[res$Species=="Haddock" & res$Location == "Iceland"] <- 1-exp(-0.2)


saveRDS(res,"D:/Github/Haddock/Data/spr_results.rds")

# So here is the final calculation to do...
alpha.cod.stan <- log(alpha.cod*spr.cod*m.cod)
alpha.had.stan <- log(alpha.had*spr.had*m.had)



# An annotated version of the above to walk through how it works.
sbpr.dk <- function (age = NULL, ssbwgt = NULL, partial = NULL, pmat = pmat, 
                     M = NULL, pF = NULL, pM = NULL, MSP = 40, plus = FALSE, oldest = NULL, 
                     maxF = 2, incrF = 1e-04, graph = TRUE) 
{
  # Just some tidy up to make sure you have data..
  if (is.null(age)) 
    stop("age vector is missing")
  if (is.null(ssbwgt)) 
    stop(" ssbwgt vector is missing.")
  if (is.null(partial)) 
    stop("partial recruitment vector is missing.")
  if (is.null(pmat)) 
    stop("pmat vector is missing.")
  if (is.null(M)) 
    stop("M value or vector is missing")
  if (is.null(pF)) 
    stop("pF value is missing.")
  if (is.null(pM)) 
    stop("pM value is missing.")
  if (plus == TRUE & is.null(oldest)) 
    stop("oldest must be specified for plus group calculation.")
  if (any(length(age) != c(length(age), length(ssbwgt), length(partial), 
                           length(pmat)))) 
    stop("Length of vectors unequal")
  # If only one M, make M the same for all ages and add it to the data
  if (length(M) == 1) 
    M <- rep(M, length(age))
  data <- as.data.frame(cbind(age, ssbwgt, partial, M, pmat, 
                              pF, pM))
  SPR <- as.data.frame(cbind(rep(NA, ceiling(maxF/incrF) + 
                                   1), rep(NA, ceiling(maxF/incrF) + 1)))
  names(SPR) <- c("F", "SPR")
  if (plus == TRUE) {
    len <- oldest - min(data$age) + 1
    if (oldest > max(data$age)) {
      pdata <- data[rep(length(data$age), times = oldest - 
                          data$age[length(data$age)]), ]
      pdata$age <- seq(max(data$age) + 1, oldest, 1)
      data <- rbind(data, pdata)
    }
  }
  if (plus == FALSE) 
    len <- max(data$age) - min(data$age) + 1
  F <- 0
  # Loop through all the F scenarios 
  for (i in 1:length(SPR$F)) {
    # So this is the survivorship for each age class... Z, this is getting a survivorship just before spawning, exponent for N_ts = N_t in Gabriel
    data$SB <- exp(-(data$partial * data$pF * F + data$pM * 
                       data$M))
    # This is cumulative survival of cohort, I believe this is at start of each year as it doesn't account for the proportion of the year (pF and pM), 
    # The exponent part is from N_t = N_(t-1) in Gabriel, taking the cumulative product gets us to the proportion of the population left in the corhort
    data$S <- cumprod(exp(-(data$partial * F + data$M)))
    # This loops just makes a vector that is S offset by an age class, 
    # so this sets us up to know what proportion of the population in each age class is kicking around at the start of the year
    data$psb[1] <- 1
    for (y in 2:len) {
      data$psb[y] <- data$S[y - 1]
    }
    # So the spawner biomass per recruit in each age class is proportion of the age class left at start of the year, * by proportion that dies before spawning 
    #  times the average individual weight in age class * proprotion mature in age class
    # This actually makes sense :-)
    data$SPR <- data$psb * data$SB * data$ssbwgt * data$pmat
    # Then you add that up for each age class and you have the lifetime contribution of Recruits for each spawner
    SPR$SPR[i] <- sum(data$SPR)
    SPR$F[i] <- F
    F <- F + incrF
  }
  browser()
  SPR$PSPR <- SPR$SPR/SPR$SPR[1] * 100 # This then standardizes this so that when F = 0 that represents the maximum that SPR can ever be.
  sss <- NULL
  # Now we have this optimizer that will figure out what F is at X% (default is 40) of the maximum Spawner potential and will spit out a table of F vs SPR values
  getF <- function(x) {
    data$SB <- exp(-(data$partial * data$pF * x + data$pM * 
                       data$M))
    data$S <- cumprod(exp(-(data$partial * x + data$M)))
    data$psb[1] <- 1
    for (y in 2:len) {
      data$psb[y] <- data$S[y - 1]
    }
    data$SPR <- data$psb * data$SB * data$ssbwgt * data$pmat
    sss <<- sum(data$SPR)
    # Here is the optimization, this returns the value of SPR that minimizes the squared difference between PSPR and the MSP you asked for in the original function call
    return(((sum(data$SPR)/SPR$SPR[1] * 100) - MSP)^2)
  }
  Fsp <- optimize(getF, c(0, maxF), tol = 1e-07)[1]
  ans <- NULL
  ans <- matrix(NA, 1L, 2L)
  ans <- rbind(cbind(Fsp, sss))
  dimnames(ans) <- list(c(paste("F at ", MSP, "%MSP", 
                                sep = "")), c("F", "SSB_Per_Recruit"))
  outpt <- list(ans, SPR)
  names(outpt) <- c("Reference_Point", "F_vs_SPR")
  if (graph == TRUE) {
    par(mfrow = c(1, 2))
    plot(SPR[, 2] ~ SPR[, 1], ylab = "SPR", xlab = "F", 
         type = "l")
    plot(SPR[, 3] ~ SPR[, 1], ylab = "% Max SPR", xlab = "F", 
         type = "l")
  }
  return(outpt)
}