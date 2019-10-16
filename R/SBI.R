library(tidyverse)
library(ggformula)
options(scipen = 999)


SimulateProportion <- function(n, x, p, alternative, reps){
 if (length(n)==1){
phat <- x/n
Heads <- c(rep(NA), reps)
for( i in 1:reps){
  Flips <- sample(1:2, prob= c(p, 1-p), size=n, replace=TRUE)
  Heads[i] <- sum(Flips == 1)
}
PropSuccess <- Heads/n
Results <- data.frame(PropSuccess)
hist <- gf_histogram(~PropSuccess, data=Results,fill="blue", color="black") %>%
  gf_labs(title="Null Distribution for Sample Proportion", x="Simulated Proportion", y="Frequency") +
  geom_vline(xintercept=phat, colour="red")

if(alternative=="less"){
pval <- sum(PropSuccess <= phat) /reps} else if (alternative=="greater"){
  pval <- sum(PropSuccess >= phat) /reps} else{
    pval <- sum(abs(PropSuccess - p) >= abs(phat-p)) /reps
    hist <- gf_histogram(~PropSuccess, data=Results,fill="blue", color="black") %>%
      gf_labs(title="Null Distribution for Sample Proportion", x="Simulated Proportion", y="Frequency") +
      geom_vline(xintercept=c(p-abs(p-phat),p+abs(p-phat) ), colour="red")}
}

  if (length(n)==2){
  nsuccess1 <- x[1]
  nfail1 <- n[1]-x[1]
  nsuccess2 <- x[2]
  nfail2 <- n[2]-x[2]

  Group1 <- c(rep(0, nfail1), rep(1, nsuccess1))
  Group2 <- c(rep(0, nfail2), rep(1, nsuccess2))
  MeanDiff <- mean(Group1)-mean(Group2)
  Group <- c(rep("Group1", length(Group1)), rep("Group2", length(Group2)))
  Response <- c( Group1, Group2) #observations
  Data <- data.frame(Group, Response) #Put data into a dataframe structure
  reps <- 1000 #number of repetitions
  Difference <- c(rep("NA", reps))

  #simulate reps repetitions
  for (i in 1:reps){
    #randomly shuffle responses
    Data$Response <- Data$Response[sample(1:nrow(Data))]
    #calculate difference in Group Means
    Difference[i] <- Data %>% filter(Group == "Group1") %>% summarize(mean(Response))-
      Data %>% filter(Group == "Group2") %>% summarize(mean(Response))
  }
  Difference <- as.numeric(as.character((Difference)))  #convert to numeric
  Results <- data.frame(Difference)  #create dataframce with results

  hist <- gf_histogram(~Difference, data=Results, color = "black", fill="blue") %>%
    gf_labs(title="Null Distribution for Differences in Sample Proportion", x="Simulated Difference in Proportions", y="Frequency")
    if(alternative=="less"){
      pval <- sum(Results <= MeanDiff)/reps
      hist <- hist + geom_vline(xintercept=MeanDiff, colour="red")
    } else if (alternative=="greater"){
        pval <- sum(Results >= MeanDiff)/reps
        hist <- hist + geom_vline(xintercept=MeanDiff, colour="red") } else{
          pval <- sum(abs(Results) > abs(MeanDiff))/reps
          hist <- gf_histogram(~Difference, data=Results, color = "black", fill="blue") +
            geom_vline(xintercept=c(MeanDiff, -MeanDiff), colour="red")   #for 1-sided test, get rid of 2nd or 3rd line
        }
  }
  if(length(n)==2){
  list <- list(hist, "Observed Difference in Proportions"=MeanDiff, "Simulation-based p-value"=pval)}else{
  list <- list(hist, "Observed Proportion"=phat, "Simulation-based p-value"=pval)}
  return(list)
  }

###########################################################################################################

SimulateMean <- function(x, y=NULL, mu=0, alternative, reps, paired=FALSE){
  if (is.null(y)){
    Values <- x
    xbar <- mean(Values)
    Values <- Values - (mean(Values)-mu)

    #Take samples of size equal to your sample and calculate sample means
    sampsize <- length(Values)
    xbarsim <- c(rep(NA), reps)
    for( i in 1:reps){
      bt <- Values[sample(1:sampsize, sampsize , replace=TRUE)]
      xbarsim[i] <- mean(bt)
    }
    Results <- data.frame(xbarsim)

    hist <- gf_dhistogram(~xbarsim, data=Results, border=0, fill="blue", color="black") %>%
      gf_labs(title="Null Distribution for Sample Mean", x="Simulated Sample Mean", y="Frequency") +
      geom_vline(xintercept=xbar, colour="red")

    if(alternative=="less"){
      pval <- sum(Results <= xbar)/reps
    } else if (alternative=="greater"){
      pval <- sum(Results >= xbar)/reps} else{
        pval <- sum(abs(Results) >= abs(xbar))/reps
        hist <- hist <- gf_dhistogram(~xbarsim, data=Results, border=0, fill="blue", color="black") %>%
          gf_labs(title="Null Distribution for Sample Mean", x="Simulated Sample Mean", y="Frequency") +
          geom_vline(xintercept=c(mu-abs(xbar-mu), mu+abs(xbar-mu)), colour="red")   #for 1-sided test, get rid of 2nd or 3rd line
      }
  } else if(paired==FALSE){
    Group1 <- x
    Group2 <- y
    MeanDiff <- mean(Group1)-mean(Group2)
    Group <- c(rep("Group1", length(Group1)), rep("Group2", length(Group2)))
    Response <- c( Group1, Group2) #observations
    Data <- data.frame(Group, Response) #Put data into a dataframe structure
    Difference <- c(rep("NA", reps))
      #simulate repetitions
    for (i in 1:reps){
      #randomly shuffle responses
      Data$Response <- Data$Response[sample(1:nrow(Data))]
      #calculate difference in Group Means
    #  Difference[i] <- Data %>% filter(Group == "Group1") %>% summarize(mean(Response))-
    #    Data %>% filter(Group == "Group2") %>% summarize(mean(Response))
      Difference[i] <- mean(Data[Data$Group=="Group1", ]$Response)-mean(Data[Data$Group=="Group2", ]$Response)
    }
    Difference <- as.numeric(as.character((Difference)))  #convert to numeric
    Results <- data.frame(Difference)  #create dataframce with results

    hist <-  gf_histogram(~Difference, data=Results, color = "black", fill="blue") %>%
      gf_labs(title="Null Distribution for Differences in Sample Mean", x="Simulated Difference in Sample Means", y="Frequency")+
      geom_vline(xintercept=MeanDiff, colour="red")
    if(alternative=="less"){
      pval <- sum(Results <= MeanDiff)/reps
      hist <- hist + geom_vline(xintercept=MeanDiff, colour="red")
    } else if (alternative=="greater"){
      pval <- sum(Results >= MeanDiff)/reps
      hist <- hist + geom_vline(xintercept=MeanDiff, colour="red") } else{
        pval <- sum(abs(Results) > abs(MeanDiff))/reps
        hist <- gf_histogram(~Difference, data=Results, color = "black", fill="blue") %>%
          gf_labs(title="Null Distribution for Differences in Sample Proportion", x="Simulated Difference in Sample Means", y="Frequency")+
          geom_vline(xintercept=c(MeanDiff, -MeanDiff), colour="red")   #for 1-sided test, get rid of 2nd or 3rd line
      }
  } else{#for paired data
    Group1 <- x
    Group2 <- y
    MeanDiff <- mean(Group1)-mean(Group2)
    Data <- data.frame(Group1, Group2) #Put data into a dataframe structure
    Data$Diff <- Data$Group1 - Data$Group2 #compute pairwise differences
    Difference <- c(rep("NA", reps))

    #simulate reps repetitions
    for (i in 1:reps){ #paired data
      #randomly shuffle within each pair (i.e. change sign for randomly selected pairs)
      samp <- sample(0:1, replace=TRUE, nrow(Data))
      Data$Diff[samp==1] <- -1*Data$Diff[samp==1]
      #calculate difference in Group Means
      Difference[i] <- mean(Data$Diff)
    }
    Difference <- as.numeric(as.character(Difference))
    Results <- data.frame(Difference)  #create dataframce with results
    hist <-  gf_histogram(~Difference, data=Results, color = "black", fill="blue") %>%
      gf_labs(title="Null Distribution for Differences in Sample Mean", x="Simulated Difference in Sample Means", y="Frequency")+
      geom_vline(xintercept=MeanDiff, colour="red")

    if(alternative=="less"){
      pval <- sum(Results <= MeanDiff)/reps
      hist <- hist + geom_vline(xintercept=MeanDiff, colour="red")
    } else if (alternative=="greater"){
      pval <- sum(Results >= MeanDiff)/reps
      hist <- hist + geom_vline(xintercept=MeanDiff, colour="red") } else{
        pval <- sum(abs(Results) > abs(MeanDiff))/reps
        hist <- gf_histogram(~Difference, data=Results, color = "black", fill="blue") %>%
          gf_labs(title="Null Distribution for Differences in Sample Mean", x="Simulated Difference in Sample Means", y="Frequency")+
          geom_vline(xintercept=c(MeanDiff, -MeanDiff), colour="red")   #for 1-sided test, get rid of 2nd or 3rd line
      }
  }
  if(is.null(y)==FALSE){
    list <- list(hist, "Observed Difference in Sample Means"=MeanDiff, "Simulation-based p-value"=pval)}else{
      list <- list(hist, "Observed Sample Mean"=xbar, "Simulation-based p-value"=pval)}
  return(list)
}
###########################################################################################################

SimulateRegression <- function(data, x, y, reps){
Slopes <- rep(NA, reps)
names(data)[which(names(data)==x)] <- "x"
names(data)[which(names(data)==y)] <- "y"
ObsSlope <- summary(lm(data=data, y ~ x))$coefficients[2]
for (i in 1:reps){
  data$Shuffled <- data$x[sample(1:nrow(data))]
  Slopes[i] <- summary(lm(data=data, y ~ Shuffled))$coefficients[2]
}
Slopesdf <- data.frame(Slopes)
hist <- gf_histogram(~Slopes, data=Slopesdf, fill="blue", color="black") %>%
  gf_labs(title="Null Distribution for Slope", x="Simulated Slope", y="Frequency")+
  geom_vline(xintercept=-ObsSlope, colour="red") +
  geom_vline(xintercept=ObsSlope, colour="red")
  pval <- mean(abs(Slopesdf$Slopes)>=abs(ObsSlope))
  list <- list(hist, "Observed Slope"=ObsSlope,  "Simulation-based p-value"=pval)
  return(list)
}

##########################################################################3

SimulateChiSq <- function(data, x, y, reps){
  ChiSq <- rep(NA, reps)
  names(data)[which(names(data)==x)] <- "x"
  names(data)[which(names(data)==y)] <- "y"
  T <- table(data$y, data$x)
  ObsChiSq <- chisq.test(T)$statistic
  for (i in 1:reps){
    data$Shuffled <- data$x[sample(1:nrow(data))]
    Ts <- table(data$y, data$Shuffled)
    ChiSq[i] <- chisq.test(Ts)$statistic
  }
  ChiSqdf <- data.frame(ChiSq)
  hist <- gf_histogram(~ChiSq, data=ChiSqdf, fill="blue", color="black") %>%
    gf_labs(title="Null Distribution for Chi-Square Statistic", x="Simulated Chi-Squared", y="Frequency")+
    geom_vline(xintercept=ObsChiSq, colour="red")
  pval <- mean(abs(ChiSqdf$ChiSq)>=ObsChiSq)
  list <- list(hist, "Observed Chi-Square Statistic"=ObsChiSq, "Simulation-based p-value"=pval)
  return(list)
  }

SimulateF <- function(data, x, y, reps){
  Fsim <- rep(NA, reps)
  names(data)[which(names(data)==x)] <- "x"
  names(data)[which(names(data)==y)] <- "y"
  ObsF <- summary(lm(data=data, y~x))$fstatistic[1]
  for (i in 1:reps){
    data$Shuffled <- data$x[sample(1:nrow(data))]
    Fsim[i] <- summary(lm(data=data, y~Shuffled))$fstatistic[1]
  }
  Fsimdf <- data.frame(Fsim)
  hist <- gf_histogram(~Fsim, data=Fsimdf, fill="blue", color="black") %>%
    gf_labs(title="Null Distribution for F-Square Statistic", x="Simulated F", y="Frequency")+
    geom_vline(xintercept=ObsF, colour="red")
  pval <- mean(abs(Fsimdf$Fsim)>=ObsF)
  list <- list(hist, "Observed F Statistic"=ObsF, "Simulation-based p-value"=pval)
    return(list)
}
