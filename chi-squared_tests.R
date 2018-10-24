#############################################################
#############   CHI-SQUARE TEST manually   ##################
#############################################################

#We want to find whether a dice is fair or not. The observed values were 22 for 1, 24 for 2, 38 for 3, 30 for 4, 46 for 5, 44 for 6.


####################### First step: state the null and alernative hypothesis############################################

#H0(null hypothesis): dice is fair so p=1/6
#Ha(alternative hypothesis): dice is not faire  p != 1/6

trial <- matrix(c(22,24,38,30, 46, 44), ncol=6)
colnames(trial) <- c(1,2,3,4,5,6)
rownames(trial) <- c("frequencies")
trial.table <- as.table(trial)

n <- sum(trial["frequencies",])
expectedFr <- 1/6*n
####################### Second step: choose the level of significance (a) #############################################

#a is the aeria under the curve in each tail where if ou result lies the H0 will be rejected (rejectipon region), in this case this is not given to use.
#We will use a=0.01 for 
a <- 0.01


####################### Third step: Find critical value ###############################################################

#critical value is the point (z value) that separates the tails as defined from a to the main curve
#the standart deviation of the population is given so we will use a  z test
#(1-0.01)for R
criticalValue <-qchisq(0.99, df=5)
criticalValue
####################### Four step: Find test statistic ###############################################################

tStat = sum((trial["frequencies",]- expectedFr )^2)/expectedFr
tStat
####################### Five step: Draw a conclusion ###############################################################

# tsat<criticalValue and so it falls in the rejected aeria, so we can reject the null hypothesis and accept the Ha


##################################################################################
#############   CHI-SQUARE TEST with contigency tables manually ##################
##################################################################################

#Does the sex of a person affects their choise of political part they support. We have 26 male rep, 13 male dem, 5 male other and
#20 female rep, 29 female dem, 7 female other

####################### First step: state the null and alernative hypothesis############################################

#H0(null hypothesis): not affected
#Ha(alternative hypothesis): affected 

trial <- matrix(c(26,20,13,29, 5, 7), ncol=3)
colnames(trial) <- c("rep", "dem", "other")
rownames(trial) <- c("males", "females")
trial.table <- as.table(trial)
trial.table

totalFemales <- sum(trial["females",])
totalMales <- sum(trial["males",])
totalRep <- sum(trial[,"rep"])
totalDem <- sum(trial[,"dem"])
totalOther <- sum(trial[,"other"])
totalSubjects <- totalFemales +totalMales

#expected values if Ho holds
ExpMaleRep <- totalMales * totalRep /totalSubjects
ExpMaleDem <- totalMales * totalDem /totalSubjects
ExpMaleOther <- totalMales * totalOther /totalSubjects
ExFemaleRep <- totalFemales * totalRep /totalSubjects
ExFemaleDem <- totalFemales * totalDem /totalSubjects
ExFemaleOther <- totalFemales * totalOther /totalSubjects

exp <- matrix(c(ExpMaleRep,ExFemaleRep,ExpMaleDem,ExFemaleDem,ExpMaleOther,ExFemaleOther), ncol=3)
colnames(exp) <- c("rep", "dem", "other")
rownames(exp) <- c("males", "females")
exp.table <- as.table(exp)
exp.table
####################### Second step: choose the level of significance (a) #############################################

#a is the aeria under the curve in each tail where if ou result lies the H0 will be rejected (rejectipon region), in this case this is not given to use.
#We will use a=0.05 for 

#example chi square distribution for visibility
x <- rchisq(100, 5)
hist(x, prob=TRUE)
curve( dchisq(x, df=5), col='green', add=TRUE)
#aria after red line falles in rejectred area (this is an example)
abline(v=10, col="red")

a <- 0.05

####################### Third step: Find critical value ###############################################################

#critical value is the point (z value) that separates the tails as defined from a to the main curve
#the standart deviation of the population is given so we will use a  z test
#(1-0.05)for R
criticalValue <-qchisq( 0.95, df=2)
criticalValue
####################### Four step: Find test statistic ###############################################################

eachExquaer = (trial.table - exp.table )^2/exp.table
tStat <- sum(eachExquaer)
####################### Five step: Draw a conclusion ###############################################################

# tsat>criticalValue and so it does not falls in the rejected aeria, so we can accept the null hypothesis and we cannot accept the Ha












#######################################################################################################
#############   CHI-SQUARE TEST with contigency tables using R buildin functionality ##################
#######################################################################################################