#############################################################
#############   CHI-SQUARE TEST manually   ##################
#############################################################

#We want to find whether a dice is fair or not. The observed values were 22 for 1, 24 for 2, 38 for 3, 30 for 4, 46 for 5, 44 for 6.


####################### First step: state the null and alernative hypothesis############################################

#H0(null hypothesis): dice is fair so p=1/6
#Ha(alternative hypothesis): dice is not faire  p != 1/6

trialDice <- matrix(c(22,24,38,30, 46, 44), ncol=6)
colnames(trialDice) <- c(1,2,3,4,5,6)
rownames(trialDice) <- c("frequencies")
trial.tableDice <- as.table(trial)

n <- sum(trialDice["frequencies",])
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

tStat = sum((trialDice["frequencies",]- expectedFr )^2)/expectedFr
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

#create our contingency table
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
tStat
####################### Five step: Draw a conclusion ###############################################################

# tsat>criticalValue and so it does not falls in the rejected aeria, so we can accept the null hypothesis and we cannot accept the Ha

######################################################################################################
########################## CHI-SQUARE TEST Of INDEPENDANCY IN R ######################################
######################################################################################################

chisq.test(trial.table)
#this returned our x-squared value wich validated our t-statistic and a p-value of 0.05352, which is significant and therefore we can not reject the H0


chisq.test(trial.table)$expected
#this returns the expected values, it validetes the ones we calculated previously on exp.table and it can be used to compere with our actuall values

#to confirm this
Rexpected <- chisq.test(trial.table)$expected
#returns true for all values :) :)
Rexpected == exp.table

#lets make some charts
barplot(trial.table, legend= TRUE, beside = T)

######################################################################################################
########################## CHI-SQUARE GOODNESS OF FIT  IN R ##########################################
######################################################################################################

#we use the dice exaple again
frequeciesGiven<- c(22,24,38,30, 46, 44)
#calculated on top of the page
pForEach<- c(1/6,1/6,1/6,1/6, 1/6, 1/6)

#this validetes our previous results and so it gives a p=0.009177 which is a very small propability for the H0 to be tru, therefore we accept the Ha
chisq.test(frequeciesGiven, p=pForEach)


######################################################################################################
################################ FISHER'S EXACT TESTIN R #############################################
######################################################################################################


#this is used for non-paremetric data (not following a normal distribution)
#legent has it, it was discovered when testing if a lady in the UK chould tell if milk was poured before or after
#so lets take this example. Assume 20 trials out of which the lady gets guesses 9 times correctly that tea was poured before, out of the 10 in which it actally was.
n <- 20
s <- 9
milkBefore <- 10

#there are 4 ways out of 8 to choose the tea that was made with milk before tea
comb = function(n, x) {
  factorial(n) / (factorial(n-x) * factorial(x))
}

totalWays <- comb(n,milkBefore)
totalWays
#the lady got 4, so we need to calculate in how many ways she chould have gotten 4 out of 5
#there are ten ways in 10 orders
waysToGuessSuc <- 10*10
waysToGuessSuc
#if we assume the H0, the propability that she got it corectly at random whould be:
p <- waysToGuessSuc / totalWays
p
# p= 0.02, which is quite small smaller that 0.05 which is usually the threshold so we can reject the H0. According to this triall the lady can, most likly,
#tell whether or not milk was pured before or after the tea

#let's try this using R built in functionality
TeaTasting <-
  matrix(c(9, 1, 1, 9),
         nrow = 2,
         dimnames = list(Guess = c("Milk", "Tea"),
                         Truth = c("Milk", "Tea")))

fisher.test(TeaTasting, alternative = "greater")




