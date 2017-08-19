jhar = jhajjar

summary(jhar)

library(dplyr)



jhar$Seven = ifelse(jhar$Seven>5,5,jhar$Seven)
jhar$Ten = ifelse(jhar$Ten>5,5,jhar$Ten)

summary(jhar$Seven)

library(tableone)
jhar$SatisfactionGroups = factor(jhar$SatisfactionGroups,levels=c("Low","Medium","High"))
jhar$Education = as.factor(jhar$Education)
jhar$Occupation = as.factor(jhar$Occupation)
jhar$Sex = as.factor(jhar$Sex)
jhar$Income = as.factor(jhar$Income)

myVars=c("Age","Sex","Education","Occupation","Income","one", "Two", "Three", "Four", "Five", "Six", "Seven", 
         "Eight", "Nine", "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", 
         "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen", "Twenty", 
         "Twenty.one", "twenty.two", "Twenty.three")


tab1 <- CreateTableOne(vars = myVars, strata = "SatisfactionGroups" , data = jhar)
tab1

dput(names(jhar))



newdata = jhar %>% dplyr::select(one:Twenty.three)
summary(newdata$Ten)
newdata = as.matrix(newdata)
jharu = newdata
library(nFactors)

ev <- eigen(cor(jharu))# get eigenvalues
ev
ap <- parallel(subject=nrow(newdata),var=ncol(newdata),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
nS # Sisplays number of components retained by various methods kaiser(Eigenvalues>mean,Parallel analysis,optimal coordinates and Acceleration Factor)
summary(nS)
plotnScree(nS)

# Ggally 
library(GGally)


ggcorr(newdata, palette = "RdBu", label = TRUE)
library(MASS)
library(dplyr)

new = jhar%>%dplyr::select(one:Twenty.three) # Select Mask with MASS https://stackoverflow.com/questions/24202120/dplyrselect-function-clashes-with-massselect
ggpairs(new, columns = 1:ncol(new), title = "Correlation Matrix",  
        axisLabels = "show", columnLabels = colnames(new[, columns]))

ggpairs(new)

summary(jhar$Nine)