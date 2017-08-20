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

cor.test(jhar$MeanScore,as.numeric(jhar$Education))
cor.test(jhar$MeanScore,as.numeric(jhar$Income))

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

library(dplyr)
Interpersonal_Skills =rowMeans(jhar[,7:29])

mutate(jhar, Accessibility = rowMeans(select(jhar,one:Four, na.rm = TRUE)))


jhar$Interpersonal<-rowMeans(jhar[,8:11])

x = mean(jhar$Interpersonal) + sd(jhar$Interpersonal)
y = mean(jhar$Interpersonal) - sd(jhar$Interpersonal)
jhar = mutate(jhar,Interpersonal_Skills = ifelse(Interpersonal<= 4.25,"Low",ifelse(Interpersonal >= 4.6,"High","Medium")))
summary(as.factor(jhar$Interpersonal_Skills))


jhar$Accessibility<-rowMeans(jhar[,12:16])

summary(jhar$Accessibility)

jhar$Accessibilty_Perception= ifelse(Accessibility< 4,"Low",ifelse(Accessibility >= 4.8,"High","Medium"))
summary(as.factor(jhar$Accessibilty_Perception))


jhar$Physical_Environment<-rowMeans(jhar[,17:20])

summary(jhar$Physical_Environment)

jhar = mutate(jhar,Environment_Perception= ifelse(Physical_Environment< 4,"Low",ifelse(Physical_Environment >= 5,"High","Medium")))
summary(as.factor(jhar$Environment_Perception))

jhar$Availability<-rowMeans(jhar[,21:22])

summary(jhar$Availability)


jhar = mutate(jhar,Availability_Perception= ifelse(Availability< 4,"Low",ifelse(Availability >= 5,"High","Medium")))
summary(as.factor(jhar$Availability_Perception))


jhar$Quality<-rowMeans(jhar[,23:30])

summary(jhar$Quality)


jhar = mutate(jhar,Quality_Perception= ifelse(Quality< 4.25,"Low",ifelse(Quality >= 5,"High","Medium")))
summary(as.factor(jhar$Quality_Perception))


library(ggplot2)

x =ggplot(jhar, aes(Accessibility)) + geom_density(position='dodge')
y = ggplot(jhar, aes(Quality)) + geom_density(position='dodge')
a = ggplot(jhar, aes(Physical_Environment)) + geom_density(position='dodge')
b= ggplot(jhar, aes(Interpersonal)) + geom_density(position='dodge')
c = ggplot(jhar, aes(Availability)) + geom_density(position='dodge')

d= ggplot(jhar, aes(MeanScore)) + geom_density(position='dodge')

summary(jhar$Education)

jhar$Education = ifelse(jhar$Education==10,1,jhar$Education)

hist(jhar$Quality)
boxplot(jhar$Quality)

ggplot(jhar,aes(x=MeanScore,y=as.numeric(Education)))+geom_point()

ggplot(jhar,aes(x=MeanScore,y=as.numeric(Income)))+geom_point()

library(ggjoy)

ggplot(jhar, aes(x = MeanScore, y = as.factor(Education))) + geom_joy()
ggplot(jhar, aes(x = MeanScore, y = as.factor(Income))) + geom_joy()


# https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html

library(gridExtra)

grid.arrange(x,y,a,b,c,d,ncol=3)

library(tidyr)
jhar %>% select(Accessibility,Quality,Physical_Environment,Interpersonal,Availability,MeanScore) %>% rename(Interpersonal_Skill = Interpersonal,Total_AverageScore=MeanScore) %>% gather("Parameter","Score",1:6) %>% ggplot(aes(x = Score, y = as.factor(Parameter),fill=Parameter)) + geom_joy()+
  labs(x = "Score",y = "Domains")

library(tableone)
jhar$Education = as.factor(jhar$Education)
# convert every categorical variable into factor before going intableone



myVars1=c("Age","Sex","Education","Occupation","Income","Accessibility","Availability","Interpersonal","Physical_Environment","Quality","MeanScore")


tab2 <- CreateTableOne(vars = myVars1, data = jhar)
print(tab2,quote = TRUE,noSpaces = TRUE) #https://youtu.be/IZgDKmOC0Wg?t=34

# save print o file, import into libre office by separated as ""

myVars2=c("Age","Sex","Education","Occupation","Income","Accessibility","Availability","Interpersonal","Physical_Environment","Quality")

jhar$Sex = as.factor(jhar$Sex)

tab3 <- CreateTableOne(vars = myVars2, strata = "SatisfactionGroups" , data = jhar)
print(tab3,quote = TRUE,noSpaces = TRUE)

tab3
a=read.delim()

save.image(file="factor.RData")

