library(ggplot2)
library(mice)

data = read.table('/home/keir/Downloads/PSTAT/FINALPROJECT/data/Data.csv',quote = "", sep = ',', fill = TRUE, header = T)

#Cleaning data for PCA
data1 = data[-1]
data1 = data1[-29,]
X = as.matrix(data1)

centr <- apply(X,2,mean, na.rm=T)
disp <- apply(X,2,sd, na.rm=T)
pca = prcomp(~.,data=data1, center = centr, scale = disp, na.action = na.omit)
summary(pca)
head(pca)
#PCs have high explanatory power (first 6 explain 90% of variance) but are very difficult to interpret

corelate = cor(data1, use = "complete")
corelate
#From correlation matrix, a few high (>0.75) correlations exist. Food and Journal articles, emissions and energy
#consumption and GDP and research, energy consumption and health expenditure and research, health expenditure and economic freedom
# Thus these 6 variables will be reduced to two in the linear models: Food and energy consumption. Furthermore, since happiness
#is the major variable of interest, all countries with no happiness data will be deleted; additionally
#countries and variables with high numbers of missing values will be deleted.

Happy <- complete.cases(data[, 9])
data2 = data[Happy,]
data2 = data2[1:158,]
info = md.pattern(data2)
data2 = data2[,-19]
data2 = data2[,-17]
data2 = data2[,-16]
data2 = data2[,-13]
data2 = data2[,-10]
data2 = data2[,-8]
data2 = data2[,-6]
data2=data2[,-4]
data3 = data2[,-1]

#Fitting simple lm of happiness on reduced variable set (not shown, full set had no significant associations)
model = lm(Happiness ~.,data=data3, na.action = na.omit)
summary(model)

#Deleting all incomplete countries
newdata = na.omit(data2)
newdata1 = newdata[-1]
newdata1 = newdata1[-5]
scaledata = scale(newdata1)
X1 = as.matrix(newdata1)
centr1 <- apply(X1,2,mean, na.rm=T)
disp1 <- apply(X1,2,sd, na.rm=T)
pca1 = prcomp(~.,data=newdata1,center = centr1, scale = disp1)
summary(pca1)

#The first PC seemed to be related to degree of democracy (eg high loading on democracy, women's 
#empowerment), while the second was more highly loaded on military, population density, and
# (negatively) on food production and biodiversity.
DemocraticDevelopment = pca1$x[,1]
Population = -pca1$x[,2]
Happiness = newdata$Happiness
Democracy = newdata$Democracy

#Graph of countries on PCs, with most and least happy countries labeled
ggplot(newdata1, aes(DemocraticDevelopment,Population, color = Happiness)) + geom_point() +geom_text(aes(label=ifelse(Happiness>7, as.character(newdata$Country), ''), hjust=0, vjust=0))
ggplot(newdata1, aes(DemocraticDevelopment,Population, color = Happiness)) + geom_point() +geom_text(aes(label=ifelse(Happiness<4, as.character(newdata$Country), ''), hjust=0, vjust=0))

#K-means clustering, no very interesting results
cluster5 = kmeans(newdata1,5)
ggplot(newdata1, aes(EnergyConsumption,Happiness, color = cluster5$cluster )) + geom_point()+geom_text(aes(label=ifelse(Happiness>7, as.character(newdata$Country), ''), hjust=0, vjust=0))+scale_colour_gradientn(colours=rainbow(5))

cluster4 = kmeans(newdata1,4)
ggplot(newdata1, aes(EnergyConsumption,Happiness, color = cluster4$cluster )) + geom_point()+geom_text(aes(label=ifelse(Happiness>7, as.character(newdata$Country), ''), hjust=0, vjust=0)) +scale_colour_gradientn(colours=rainbow(4))

cluster6 = kmeans(newdata1,6)
ggplot(newdata1, aes(EnergyConsumption,Happiness, color = cluster6$cluster )) + geom_point()+geom_text(aes(label=ifelse(Happiness>7, as.character(newdata$Country), ''), hjust=0, vjust=0))+scale_colour_gradientn(colours=rainbow(6))


#Quadratic model using the two most significant variables from the linear model
E = newdata$Energy.Consum
D = newdata$Democracy
P = newdata$Population.Density

model1 = lm(Happiness~poly(E,2)+poly(D,2)+poly(P,2), data=newdata)
summary(model1)

corr2=cor(data3, use="complete")

Happiness = data2$Happiness
Suicide = data2$Suicide.Rate
ggplot(data2, aes(Happiness,Suicide, size = data2$Energy.Consum, color = data2$Military.Expenditure)) + geom_point() +geom_text(aes(label=ifelse(Suicide<2.2, as.character(data2$Country), ''), hjust=0, vjust=0))

HighTechExports = data2$High.Tech.Exports
ggplot(data2, aes(Happiness,HighTechExports, size = data2$Energy.Consum, color = data2$Military.Expenditure)) + geom_point() +geom_text(aes(label=ifelse(data2$Energy.Consum>570, as.character(data2$Country), ''), hjust=0, vjust=0))

#Biodiversity seems to have a positive effect on happiness, but having low doesn't have a negative effect
Biodiversity = data2$Biodiversity
ggplot(data2, aes(Happiness,Biodiversity, size = data2$Energy.Consum, color = data2$Food)) + geom_point() +geom_text(aes(label=ifelse(Biodiversity>50, as.character(data2$Country), ''), hjust=0, vjust=0))+scale_colour_gradientn(colours=rainbow(5))

PopulationDensity = data2$Population.Density
ggplot(data2, aes(Happiness,PopulationDensity, size = data2$Energy.Consum, color = data2$Food)) + geom_point() +geom_text(aes(label=ifelse(Biodiversity>50, as.character(data2$Country), ''), hjust=0, vjust=0))


#Constructing dataset with no happiness score
Unhappy = !Happy
data4 = data[Unhappy,]
data4 = data4[,-19]
data4 = data4[,-17]
data4 = data4[,-16]
data4 = data4[,-13]
data4 = data4[,-10]
data4 = data4[,-8]
data4= data4[,-6]
data4=data4[,-4]

info1 = md.pattern(data4)

hap = predict(model, data4)
#Model predicts Cabo Verde has happiness 5.4; Fiji,4.9;Gambia,3.9;Guyana,5.0.
#An alternative happiness report gives Guayana 51/100, or 5.1 out of 10, so this model was correct
#for Guyana. According to a WIN/Gallup poll, Fiji would rank 9.3/10, so its prediction is off.

df = data.frame(data3$Biodiversity,data3$Energy.Consum,data3$Population.Density,data3$Food)
names(df) = c("Biodiversity","Energy.Consum", "Population.Density","Food")
model2 = lm(Happiness~., df)
summary(model2)
new=data.frame(data4$Country,data4$Biodiversity,data4$Energy.Consum,data4$Population.Density,data4$Food)
names(new) = c("Country","Biodiversity","Energy.Consum", "Population.Density","Food")
hap1=predict.lm(model2,new, na.action = na.omit)
#The reduced model predicts a high happiness score for Brunei (6.6)
#which has a score of 7.6 in life satisfaction in the Happy Planet Index (HPI)
#Gambia is predicted to have thelow score of 4.7, which is one lower the HPI
#measuremnt of 5.7. Other values seem to lag behind the HPI score by about one,
#which could be an artifact of the survery methods or the model.