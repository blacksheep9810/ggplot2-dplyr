library(ggplot2)
library(plotly)
titanic <- read.csv("titanic.csv")
View(titanic)

#Set up factors 
titanic$class <- as.factor(titanic$Pclass)
titanic$Survived <- as.factor(titanic$Survived)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

#What is the survival rate? 
ggplot(titanic, aes(x = Survived)) + geom_bar()
#want to know percentages 
prop.table(table(titanic$Survived))

ggplot(titanic, aes(x = Survived)) + 
  theme_light() + 
  geom_bar() + 
  labs (y = "Passenger Count", 
        title = "Titanic Survival Rates")

ggplot(titanic) + geom_bar(aes(x = Sex, fill = Survived), 
position = "dodge") + ggtitle ("Survivor's gender ") + labs(x = "Gender",
                                                         y = "survived")
ggplot(titanic, aes(x = Sex, fill = Survived)) + 
  theme_bw() + 
  geom_bar() + 
  labs(y = "Passenger Count", 
       title = "Titanic Survival Rates by sex")

#facet_wrap - facet the titanic data by pclass 
ggplot(titanic, aes(x = Sex, fill = Survived)) + 
  theme_bw() + 
  facet_wrap(~ Pclass) + 
  geom_bar() + 
  labs( y = "Passenger Count", 
        title = "Titanic Survival Rates by classes")
ggsave("facet_data.png")

#What is the distribution of passenger ages? 
ggplot(titanic, aes(x = Age, fill = Survived)) + 
  theme_bw() + 
  geom_histogram(binwidth = 5 ) + 
  labs( y = "Age", 
        title = "Titanic Survival Rates by Age")

ggplot(titanic, aes(x = Survived, y = Age)) + 
  theme_bw() + 
  geom_boxplot() + 
  labs(y = "Age", x = "Survived", 
       title = "Titanic Survival Rate by Age")

ggplot(titanic, aes(x = Age, fill = Survived)) + 
  theme_bw() + 
  facet_wrap(Sex ~ Pclass) + 
  geom_density(alpha = 0.5) + 
  labs (y = "Age", 
        x = "Survived", 
        title = "Titanic Survival Rates by Age, Pclass and Sex")

#Distribution of passenger's Age and Fare in comparision 
#to Sex and Survival rate
pdf("plot.pdf")
ggplot(titanic, aes(x = Age, y = Fare, color = Age)) + 
  geom_point(shape = 16, size = 2) + 
  facet_wrap(Sex ~ Survived)
  theme_minimal()
print(plot)
dev.off()

