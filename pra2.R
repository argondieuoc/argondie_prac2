install.packages('VIM')
suppressWarnings(suppressMessages(library(VIM)))

titanic <- read.csv("C:\\Users\\Arturo\\Documents\\Master\\Tipología y ciclo de vida de los datos\\PRA\\PRA2\\train.csv")

#View(titanic)

#Eliminamos las columnas 1,3,8 ya que no nos aporta información relevante.
titanic <- titanic[c(2,3,5,6,7,8,10,11,12)]
#View(titanic)

#Comprobamos valores NULL.
sapply(titanic, function(x) sum(is.na(x)))

#PCLASS
titanic$Pclass[is.na(titanic$Pclass)] <- 4

#SEX
titanic$Sex <- ifelse(titanic$Sex == 'female', 0, 1)

#AGE
titanic$Age <- kNN(titanic)$Age
summary(titanic$Age)

#SIBSP 
titanic$SibSp[is.na(titanic$SibSp)] <- 0
summary(titanic$SibSp)

#PARCH
titanic$Parch[is.na(titanic$Parch)] <- 0
summary(titanic$Parch)

#FARE
unique.data.frame(titanic['Fare'])
#>400 valores. No nos da una información clara de lo que es.

titanic <- titanic[c(1,2,3,4,5,6,8,9)]

#EMBARKED
is.na(titanic$Embarked[titanic$Embarked =='']) <- TRUE
sum(is.na(titanic$Embarked))
titanic$Embarked <- kNN(titanic)$Embarked

#CABIN
titanic$Cabin <- ifelse(titanic$Cabin == '', 0, 1)


#Exportación
#write.csv(titanic,"titanicResults.csv")

#Analisis de los datos

titanic.Status.Survivor <- titanic[titanic$Survived == 1,]
titanic.Status.Dead <- titanic[titanic$Survived == 0,]

titanic.Class.First <- titanic[titanic$Pclass == "1",]
titanic.Class.Second <- titanic[titanic$Pclass == "2",]
titanic.Class.Third <- titanic[titanic$Pclass == "3",]

titanic.Embarked.Southampton <- titanic[titanic$Embarked == "S",]
titanic.Embarked.Cherbourg <- titanic[titanic$Embarked == "C",]
titanic.Embarked.Queenstown <- titanic[titanic$Embarked == "Q",]

titanic.Sex.Male <- titanic[titanic$Sex == 1,]
titanic.Sex.Female <- titanic[titanic$Sex == 0,]

titanic.Cabin.Assigned <- titanic[titanic$Cabin == 1,]
titanic.Cabin.NonAssigned <- titanic[titanic$Cabin == 0,]

#NORMALIDAD
library(nortest)

alpha = 0.05
col.names = colnames(titanic)
for (i in 1:ncol(titanic)) {
  if (i == 1) cat("Variables que no siguen una distribución normal:\n")
  if (is.integer(titanic[,i]) | is.numeric(titanic[,i])) {
    p_val = ad.test(titanic[,i])$p.value
    if (p_val < alpha) {
      cat(col.names[i])
      # Format output
      if (i < ncol(titanic) - 1) cat(", ")
      if (i %% 3 == 0) cat("\n")
    }
  }
}

#VARIANZA
fligner.test(Age ~ Survived, data = titanic)

#COMPARACIÓN DE DATOS ESTADÍSTICOS

totalFirst <- nrow(titanic.Class.First)
firstSurvived <- sum(titanic.Class.First$Survived)
porcentaje1 <- (firstSurvived * 100) / totalFirst

totalSecond <- nrow(titanic.Class.Second)
secondSurvived <- sum(titanic.Class.Second$Survived)
porcentaje2 <- (secondSurvived * 100) / totalSecond

totalThird <- nrow(titanic.Class.Third)
thirdSurvived <- sum(titanic.Class.Third$Survived)
porcentaje3 <- (thirdSurvived * 100) / totalThird

hist(titanic.Class.First$Survived)
hist(titanic.Class.Second$Survived)
hist(titanic.Class.Third$Survived)

totalMale <- nrow(titanic.Sex.Male)
maleSurvived <- sum(titanic.Sex.Male$Survived)
porcentaje4 <- (maleSurvived * 100) / totalMale

totalFemale <- nrow(titanic.Sex.Female)
femaleSurvived <- sum(titanic.Sex.Female$Survived)
porcentaje5 <- (femaleSurvived * 100) / totalFemale


hist(titanic.Sex.Male$Survived)
hist(titanic.Sex.Female$Survived)

hist(titanic.Status.Survivor$Age)
hist(titanic.Status.Dead$Age)

hist(titanic.Embarked.Cherbourg$Survived)
hist(titanic.Embarked.Queenstown$Survived)
hist(titanic.Embarked.Southampton$Survived)