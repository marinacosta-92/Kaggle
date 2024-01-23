#Desafio Kaggle - Titanic

#Instalando pacotes
install.packages("rpart")

#Criando base de treino
treino <- read.csv("train.csv")

#Verificando base
summary(treino)

#Removendo os NA's da base
treino <-na.omit(treino)

#Criando modelo
set.seed(1)

arvore <- rpart::rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                       data = treino, #base
                       parms = list(split='gini'), # splitting?
                       method = 'class' # resposta qualitativa
)

#Visualizando a árvore
rpart.plot::rpart.plot(arvore)

#Avaliando o modelo

#Gerando a previsão de sobrevivência
previsao=predict(arvore,treino)

#Classificação dos sobreviventes - probabilidade >0.5 sobreviveu
classificacao = previsao [,2]>.5

#Matriz de confusão
matriz <- table(classificacao,treino$Survived)
matriz

#Calculando acurácia
acuracia <- (matriz[1,1]+matriz[2,2])/sum(matriz)

#Gerando resultado para entrega
teste<-read.csv("test.csv")
prev_resultado=predict(arvore,teste)
class_resultado=as.integer(prev_resultado[,2]>.5)
resultado<- data.frame(PassengerId=teste$PassengerId,Survived=class_resultado)
write.csv(resultado,"C:\\Users\\eduar\\Documents\\Fotos Marina\\Projetos R\\Titanic\\resultado.csv",row.names=FALSE)
