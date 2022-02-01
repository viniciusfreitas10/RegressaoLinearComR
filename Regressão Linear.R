library(corrgram)
library(readxl)

cars   
dim(cars)

#Saber a correlação
cor(cars)
#criando o modelo
modelo = lm(speed ~ dist , data = cars)
modelo
#plotando o gráfico 
plot(speed ~ dist, data = cars, xlab = 'Distância',ylab = 'Velocidade', main = 'Carros')
abline(modelo)
#fazer a previsão modo 1
modelo$coefficients
modelo$coefficients[1] + modelo$coefficients[2] * 22
#fazer a previsão modo 2
predict(modelo, data.frame(dist = 20))

#para saber mais informações
summary(modelo)
modelo$coefficients
modelo$residuals
hist(modelo$residuals)
modelo$fitted.values
plot(modelo$fitted.values, cars$dist)

mtcars
dim(mtcars)
cor(mtcars)

plot(mtcars$mpg ~ mtcars$disp, ylab = 'Consumo', xlab = 'Cilindradas', main = 'Carros')
modelo <- lm(mpg ~ disp, data = mtcars)
modelo
summary(modelo)$r.squared
summary(modelo)$adj.r.squared
plot(mpg~disp, data = mtcars, xlab = 'Cilindradas', ylab = 'Consumo', main = 'Carros')
abline(modelo)

predict(modelo, data.frame(disp = 500))

#modelo com mais de uma variável no modelo 
modelo <- lm(mpg ~ disp + hp + cyl, data = mtcars) 
modelo

summary(modelo)$r.squared
summary(modelo)$adj.r.squared             

predict(modelo, data.frame(disp = 200, hp = 100, cyl = 4))             
 
#atividade prática             

df <- read_excel('C:/Users/Intel/Desktop/Formação Cientista De Dados/Dados/regre.xlsx')            
df  
dim(df)

plot(FrqAnual ~ CusInic, data = df)
plot(CusInic ~ FrqAnual, data = df)
modelo <- lm(FrqAnual ~ CusInic, data = df)
modelo             
modelo2
plot(FrqAnual ~CusInic, data = df, ylab = 'Fanquia Anual', xlab = 'Custo inicial', main = 'Previsão de Franquia anual')
abline(modelo)              
 
predict(modelo, data.frame(CusInic = 1800))
