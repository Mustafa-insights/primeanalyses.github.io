# Loading the data
camera <- read.csv('C:/Users/musta/Desktop/Coursework/Statistical Inference/R assignment/Nikon.csv')

# Data structure
str(camera)
summary(camera)

hist(camera$Price_.)
qqnorm(camera$Price_.)
boxplot(camera$Price_.)
ks.test(camera$Price_.,'pnorm')
shapiro.test(camera$Price_.)

hist(camera$Megapixels)
qqnorm(camera$Megapixels)
boxplot(camera$Megapixels)
ks.test(camera$Megapixels,'pnorm')
shapiro.test(camera$Megapixels)

hist(camera$Weight_oz)
qqnorm(camera$Weight_oz)
boxplot(camera$Weight_oz)
ks.test(camera$Weight_oz,'pnorm')
shapiro.test(camera$Weight_oz)

hist(camera$Score)
qqnorm(camera$Score)
boxplot(camera$Score)
ks.test(camera$Score,'pnorm')
shapiro.test(camera$Score)

camera1 <- camera[ ,-c(1,2,7)] #removing non-numeric variables
pairs(camera1, main = 'Scatterplot Matrix')

#Part d - 3)
cor_matrix <- cor(camera1)
cor_matrix

install.packages("psych")
library("psych")

cor_test_mat <- corr.test(camera1)$p
cor_test_mat

m1 <- lm(Price_. ~ Megapixels, data=camera1)

m2 <- lm(Price_. ~ Megapixels + Weight_oz, data=camera1)

m3 <- lm(Price_. ~ Megapixels + Weight_oz + Score, data=camera1)

summary(m1)
summary(m3)
qqnorm(m1$residuals)
qqnorm(m3$residuals)

summary(m2)
summary(m3)
qqnorm(m2$residuals)
qqnorm(m3$residuals)

camera$Nikon <- ifelse(camera$Brand_code == 0,0,1)

m4 <- lm(Price_. ~ Weight_oz + Score + Nikon, data=camera)

nikon_df <- subset(camera, Brand=='Nikon')
canon_df <- subset(camera, Brand=='Canon')

m4_nikon <- lm(Price_. ~ Weight_oz + Score + Nikon, data=nikon_df)
m4_canon <- lm(Price_. ~ Weight_oz + Score + Nikon, data=cannon_df)

plot(m4_nikon)
plot(m4_canon)

new_data <- data.frame(
  Brand = c("Canon", "Canon", "Nikon", "Nikon"),
  Price_ = c(100, 90, 270, 300),
  Megapixels = c(10, 12, 16, 16),
  Weight_oz = c(6, 7, 5, 7),
  Score = c(51, 46, 65, 63),
  Brand_code = c(1, 1, 0, 0)
)

pred_m1 <- predict(m1, new_data)  
pred_m2 <- predict(m2, new_data)  
pred_m3 <- predict(m3, new_data)

new_data1 <- new_data
new_data1$Nikon <- ifelse(new_data1$Brand_code == 0,0,1)
pred_m4 <- predict(m4, new_data1)

#Error values
error_m1 <- new_data$Price_ - pred_m1
error_m1

error_m2 <- new_data$Price_ - pred_m2
error_m2

error_m3 <- new_data$Price_ - pred_m3
error_m3

error_m4 <- new_data1$Price_ - pred_m4
error_m4
