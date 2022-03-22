veriseti <- read.csv("fin500.csv")

dataset <- read.csv("https://bahadirfyildirim.com/media/documents/Fin500.csv")

head(veriseti,15) #verisetinin ilk 15 satırını göstermek için komut

tail(veriseti,2) #verisetinin son 2 satırını göstermelik komut

str(veriseti)    

veriseti$Industry #sadece industry stünunu okur

summary(veriseti)

veriseti$Indusrty <- as.factor(veriseti$Industry)

veriseti$Inception <- as.factor(veriseti$Inception)
veriseti$State <- as.factor(veriseti$State)

veriseti$Expenses <- gsub(" Dollars", "", veriseti$Expenses)  #dolar kelimesini atacak Sub'ta ilk gördüğünü arar gsub'ta verilen argümanı komple arar
veriseti$Expenses <- gsub(",", "", veriseti$Expenses)         #virgülü sildik
veriseti$expenses <- as.numeric(veriseti$Expenses)            #numeric veri haline getirdikg

veriseti$Growth <- as.numeric(gsub("%", "", veriseti$Growth)) #yukarıdaki iki işlemi bu şekilde birleştirebilirsin

veriseti$Revenue <- gsub("\\$","", veriseti$Revenue)          #dolar kodlama değil gerçekten dolar işareti demek

veriseti$Revenue <- as.numeric(gsub(",", "", veriseti$Revenue))
veriseti$City <- as.factor(veriseti$City)

