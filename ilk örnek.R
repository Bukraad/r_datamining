
veriseti <- read.csv("https://bahadirfyildirim.com/media/documents/Fin500.csv", na.strings = c("")) #veriseti aktarÄ±lÄ±rken oluÅŸan boÅŸluklarÄ±n silinip yerine NA geliyor 

head(veriseti,15) #verisetinin ilk 15 satÄ±rÄ±nÄ± gÃ¶stermek iÃ§in komut

tail(veriseti,2) #verisetinin son 2 satÄ±rÄ±nÄ± gÃ¶stermelik komut

str(veriseti)    

veriseti$Industry #sadece industry stÃ¼nunu okur

summary(veriseti)

veriseti$Indusrty <- as.factor(veriseti$Industry)

veriseti$Inception <- as.factor(veriseti$Inception)
veriseti$State <- as.factor(veriseti$State)

veriseti$Expenses <- gsub(" Dollars", "", veriseti$Expenses)  #dolar kelimesini atacak Sub'ta ilk gÃ¶rdÃ¼ÄŸÃ¼nÃ¼ arar gsub'ta verilen argÃ¼manÄ± komple arar
veriseti$Expenses <- gsub(",", "", veriseti$Expenses)         #virgÃ¼lÃ¼ sildik
veriseti$Expenses <- as.numeric(veriseti$Expenses)            #numeric veri haline getirdik

veriseti$Growth <- as.numeric(gsub("%", "", veriseti$Growth)) #yukarÄ±daki iki iÅŸlemi bu ÅŸekilde birleÅŸtirebilirsin

veriseti$Revenue <- gsub("\\$","", veriseti$Revenue)          #dolar kodlama deÄŸil gerÃ§ekten dolar iÅŸareti demek

veriseti$Revenue <- as.numeric(gsub(",", "", veriseti$Revenue))
veriseti$City <- as.factor(veriseti$City)

str(veriseti)    

library(Amelia)        #missing value gÃ¶rselleÅŸtirme fonksiyonu
missmap(veriseti)      #gÃ¶rselleÅŸtirme

head(veriseti, 25)
complete.cases(veriseti) #true-false state - veriesti iÃ§indeki verileri true false olarak doldurur. false'lar missing value yada NA. 

dim(veriseti) # 500 satÄ±r 11 sÃ¼tun dimension gÃ¶sterir

veriseti[3,2] #verisetinde 3. satÄ±rÄ±n 2. sÃ¼tunundaki veriyi Ã§eker

veriseti[3,]  #3. satÄ±rÄ±n tamamÄ±nÄ± gÃ¶zlemleme



veriseti[!complete.cases(veriseti),]    #sorgunun baÅŸÄ±na ! koyarsan olumsuzlama demektir !true= false


which(veriseti$Revenue == 9746272)   #which komutu hangi revenue deÄŸerinin 9746272 olduÄŸunu gÃ¶sterir

veriseti[which(veriseti$Revenue == 9746272),] #bu ise hangi veri o sayÄ± ise onun satÄ±rÄ±nÄ± getirir



is.na(veriseti$expenses)  #expenses kolonundaki NA verileri true olarak verilir false olanlar isnot NA

veriseti[is.na(veriseti$expenses), ]  #expenses kÄ±smÄ±nda true olanlarÄ±n -NA olanlarÄ±n yani- gerÃ§ek satÄ±rlarÄ±nÄ± gÃ¶sterir


#missing datalar iÃ§in Ã§Ã¶zÃ¼m yollarÄ± 

#1 remove rows  - veri sonuÃ§larÄ±nÄ± en Ã§ok etkiletecek verileri sadece silmelisin. en son seÃ§enek bu
backup <- veriseti  #verisetini yedekliyor 


veriseti[is.na(veriseti$Industry), ]  #Ä±ndÄ±stry'deki na verileri
veriseti <- veriseti[complete.cases(veriseti), ]  #complete olanlarla yeniden veriseti oluÅŸtur #silinide 488 veri kaldÄ±

veriseti <- backup

veriseti <- veriseti[!is.na(veriseti$Industry),] #is.na olmayan verilerden yeni bir veriseti oluÅŸtur bunu yapÄ±nca 498 veri kaldÄ±


row.names(veriseti) #satÄ±rlara verilen isimler
row.names(veriseti) <- 1:nrow(veriseti)  #1den nrow'a kadar olan veri isimlerini gÃ¼ncelle (Ã§Ã¼nkÃ¼ veri Ã§Ä±karma yaptÄ±k bundan Ã¶nce ise Ã§Ä±karÄ±lan verilerin isimleri atlanmÄ±ÅŸtÄ± 1 2 3 5 gibiydi 4 silindiyse eÄŸer)


#2 kayÄ±p verilerin deÄŸiÅŸtirilmesi #eksik verileri kendin doldurma

head(veriseti, 25) 

veriseti[is.na(veriseti$State) & veriseti$City == "New York", ] #verinin iÃ§inde state'i boÅŸ olan ve city newyork olanlarÄ± getiri

veriseti[is.na(veriseti$State) & veriseti$City == "New York", "State" ] <- "NY" #yukarÄ±dakine ek olarak State sÃ¼tuna NY ekle
veriseti[11,]  # NY eklemiÅŸ mi kontrol etmek iÃ§in ve eklemiÅŸ 

veriseti[is.na(veriseti$State) & veriseti$City == "San Francisco", "State" ] <- "CA"

#3 Median Mean alarak veri doldurma

summary(veriseti$Employees)                   #ortalamasÄ±nÄ± al employe'nin  eÄŸer en bÃ¼yÃ¼k ve en kÃ¼Ã§Ã¼k olan deÄŸerin arasÄ± fazlatsa median almak mantÄ±klÄ±
veriseti[is.na(veriseti$Employees), ]         #NA olanlarÄ± Ã§ek


median (veriseti[, "Employees"], na.rm = T) #NA'leri T yaptÄ±k
med_emp_ret <- median(veriseti[veriseti$Industry == "Retail", "Employees"], na.rm = T) #OrtalamayÄ± kenarda values kÄ±smÄ±na attÄ±

veriseti[is.na(veriseti$Employees) & (veriseti$Industry) == "Retail", "Employees"] <- med_emp_ret
veriseti[3,] #kontrol etmek iÃ§in

med_emp_finservices <- median(veriseti[veriseti$Industry == "Financial Services", "Employees"], na.rm = T)   #bu neden olmadÄ± kontrol et
veriseti[is.na(veriseti$Employees) & veriseti$Industry == "Financial Services" , "Employees"] <- med_emp_finservices 


#revenue iÃ§in sektÃ¶r ortalamasÄ±nÄ± al yaz, expense iÃ§in al yaz yada expenses iÃ§in aldÄ±ÄŸÄ±nÄ± gÃ¶zlemle doldur growth iÃ§in de doldur

veriseti[is.na(veriseti$Revenue), ]  
med_rev_constr <- median(veriseti[veriseti$Industry == "Construction", "Revenue"], na.rm = T) 
veriseti[is.na(veriseti$Revenue) & veriseti$Industry == "Construction", "Revenue"] <- med_rev_constr

veriseti[is.na(veriseti$Expenses), ]
med_exp_constr <- median(veriseti[veriseti$Industry == "Construction", "Expenses"], na.rm = T) 
veriseti[is.na(veriseti$Expenses) & veriseti$Industry == "Construction", "Expenses"] <- med_rev_constr

med_exp_itservices <- median(veriseti[veriseti$Industry == "IT Services", "Expenses"], na.rm = T) 
veriseti[is.na(veriseti$Expenses) & veriseti$Industry == "IT Services", "Expenses"] <- med_exp_itservices

veriseti[is.na(veriseti$Growth), ]
med_growth_constr <- median(veriseti[veriseti$Industry == "Construction", "Growth"], na.rm = T) 
veriseti[is.na(veriseti$Growth) & veriseti$Industry == "Construction", "Growth"] <- med_growth_constr

veriseti[is.na(veriseti$Profit), ]
med_profit_constr <- median(veriseti[veriseti$Industry == "Construction", "Profit"], na.rm = T) 
veriseti[is.na(veriseti$Profit) & veriseti$Industry == "Construction", "Profit"] <- med_profit_constr

summary(veriseti)

veriseti[is.na(veriseti$Profit) & !is.na(veriseti$Revenue), "Profit"] <- veriseti[is.na(veriseti$Profit) & !is.na(veriseti$Revenue), "Revenue"] - veriseti[is.na(veriseti$expenses) & !is.na(veriseti$Revenue), "Expenses"] 
#yukarıda profiti doldurmak için revenue'dan expenses'i çıkardık ve profiti bulduk


veriseti[is.na(veriseti$Profit) , ]  #profitte na olan veri var mı kontrolü 

library(caret)
data(iris)   #üç tip bitki sınıflanmış onun verileri çanakları taç yaprakları falan filan bu veriler üzerinden prediction yapacağız. iris prediction için temel veriseti. İris ile benchmarking yapılıyor insanlar kendi verileriyle doğruluğunu kıyaslıyor
str(iris)

summary(iris[ ,1:4]) #1'den 4'e kadar olan sütunlardaki tüm satır verileri. Veriseti[ ,c(1,3,5)] ise 1. 3. ve 5. kolonları sadece alır

#alttaki kısma  ?preProcess yazıp run ederek preprecess ne işe yarıyor onu okuyabilirsin.


preProcessParams <- preProcess(iris[ ,1:4], method='scale') #scale tüm gözlem değerleri için o serinin standart sapmasına oranlıyor. bunun açıklaması için rmd'de formül şeysi yazdım

print(preProcess)

scaled <- predict(preProcessParams, iris[ ,1:4])

summary(scaled) #scale edilen yeni verisetinin özet bilgileri

#center işlemi her bir gözlem için ortalamayı hesaplıyor ve bu ortalamayı her bir değerden çıkarıyor x{i}-\bar{X}_{x} alt x olmasının nedeni her bir sütunun ortalamasını alıyor

preProcessParams <- preProcess(iris[ ,1:4], method='center')

centered <- predict(preprocessParams, iris[ ,1:4])

preprocessParams <- preProcess(iris[ ,1:4], method='center','scale')
standardized <- predict(preProcessParams, iris[ ,1:4])

#range normalizasyon

preProcessParams <- preProcess(iris[ ,1:4], method='range')

normalized <- predict(preprocessParams, iris[ ,1:4])

summary(normalized)

#boxcox özellikle çarpık verilerde çarpıklığı gideriyor

preProcessParams <- preProcess(iris[ ,1:4], method='BoxCox')
boxcox <- predict(preprocessParams, iris[ ,1:4])

print(preProcessParams)  #boxcox içerisinde uygun lambda değerlerini bulmuştuk boxcox ile bu formül de onu gösteriyor her sütun için ayrı tabii en idealini buldu



















