
veriseti <- read.csv("https://bahadirfyildirim.com/media/documents/Fin500.csv", na.strings = c("")) #veriseti aktarılırken oluşan boşlukların silinip yerine NA geliyor 

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
veriseti$Expenses <- as.numeric(veriseti$Expenses)            #numeric veri haline getirdik

veriseti$Growth <- as.numeric(gsub("%", "", veriseti$Growth)) #yukarıdaki iki işlemi bu şekilde birleştirebilirsin

veriseti$Revenue <- gsub("\\$","", veriseti$Revenue)          #dolar kodlama değil gerçekten dolar işareti demek

veriseti$Revenue <- as.numeric(gsub(",", "", veriseti$Revenue))
veriseti$City <- as.factor(veriseti$City)

str(veriseti)    

library(Amelia)        #missing value görselleştirme fonksiyonu
missmap(veriseti)      #görselleştirme

head(veriseti, 25)
complete.cases(veriseti) #true-false state - veriesti içindeki verileri true false olarak doldurur. false'lar missing value yada NA. 

dim(veriseti) # 500 satır 11 sütun dimension gösterir

veriseti[3,2] #verisetinde 3. satırın 2. sütunundaki veriyi çeker

veriseti[3,]  #3. satırın tamamını gözlemleme



veriseti[!complete.cases(veriseti),]    #sorgunun başına ! koyarsan olumsuzlama demektir !true= false


which(veriseti$Revenue == 9746272)   #which komutu hangi revenue değerinin 9746272 olduğunu gösterir

veriseti[which(veriseti$Revenue == 9746272),] #bu ise hangi veri o sayı ise onun satırını getirir



is.na(veriseti$expenses)  #expenses kolonundaki NA verileri true olarak verilir false olanlar isnot NA

veriseti[is.na(veriseti$expenses), ]  #expenses kısmında true olanların -NA olanların yani- gerçek satırlarını gösterir


#missing datalar için çözüm yolları 

#1 remove rows  - veri sonuçlarını en çok etkiletecek verileri sadece silmelisin. en son seçenek bu
backup <- veriseti  #verisetini yedekliyor 


veriseti[is.na(veriseti$Industry), ]  #ındıstry'deki na verileri
veriseti <- veriseti[complete.cases(veriseti), ]  #complete olanlarla yeniden veriseti oluştur #silinide 488 veri kaldı

veriseti <- backup

veriseti <- veriseti[!is.na(veriseti$Industry),] #is.na olmayan verilerden yeni bir veriseti oluştur bunu yapınca 498 veri kaldı


row.names(veriseti) #satırlara verilen isimler
row.names(veriseti) <- 1:nrow(veriseti)  #1den nrow'a kadar olan veri isimlerini güncelle (çünkü veri çıkarma yaptık bundan önce ise çıkarılan verilerin isimleri atlanmıştı 1 2 3 5 gibiydi 4 silindiyse eğer)


#2 kayıp verilerin değiştirilmesi #eksik verileri kendin doldurma

head(veriseti, 25) 

veriseti[is.na(veriseti$State) & veriseti$City == "New York", ] #verinin içinde state'i boş olan ve city newyork olanları getiri

veriseti[is.na(veriseti$State) & veriseti$City == "New York", "State" ] <- "NY" #yukarıdakine ek olarak State sütuna NY ekle
veriseti[11,]  # NY eklemiş mi kontrol etmek için ve eklemiş 

veriseti[is.na(veriseti$State) & veriseti$City == "San Francisco", "State" ] <- "CA"

#3 Median Mean alarak veri doldurma

summary(veriseti$Employees)                   #ortalamasını al employe'nin  eğer en büyük ve en küçük olan değerin arası fazlatsa median almak mantıklı
veriseti[is.na(veriseti$Employees), ]         #NA olanları çek


median (veriseti[, "Employees"], na.rm = T) #NA'leri T yaptık
med_emp_ret <- median(veriseti[veriseti$Industry == "Retail", "Employees"], na.rm = T) #Ortalamayı kenarda values kısmına attı

veriseti[is.na(veriseti$Employees) & (veriseti$Industry) == "Retail", "Employees"] <- med_emp_ret
veriseti[3,] #kontrol etmek için

med_emp_finservices <- median(veriseti[veriseti$Industry == "Financial Services", "Employees"], na.rm = T)   #bu neden olmadı kontrol et
veriseti[is.na(veriseti$Employees) & veriseti$Industry == "Financial Services" , "Employees"] <- med_emp_finservices 


#revenue için sektör ortalamasını al yaz, expense için al yaz yada expenses için aldığını gözlemle doldur growth için de doldur

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
#yukar�da profiti doldurmak i�in revenue'dan expenses'i ��kard�k ve profiti bulduk


veriseti[is.na(veriseti$Profit) , ]  #profitte na olan veri var m� kontrol� 

library(caret)
data(iris)   #�� tip bitki s�n�flanm�� onun verileri �anaklar� ta� yapraklar� falan filan bu veriler �zerinden prediction yapaca��z. iris prediction i�in temel veriseti. �ris ile benchmarking yap�l�yor insanlar kendi verileriyle do�rulu�unu k�yasl�yor
str(iris)

summary(iris[ ,1:4]) #1'den 4'e kadar olan s�tunlardaki t�m sat�r verileri. Veriseti[ ,c(1,3,5)] ise 1. 3. ve 5. kolonlar� sadece al�r

#alttaki k�sma  ?preProcess yaz�p run ederek preprecess ne i�e yar�yor onu okuyabilirsin.


preProcessParams <- preProcess(iris[ ,1:4], method='scale') #scale t�m g�zlem de�erleri i�in o serinin standart sapmas�na oranl�yor. bunun a��klamas� i�in rmd'de form�l �eysi yazd�m

print(preProcess)

scaled <- predict(preProcessParams, iris[ ,1:4])

summary(scaled) #scale edilen yeni verisetinin �zet bilgileri

#center i�lemi her bir g�zlem i�in ortalamay� hesapl�yor ve bu ortalamay� her bir de�erden ��kar�yor x{i}-\bar{X}_{x} alt x olmas�n�n nedeni her bir s�tunun ortalamas�n� al�yor

preProcessParams <- preProcess(iris[ ,1:4], method='center')

centered <- predict(preprocessParams, iris[ ,1:4])

preprocessParams <- preProcess(iris[ ,1:4], method='center','scale')
standardized <- predict(preProcessParams, iris[ ,1:4])

#range normalizasyon

preProcessParams <- preProcess(iris[ ,1:4], method='range')

normalized <- predict(preprocessParams, iris[ ,1:4])

summary(normalized)

#boxcox �zellikle �arp�k verilerde �arp�kl��� gideriyor

preProcessParams <- preProcess(iris[ ,1:4], method='BoxCox')
boxcox <- predict(preprocessParams, iris[ ,1:4])

print(preProcessParams)  #boxcox i�erisinde uygun lambda de�erlerini bulmu�tuk boxcox ile bu form�l de onu g�steriyor her s�tun i�in ayr� tabii en idealini buldu



















