library("openxlsx")

#Mempersiapkan data
dataCreditRating <- read.xlsx(xlsxFile = "C:\\Users\\ASUS\\Downloads\\credit_scoring1.xlsx")
str(dataCreditRating)

#Mempersiapkan class dan input variables 
dataCreditRating$risk_rating <- as.factor(dataCreditRating$risk_rating) 
input_columns <- c("durasi_pinjaman_bulan", "jumlah_tanggungan")
datafeed <- dataCreditRating[ , input_columns ]

#Mempersiapkan training dan testing set
set.seed(100) #untuk menyeragamkan hasil random antar tiap komputer
indeks_training_set <- sample(900, 800)

#Membuat dan menampilkan training set dan testing set
input_training_set <- datafeed[indeks_training_set,]
class_training_set <- dataCreditRating[indeks_training_set,]$risk_rating
input_testing_set <- datafeed[-indeks_training_set,]

str(input_training_set)
str(class_training_set)
str(input_testing_set)

#menghasilkan dan menampilkan summary model
library("C50")
risk_rating_model <- C5.0(input_training_set, class_training_set, control = C5.0Control(label="Risk Rating"))
summary(risk_rating_model)
plot(risk_rating_model)

#menggunakan model untuk prediksi testing set
predict(risk_rating_model, input_testing_set)
#menyimpan hasil prediksi testing set ke dalam kolom hasil_prediksi
input_testing_set$risk_rating <- dataCreditRating[-indeks_training_set,]$risk_rating
input_testing_set$hasil_prediksi <- predict(risk_rating_model, input_testing_set)
print(input_testing_set)

#membuat confusion matrix
library("reshape2")
dcast(hasil_prediksi ~ risk_rating, data=input_testing_set)
#Menghitung jumlah prediksi yang benar
nrow(input_testing_set[input_testing_set$risk_rating==input_testing_set$hasil_prediksi,])
#Menghitung jumlah prediksi yang salah
nrow(input_testing_set[input_testing_set$risk_rating!=input_testing_set$hasil_prediksi,])

#Membuat data frame new application
aplikasi_baru <- data.frame(jumlah_tanggungan = 6, durasi_pinjaman_bulan = 12)
#melakukan prediksi
predict(risk_rating_model, aplikasi_baru)

#Membuat data frame new application
aplikasi_baru <- data.frame(jumlah_tanggungan = 6, durasi_pinjaman_bulan = 64)
#melakukan prediksi
predict(risk_rating_model, aplikasi_baru)


#Membuat data frame new application dengan mengubah durasi pinjaman
aplikasi_baru <- data.frame(jumlah_tanggungan = 3, durasi_pinjaman_bulan = 64)
#melakukan prediksi
predict(risk_rating_model, aplikasi_baru)

