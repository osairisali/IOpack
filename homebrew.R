#MEMBUAT AGREGASI IO SESUAI DENGAN KELOMPOK SEKTOR TABEL PDRB JATIM

#membuat fungsi agregasi matriks persegi sesuai index agregasi
agr <- function(data, index){
  nol <- matrix(0, 
                length(unique(index[,1])), 
                ncol(data)) #matriks nol nrow(index) x ncol(data)
  #memasukkan angka 1 pada matriks nol sesuai index agregasi
  nol[index] <- 1
  #agregasi
  agr <- nol %*% data %*% t(nol)
  agr
}
#membuat fungsi agregasi matriks bukan persegi sesuai index agregasi
agr2 <- function(data, index){
  nol <- matrix(0, 
                length(unique(index[,1])),
                nrow(data)) #matriks nol nrow(index) x ncol(data)
  #memasukkan angka 1 pada matriks nol sesuai index agregasi
  nol[index] <- 1
  #agregasi
  agr2 <- nol %*% data
  agr2
}

#versi fungsi agr yg lbh kompak
agr3 <- function(data, index) {
  ifelse( nrow(data) == ncol(data),
          nol <- matrix(0,
                        length(unique(index[ , 1])),
                        ncol(data)),
          nol <- matrix(0,
                        length(unique(index[ , 1])),
                        nrow(data))
          )
  nol[index] <- 1
  ifelse(nrow(data) == ncol(data), 
         agr3 <- nol %*% data %*% t(nol),
         agr3 <- nol %*% data)
  agr3
}
#1. AGREGASI IO 47X47 SEKTOR--------------------------------------------

#membaca IO 2010
IO2010 <- read.csv("D:/Dropbox/R Works/Input-Output Package/DATASET IO 2010.CSV", 
                   header=T, 
                   row.names=1)
#membaca index agregasi sesuai PDRB 2010
ind2010 <- data.matrix(read.csv("D:/Dropbox/R Works/Input-Output Package/INDEX AGREGASI PDRB 2010.csv", 
                                header=F))
#fungsi def untuk mendefinisikan struktur tabel IO
#def <- function(data,
#                Am,
#                Fm) {
#  a1 <- data.matrix(data[Am])
#  f1 <- data.matrix(data[Fm])
#}
#mendefinisikan data IO 2010 cakupan matriks A, final demand, value added, dan output
A2010 <- data.matrix(IO2010[1:110,1:110]) #matriks 110x110
#tesA2010 <- def(IO2010, Am =c(1:110, 1:110), Fm = c(1:110, 112:117))
F2010 <- data.matrix(IO2010[1:110, 112:117]) #matriks 110x6

#membaca IO 2006
IO2006 <- read.csv("D:/Dropbox/R Works/Input-Output Package/DATASET IO 2006.csv", 
                   header=T, 
                   row.names=1)
#membaca index agregasi sesuai PDRB 2006
ind2006 <- data.matrix(read.csv("D:/Dropbox/R Works/Input-Output Package/INDEX AGREGASI PDRB 2006.csv", 
                                header=F))

#mendefinisikan data IO 2000 cakupan matriks A, final demand, value added, dan output
A2006 <- data.matrix(IO2006[1:110,1:110]) #matriks 110x110
F2006 <- data.matrix(IO2006[1:110, 112:117]) #matriks 110x6

#Melakukan Agregasi
#data dan variabel menggunakan definisi yang ada di script bagian awal ini
#loading index agregasi IO 2010 awal (47x47 sektor)
ind2010c <- data.matrix(read.csv("D:/Dropbox/R Works/Input-Output Package/INDEX agregasi 2010.csv", 
                                 header=F))
#loading index agregasi IO 2006 awal (47x47 sektor)
ind2006c <- data.matrix(read.csv("D:/Dropbox/R Works/Input-Output Package/INDEX agregasi 2006.csv",
                                 header=F)) 
#loading index pengurutan (mengurutkan semua sektor IO sesuai dengan kelompoknya)
indurut <- data.matrix(read.csv("D:/Dropbox/R Works/Input-Output Package/INDEX agregasi pengurutan.csv",
                                header=F)) 

#agregasi matrix A
gA2010c <- agr(A2010, ind2010c)
tes1 <- agr3(A2010, ind2010c)
gF2010c <- agr2(F2010, ind2010c)
tes2 <- agr3(F2010, ind2010c)

#hasil agregasi kemudian diurutkan dengan index matrix pengurutan
ugA2010c <- agr(gA2010c, indurut)
tesgA2010c <- agr3(gA2010c, indurut)
gF2010c <- agr2(gA2010c, gF2010c, indurut)

#AGREGASI IO 2006 (47X47 SEKTOR)
#agregasi matrix A
gA2006c <- agr(A2006, ind2006c)#input antara
gF2006c <- agr2(F2006, ind2006c)#final demand

#hasil agregasi kemudian diurutkan dengan index matrix pengurutan
gA2006c <- agr(gA2006c, indurut)
gF2006c <- agr2(gF2006c, indurut)


#PERHITUNGAN HARGA KONSTAN METODE INFLASI-----------------------------------

#MEMBUAT FUNGSI DEFLATOR UNTUK KOMPONEN matriks A, findem, output
deflator <- function(matA, findem, O, kVA, VA){
  #menghitung deflator phi
  sumA <- matrix(1, 1, ncol(matA)) #membuat matriks row sums untuk matA
  phi <- kVA/as.numeric(O-(sumA %*% matA)) #menghitung deflator phi
  #menghitung harga konstan
  kA <- matA * phi #harga konstan matriks A
  kO <- O * phi #harga konstan output
  #sebelum menghitung kF... perlu dihitung rasio kF terhadap outputnya
  rF <- findem * (1/(sum(VA)))
  kF <- rF * kVA #harga konstan findem
  cbind(kA, kF, kO)
}

#MEMBUAT FUNGSI INFLATOR JATIM KE HARGA IO TAHUN 2010
inflator <- function(data, x){
  datai <- data * x
  datai
}

#PERHITUNGAN HARGA KONSTAN  (TAHUN DASAR 2011) UNTUK IO DENGAN AGREGASI AWAL 47X47 SEKTOR
#MENGHITUNG HARGA KONSTAN IO 2000 HARGA TAHUN DASAR 2011
#Loading data IO tahun 2000
IO2000 <- data.matrix(read.csv("D:/Dropbox/R Works/Input-Output Package/AGREGASI AWAL PENELITIAN IO 2000 NO VA.csv", 
                               header=T, 
                               row.names=1))
VA2000 <- data.matrix(read.csv("D:/Dropbox/R Works/Input-Output Package/AGREGASI AWAL PENELITIAN VA 2000.csv",
                               header=T, 
                               row.names=1))
VA2006b <- data.matrix(read.csv("D:/Dropbox/R Works/Input-Output Package/AGREGASI AWAL PENELITIAN VA 2006.csv", 
                                header=T, 
                                row.names=1))
IO2006b <- data.matrix(read.csv("D:/Dropbox/R Works/Input-Output Package/AGREGASI AWAL PENELITIAN IO 2006 NO VA.csv", 
                                header=T, 
                                row.names=1))

#Menghitung harga konstan tahun dasar 2013
#indeks inflasi tahun 2000 hingga 2013
infl <- (c(10.34, 14.13, 9.15, 4.23, 5.92, 15.19, 6.76, 6.48, 9.66, 3.62, 6.96, 4.09, 4.5, 7.59) / 100) + 1
#menghitung inflasi total dari tahun 2000 ke 2013
infl2 <- prod(infl)
#menghitung inflasi tahun 2006 ke 2013
infl3 <- prod(infl[7:length(infl)])
#menghitung inflasi tahun 2010 ke 2013
infl4 <- prod(infl[11:length(infl)])
#menghitung harga IO 2000 jadi harga tahun dasar 2013
ZF2000k <- IO2000[1:47, 1:53] * infl2
M2000k <- rowSums(gM2010 * infl2)
X2000k <- rowSums(ZF2000k)
IO2000k <- cbind(ZF2000k, X2000k)
#menghitung VA 2000 jadi harga tahun 2011
VA2000k <- t(X2000k) - colSums(ZF2000k[1:47, 1:47])
#Menghitung harga IO 2006 tahun dasar 2011
ZF2006k <- IO2006b[1:47, 1:53] * infl3
X2006k <- rowSums(ZF2006k)
IO2006k <- cbind(ZF2006k, X2006k)
#menghitung VA 2006 jadi harga tahun dasar 2011
VA2006k <- t(X2006k) - colSums(ZF2006k[1:47, 1:47])
#Menghitung harga IO 2010 jadi harga tahun dasar 2013
ZF2010k <- res2010c[,1:53] * infl4 #perubahan ke harga 2013 hanya butuh elemen ke-12 vector infl
X2010k <- rowSums(ZF2010k)
IO2010k <- cbind(ZF2010k, X2010k)
#menghitung VA tahun 2010
VA2010k <- t(X2010k) - colSums(ZF2010k[,1:47])



#DEKOMPOSISI PERMINTAAN AKHIR----------------------------------------------------

#MEMBUAT FUNGSI MATRIKS KEBALIKAN LEONTIEF dan FUNGSI DEKOMPOSISI FINAL DEMAND
Leontief <- function(matZ, vecO){
  matI <- diag(1, nrow(matZ), ncol(matZ))  #membuat matriks identitas I
  iO <- diag((vecO))
  iO <- solve(iO)
  matA <- matZ %*% iO
  L <- matI - matA
  matL <- solve(L)
  matL
}

#MEMBUAT FUNGSI DEKOMPOSISI FINAL DEMAND
dfindem <- function(matfindem1, matfindem2, matL1, matL2){
  Sr1 <- matrix(1, ncol(matfindem1), 1) #summation matrix S for row sums
  ft1 <- matfindem1 %*% Sr1 #menghitung row sums
  Sc1 <- matrix(1, 1, nrow(ft1)) #summation matrix S for column sums
  skl1 <- as.numeric(Sc1 %*% ft1) #menghitung column sums & dijadikan numeric agar bisa jd pembagi
  yt1 <- t(Sc1 %*% matfindem1) #menghitung matriks Yt
  dt1 <- yt1/skl1 #menghitung matriks dt
  #membuat matriks Bt   
  diayt1 <- diag(as.numeric(yt1), ncol(matfindem1), ncol(matfindem1)) #membuat matriks diagonal Yt
  Bt1 <- matfindem1 %*% solve(diayt1) #menghitung matriks Bt
  
  #melakukan perhitungan serupa untuk matfindem 2
  Sr2 <- matrix(1, ncol(matfindem2), 1) #summation matrix S for row sums
  ft2 <- matfindem2 %*% Sr2 #menghitung row sums
  Sc2 <- matrix(1, 1, nrow(ft2)) #summation matrix S for column sums
  skl2 <- as.numeric(Sc2 %*% ft2) #menghitung column sums & dijadikan numeric agar bisa jd pembagi
  yt2 <- t(Sc2 %*% matfindem2) #menghitung matriks Yt
  dt2 <- yt2/skl2 #menghitung matriks dt 
  #membuat matriks Bt
  diayt2 <- diag(as.numeric(yt2), ncol(matfindem2), ncol(matfindem2)) #membuat matriks diagonal Yt
  Bt2 <- matfindem2 %*% solve(diayt2) #menghitung matriks Bt
  
  #dekomposisi matfindem1 dan matfindem2
  #membuat variabel final-demand level effect
  matlev <- (1/2 * (skl2 - skl1)) * ((Bt1 %*% dt1) + (Bt2 %*% dt2))  
  #membuat variabel final-deman mix effect
  matmix <- 1/2 * (((skl1 * (Bt2 - Bt1)) %*% dt2) + ((skl2 * (Bt2 - Bt1) %*% dt1)))
  #membuat variabel final-demand distribution effect
  matdis <- 1/2 * ((skl1 * Bt1) + (skl2 * Bt2)) %*% (dt2 - dt1)
  #kalkulasi akhir dekomposisi final demand langsung pengaruhnya pada output IO
  matfin <- (1/2 * (matL2 + matL1))
  matlevf <- matfin %*% matlev
  matmixf <- matfin %*% matmix
  matdisf <- matfin %*% matdis
  #Mulai menggabungkan hasil dekomposisi ke dalam satu file
  matgab <- cbind(matlevf, matmixf, matdisf)
  colnames(matgab) <- c("Level-Effect", "Mix-Effect", "Distribution-Effect")
  matgab
}

#MENGHITUNG DEKOMPOSISI IO PERMINTAAN AKHIR
#MENGHITUNG MATRIX KEBALIKAN LEONTIEF PADA HARGA KONSTAN 2010 (Variabel sama dengan variabel perhitungan harga konstan tahun dasar 2010)
#Menghitung matrix kebalikan Leontief IO 2010
L2010 <- Leontief(IO2010k[1:47, 1:47], rowSums(IO2010k[,1:53]))
#Menghitung matrix kebalikan Leontief IO 2006
L2006 <- Leontief(IO2006k[1:47, 1:47], IO2006k[1:47, 54])
#Menghitung matrix kebalikan Leontief IO 2000
L2000 <- Leontief(IO2000k[1:47,1:47], IO2000k[1:47, 54])

#Menghitung analisa dekomposisi IO 2010 dan 2000 (1)
res1 <- round(dfindem(IO2000k[1:47, 48:53], IO2010k[1:47, 48:53], L2000, L2010), digits=3)
#Menghitung analisa dekomposisi IO 2010 dan 2006 (2)
res2 <- round(dfindem(IO2006k[1:47, 48:53], IO2010k[1:47, 48:53], L2006, L2010), digits=3)
#Menghitung analisa dekomposisi IO 2006 dan 2000 (3)
res3 <- round(dfindem(IO2000k[1:47, 48:53], IO2006k[1:47, 48:53], L2000, L2006), digits=3)

#Menghitung perubahan output tahun 2010 dan 2000
dX1 <- round(rowSums(IO2010k[,1:53]) - X2000k, digits=3)
#Menghitung perubahan output tahun 2010 dan 2006
dX2 <- round(rowSums(IO2010k[,1:53]) - X2006k, digits=3)
#Menghitung perubahan output tahun 2006 dan 2000
dX3 <- round(X2006k - X2000k, digits=3)

#Menggabungkan variabel dX ke variabel res
res1 <- cbind(dX1, res1); colnames(res1) <- c("Perubahan Output", "Level-Effect", "Mix-Effect", "Distribution-Effect")
res2 <- cbind(dX2, res2); colnames(res2) <- c("Perubahan Output", "Level-Effect", "Mix-Effect", "Distribution-Effect")
res3 <- cbind(dX3, res3); colnames(res3) <- c("Perubahan Output", "Level-Effect", "Mix-Effect", "Distribution-Effect")

#Menyalin data ke dalam file
write.csv(res1, file="Dekomposisi Final Demand IO 2010-2000.csv", row.names=T)
write.csv(res2, file="Dekomposisi Final Demand IO 2010-2006.csv", row.names=T)
write.csv(res3, file="Dekomposisi Final Demand IO 2006-2000.csv", row.names=T)

#Dekomposisi Matriks Teknologi-------------------------------------------------------------
#Variabel bersesuaian dengan variabel yang ada di sript sebelumnya
#Membuat Fungsi substitusi matrix nol dengan kolom yang bersesuaian
demat <- function(difA, L1, L2, f1, f2 ){
  mat <- matrix(0, nrow(difA), ncol(difA))
  
  mkmat <- function(difA, indklm, L1, L2, f1, f2){
    matA <- matrix(difA[,indklm])
    res <- matrix(0, nrow(difA), ncol(difA))
    res[,indklm] <- matA
    res2 <- rowSums(1/2 * ((L2 %*% res %*% L1) %*% (f1 + f2)))
    res2    
  }
  for(i in 1:ncol(difA)) {
    mat[,i] <- mkmat(difA, i, L1, L2, f1, f2) #menempatkan hasil fungsi mkmat ke kolom i matrix mat
  }
  mat
}

#Dekomposisi Matrix Teknologi IO 2010 dan 2000
#Mendefinisikan Variabel matrix A untuk semua data IO
A2000 <- IO2000k[1:47, 1:47] %*% solve(diag(IO2000k[,54]))
A2006 <- IO2006k[1:47, 1:47] %*% solve(diag(IO2006k[,54]))
A2010 <- IO2010k[1:47, 1:47] %*% solve(diag(IO2010k[,54]))

#kalkulasi selisih matriks A 2010 dan 2000
difA1 <- A2010 - A2000
#kalkulasi selisih matriks A 2010 dan 2006
difA2 <- A2010 - A2006
#kalkulasi selisih matriks A 2006 dan 2000
difA3 <- A2006 - A2000

#Perhitungan Dekomposisi Teknologi IO 2010 dan 2000
lres1 <- round(demat(difA1, L2000, L2010, IO2000k[,48:53], IO2010k[,48:53]), digits=3) 
#Perhitungan Dekomposisi Teknologi IO 2010 dan 2006
lres2 <- round(demat(difA2, L2006, L2010, IO2006k[,48:53], IO2010k[,48:53]), digits=3)
#Perhitungan Dekomposisi Teknologi IO 2006 dan 2000
lres3 <- round(demat(difA3, L2000, L2006, IO2000k[,48:53], IO2006k[,48:53]), digits=3)

#Menyalin data ke dalam file
write.csv(lres1, file="Dekomposisi Teknologi IO 2010-2000.csv", row.names=T)
write.csv(lres2, file="Dekomposisi Teknologi IO 2010-2006.csv", row.names=T)
write.csv(lres3, file="Dekomposisi Teknologi IO 2006-2000.csv", row.names=T)

#MENYUSUN SEMUA DATA UNTUK DIANALISA-----------------------------------------------------

#MENYUSUN TABEL HASIL ANALISA DEKOMPOSISI

#Total efek perubahan teknologi
#Total dekomposisi teknologi 2010-2000
tot1 <- rowSums(lres1)
#Total dekomposisi teknologi 2010-2006
tot2 <- rowSums(lres2)
#Total dekomposisi teknologi 2006-2000
tot3 <- rowSums(lres3)

#Total efek perubahan final demand
#Total efek final deman 2010-2000
tf1 <- round(rowSums(res1[,2:ncol(res1)]), digits=3)
#Total efek final deman 2010-2006
tf2 <- round(rowSums(res2[,2:ncol(res2)]), digits=3)
#Total efek final deman 2006-2000
tf3 <- round(rowSums(res3[,2:ncol(res3)]), digits=3)

#Total efek dari dekomposisi
td1 <- round(tot1 + tf1, digits=3)
td2 <- round(tot2 + tf2, digits=3)
td3 <- round(tot3 + tf3, digits=3)

#Menghitung selisih perubahan total output dengan total efek dekomposisi
se1 <- round(res1[,1] - td1, digits=3)
se2 <- round(res2[,1] - td2, digits=3)
se3 <- round(res3[,1] - td3, digits=3)

#Menyusun data dekomposisi IO
namakol <- c("Perubahan Output", "Level-Effect", "Mix-Effect", "Distribution-Effect", "Total Efek Final Demand", "Total Efek Perubahan Teknologi", "Total Dekomposisi", "Selisih Output & Dekomposisi")
#Dekomposisi 2010-2000
fin1 <- cbind(res1, tf1, tot1, td1, se1); colnames(fin1) <- namakol
#Dekomposisi 2010-2006
fin2 <- cbind(res2, tf2, tot2, td2, se2); colnames(fin2) <- namakol
#Dekomposisi 2006-2000
fin3 <- cbind(res3, tf3, tot3, td3, se3); colnames(fin3) <- namakol

#Menyalin data ke file
write.csv(fin1, file="hasil analisa dekomposisi 2010-2000.csv", row.names=T)
write.csv(fin2, file="hasil analisa dekomposisi 2010-2006.csv", row.names=T)
write.csv(fin3, file="hasil analisa dekomposisi 2006-2000.csv", row.names=T)

#MENYUSUN TABEL ANALISA DATA IO

#Total Permintaan Antara
per1 <- rowSums(IO2010k[,1:47]) #IO 2010 harga konstan
per2 <- rowSums(IO2006k[,1:47]) #IO 2006 harga konstan
per3 <- rowSums(IO2000k[,1:47]) #IO 2000 harga konstan

#Permintaan akhir domestik
dom1 <- rowSums(IO2010k[,48:51]) #IO 2010
dom2 <- rowSums(IO2006k[,48:51])
dom3 <- rowSums(IO2000k[,48:51])

#Permintaan akhir eskpor
eks1 <- rowSums(IO2010k[,52:53])
eks2 <- rowSums(IO2006k[,52:53])
eks3 <- rowSums(IO2000k[,52:53])

#Jumlah permintaan keseluruhan
jum1 <- per1+dom1+eks1
jum2 <- per2+dom2+eks2
jum3 <- per3+dom3+eks3

#Jumlah Import domestik (dari luar provinsi)
M1 <- gM2010[,2] * infl[12]
M2 <- gM2006[,2] * infl3
M3 <- gM2000[,2] * infl2

#Jumlah Import dari luar negeri
m1 <- gM2010[,1] * infl[12]
m2 <- gM2006[,1] * infl3
m3 <- gM2000[,1] * infl2

#Komponen Nilai Tambah
V1 <- t(VA2010k)
V2 <- t(VA2006k)
V3 <- t(VA2000k)

#PERLU DIBUATKAN DAFTAR NAMA SEKTOR!

#Membuat nama kolom untuk tabel
nama <- c("Permintaan Antara", "Permintaan Akhir(Domestik)", "Permintaan Akhir(Ekspor)", "Output Domestik",
          "Import Antar Provinsi", "Import Luar Negeri", "Nilai Tambah Bruto (PDRB)")

#Menyusun tabel
tbl1 <- cbind(per1, dom1, eks1, jum1, M1, m1, V1); colnames(tbl1) <- nama
tbl2 <- cbind(per2, dom2, eks2, jum2, M2, m2, V2); colnames(tbl2) <- nama
tbl3 <- cbind(per3, dom3, eks3, jum3, M3, m3, V3); colnames(tbl3) <- nama
