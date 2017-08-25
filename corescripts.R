#FUNGSI AGREGASI MATRIX
agr <- function(data, index) {
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

#FUNGSI DEFLATOR MATRIX
deflator <- function(matA, findem, O, kVA, VA){
  #menghitung deflator phi
  sumA <- matrix(1,
                 1, 
                 ncol(matA)) #membuat matriks row sums untuk matA
  phi <- kVA/as.numeric(O-(sumA %*% matA)) #menghitung deflator phi
  #menghitung harga konstan
  kA <- matA * phi #harga konstan matriks A
  kO <- O * phi #harga konstan output
  #sebelum menghitung kF... perlu dihitung rasio kF terhadap outputnya
  rF <- findem * (1/(sum(VA)))
  kF <- rF * kVA #harga konstan findem
  cbind(kA, kF, kO)
}

#FUNGSI INFLATOR MATRIX
inflator <- function(data, x){
  datai <- data * x
  datai
}

#FUNGSI MATRIX KEBALIKAN LEONTIEF
Leontief <- function(matZ, vecO){
  matI <- diag(1, 
               nrow(matZ), 
               ncol(matZ))  #membuat matriks identitas I
  iO <- diag((vecO))
  iO <- solve(iO)
  matA <- matZ %*% iO
  L <- matI - matA
  matL <- solve(L)
  matL
}

#FUNGSI DEKOMPOSISI FINAL DEMAND
dfindem <- function(matfindem1, matfindem2, matL1, matL2){
  Sr1 <- matrix(1, 
                ncol(matfindem1), 
                1) #summation matrix S for row sums
  ft1 <- matfindem1 %*% Sr1 #menghitung row sums
  Sc1 <- matrix(1, 
                1,
                nrow(ft1)) #summation matrix S for column sums
  skl1 <- as.numeric(Sc1 %*% ft1) #menghitung column sums & dijadikan numeric agar bisa jd pembagi
  yt1 <- t(Sc1 %*% matfindem1) #menghitung matriks Yt
  dt1 <- yt1/skl1 #menghitung matriks dt
  #membuat matriks Bt   
  diayt1 <- diag(as.numeric(yt1), 
                 ncol(matfindem1), 
                 ncol(matfindem1)) #membuat matriks diagonal Yt
  Bt1 <- matfindem1 %*% solve(diayt1) #menghitung matriks Bt
  
  #melakukan perhitungan serupa untuk matfindem 2
  Sr2 <- matrix(1, 
                ncol(matfindem2), 
                1) #summation matrix S for row sums
  ft2 <- matfindem2 %*% Sr2 #menghitung row sums
  Sc2 <- matrix(1, 
                1, 
                nrow(ft2)) #summation matrix S for column sums
  skl2 <- as.numeric(Sc2 %*% ft2) #menghitung column sums & dijadikan numeric agar bisa jd pembagi
  yt2 <- t(Sc2 %*% matfindem2) #menghitung matriks Yt
  dt2 <- yt2/skl2 #menghitung matriks dt 
  #membuat matriks Bt
  diayt2 <- diag(as.numeric(yt2), 
                 ncol(matfindem2), 
                 ncol(matfindem2)) #membuat matriks diagonal Yt
  Bt2 <- matfindem2 %*% solve(diayt2) #menghitung matriks Bt
  
  #dekomposisi matfindem1 dan matfindem2
  #membuat variabel final-demand level effect
  matlev <- (1/2 * (skl2 - skl1)) * ((Bt1 %*% dt1) + (Bt2 %*% dt2))  
  #membuat variabel final-demand mix effect
  matmix <- 1/2 * (((skl1 * (Bt2 - Bt1)) %*% dt2) + ((skl2 * (Bt2 - Bt1) %*% dt1)))
  #membuat variabel final-demand distribution effect
  matdis <- 1/2 * ((skl1 * Bt1) + (skl2 * Bt2)) %*% (dt2 - dt1)
  #kalkulasi akhir dekomposisi final demand langsung pengaruhnya pada output IO
  matfin <- (1/2 * (matL2 + matL1))
  matlevf <- matfin %*% matlev
  matmixf <- matfin %*% matmix
  matdisf <- matfin %*% matdis
  #Mulai menggabungkan hasil dekomposisi ke dalam satu file
  matgab <- cbind(matlevf, 
                  matmixf, 
                  matdisf)
  colnames(matgab) <- c("Level-Effect", 
                        "Mix-Effect", 
                        "Distribution-Effect")
  matgab
}

#FUNGSI DEKOMPOSISI MATRIX TEKNOLOGI
demat <- function(difA, L1, L2, f1, f2 ){
  mat <- matrix(0, 
                nrow(difA), 
                ncol(difA))
  
  mkmat <- function(difA, indklm, L1, L2, f1, f2){
    matA <- matrix(difA[,indklm])
    res <- matrix(0, 
                  nrow(difA), 
                  ncol(difA))
    res[,indklm] <- matA
    res2 <- rowSums(1/2 * ((L2 %*% res %*% L1) %*% (f1 + f2)))
    res2    
  }
  for(i in 1:ncol(difA)) {
    mat[,i] <- mkmat(difA, i, L1, L2, f1, f2) #menempatkan hasil fungsi mkmat ke kolom i matrix mat
  }
  mat
}