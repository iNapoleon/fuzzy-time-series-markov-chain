data <- read.csv("D:/Skripsi_Muhammad Awaludin/DataSaham.csv", sep=";")
data = as.vector(data$x)
data
#Membuat interval linguistik
interval_fuzzy <- function(data, D1 = 10, D2= 40){
  Umin = min(data)- D1
  Umax = max(data)+ D2
  n = round(1 +3.322 *logb(length(data), base  = 10))
  l = (Umax - Umin)/n
  intervals = data.frame(mins = 0, maxs = 0)
  intervals[1,1] = Umin
  intervals[1,2] = Umin + l
  for (i in 2:n){
    intervals[i,1] = intervals[i-1,2]
    intervals[i,2] = intervals[i,1] + l
  }
  return((intervals = intervals))
}
b= interval_fuzzy(data, D1 = 10, D2 = 40)
b
#Algoritma FTSMC
fuzzy_tsmc <- function(data,interval){
  m = as.vector(rowMeans(interval)) #mencari rata-rata subinterval
  A=c()
  for (i in 1:length(m)){
    if (i==1){
      A[i] = (1/m[i])+(0.5/m[i+1])
    }
    else if (i==length(m)){
      A[i] = (0.5/m[i-1])+(1/m[i])
    }
    else {
      A[i] = (0.5/m[i-1])+(1/m[i])+(0.5/m[i+1])
    }
  }
  fuzzify=c() #Deklarasi Fuzzifikasi
  for (i in 1:length(data)){
    for (j in 1:nrow(interval)){
      if (i!=which.max(data)){
        if (data[i]>=(interval[j,1])&data[i]<(interval[j,2])){
          fuzzify[i]=j
          break
        }
      }
      else {
        if (data[i]>=(interval[j,1])&data[i]<=(interval[j,2])){
          fuzzify[i]=j
          break
        }
      }
    }
  }
  #Membuat FLR
  flr <- data.frame(current_state=0, next_state=0)
  for(i in 1: length(fuzzify)){
    if(i < length(fuzzify)){
      flr[i,]=c(fuzzify[i],fuzzify[i+1])
    }
    else{
      flr[i,]=c(fuzzify[i],0)
    }
  }
  print(flr)
  #Membuat Matriks Transisi
  state=matrix(data=as.vector(table(flr)[,-1]),nrow = nrow(interval), byrow = F)
  matriks_transisi = state/rowSums(state)
  print(matriks_transisi)
  #Menghitung ramalan
  ramalan = c()
  #Mnghitung Koreksi Ramalan
  d = c()
  for (i in 2:(length(data))){
    ramalan [i] = sum(m*as.vector(matriks_transisi[flr[i-1,1],]))
    d[i] = (interval[1,2]-interval[1,1])*-(flr[i-1,1]-flr[i-1,2])/2
  }
  #Menghitung Ramalan Sebenarnya
  ramalan_benar = ramalan + d
  #Forecasting
  D = (interval[1,2]-interval[1,1])*-(flr[length(data),1]-flr[length(data)-1,2])/2
  Forecasting= m[flr[length(data),1]]-D
  #Menghitung Galat
  kesalahan = abs(data-ramalan_benar)
  MSE=mean(kesalahan^2, na.rm = TRUE)
  MAPE = mean(kesalahan/data*100, na.rm=TRUE)
  return(list(ramalan_awal=ramalan, data_ramalan = ramalan_benar, Forescast = Forecasting, MSE = MSE, MAPE = MAPE))
}

###Mencoba Fungsi FTSMC pada data yang sudah dipanggil
hasil=fuzzy_tsmc(data,b)
hasil

periode<- c(1:218)
plot(periode,data, type="l", col="blue", ylab="Harga Saham (Rp)")
par(new=TRUE)
plot(periode, hasil$data_ramalan,type="l",col="red", lty=6, ylab="Harga Saham", xaxt="n", yaxt="n")
axis(side=4)
legend("topleft", c("data aktual", "data ramalan"), col=c("blue", "red"),lty=c(1,2))