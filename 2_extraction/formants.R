#
# WAZNE!! Przed pierwszym uzyciem wykonaj nastepujace komendy:
#
#install.packages('tuneR', dep=TRUE)
#install.packages('quantmod',dep=TRUE)
# 
# Funkcja znajduje maksymalne wartosci mocy sygnalu
# w trzech przedzialach, w ktorych z grubsza znajduja
# sie formanty glosek mowy polskiej (przedzialy z wykladu)
# 
# Zwraca wektor z trzema wartosciami (czestotliwosci formantow)
# zakresy wyjsc:
# F1 [200; 880]
# F2 [850; 2350]
# F3 [2100; 3100]
#
#----------------------------------------
# przyklad uzycia:
#
#sndObj <- tuneR:::readWave('probka2.wav')
#sndVec <- sndObj@left
#wave_rate <- sndObj@samp.rate
#wave_input <- sndVec[3000:5500]
#formants(wave_input,wave_rate)
#
#output:
#[1]  476.0896 1833.8265 2874.1703
#----------------------------------------
#

  
formants <- function(wave_input, wave_rate){
  
  
  # obliczenie widma FFT
  p <- fft(wave_input)
  n <- length(wave_input)
  nUniquePts <- ceiling((n+1)/2)
  p <- p[1:nUniquePts] # wybor tylko pierwszej polowy (symetryczne)
  p <- abs(p)
  p <- p/n # skalowanie
  # Jakies mnozenie przez 2 (??)
  if (n %% 2 > 0){
    p[2:length(p)] <- p[2:length(p)]*2 # nieparzysta liczba punktow
  } else {
    p[2: (length(p) -1)] <- p[2: (length(p) -1)]*2 # parzysta liczba punktow
  }
  freqArray <- (0:(nUniquePts-1)) * (wave_rate / n) # stworzenie tablicy czestotliwosci 
  
  #filtracja dolnoprzepustowa widma
  timeArray <- (0:(length(p)-1)) / wave_rate
  myfilter <- signal:::butter(3, 3000/wave_rate*2, type = "low", plane = "z")
  pf <- signal:::filter(myfilter, p)
  
  #wstepne obciecie zakresu czestotliwosci
  #przejscie na skale logarytmiczna
  ind <- which(freqArray <= 3100 & freqArray >= 200)
  freqArray <- freqArray[ind]
  pf <- 10*log10(pf[ind])

  #usuniecie trendu
  gfit <- lm(pf~freqArray)
  pft <- pf - as.numeric(gfit$fitted.values)
  pft <- pft - min(pft) + 1
  
  #plotowanie czestotliwosci - do testow
  #plot(freqArray[1:200]/1000, pf[1:200], type='p', col='black', xlab='Frequency (kHz)', ylab='Power (dB)', main='pf')
  
  #podzielenie na 3 zakresy dla 3 formantow
  ind_1 <- which(freqArray <= 880 & freqArray >= 200)
  freqArray_1 <- freqArray[ind_1]
  pft_1 <- pft[ind_1]
  ind_2 <- which(freqArray <= 2350 & freqArray >= 850)
  freqArray_2 <- freqArray[ind_2]
  pft_2 <- pft[ind_2]
  ind_3 <- which(freqArray <= 3100 & freqArray >= 2100)
  freqArray_3 <- freqArray[ind_3]
  pft_3 <- pft[ind_3]

  #znalezienie formantow - wersja uproszczona
  F1 <- freqArray_1[which(pft_1 == max(pft_1))]
  F2 <- freqArray_2[which(pft_2 == max(pft_2))]
  F3 <- freqArray_3[which(pft_3 == max(pft_3))]

  #wyznaczanie maksimow
  #threshold <- 0.05 #??
  #maxima_1 <- quantmod:::findPeaks(pft_1,threshold)
  #maxima_2 <- quantmod:::findPeaks(pft_2,threshold)
  #maxima_3 <- quantmod:::findPeaks(pft_3,threshold)

  #znalezienie formantow
  #F1 <- freqArray_1[maxima_1][which(pft_1[maxima_1] == max(pft_1[maxima_1]))]
  #F2 <- freqArray_2[maxima_2][which(pft_2[maxima_2] == max(pft_2[maxima_2]))]
  #F3 <- freqArray_3[maxima_3][which(pft_3[maxima_3] == max(pft_3[maxima_3]))]

  #obsluzyc przypadek wystapienia formantu w przedziale wspolnym dla podprzedzialow

  return (c(F1,F2,F3))
}