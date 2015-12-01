BandpassFiltr <- function(frames,sample_rate,high,low){
  library(tuneR)
  library(signal)
  library(seewave)
  library(dplyr)
  #filtr butterwortha
  
#   low <- 0.095
#   high <- 0.22
  
  long <- length(frames)              #Dlugosc ramki danych
  ts <- 1/sample_rate                 #Okres probkowania
  nyquist_f <- sample_rate/2          #Czestotliwosc nyquista
  
  signal <- frames
  time <- seq(0, long*ts,by=ts)       #Czas trwania u nas bedzie 20ms
  
  temp1 <- butter(2, c(low, high), 'pass')      #filtr pasmowy buterwortha
  bandpass <- filtfilt(temp1, signal)           #filtrowanie naszyszych danych wejsciowych
  bandpass <- bandpass* 2                       #wzmocnienie syganalu
  
  #///// ZAPISYWANIE DO PLIKU
#   save_butter<-cutw(bandpass, f=sample_rate, to=0.5, output="Wave")
#   savewav(save_butter, filename = "bandpass_butter.wav")
  #/////
 
  long <- length(bandpass)                          #sprawdzanie dlugosci przefiltrowanego syganlu
  
  bandpass<-bandpass/2^(sndObj@bit -1)              #normalizacja sygalu
  timeArray_bandpass <- (0:(long-1))/sample_rate    #os czasu
  
#   timeArray_bandpass<- timeArray_bandpass * 1000    #scale to miliseconds
#   plot(timeArray_bandpass, bandpass, type= 'l', col='black',xlab='Tmie(ms)',ylab='Amplitude')
 
#   PRZELICZANIE DANYCH NA POSTAC CZESTOTLIWOSCIOWA
#   n_bandpass <- length(bandpass)
#   p_bandpass <- fft(bandpass)
#   
#   nUniquePts <- ceiling((n_bandpass+1)/2)
#   p_bandpass <- p_bandpass[1:nUniquePts]            #select just the first half since the second half is a mirror image of the first
#   p_bandpass <- abs(p_bandpass)                     #take the absolute value, or the magnitude
#   p_bandpass <- p_bandpass/n_bandpass                        #scale by the number of points so that the magnitude does not depend on the length of the signal or on its sampling frequency
#   p_bandpass <- p_bandpass^2                        #square it to get the power "widmo mocy"
#   
#   #multiply by two (see technical documents for details)
#   #odd nfft excludes Nyquist point
#   
#   if (n_bandpass %% 2 > 0 ){
#     p_bandpass[2:length(p_bandpass)] <- p_bandpass[2:length(p_bandpass)]*2            #we've got odd number of points fft
#   } else {
#     p_bandpass[2:(length(p_bandpass)-1)] <- p_bandpass[2:(length(p_bandpass)-1)]*2    #we've got even number of points fft
#   }
#   freqArray_bandpass <- (0:(nUniquePts-1)) * (sample_rate/n_bandpass)  #create the frequency array
#   
#   plot (freqArray_bandpass/1000, 10*log10(p_bandpass),type='l',col='black', xlab = 'Frequency(kHz)', ylab='Power(dB)')
#   plot(freqArray_bandpass, p_bandpass, type= 'l', col='black',xlab='Frequency',ylab='Amplitude')
  
  
  
  butterworth <- bandpass
  return(butterworth)
}
