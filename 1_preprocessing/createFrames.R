library(tuneR)
library(signal)
library(dplyr)

#
# createFrames
#
# Funkcja dzielaca wektor na ramki o dlugosci odpowiadajacej 20ms
# Ramki sa umieszczone w macierzy jako kolejne wiersze
# Nakładkowanie o 50% oznacza, ze miedzy kolejnymi wierszami jest roznica 10ms
#
# 
#    signal  --  wektor danych
#    sample_rate  -- czestotliwosc probkowania, np. waveObj@samp.rate
# 
# 
# Zeby zobaczyc jak dane sa ukladane w macierzy, odpal:
# output_matrix = createFrames(seq(1,100), 500)
#


createFrames <- function(signal, sample_rate)
{
  # Dlugosc ramki 20ms
  frame_length <- 20e-3
  samples_per_frame <- ceiling(frame_length*sample_rate) # 882 dla 44.1kHz
  
  
  # krok ma byc parzysty
  if (samples_per_frame %% 2 != 0){
    step = samples_per_frame+1
  }
  else {
    step = samples_per_frame
  }
  
  # pierwsza i druga ramka
  frames_matrix = rbind(signal[1:step],signal[(1/2*step+1):(3/2*step)])
  # kolejne ramki, nakladkowane 0 50%
  for(i in seq(step,length(signal)-step, by = step/2)){
    frames_matrix = rbind(frames_matrix,signal[(i+1):(i+step)])
  }
  
  return(frames_matrix)
}





'Przyklad uzycia'

sndObj<-readWave('probka2.wav')
# 'Rozpakowanie' obiektu wave
fs <- sndObj@samp.rate
signal <- sndObj@left

test_matrix = createFrames(signal, fs)
#output_matrix = createFrames(seq(1,100), 500)

