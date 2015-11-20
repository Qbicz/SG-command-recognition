# Normalizacja poziomu nagrania

# Wyczysc przestrzen robocza
rm(list = ls())

# Odczytaj dzwiek
sndObj <- readWave('1_scisz_telewizor.wav')
str(sndObj)
s1 <- sndObj@left

# konwertuj do float (-1; 1)
s1 <- s1 / 2^(sndObj@bit -1)

# Normalizacja - wykorzystanie pełnego zakresu
s1 <- s1/max(s1);

n = length(s1)

# Wykres w dziedzinie czasu
timeArray <- (0:(n-1)) / sndObj@samp.rate
timeArray <- timeArray * 1000 # czas w milisekunadach

plot(timeArray, s1, type='l', col='green', xlab='Time [ms]', ylab='Amplitude')

