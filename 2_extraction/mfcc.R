library(tuneR)

'
Funkcja licz¹ca wspó³czynniki mfcc. Implementacja zportowana ze skryptu Matlabowego:
https://github.com/bootphon/features_extraction/blob/master/rastamat/melfcc.m

Uwaga ! Implementacja funkcji w bibliotece tunerR u¿ytych do policzenia wspó³czynników mfcc (powspec, 
audspec) nie potrafi obs³u¿yæ zbyt wielkich wektorów liczbowych. Czêœæ wspó³czynników przepe³nia siê 
i tablica jest wyp³eniana wartoœciami NaN. 
Przyk³adowo dla pliku 1_preprocessing/probka1.wav wartoœci¹ graniczn¹ jest d³ugoœæ wektora 
60 000 - 70 000 (oko³o 1.5s).

Wejœcie:
  1) wave_array     - tablica liczb ca³kowitych reprezentuj¹ca plik .wav
  2) wave_samp_rate - czêstotliwoœæ próbkowania [1/s]

Wyjœcie
  1) cepstra        - zwraca 12 wspó³czynników mfcc jako tablicê liczby zmiennoprzecinkowych (typ num)

'
mfcc_calculate <- function(wave_array, wave_samp_rate){
  
  ##Oblicz czas trwania próbki
  wave_length     <- length(wave_array)
  window_time     <- wave_length/wave_samp_rate

  ##Wymuœ jedno okno czasowe na cala probke przy obliczaniu spektrum mocy
  power_spectrum  <- powspec(x = wave_array, sr = wave_samp_rate, wintime = window_time)
  aud_spectrum    <- audspec(pspectrum = matrix(power_spectrum), sr = wave_samp_rate)[[1]]
  
  ##Transformacja cosinusowa i liftering wspó³czynników
  cepstra         <- spec2cep(spec = matrix(aud_spectrum))[[1]]
  cepstra         <- lifter(cepstra)
  
  ##Sp³aszcz macierz do tablicy i zwróæ wartoœæ
  cepstra         <- cepstra[1:length(cepstra)]
  return          (cepstra)
}

'
##Przyklad uzycia
wave_file_input <- file.path("1_preprocessing", "probka1.wav")
wave_input      <- readWave(wave_file_input)
wave_array      <- wave_input@left
wave_samp_rate  <- wave_input@samp.rate

##Obciecie czeœci przy zbyt d³ugich próbkach
wave_array      <- wave_array[1:30000]
cepstra         <- mfcc_calculate(wave_array, wave_samp_rate)
'
