library(tuneR)

'
Funkcja licz¹ca wspó³czynniki mfcc. Implementacja w bibliotece tunerR
nie obs³uguje przetwarzanie wiêcej ni¿ jednego kana³u, dlatego w przypadku
stereo przetwarzamy tylko wybrany kana³ (lewy).

Wejœcie:
  1) wave_input - za³adowane próbka w formacie wav z sygna³em wejœciowym
  2) wintime_in - d³ugoœæ okna czasowego w  [s]
  3) hoptime_in - d³ugoœæ kroku czasowego w [s] przy tworzeniu kolejnego
                  okna czasowego

Zwraca wspó³czynniki mfcc:
  1) macierz cepstra:   (12 typu cepstral na ka¿de okno czasowe).
  2) macierz aspectrum  (forma jak w 1) - spektrum s³uchowe?) 
  3) macierz pspectrum: (forma jak w 1) - spektrum mocy)  
'
mfcc_calculate <- function(wave_input, wintime_in, hoptime_in){
  if ( wave_input@stereo == TRUE ){
    mono_left           <- mono(wave_input, "left")
    mfcc_output_left    <- melfcc(mono_left, wintime = wintime_in,
                                  hoptime = hoptime_in, spec_out = TRUE)
    return (mfcc_output_left)
  } else {
    mmfcc_output        <- melfcc(wave_input, wintime = wintime_in,
                                  hoptime = hoptime_in, spec_out = TRUE)
    return (mfcc_output)
  }
}

'Przyklad uzycia'
wave_file_input <- file.path("1_preprocessing", "probka1.wav")
wave_input      <- readWave(wave_file_input)
mfcc            <- mfcc_calculate(wave_input, 1.0, 0.5)