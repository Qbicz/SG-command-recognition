Spectrum <- function(wave, sampling_freq = 44100, freq_range = 3000, sections_num = 10){
# Funkcja dzieli widmo na kilka podprzedzialow i zwraca sume kazdego z nich
# wave - macierz danych wejsciowych
# sampling_freq - czestotliwosc probkowania
# freq_range - zakres badanego widma 0 [Hz] - freq_range [Hz]
# sections_num - ilosc przedzialow na jaki zostanie podzielony zakres badanego widma. Funkcja zwroci macierz o ilosci kolumn 
# rownej sections_num, przy czym kazda kolumna to suma widma w zakresie od (n-1)*(freq_range / sections_num)
# do (n * freq_range / sections_num), gdzie n = 1...sections_num
# Zwraca macierz o ilosci wierszy rownej ilosci wierszy macierzy wejsciowej oraz ilosci kolumn rownej "sections_num"
  
  if(sampling_freq < freq_range)
    return (NULL)
  
  ret_matrix <- matrix(0, nrow = nrow(wave), ncol = sections_num)
  
  for(sample_number in 1:nrow(wave))
  {
      ft <- fft(wave[sample_number,])
      wave_length <- length(wave[sample_number,])
      ft_length <- ceiling((wave_length+1)/2)
      ft <- ft[1:ft_length]                      # wybor tylko pierwszej polowy (symetryczne)
      ft <- abs(ft)                              # modul
      ft <- ft / wave_length                     # skalowanie
      ft = ft / max(ft)                          # normalizacja
      ft_spaces <- sampling_freq / wave_length
      section_size <- round(freq_range / (ft_spaces * sections_num))
      
      for(i in 1:sections_num)
      {
        sum <- 0
        for(j in ((i-1)*section_size + 1):(i*section_size))
        {
          sum <- sum + ft[j]     
        }
        ret_matrix[sample_number, i] <- sum
      }
  }
  
  return (ret_matrix)
}