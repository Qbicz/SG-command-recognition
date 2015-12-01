Energy <- function(wave, pieces = 5){
# Funkcja dzieli wektor danych wejsciowych na "pieces" podprzedzialow i liczy sume kwadratow (energie) w kazdym z nich
# wave - macierz danych wejsciowych
# pieces - okresla na ile czesci podzielic kazdy zestaw danych wejsciowych
# Zwraca macierz o ilosci wierszy rownej ilosci wierszy macierzy wejsciowej oraz ilosci kolumn rownej "pieces"
  
  piece_length <- floor(ncol(wave) / pieces)
  ret_matrix <- matrix(0, nrow = nrow(wave), ncol = pieces)
  
  for(sample_number in 1:nrow(wave))
  {
    for(i in 1:pieces){
      sum <- 0
      for(j in ((i-1)*piece_length+1):(i*piece_length)){
        sum <- sum + (wave[sample_number, j]^2)
      }
      ret_matrix[sample_number, i] <- sum
    }
  }
  
  return (ret_matrix)
  
}