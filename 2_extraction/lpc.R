library(tuneR)

'
Funkcja pomocnicza zwaracj¹ca wspó³czynnki LPC dla wektora danych.
Wejœcie:
1) vector_in  - wektor danych wejœciowych
2) p          - rz¹d predykcji (iloœæ zwracanych wspó³czynników)

Zwraca poziomy wektor wspó³czynników o rozmiarze p

'
    
lpc_single_vector <- function(vector_in, p){
	
	len <- length(vector_in)
	
	if(len%%p == 0){
	  len <- len-1
	}
	
	N <- as.integer(len/p)*p + 1
	U <- matrix(0, p, N-p)
  
	for(i in 1:(N-p)){
		  U[,i] <- vector_in[i:(i+p-1)]
	}
	
	x <- vector_in[(p+1):N]
	
	a <- solve((U %*% t(U))) %*% U %*% x
  
	return (t(a))
}

'
Funkcja zwaracj¹ca wspó³czynnki LPC dla próbki w formacie wav.
Wejœcie:
1) wave_input  - próbka w formacie wav
2) p           - rz¹d predykcji (optymalnie w przedziale 10-15) 

W przypadku sygna³u stereo zwraca osobny zestaw wspó³czynników dla ka¿dego kana³u.

Przyklad uzycia:
wave_file_input <- file.path("scie¿ka_do_pliku_wav")
wave_input <- readWave(wave_file_input)
lpc <- lpc_coefficients(wave_input, 14)

'

lpc_coefficients <- function(wave_input, p){
  
  if(wave_input@stereo == TRUE){
      lpc_l <- lpc_single_vector(wave_input@left, p)
      lpc_r <- lpc_single_vector(wave_input@right, p)
      
      return (rbind(lpc_l, lpc_r))
  }
  else{
      return (lpc_single_vector(wave_input@left, p))
  }
    
}