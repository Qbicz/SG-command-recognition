#Aleksander Dzień
#Skrypt testujący funkcje speechBorder_wav()
#testSpeech.R

#library(tuneR); #jeżeli wcześniej była załadowana, to można zakomentować
# source("speech.R");
# #podaj nazwę pliku z rozszerzeniem wav
# fileName = "nagranie5.wav";
# #opis użycia funkcji w plku z funkcją
# #Wersja z podawanym plikiem .wav
# obj <- speechBorder_wav(fileName,1,1,1);
# str(obj)  #jeżeli returnValue jest 2, to zakomentować -zwracany jest przedział!

#Wersja z podawaniem wektora danych:
source("speechWektor.R");
Y<- nowe; #załaduj ISTNIEJACE dane
Fp<-8000;
obj<-speechBorder_Wektor(Y,Fp,3,1,1);
str(obj)  #jeżeli returnValue jest 2, to zakomentować -zwracany jest przedział!
