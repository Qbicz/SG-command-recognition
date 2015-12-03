#Aleksander Dzień
#Funkcja do wykrywania przedziału z mową - wersja 1
#nazwa pliku: 	speechWektor.R
#nazwa funkcji:	speechBorder_Wektor()
#Wymagania:    	tuneR library
#			Pierwsze 100ms transmisji jest ciszą!!! 
#Argumenty wejściowe:
#Y    - wektor z danymi
#Fp   - podany czas próbkowania, format: podawane w Hz np: 8000
#mode     - wybór algorytmu przetwarzania danych:
#	 1  - okienko 20 ms, kwadrat próbek, normalizacja -> [0 1]
#	 2  - okienko 20 ms, wart. bezwzględna próbek, normalizacja -> [0 1]
#	 3  - brak okna, kwadrat próbek, normalizacja -> [0 1]
#	 4  - brak okna, wart. bezwzględna próbek, normalizacja -> [0 1]
#returnValue - zwracana wartość
#	 1  - obiekt wave
#	 2  - para - początek i koniec przedziału, gdzie wykryto mowe
#drawPlot - rysjue wykresy wynikowych plików -POWODUJE POJAWIANIE SIĘ OSTRZEŻEŃ W KONSOLI
#  0  -  nieaktywna                          -ALE NIE POWODUJE TO BŁĘDÓW PRACY ALGORYTMU
#  1  -  aktywna

#Wartość zwracana: 
#W zależności od wybranej opcji returnValue - obiekt wave lub dwuelementowy wektor

speechBorder_Wektor <- function(Y,Fp,mode,returnValue, drawPlot){
  
  library(tuneR); #jeżeli wcześniej była załadowana, to można zakomentować
  
  y <- Y;     # dla dalszego przetwarzania 
  doZapisu<-Y;
  Fs = Fp;
  #pozbycie się końcówek nagrania - mogą występować zakłócenia od włączania/wyłączania nagrywani
  #jeżeli wcześniej dokonano filtracji to ZAKOMENTOWAĆ
  
  #UWAGA--------------------------------------------------#
  bin <- 0.05*Fs;  #50ms #było 20ms
  y <- y[bin:(length(y)-bin)];
  #Tą część zakomentować jeżeli plik poddany był obróbce i nie ma na końcach przedziałów pierdolnięć od włącznika
  
  maxY = max(abs(y));
  #normalizacja -> [0 1], typ float
  
  if(max(y) > 0){       #jeżeli dostarczony plik nie był znormalizowany
  ynorm = abs(y/maxY);
  }
  #ustalenie progów na podstawie wstępnych 100 ms nagrania, gdzie powinna panować 'względna' cisza
  
  cisza <- 0.1*Fs; #100ms
  interval <- ynorm[1:cisza];	#pierwsze 100 ms dla obliczeń
  
  #pomocnicze flagi dla ustalenia progow, oraz przedział dla for
  
  flagFirst = 0;
  iteracje <- 1:cisza;
  
  #przelicznik dla nieznormalizowanych wyników
  
  STE = 0;
  wektorSTE = 0;
  tresholdup = 0;
  tresholddown = 0;
  
  ramka = 0.02*Fs; 	    #20 ms
  moveRamka = ramka/2;  #przesunięcie ramki o 10 ms
  #indeksRamka = 0;      #liczba ramek	
  energiaRamki = 0;	    #wektor energii ramek	 
  
  pBeg = cisza;
  pEnd = cisza + ramka;
  koniecDanych = length(ynorm); 
  
  if(mode == 1 | mode == 2){
    if(mode == 1){
      yp2 <- ynorm^2;
    }else if (mode == 2){
      yp2 <- ynorm;         #brak zmian, zmienna potrzebna do przetwarzania
    }
    #Stworzenie zbioru ramek czasowych sygnału z wyliczoną energią:
    while(pBeg < koniecDanych){		
      if (pEnd > koniecDanych){
        pEnd = koniecDanych; 
      }
      sumaSTE = 0;				#zerowanie sumy dla następnej ramki
      k <- yp2[pBeg:pEnd];
      sumaSTE = sum(k);
      
      if (flagFirst == 0){
        flagFirst = 1;
        #indeksRamka = 1;
        energiaRamki = sumaSTE;
      }else{
        #indeksRamka = idneksRamka + 1;
        energiaRamki<-append(energiaRamki, sumaSTE,after = length(energiaRamki)); 
      }
      #Przesunięcie ramki w prawo
      pBeg = pBeg + moveRamka;
      pEnd = pEnd + moveRamka;	 
    }
    #obliczenie progów
    maxSTE = max(energiaRamki);
    IE = 0.25*maxSTE;
    
    flagFirst = 0;
    
    for (i in seq(along = iteracje)){
      if (flagFirst == 0){
        if (mode == 1){
          STE = interval[i]^2;
        } else if (mode == 2){
          STE = interval[i];
        }
        flagFirst = 1;
      }else{
        if (mode == 1){
          value = interval[i]^2;
        } else if (mode == 2){
          value = interval[i];
        }
        STE<-append(STE,value, after = length(STE)); 
      }
    }
    
#PROGI------------------------------MOŻNA ZMIENIAĆ WSPÓŁCZYNNIKI#	
        
    #Dla algorytmu Rabinery 
    MINSTE = min(IE,(mean(STE)+sd(STE)));
    if(mode == 1){
      tresholdup = 3000*MINSTE; #2500
      tresholddown = 2500*MINSTE;   #2000
    } else if (mode == 2){
      tresholdup = 3000*MINSTE; 
      tresholddown = 2500*MINSTE;    
    }
    
  }else if (mode == 3 | mode == 4){
    if(mode == 3){
      yp2 <- interval^2;
      tresholdup <- 1000*mean(yp2);
      tresholddown <- 800*mean(yp2);
      print(tresholdup)
      print(tresholddown)
    }else if (mode == 4){
      #yp2 <- interval;         #brak zmian, zmienna potrzebna do przetwarzania
      tresholdup <- 5*max(interval);
      tresholddown <- 2*max(interval);
    }
    
  }
  
  #KONIEC SEKCJI Z PROGAMI-----------------------------------------#
  
  #selekcjonowanie końca i początku wypowiedzi
  flagInside = 0;		#okresla czy znajdujemy się pod obwiednią funkcji czy nad
  znacznikMowy = 0;		#przechowuje przedziały czasowe dla których wykryto aktywność
  Wynik = 0;			#ostateczny wynik przetwarzania - przedział czasowy wypowiedzi
  
  flagFirst = 0;
  if(mode == 1 | mode == 2){
    k<-1:length(energiaRamki);
    for (i in seq( along = k)){
      if (flagInside == 0){
        if (energiaRamki[i] > tresholdup){
          flagInside = 1;
          if (flagFirst == 0){
            flagFirst = 1;
            znacznikMowy = 1+moveRamka*(i-1);
          }else{
            value <- 1+moveRamka*(i-1);
            znacznikMowy<-append(znacznikMowy, value,after = length(znacznikMowy)); 
          }			
        }
      }else if (flagInside == 1){
        if (energiaRamki[i] < tresholddown){
          flagInside = 0;
          if ((moveRamka*i+ramka) < length(yp2)){
            value <- ramka+moveRamka*(i-1);
            znacznikMowy<-append(znacznikMowy, value,after = length(znacznikMowy));
          }
          
        }
        
      } 	
    }
  }else if (mode == 3 | mode == 4){
    k<-cisza:length(ynorm);       #przeszukiwanie zaczynamy poza obszarem ciszy
    if(mode == 3){
      yp2<-ynorm^2;
    }else if (mode == 4){
      yp2<-ynorm;
    }
    for (i in seq( along = k)){
      if (flagInside == 0){
        if (yp2[i] > tresholdup){
          flagInside = 1;
          if (flagFirst == 0){
            flagFirst = 1;
            znacznikMowy = i;
          }else{
            znacznikMowy<-append(znacznikMowy,i,after = length(znacznikMowy)); 
          }			
        }
      }else if (flagInside == 1){
        if (yp2[i] < tresholddown){
          flagInside = 0;
          znacznikMowy<-append(znacznikMowy,i,after = length(znacznikMowy));
        }
        
      } 	
    }
    
  }
  
  #Wybranie odpowiedniego kawałka mowy
  if(length(znacznikMowy)<2){
    print("znacznikMowy mniejszy od 2 - nie wykryto poprawnie mowy!");
    Wynik <- c(cisza,length(ynorm));
  } else if (length(znacznikMowy)%%2 & length(znacznikMowy)>2) {
    print("Nie parzysta liczba przedziałów w znacznikMowy");
    Wynik <- c(znacznikMowy[1],znacznikMowy[length(znacznikMowy)-1]);
  } else{
    Wynik <- c(znacznikMowy[1],znacznikMowy[length(znacznikMowy)]); #Wybór wspólnego przedziału
  }
  
  #utworzenie obiektu wave
  z <- doZapisu[Wynik[1]:Wynik[2]];  #przepisanie próbek sygnału jeszcze przed normalizacją!
  wObj = Wave(z,samp.rate = Fs, bit = 16);
  #zapis do pliku
  writeWave(wObj,"nagranieObrobioneR.wav");
  
  #wyrysowanie obiektu: normalizacja:
  doPlot<-doZapisu/maxY;
  normPlot<-wObj@left/maxY;
  
  
  if(drawPlot == 1){
    if(mode == 1 | mode == 2){
      attach(mtcars)
      layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
      plot(wObj@left,main="Po obróbce",col="blue")
      plot(energiaRamki, main="Energia ramek")
      abline(v = Wynik[1]/moveRamka, lwd=2,col="red")
      abline(v = Wynik[2]/moveRamka, lwd=2,col="red")
      abline(h = tresholdup, lwd=1,col="green")
      abline(h = tresholddown, lwd=1,col="red")
      plot(sndObj@left,main="Dane wejściowe")
      
    }else if(mode == 3 | mode == 4){
      attach(mtcars)
      par(mfrow = c(2,1))
      plot(doPlot, main="Dane wejściowe")
      abline(v = Wynik[1], lwd=2,col="red")
      abline(v = Wynik[2], lwd=2,col="red")
      plot(normPlot,col="blue", main="Po obróbce")
      abline(h = tresholdup, lwd=1,col="black")
      abline(h = tresholddown, lwd=1,col="red")
      
    }
  }
  
  if(returnValue == 1){
    
    return(wObj);
  }else if (returnValue == 2){
    Wynik[1]<- Wynik[1]+bin;  #powrót indeksacji do standardu przzekazanego pliku
    Wynik[2]<- Wynik[2]+bin;
    return(Wynik);
  }	
  
}
