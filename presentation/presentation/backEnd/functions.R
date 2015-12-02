# functions.R

# chlopie nic sie nie zastanawiaj tylko ctrlC ctrlV wszystko co masz tutaj i ma zwracac int i wszystko
#bedzie dobrze

# a jak chcesz zeby parametry jakies pobierala ta funkcja to tez je dopisz, wszystko zadziala cacy
# nie ma co marudzic



myfunction <- function(){
  # O TUTAJ
  k<- sample(1:6,1) ## Roboczo losuje liczbe od 1 do 6 i ja zwraca, ale ma liczyc i zwracac (:
  print(k)
  return(k)
}

## a i jak jakies parametry bierze ta funkcja jak na przyklad probke dzwieku to trzeba to dodac
## w server.R i tam na dole jest linijka :           switch(myfunction(),  
## i ta funkcja jest TYLKO TAM, KURWA.