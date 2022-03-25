# Giorno 1: comandi di base
# ...................................................................

# questo è un commento (seguito dal simbolo '#')

# per lanciare un comando, copialo nelle Console e premi Invio, oppure vai alla riga del comando e premi Ctrl + Invio

# Operazioni matematiche
2 + 2 # addizione
2 * 2 # moltiplicazione
log(3) # algoritmo naturale
exp(1) # funzione esponenziale

# Espressioni più lunghe
sqrt(5) * ( (4 - 1/2)^2 - pi/2^(1/3) )

# per maggiori info, vedere: https://psicostat.github.io/Introduction2R/first-comands.html#math-operators

# Assegnare un valore a un oggetto
x <- 3 # creo oggetto 'x' associato al valore 3
x # stampo il valore di x
pippo_32 <- x / 3
pippo_32 # stampo il valore di pippo_32

# R è sensibile alle maiuscole!
pippo_32
Pippo_32 # questo comando da ERRORE perché la P è maiuscola: Error: object 'Pippo_32' not found

# Operatori relazionali
3 == 3 # uguale
3 != 3 # diverso
x >= 3 # maggiore o uguale
5 %in% c(3, 5, 8) # inclusione

# Operatori logici
x <- TRUE
y <- !x # negazione
y
x & (5 < 2) # congiunzione
x | (5 < 2) # disgiunzione inclusiva

# per maggiori info, vedere: https://psicostat.github.io/Introduction2R/first-comands.html#operators-rel-log

# Oggetti (R objects)
pippo_32 <- 2 # assegno valore a oggetto
pippo_32 # stampo oggetto
pippo_32 <- pippo_32 + 1 # aggiorno oggetto
pippo_32

# classi di oggetti
x <- TRUE # logical
x <- T # si può scrivere TRUE oppure T (stessa cosa per FALSE oppure F)
class(x) # funzione class() stampa la classe dell'oggetto
x <- 1.4 # numeric
class(x)
as.integer(x) # integer (numeri interi) - nota: scrivendo "as.nomeClasse(nomeOggetto)" l'oggetto viene convertito nella classe desiderata
x <- "Mi    piace R" # character
class(x)

# vettori
x <- c(1, 10.5, 3, 2) # si creano con la funzione c(), inseredo gli elementi (tutti della stessa classe!) seprati dalla virgola
x + 1 # aggiunge 1 a tutti gli elementi del vettore x
sqrt(x) # radice quadrata di tutti gli elementi di x
y <- c("mi","piace", "R") # vettore di character
y

# matrici
x <- matrix(1:12, nrow = 3, ncol = 4) # matrice di numeri da 1 a 12, con 3 righe e 4 colonne
rownames(x) <- y # per impostare i nomi delle righe
x

# Funzioni (R functions)
sqrt(x = 9) # radice quadrata dell'argomento x
seq(from = 1, to = 5) # sequenza numerica dal valore 'from' al valore 'to' 

# R Help system
?sqrt # per visualizzare le informazioni essenziali di una funzione basta eseguire il comando ?nomeFunzione

# pacchetti (= librerie di funzioni)
install.packages("ggplot2") # installare un pacchetto con il comando "install.packages("nome_pacchetto")
library(ggplot2) # apripre un pacchetto con il comando "library(nome_pacchetto)"
ggplot2::ggplot() # usare funzione senza aprire il pacchetto con nome_pacchetto::nome_funzione()


