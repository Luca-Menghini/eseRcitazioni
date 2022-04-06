# Giorno 2: vettori, fattori, matrici e data.frame
#.................................................................................................
# VETTORI
#.................................................................................................

# creare un vettore con la funzione c()
x <- c(1, 10.5, 3, 2) # creo vettore numerico
y <- 1:10 # un altro vettore numerico
y # stampo i valori di y
pippo32 <- c("uno", "due", "tre", "quattro", "cinque") # vettore di caratteri
pippo32
(z <- rep(c(TRUE, FALSE), each = 2)) # vettore logico (le parentesi tonde prima e dopo fanno stampare il comando, oltre a salvare l'oggetto z)

# cambiare la classe di un vettore
as.character(x) # converte la classe del vettore da numeric a character
as.numeric(z) # da logical a numeric (FALSE = 0, TRUE = 1)

# operazioni con i vettori
y*2 # moltiplica tutti i valori per 2
round(sqrt(y), 2) # rad. quadrata dei valori di y, arrotondati a 2 decimali
length(y) # numero di elementi nel vettore
sum(y) # somma gli elementi nel vettore
max(y) # valore massimo
mean(y) # media
median(y) # mediana
var(y) # varianza
sd(y) # deviazione standard

# selezionare gli elementi di un vettore con le parentesi quadre
pippo32[3] # seleziono il terzo valore di pippo32
pippo32[3:4] # terzo e quarto valore del vettore pippo32
pippo32[c(4, 2)] # quarto e secondo valore (non scrivere pippo32[4,3]!)
y[y <= 3 | y > 8] # valori di y minori o uguali a 3 o maggiori di 8
pippo32[pippo32 != "due"] # valori di pippo32 diversi da "due"
pippo32[substr(pippo32, 2, 2) == "u"] # valori con lettera "u" in 2a posizione


# Esercizio 1.5......................................

# 1. Si crei un vettore con nome PI che contenga 10 valori in sequenza e passo costante da ????? a +?? e si visualizzino i valori ottenuti.
(PI <- seq(from=-pi, to=pi, length.out = 10))
length(PI)

# 2. Si calcolino media e varianza degli elementi contenuti in PI.
mean(PI) # media (nota: "e-17" significa moltiplicato per 10^(-17) cioè con 17 zeri dopo la virgola)
var(PI) # varianza

# 3. Si calcoli la somma del terzo, quinto e ottavo elemento del vettore PI (risultato = -0.3490659).
PI[3] + PI[5] + PI[8] # in alternativa

# 4. Si calcolino e si visualizzino le radici quadrate degli elementi del vettore PI
sqrt(PI) # restituisce "NaN" ("Not a Number") per i primi 5 valori (radice quadrata di numero negativo)


#.................................................................................................
# FATTORI
#.................................................................................................

# trasformare vettore in factor
as.factor(pippo32) # da character a factor
summary(pippo32) # summary() mostra un sommario della variabile
summary(as.factor(pippo32)) # # per i fattori, mostra la freq. di ogni livello -> equivale a table()
(y <- rep(c(2,4,6),3))
as.factor(y) # da numeric a factor
summary(y)
summary(as.factor(y))

# creare un fattore
factor(x = c("C",rep("A",3),c("B","A","C")))
(x <- factor(x = c("C","A","B","A"), # vettore
             levels = c("C","A","B"))) # livelli

# livelli di un fattore
levels(x) # levels() stampa i nomi dei livelli
factor(x, levels=c("B","A","C")) # cambia ordine
levels(x) <- c("Uno","Due","Tre") # cambia nomi
x

# fattore ordinato (default: ordine alfabetico)
factor(x = c("Maria","Mauro","Teresa","Carlo"), ordered = TRUE)


# Esercizio 1.8.............................................

# 1. Si crei un vettore numerico X contenente 50 valori estratti a caso dall'insieme {0, 1, 2, 3}.
?sample # funzione sample()
X <- sample(x = c(0,1,2,3), size = 50, replace = TRUE) # in alternativa: sample(x=1:3,size=50,replace=TRUE)

# 2. Si produca la tabella di frequenze di X.
table(X)

# 3. Si converta il vettore X in un vettore categoriale, Y, usando il comando factor().
Y <- factor(X)
Y

# 4. Si visualizzino i livelli del vettore Y.
levels(Y)

# 5. Si converta il vettore X in un vettore categoriale, Z, assegnando ai valori {0, 1, 2, 3} le
# seguenti etichette: 0 = giallo, 1 = verde, 2 = rosso, 3 = blu.
?factor # funzione factor(), l'argomento "labels" è quello che ci interessa in questo caso
Z <- factor(Y, levels = 0:3, labels = c("giallo","verde","rosso","blu"))

# 6. Si produca la tabella di frequenze di Z.
table(Z)


#.................................................................................................
# MATRICI
#.................................................................................................

# creare una matrice
?matrix # funzione matrix()
(x <- matrix(1:12, nrow = 3, ncol = 4)) # matrice di numeric
matrix(1:12, nrow = 3, ncol = 4, byrow = TRUE) # byrow=TRUE per inserire i valori andando per riga
matrix(c("Mar","Mau","Ter","Car"), nrow = 2) # matrice di character

# selezionare gli elementi di una matrice
x[1,2] # prima riga, seconda colonna
x[2,1] # seconda riga, prima colonna
x[1:3,2] # righe 1-3, seconda colonna
x[1,] # prima riga, tutte le colonne
x[,2] # seconda colonna, tutte le righe

# unire due matrici
cbind(x, # matrice 1
      matrix(rep(3,6),nrow=3)) # matrice 2 (per colonna)
rbind(x, # matrice 1
      matrix(rep(3,4),ncol=4)) # matrice 2 (per riga)

# nomi e numero di righe e colonne
rownames(x) <- c("a","b","c") # nomi righe
colnames(x) <- 1:4 # nomi colonne
x
c(nrow(x),ncol(x)) # numero righe e col. = dim(x)

# matrice trasposta (inverte righe e col.)
t(x) 


# Esercizio 1.6 #.............................................

# 1. Si crei una matrice A di valori compresi tra 1 e 5 (a scelta) composta da 10 righe e 4 colonne.
A <- matrix(sample(x=1:5, size=10*4, replace=TRUE), nrow=10, ncol=4) # dice "a scelta", quindi farlo a caso andrà bene...
A

# 2. Si calcolino le medie delle righe e delle colonne di A.
?colMeans # funzione colMeans()
?rowMeans # funzione rowMeans()
rowMeans(A) # media per ogni riga
colMeans(A) # media per ogni colonna

# 3. Si costruisca una matrice B composta da 5 righe estratte a caso della matrice A.
B <- A[sample(1:nrow(A), size=5),]
B

# 4. Si rappresenti graficamente la tabella di frequenze ottenuta dalla matrice A (funzione barplot()).
table(A) # tabella di frequenze
barplot(table(A)) # rappresentazione grafica della tabella di frequenze


#.................................................................................................
# DATAFRAME
#.................................................................................................

# creare un dataframe di 3 colonne con la funzione data.frame()
?data.frame
(x <- data.frame(Num = 1:4, # colonna di classe Integer
                 Char = c("a","b","c","d"), # colonna di classe Character
                 Logi = rep(c(TRUE,FALSE),2))) # colonna di classe Logic
str(x) # struttura del dataframe
summary(x) # sommario per ogni variabile in base alla sua classe

# manipolare un dataframe (come per le matrici!)
x[2, 2:3] # selezione seconda riga, colonne 2 e 3
cbind(x,data.frame(new=4:1)) # unione per colonna
Y <- data.frame(Num=10,Char="z",Logi=FALSE) # creo nuovo data frame
rbind(x[1:3,],Y) # unione per riga
rownames(x) # nomi di riga: default = 1:nrow(x)
colnames(x)[2] # nome della seconda colonna
nrow(x) # numero di righe
ncol(x) # numero di colonne
t(x) # trasporre un dataframe

# selezione di una colonna con il simbolo "$"
x$Char # seleziono colonna Char
x$Char[2] # secondo elemento della colonna Char
x$Char[2] == x[2,2] # due comandi equivalenti (prova a lanciarli separatamente)
x$Char <- NULL # elimino colonna Char
x[x$Num < 3,] # seleziono solo le righe con Num < 3
x[,"Logi"] # seleziono colonna scrivendo il suo nome tra virgolette
x[1:2,c("Num","Logi")] # prime due righe delle colonne "Num" e "Logi"

# testa e coda
head(x) # prime 4 righe
head(x, 2) # prime 2 righe
tail(x, 1) # ultima riga


# Esercizio 1.9 #.............................................

# Si considerino 5 studenti (Gino, Pino, Tino, Beppe, Lino) con i rispettivi numeri di matricola
# (507, 535, 566, 955, 515) ed il voto preso ad un esame (19, 27, 28, 25, 25).

# 1. Si producano tre vettori contenenti rispettivamente i nomi, la matricola ed il voto dei CINQUE studenti.
nomi <- c("Gino", "Pino", "Tino", "Beppe", "Lino")
matricole <- c(507, 535, 566, 955, 515)
voti <- c(19, 27, 28, 25, 25)

# 2. Si uniscano opportunamente i tre vettori per produrre un dataframe.
df <- data.frame(nomi, matricole, voti)
df

# 3. Si cancellino dal workspace i tre vettori prodotti al punto 1.
?rm # funzione rm()
rm(nomi, matricole, voti)

# 4. Si selezioni il voto di Beppe.
df[df$nomi=="Beppe","voti"]
df[df$nomi=="Beppe",3] # comando equivalente

# 5. Si selezioni la riga con i valori relativi a Pino.
df[df$nomi=="Pino",] # va messa la virgola prima della parentesi quadra per selezionare tutte le colonne!

# 6. Si selezionino (con il comando subset()) gli studenti con voto pari a 25.
?subset # funzione subset()
subset(df, voti==25)

# 7. Si selezionino (con il comando subset()) gli studenti con voto superiore a 20.
subset(df, voti>20)


#.................................................................................................
# LISTE
#.................................................................................................

# creare una lista con la funzione list()
?list
x <- list(Num = 1:4, # vettore numerico
          Matr = matrix(1:12, nrow=3), # matrice
          df = x, # data.frame
          lst = list(1:3,2:3)) # lista
str(x) # struttura del dataframe

# seleziono gli elementi di una lista con le parentesi quadre
x[1] # parentesi quadre singole = crea sotto-lista
class(x[1])
x[[1]] # parentesi quadre doppie = estrae l'oggetto
class(x[[1]])
x[[3]][2,1] # seconda riga e prima colonna del dataframe incluso nel terzo elemento di x