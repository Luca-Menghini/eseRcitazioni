# Giorno 4: Grafici
#.................................................................................................
# PRINCIPALI FUNZIONI
#.................................................................................................

# Funzioni di alto livello (stanno in piedi da sole)
# ...............................
plot() # scatter plot (grafico a dispersione)
boxplot() # boxplot (diagramma a scatola e a baffi)
qqnorm() # quantile-quantile plot

# frequenze
barplot() # grafico a barre (variabili categoriali)
hist() # istogramma (variabili continue)
pie() # pie chart

# interazioni
interaction.plot() # lo vedremo meglio nel prossimo incontro

# Funzioni di basso livello (aggiungono elementi alle prime)
# ................................

points() # aggiunge punti
lines() # aggiunge linee
text() # aggiunge del testo
abline(a = ..., b = ...) 
# retta di regressione lineare

# elementi più complessi
rect() # aggiunge rettangoli
polygon() # aggiunge poligoni

# altre caratteristiche del grafico
axis() # per modificare gli assi
legend() # per aggiungere una legenda

#.................................................................................................
# Parametri grafici
#.................................................................................................

# Esegui il comando ?par per vedere tutti i parametri grafici modificabili.

# Alcuni possono essere impostati usando gli argomenti delle funzioni grafiche:

# dimensioni
cex = 2 # grandezza testo e simboli (moltiplicatore)
lwd = 0.5 # spessore linee

# colore e forma
col = "red" # (vedi ?colors)
lty = 2 # tipo di linee (1=solid, 2=dashed, ...)
pch = 19 # forma dei punti (vedi ?points)

# titoli
main = "Titolo del grafico"
xlab = "Titolo asse x"

# Altri devono essere impostati all’interno della funzione par(), che va eseguita prima di generare il grafico:

# margini (bottom,left,top,right)
mai # dimensione margini (in ince)
mar # dimensione margini (in linee di testo)

# struttura griglie di grafici
# mfrow=c(nrow, ncol)
mfrow=c(2,1) # es. due grafici uno di fianco all'altro
mfrow=c(1,2) # es. due grafici uno sotto l'altro

#.................................................................................................
# Distribuzioni univariate: variabili continue
#.................................................................................................

X <- rnorm(n=100, mean=0, sd=1) # genero valori casuali dalla distribuzione normale standard
par(mfrow = c(2,2), mai = c(0.3,0.5,0.2,0.5)) # voglio 4 grafici in una sola finestra

plot(X) # scatter plot: grafico a dispersione (variabilità)

hist(X) # istogramma: distribuzione di frequenze per classi

boxplot(X) # box plot: diagramma a scatola (1° e 3° quartile) e a baffi (+/- 1.5 IQR)

qqnorm(X) # Q-Q plot: distribuzione cumulata di X vs. distribuzione cumulata normale)

#.................................................................................................
# Distribuzioni univariate: variabili categoriali
#.................................................................................................

Y <- factor(sample(x=c("A","B","C"),size=100,replace=TRUE)) # frequenze casuali per A, B e C

barplot(table(Y), main="Frequenze assolute") # grafico a barre: mostra le frequenze assolute

barplot(round(cumsum(table(Y)/length(Y)),2), main="Frequenze cumulate relative")

barplot(table(Y)) # stesso grafico ma aggiungo del testo in blu
text(x=c(0.7,1.9,3.1),y=10,labels=c("gruppo A","gruppo B","gruppo C"),col="blue",cex=0.7)

#.................................................................................................
# Valutare la normalità di una distribuzione
#.................................................................................................

X <- rnorm(n = 1000, mean = 60, sd = 20) # genero variabile casuale da distribuzione normale
par(mfrow=c(2,1),mai=rep(0.3,4),mar=rep(1.8,4))

hist(X,main="",xlab="",ylab="") # istogramma
qqnorm(X,cex=0.5,main="",pch=20) # Q-Q plot
qqline(X,col="red") # aggiungo linea distr. norm.

Y <- runif(n=1000,min=min(X),max=max(X)) # genero variabile casuale da distribuzione uniforme

hist(Y,main="",xlab="",ylab="") # istogramma
qqnorm(Y,cex=0.5,main="",pch=20) # Q-Q plot
qqline(Y,col="red") # aggiungo linea distr. norm.

# Distribuzione empirica
par(mfrow = c(1,1))
hist(X, freq = FALSE)
# Distribuzione teorica (attesa)
curve(dnorm(x, mean = 60, sd = 20),
      from = min(X), to = max(X),
      col = "red", lwd = 2,
      add = TRUE)

# Distribuzione empirica
hist(Y, freq = FALSE, ylim = c(0,0.02))
# Distribuzione teorica (attesa)
curve(dnorm(x, mean = 60, sd = 20),
      from = min(Y), to = max(Y),
      col = "red", lwd = 2,
      add = TRUE)

# asimmetria e curtosi
library(moments) 
c(skewness(X), kurtosis(X)) # (buono se ~ 0)
c(skewness(Y), kurtosis(Y))

# test Kolmogorov-Smirnov (H0: X è normale)
ks.test(X, y="pnorm", mean=mean(X), sd=sd(X))
ks.test(Y, y="pnorm", mean=mean(Y), sd=sd(Y))

# test Shapiro-Wilk (H0: X è normale)
shapiro.test(X)
shapiro.test(Y)


#.................................................................................................
# Distribuzioni bivariate
#.................................................................................................

# continua vs. continua
# .................................

Y <- rnorm(1000, mean=20, sd=1) # Y (indipendente da X)
Z <- X + rnorm(1000, sd = 20) # Z (proporzionale a X)

# Scatter plot:
par(mfrow=c(2,1),mai=rep(0.3,4),mar=rep(1.8,4),cex=.5)
plot(X, Y, main = paste("XY r =",round(cor(X, Y),2)))
abline(lm(Y~X),col="red") # retta di regr. lineare
plot(X, Z, main = paste("XZ r =",round(cor(X, Z),2)))
abline(lm(Z~X),col="red") # tilde (~) con Alt + 1-2-6

# Correlation plot:
df <- data.frame(X, Y, Z, P = -Z) # creo dataframe con 4 variabili
library(corrplot)
par(mfrow = c(1,1))
corrplot(cor(df),method="color",
         tl.cex=2,cl.cex=1,
         mar = c(0,0,0,15))

# categoriale vs. categoriale
# .................................

# creo data.frame con sesso (M/F) e livello di educazione (primaria, medie, superiori, università)
df <- data.frame(sex = sample(x = rep(c("F","M"),50), size = 100),
                 edu = sample(x = rep(c("prim","med","sup","uni"),25), size = 100))

table(df) # frequenze incrociate

# bar plot
barplot(table(df),legend.text = TRUE)
barplot(table(df),legend.text = TRUE, # versione con 'gruppi affiancati'
        beside = TRUE)

# continua vs. categoriale
# .................................

par(mfrow = c(2,2), mai = c(0.5,1,0.2,0.5))

# grafico a barre x le medie (SCONSIGLIATO!)
barplot(tapply(sleep$extra, sleep$group, FUN=mean), 
        main="SCONSIGLIATO!",cex.main=1.5, ylab = "extra"); abline(a=0,b=1,col="red",lwd=2)

# istogramma per gruppo
hist(sleep[sleep$group==1,"extra"], breaks=10, col = rgb(0,0,1,alpha=0.5),
     xlim = c(min(sleep$extra), max(sleep$extra)), xlab="", main="") 
hist(sleep[sleep$group==2,"extra"], breaks=10, col = rgb(1,0,0,alpha=0.5), add = TRUE)

# box plot per gruppo
boxplot(extra ~ group, data = sleep) 

# grafico a violino: densità per gruppo
library(vioplot)
vioplot(extra ~ group, data = sleep) 

# Hands on ...........................................................................

# Esercizio 1.22 ...............................................................

# Si costruisca un data.frame formato da 100 righe e 4 colonne ottenute come segue:
df <- data.frame(
  # Colonna 1: 100 numeri estratti con la funzione rnorm(), con media 4 e dev. standard 2 (USA LA DISTR. NORMALE)
  colonna1 = rnorm(n = 100, mean = 4, sd = 2),
  # Colonna 2: 100 numeri estratti con la funzione rt(), con 3 gradi di libertà (USA LA DISTR. t DI STUDENT)
  colonna2 = rt(n = 100, df = 3),
  # Colonna 3: 100 numeri estratti con la funzione rf(), con 4 e 8 gradi di libertà (USA LA DISTR. f di FISHER)
  colonna3 = rf(n=100, df1 = 4, df2 = 8),
  # Colonna 4: 100 numeri estratti con la funzione rchisq() con 4 gradi di libertà (USA LA DISTR. chi quadrato)
  colonna4 = rchisq(n = 100, df = 4))
head(df) # vedo le prime 6 righe

# Sul data.frame ottenuto:
#   1. Si producano le statistiche descrittive di base con il comando summary(). 
summary(df)

# Dalla lettura delle statistiche ottenute si cerchi di stabilire quanto siano simili tra loro le quattro variabili.
# - la media della prima colonna (rnorm) assomiglia a quella della quarta (rchisq), mentre mostra un valore maggiore a quello per la seconda (rt) e la terza colonna (rf)
# - guardando i minimi e i massimi, vediamo che solo le prime due colonne (rnorm e rt) hanno valori negativi, mentre la 3 e la 4 sono distribuite positivamente
# - la quarta colonna (rchisq) è quella con il range di valori più ampio, seguita dalla seconda (rt), dalla prima (rnorm) e dalla terza (rf)
# - a quarta colonna (rchisq) è anche quella con la differenza maggiore tra media e mediana (la distr. non è simmetrica!)
# - anche calcolando la differenza tra la mediana e, rispettivamente, il valore minimo e massimo per ogni variabile,
#   notiamo che le colonne 3 (rf) e 4 (rchisq) sono distribuite asimmetricamente (asimmetria positiva), con maggiore distanza tra
#   il vaolre massimo e la mediana rispetto a quella tra mediana e valore minimo

# 2. Si rappresentino con istogrammi le distribuzioni univariate delle variabili.
par(mfrow=c(2,2)) # per avere 4 grafici in un unica finestra
hist(df$colonna1, main = "distr. normale")
hist(df$colonna2, main = "distr. t di Student")
hist(df$colonna3, main = "distr. f di Fisher")
hist(df$colonna4, main = "distr. Chi quadrato")

# 3. Si rappresentino graficamente le distribuzioni bivariate con il comando plot().
par(mfrow=c(2,3), pch=19) # per avere 6 grafici in un unica finestra + forma dei punti numero 19
plot(colonna2 ~ colonna1, data=df, main = "t vs. norm.")
plot(colonna3 ~ colonna1, data=df, main = "f vs. norm.")
plot(colonna4 ~ colonna1, data=df, main = "chisq. vs. norm.")
plot(colonna3 ~ colonna2, data=df, main = "f vs. t")
plot(colonna4 ~ colonna2, data=df, main = "chisq. vs. t")
plot(colonna4 ~ colonna3, data=df, main = "chisq. vs. f")

# Esercizio 1.26 ...............................................................

# Il dataset gambling8, nel pacchetto ADati, contiene i dati relativi ad un campione di 1221studenti
# di età compresa tra 15 e 19 anni selezionati in 42 classi di 5 scuole. A ciascun soggetto viene
# somministrato un questionario che rileva le seguenti misure: 
# - ID = codice identificativo 
# - school = scuola frequentata 
# - class = classe frequentata 
# - age = età 
# - gender = genere 
# - frequency = frequenza di gioco 
# - perc_peers = percezione di quanto giocano i pari 
# - disapproval = grado di disapprovazione 
# - risk = percezione del rischio 
# - par_know = percezione del controllo genitoriale

# 1. Si importi il file in R.
data(gambling,package = "ADati")

# 2. Si valuti se nel dataset ci siano casi mancanti.
summary(gambling) # dal summary si direbbe che non ci sono dati mancanti
gambling[which(is.na(gambling)),] # anche con questo comando non sembrano esserci righe con valori mancanti

# 3. Si producano i grafici appropriati per le distribuzioni univariate delle variabili del data set.
par(mfrow=c(1,1)) # ritorno alla visualizzazione di un grafico alla volta
# nota: non ha senso visualizzare i codici identificativi (ID) perché c'è solo un caso (un partecipante) per ogni valore
barplot(table(gambling$school), main = "school") # school è categoriale --> barplot
barplot(table(gambling$class), main = "class") # stessa cosa per class

hist(gambling$age, main = "age (histogram") # età è continua -> istogramma
barplot(table(gambling$age), main = "age (barplot)") # però è divisa in classi discrete

barplot(table(gambling$gender), main = "gender") # gender è categoriale --> barplot

hist(gambling$frequency, main = "frequency") # frequency è continua -> istogramma

hist(gambling$perc_peers, main ="perc_peers (hist)") # perc_peers è stata probabilmente valutata su scala Likert (assunta come continua)
barplot(table(gambling$perc_peers), main = "perc_peers (barplot)") # però è divisa in classi discrete (frequenza percepita)

hist(gambling$disapproval, main = "disapproval") # disapproval sembra continua --> istogramma
hist(gambling$risk, main ="risk") # risk sembra continua --> istogramma
hist(gambling$par_know, main = "par_know") # par_know sembra continua --> istogramma

# 4. Si calcolino media, mediana e i decili della variabile disapproval.
mean(gambling$disapproval) # media
median(gambling$disapproval) # mediana (nota: è molto simile alla media)
quantile(gambling$disapproval, probs = seq(0, 1, by = 0.10)) # decili

# 5. Si rappresenti graficamente la distribuzione dei valori di frequency separatamente per
# maschi e femmine. Qundi si valuti in quale dei due gruppi presenti una frequenza media più alta.
hist(gambling[gambling$gender=="m","frequency"], col = rgb(0,0,1, alpha = 0.5), # istogramma femmine (uso rgb per dare trasparenza con l'argomento alpha)
     main = "frequency in femmine (rosso) e maschi (blu)", xlab = "Frequency", ylab = "Numero di casi", breaks = 20)
hist(gambling[gambling$gender=="f","frequency"], col = rgb(1,0,0, alpha = 0.5), # istogramma maschi (uso rgb per dare trasparenza con l'argomento alpha)
     add = TRUE, breaks = 20) # add = TRUE per sovraimporre il grafico a quello precedente
# dal grafico, si direbbe che i maschi abbiano valori di frequenza mediamente più alti delle femmine
# vediamo se è vero con la funzione mean
mean(gambling[gambling$gender=="m","frequency"]) # maschi: media 12.4
mean(gambling[gambling$gender=="f","frequency"]) # femmine: media 9.3

# 6. Si utilizzi una rappresentazione grafica che permetta di valutare se vi sia una relazione tra
#    la percezione di rischio e l'età dei soggetti. Si interpreti il grafico ottenuto.
plot(risk ~ age, data = gambling, pch = 19) # chiaramente questa rappresentazione non va bene perché l'età è discretizzata
boxplot(risk ~ age, data = gambling) # il boxplot mostra valori mediani, massimi, e del 3° quartile molto simili tra i gruppi di età
                                     # quello che cambia è piuttosto la parte 'bassa' delle distribuzioni
                                     # con valori leggermente maggiori per i sedicenni rispetto alle altre classi di età
                                     # ma non sembra esserci, in generale, una relazione sostanziale tra le due varaibili
library(vioplot)
vioplot(risk ~ age, data = gambling) # i grafici a violino confermano quanto osservato: le cinque distribuzioni sono molto simili
                                     # e non sembra quindi esserci una relazione tra le due variabli

# Esercizio 2.2 ................................................................

# Data la popolazione  omega = {0; 1; 2; 2; 5}

# 1. Si determini la distribuzione campionaria della media dei campioni ordinati di numerosità n = 3 senza reinserimento.
omega <- c(0,1,2,2,5) # creo popolazione omega (vettore numerico)

# combinazioni = possibili campioni ottenibili senza considerare l'ordine degli elementi -> N = 10
# nota: non equivalgono ai campioni ordinati! Ad es., il campione 0-1-2 è considerato uguale al campione 1-0-2
combn(x = omega, m = 3, simplify = FALSE) # restituisce tabella di frequenza per ogni combinazione senza reinserimento
combn(x = omega, m = 3, FUN = mean) # calcola la media di ogni combinazione
unlist(lapply(combn(x = omega, m = 3, simplify = FALSE),mean)) # comando alternativo

# permutazioni = possibili campioni ottenibili cambiando l'ordine degli elmenti (campioni ordinati) -> N = 60
# nota: ora il campione 0-1-2 è considerato come un campione diverso da quello 1-0-2
library(gtools)
campioni.ordinati <- 
        permutations(n = length(omega), # n = dimensione della popolazione
                     r = 3, # r = dimensione campionaria (sample size)
                     v = omega, # specifico il vettore con la popolazione
                     set = FALSE, # va messo FALSE per evitare che rimuova i valori doppi (in questo caso il valore 2)
                     repeats.allowed = FALSE) # di default è senza reinserimento
campioni.ordinati
class(campioni.ordinati) # è una matrice
nrow(campioni.ordinati) # 60 possibili campioni di numerosità = 2

# calcolo media per ogni campione -> distribuzione campionaria della media
distCamp.medie <- 
        apply(X = campioni.ordinati, # matrice campioni
              MARGIN = 1, # applico la funzione per riga (1) anziché per colonna (2)
              FUN = mean) # applico la funzione mean()
distCamp.medie # vettore numerico di dimensione N = 60

# 2. Si rappresenti grafcamente la distribuzione campionaria ottenuta.

# istogramma
hist(distCamp.medie)
hist(distCamp.medie, breaks = 10) # aggiusto breaks per vedere tutti i valori possibili

# bar plot
# in alternativa, tratto i valori possibili delle medie come una variabile categoriale
table(round(distCamp.medie,1)) # frequenze assolute per ogni valore possibile
barplot(table(round(distCamp.medie,1))) # grafico a barre (è praticamente uguale all'istogramma)

# 3. Si calcoli la media della distribuzione campionaria delle medie (mu_x) e la si confronti con la media della popolazione (mu).

# media della distribuzione campionaria delle medie
mu_x <- mean(distCamp.medie)
mu_x

# media della popolazione
mu <- mean(omega)
mu

# sono uguali!
mu_x == mu

# 4. Si calcoli l'errore standard della media (sigma_x).

# errore standard della media = variabilità (dev. standard) della distribuzione campionaria delle medie

# deviazione standard campionaria: va usata per un singolo campione
sd(distCamp.medie) # nota: sd() e var() dividono la devianza per i gradi di libertà (N - 1) anziché dividerla per N (!)

# deviazione standard (non campionaria): va usata con la popolazione e con la distribuzione campionaria delle medie
sd(distCamp.medie) * sqrt( (length(distCamp.medie)-1) / 
                                   length(distCamp.medie) ) # per usare N al denominatore, moltiplico per la radice di (N-1)/N

# nota: nella dev. st. campionaria l'aggiustamento del denominatore a N - 1 per ridurre le distorsioni rispetto a quella sull'intera popolazione
# la dispersione (variabilità) nel campione è sottostimata rispetto alla popolazione solo xk il campione è più piccolo

# 5. Si produca la distribuzione campionaria delle varianze con (dividendo per N-1) e senza la correzione (dividendo per N) e si rappresentino graficamente.

# valori ottenuti dividendo per N-1 (funzione var())
distCamp.var <- 
        apply(X = campioni.ordinati, # matrice campioni
              MARGIN = 1, # applico la funzione per riga (1) anziché per colonna (2)
              FUN = var) # applico la funzione var()
distCamp.var # vettore numerico di dimensione N = 60

# valori corretti (dividendo per N anziché per N-1)
distCamp.var.nc <- distCamp.var * sqrt( (length(distCamp.var)-1) / 
                                                length(distCamp.var) ) # per usare N al denominatore, moltiplico per la radice di (N-1)/N
distCamp.var.nc

# rappresentazione grafica: forma identica, ma la distr. della varianza campionaria è centrata su valori leggermente più alti
par(mfrow=c(1,2))
barplot(table(round(distCamp.var.nc,2)),main="Corretta (varianza)") # in alternativa, bar plot
barplot(table(round(distCamp.var,2)),main="Non corretta (varianza campionaria)")

# 6. Si calcolino la media della distribuzione campionaria delle varianze non corrette (mu_s2) e corrette (mu_sigma2) e si confrontino con la varianza della popolazione sigma2.

# media della distribuzione campionaria delle varianze non corrette (varianze campionarie)
mu_s2 <- mean(distCamp.var)
mu_s2

# media della distribuzione campionaria delle varianze corrette (ottenute dividendo la devianza per N anziché per N-1)
mu_sigma2 <- mean(distCamp.var.nc)
mu_sigma2

# varianza vera della popolazione (anche in questo caso devo dividere per N anziché per N-1)
sigma2 <- var(omega) * sqrt((length(omega)-1)/
                                    length(omega)) # moltiplico per la radice di (N-1)/N, in modo da cambiare il denominatore da N-1 a N
sigma2


# Esercizio 2.8 ..................................................
# Siano date una variabile casuale X distribuita normalmente con mu = 5 e sigma = 2 ed una statistica T = SOMMATORIA ( xi^2 ) 
# in cui xi, con i = 1; ...; n è un campione di n valori estratti da X. 
# Vogliamo studiare empiricamente le proprietà della distribuzione campionaria della statistica T.

# 1. Utilizzando la funzione curve() si rappresenti graficamente la distribuzione (teorica) della variabile X.
curve(dnorm(x, mean = 5, sd = 2), 
      from = 5 - 2*2, to = 5 + 2*2) # imposto i limiti dell'asse x come media +/- 2 dev. standard

# 2. Si estraggano 10000 campioni di dimensione n = 10 e su questi si applichi la funzione T. 
# Suggerimento: si può utilizzare la funzione apply() definendo la statistica T con la funzione function(x){ sum(x^2) }.
samples <- replicate(n = 10000, # numero di campionamenti
                     rnorm(n = 10, mean = 5, sd = 2))
str(samples) # il comando sopra ha generato una matrice con 10 righe e 10,000 colonne (una colonna per campione)
T <- function(x){ sum(x^2) } # creo la funzione T
T(x = 2) # testo la funzione
T(x = c(1,1)) # 1^2 + 1^2 = 2
T(x = c(1,2,-3)) # 1^2 + 2^2 - 3^2 = 1 + 4 + 9 = 14
distCamp.T <- apply(samples, MARGIN = 2, # applico la funzione per ogni colonna (MARGIN = 2)
                    FUN = T) # applico la funzione T
str(distCamp.T) # il comando sopra ha generato un vettore numerico di 10,000 valori, corrispondenti ai risultati di T su ciascun campione

# 3. Si rappresenti la distribuzione campionaria ottenuta della statistica T.
hist(distCamp.T)

# 4. Si calcolino media e deviazione standard della distribuzione campionaria ottenuta.
mean(distCamp.T) # media
sd(distCamp.T) * sqrt( (length(distCamp.T)-1) / # SD va corretta usando N al denominatore, anziché N - 1
                         length(distCamp.T) ) # per usare N al denominatore, moltiplico per la radice di (N-1)/N
sd(distCamp.T) # trattandosi di un numero di campioni molto elevato, la SD corretta e quella non corretta sono praticamente uguali

# 5. Se aumentiamo la numerosità campionaria cosa ci possiamo attendere rispetto alla media
#    della distribuzione campionaria di T? Si provi a dare una risposta e quindi si verifichi
#    empiricamente ripetendo i punti precedenti con campioni di numerosità 50 e 100.

# mi aspetterei un aumento sia nella media che nella variabilità (SD) della distribuzione campionaria
# questo perché la funzione T fa la somma dei quadrati di tutti i valori campionati
# quindi, all'aumentare della numerosità campionaria aumenta il numero di valori considerato, risultando in una somma più elevata

# calcolo distr. campionarie della statistica T per le 3 numerosità
distCamp.T.10  <- distCamp.T # n = 10 (già calcolato)
distCamp.T.50  <- apply(replicate(n = 10000,rnorm(mean = 5, sd = 2, n = 50)),  MARGIN = 2, FUN = T) # n = 50
distCamp.T.100 <- apply(replicate(n = 10000,rnorm(mean = 5, sd = 2, n = 100)), MARGIN = 2, FUN = T) # n = 100

par(mfrow=c(1,3)) # per avere 3 grafici nella stessa finestra
hist(distCamp.T.10, breaks = 30) # uso lo stesso valore di breaks per avere grafici più comparabili
hist(distCamp.T.50, breaks = 30)
hist(distCamp.T.100,breaks = 30)
# nota: la forma della distribuzione non cambia, ma sia media che variabilità aumentano all'aumentare di n

# calcolo medie delle distr. campionarie
mean(distCamp.T.10) # 289
mean(distCamp.T.50) # 1450
mean(distCamp.T.100) # 2901

# calcolo SD (non corrette, perché cambia poco) delle distr. campionarie
sd(distCamp.T.10) # 66
sd(distCamp.T.50) # 145
sd(distCamp.T.100) # 209

# Esercizio 2.14 ..................................................

# Il file vaes2015.rda contiene un campione di 200 soggetti coinvolti in una ricerca sul pregiudizio
# verso gli immigrati. In particolare, la variabile PREGIUDIZIO misura il grado di pregiudizio su
# una scala tra 0 e 7 in cui più alti sono i punteggi più è alto il livello di pregiudizio.

# 1. Si importino i dati in R.
wd() # comando per individuare la working directory
load("data/vaes2015.rda") # nota: il file è sia su Moodle che su GitHub. Io l'ho scaricato e l'ho messo nella cartella data, che si trova nella mia working directory
str(vaes2015) # ecco la struttura del dataset vaes2015 importato

# 2. Si rappresenti graficamente la distribuzione dei punteggi di pregiudizio.
par(mfrow=c(1,1)) # torno a visualizzare un solo grafico per finestra
hist(vaes2015$PREGIUDIZIO) # variabile probabilmente misurata con scala Likert 1-7 (assunta come continua) -> istogramma

# 3. Si calcolino media (x) e varianza (s2) dei punteggi di pregiudizio.
mean(vaes2015$PREGIUDIZIO)
var(vaes2015$PREGIUDIZIO) # nota: non uso la formula corretta perché è la varianza campionaria (denominatore: N - 1)

# 4. Assumiamo che i valori campionari di x e s2 siano una buona stima dei rispettivi parametri
# (mu e sigma2) di riferimento nella popolazione. 
# Si rappresenti graficamente (con il comando curve()) la distribuzione teorica dei punteggi di pregiudizio nella popolazione.
curve(dnorm(x, mean = mean(vaes2015$PREGIUDIZIO), sd = sd(vaes2015$PREGIUDIZIO)), # assumendo che la variabile sia distribuita normalmente
      from = min(vaes2015$PREGIUDIZIO), to = max(vaes2015$PREGIUDIZIO), ylab = "Densità distr.")

# per visualizzare la distr. teorica su quella empirica (quella empirica non è molto simmetrica)
hist(vaes2015$PREGIUDIZIO, freq = FALSE)
curve(dnorm(x, mean = mean(vaes2015$PREGIUDIZIO), sd = sd(vaes2015$PREGIUDIZIO)),
      from = min(vaes2015$PREGIUDIZIO), to = max(vaes2015$PREGIUDIZIO), add = TRUE, col = "red", lwd = 2)

# 5. Si calcoli la probabilità che un soggetto, selezionato a caso dalla popolazione, abbia un punteggio superiore a 4.2.
samples <- replicate(n = 10000, rnorm(n = 1, # estraggo casualmente 10,000 soggetti dalla popolazione (10,000 è un numero arbitrario, giusto per avere un gande numero di campioni)
                                      mean = mean(vaes2015$PREGIUDIZIO), sd = sd(vaes2015$PREGIUDIZIO)))
str(samples) # il comando sopra restituisce un vettore numerico di 10,000 valori
length(samples) # con la funzione length ottengo il numero totale di soggetti campionati (N = 10,000)
length(samples[samples > 4.2]) # con le parentesi quadre seleziono soltanto i casi con punteggio > 4.2
100 * length(samples[samples > 4.2]) / length(samples) # dividendo il secondo valore per il primo e moltiplicando per 100 ottengo la % di valori superiori a 4.2
# in sintesi, simulando il campionamento di un gande numero di soggetti dalla popolazione, il 20.5% ha PREGIUDIZIO > 4.2
# quindi posso stimare che la probabilità che un soggetto abbia punteggio > 4.2 è pari al 20.5%

# 6. Si calcoli la probabilità che un campione di 8 soggetti, selezionati a caso dalla popolazione,
# abbia un punteggio medio superiore a 4.2. 
# Prima di procedere si rifletta: tale probabilità sarà maggiore o minore di quella calcolata al punto precedente?

# considerando il basso numero di casi singoli con punteggio > 4.2 (20.5%),
# e aspettandomi che tale percentuale sarà mediamente riscontrata in ogni campione di 8 soggetti,
# mi aspetto che la probabilità di un valore medio per campione > 4.2 sia ancora più bassa di quella calcolata al punto precedente

# calcolo il risultato
samples <- replicate(n = 10000, rnorm(n = 8, # come sopra ma n = 8
                                      mean = mean(vaes2015$PREGIUDIZIO), sd = sd(vaes2015$PREGIUDIZIO)))
str(samples) # ora ottengo una matrice con 8 righe e 10,000 colonne
distrCamp.media <- apply(samples, MARGIN = 2, FUN = mean) # calcolo media per ogni campione (distr. campionaria della media)
100 * length(distrCamp.media[distrCamp.media > 4.2]) / length(distrCamp.media) # % di campioni con media > 4.2


# Esercizio 3.4 ..................................................

# Nel file MFTP.dat sono raccolti i punteggi ottenuti da un campione di 97 neolaureati in Psicologia
# nel Major Field Test in Psychology II (MFTP), un test per la valutazione delle competenze
# psicologiche. Da studi effettuati in anni precedenti è emerso che il punteggio medio ottenuto al
# test risulta essere 156.5. Si vuole sapere se il campione possa considerarsi compatibile con tutti
# quelli a cui il test è stato somministrato precedentemente.

# 1. Si importino i dati in R con la funzione scan().
wd() # comando per individuare la working directory
mftp <- scan("data/MFTP.dat") # nota: il file è sia su Moodle che su GitHub. Io l'ho scaricato e l'ho messo nella cartella data, che si trova nella mia working directory
str(mftp) # il file include 97 valori numerici

# 2. Si calcolino media e deviazione standard dei punteggi.
mean(mftp)
sd(mftp)

# 3. Si producano in un unico layout il grafico ad istogrammi, il boxplot ed il qqplot relativi ai punteggi.
par(mfrow=c(1,3)) # per avere 3 grafici in un unico layout
hist(mftp) # istogramma
boxplot(mftp) # boxplot
qqnorm(mftp, pch = 19) # qqplot con distribuzione normale (l'argomento pch controlla la forma dei punti)
qqline(mftp, col = "red") # aggiungo linea rossa per normalità

# 4. Si valuti con un test opportuno se la distribuzione dei punteggi possa considerarsi normale
# formulando le ipotesi H0 e H1 relative al test.

# usand il test statistico Shapiro-Wilk, testo l'ipotesi H0 che la distribuzione possa cosiderarsi normale contro l'ipotesi H1 che la forma della distribuzione non sia normale
shapiro.test(mftp) # non significativo -> non posso rifiutare H0 (quindi concludo che è normale)

# 5. Si valuti con il test opportuno se la media campionaria si differenzia significativamente da
# quella generale del test formulando le ipotesi H0 e H1 relative al test.
t.test(x = mftp, # assumendo che i residui della variabile mftp siano distribuiti normalmente, applico l'approccio NHST usando un t-test
       mu = 156.5, # la media empirica del campione va confrontata con la media teorica della popolazione (mu = 156.5)
       alternative = "two.sided") # volendo solo sapere se "si differenzia significativamente", imposto un test bidirezionale (a due code)

# interpretazione:
# il valore stimato di t è -1.37, che con N - 1 = 96 gradi di libertà, corrisponde ad un valore di p pari a 0.17
# definendo un livello di significatività (errore di primo tipo) pari a 0.05,
# posso concludere che, non potendo rifiutare H0, la media campionaria NON si differenzia significativamente da quella generale del test

# Esercizio 3.11 ..................................................

# La scala del QI è costruita in modo da avere, nella popolazione, media 100 e ds 15. 
# Supponiamo di sapere che i soggetti affetti da una certa patologia P, che influisce sulle funzioni cognitive,
# ottengano, nella scala del QI, punteggi medi pari a 80 (ds = 15). Vogliamo calcolare la potenza
# del test statistico per rilevare la differenza tra le medie delle due popolazioni, ipotizzando che siano entrambe normali.

# 1. Si definiscano le ipotesi H0 e H1 del test statistico.

# H0: la differenza tra le due medie mu1 (popolazione) e mu2 (soggetti con patologia P) è uguale a zero
# H1: la differenza tra le due medie mu1 e mu2 è diversa da zero, e in particolare è maggiore di zero (perché mu1 = 100 e mu2 = 80)

# più sinteticamente:
# H0: mu1 - mu2 = 0
# H1: mu1 - mu2 > 0

# formula alternativa (ancora più sintetica)
# H0: mu1 = mu2
# H1: mu1 > mu2

# se consideriamo i punteggi medi teorici attesi per le due popolazioni (mu1 = 100, mu2 = 80), la differenza attesa è pari a 100 - 80 = 20
# H0: mu1 - mu2 = 0
# H1: mu1 - mu2 = 20

# 2. Si rappresentino graficamente le distribuzioni teoriche delle differenze dei punteggi del QI sotto le due ipotesi.

# qui rappresento la distribuzione teorica della popolazione (in nero) e quella dei soggetti con patologia P (in rosso)
par(mfrow=c(1,1))
curve(dnorm(x, mean = 100, sd = 15), from = 80-15*3, to = 100+15*3)
curve(dnorm(x, mean = 80, sd = 15), add = TRUE, col = "red") # questo però non risponde al quesito!

# il quesito mi chiede di rappresentare le distribuzioni teoriche delle differenze
# il ché mi richiede, in primo luogo, di calcolare la media e l'errore standard della differenza

# sotto H0, la differenza media è uguale a zero
diff.H0 <- 0

# sotto H1 la differenza è maggiore di zero, e nello specifico è pari a 20
diff.H1 <- 20

# per entrambe le ipotesi, l'errore standard della differenza è dato dala seguente formula:
# ES_diff = radice quadrata della somma tra la varianza nella pop. 1 e la varianza nella pop. 2 (entrambe pari a 15)
ES_diff <- sqrt(15^2 + 15^2)

# ora posso rappresentare graficamente la distribuzione teorica delle differenze dei punteggi del QI sotto H0 (con media diff.H0 = 0)
curve(dnorm(x, mean = diff.H0, sd = ES_diff), 
      from = 0-5*ES_diff, to = 20+5*ES_diff) # aggiungo 5 SD ai limiti del grafico

# allo stesso modo, aggiungo la distribuzione teorica delle differenze sotto H1 (con media diff.H1 = 20)
curve(dnorm(x, mean = 20, sd = ES_diff), col = "red", add = TRUE)

# 3. Si determini il valore critico per rigettare H0 al 5%.

# per farlo, uso la funzione qnorm, che restituisce il valore (quantile) associato ad una data probabilità cumulata p
# nello specifico, voglio sapere quale differenza estratta da una popolazione con forma normale, mu = 0 (sotto H0) e sigma = ES_diff
#                  ha al suo di sopra il 5% della popolazione
crit <- qnorm(p = 0.05, mean = 0, sd = ES_diff, lower.tail = FALSE)
crit # il valore critico per rigettare H0 al 5% è 34.89

# infatti, se uso la funzione pnorm() con il valore critico, ottengo la probabilità p = 0.05
alpha <- pnorm(q = crit, mean = 0, sd = ES_diff, lower.tail = FALSE) 
alpha

# 4. Si determini graficamente la regione critica

abline(v = crit, col = "blue", lty = 2, lwd = 2) # aggiungo una linea verticale corrispondente al valore critico
# la regione critica è rappresentata da quella parte della curva nera (H0) a destra della linea blu tratteggiata

# con la funzione polygon() posso anche evidenziare l'area critica di rosso
x <- seq(from=crit, to=diff.H0+5*ES_diff, length=100) # sequenza di valori da crit fino a 5 deviazioni standard
polygon(x = c(x, rev(x)), # coordinate poligono: x = coordinate asse X (metto il vettore x e il suo inverso)
        y = c(dnorm(x, mean = 0, sd = ES_diff), rep(0, 100)), col = "red") # y = dnorm(H0) + 0

# per dettagli sulla funzione polygon, vedere: https://statisticsglobe.com/r-polygon-function-plot/

# con la funzione text() posso aggiungere il simbolo alpha nelle coordinate 20, 0.002
text(x = 20, y = 0.002, labels = expression(alpha), col = "red")

# 5. Si determini graficamente la potenza del test.

# la potenza del test è rappresentata da quella parte della curva rossa (H1) a destra della linea blu tratteggiata

# con la funzione polygon() posso evidenziare questa area di blu (applico trasparenza con la funzione rgb)
polygon(x = c(x, rev(x)), # coordinate poligono: x = coordinate asse X (metto il vettore x e il suo inverso)
        y = c(dnorm(x, mean = 20, sd = ES_diff), rep(0, 100)), col = rgb(0,0,1,alpha=0.5)) # y = dnorm(H1) + 0

# con la funzione text() posso aggiungere il simbolo alpha nelle coordinate 55, 0.01
text(x = 55, y = 0.01, labels = expression(beta), col = "blue")

# 6. Si calcoli la potenza del test (1 - beta)

# userò la funzione pnorm, come quella usata a linea 641, ma impostando la differenzia media sotto H1 (cioè 20) nell'argomento mean
beta <- pnorm(q = crit, mean = 20, sd = ES_diff, lower.tail = TRUE) # nota: lower.tail = TRUE significa che considera i valori inferiori a q
beta # questa è la probabilità di ottenere un valore meno estremo di quello critico, se H1 è vera = ERRORE DI SECONDO TIPO (beta)

# infine calcolo la potenza del test facendo 1 - beta
1 - beta # il test ha una potenza del 24%

# avrei anche pututo impostare direttamente l'argomento lower.tail uguale a FALSE per calcolare la probabilità di valori di H1 maggiori di q
power <- pnorm(q = crit, mean = 20, sd = ES_diff, lower.tail = FALSE)
power

# 7. Si determini il numero di soggetti necessario per ottenere una potenza dell'80%. 
# Suggerimento: si utilizzi la funzione power.t.test().

# vediamo cosa fa la funzione power.t.test() e quali argomenti richiede
?power.t.test
power.t.test(n = NULL, # n è il numero di osservazioni per gruppo (lo lasciamo uguale a NULL perché si tratta proprio dell'info che vogliamo stimare)
             delta = diff.H1, # delta è la differenza 'vera' tra le medie delle due popolazioni (in questo caso, sotto H1 è pari a 20)
             sd = ES_diff, # sd è la deviazione standard 'vera' delle differenze (l'errore standard stimato qui sopra)
             sig.level = 0.05, # sig.level è l'errore di I tipo (alpha = 0.05, ovvero il 5%)
             power = 0.80, # power è la potenza desiderata (1 - beta = 0.80, ovvero l'80%)
             alternative = "one.sided")
# risultato: per avere una potenza dell' 80%, il numero necessario di soggetti (per gruppo) è 14.64