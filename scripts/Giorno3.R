# Giorno 3: Worskpace e working directory, caricare un dataset e calcolare le principali statistiche descrittive
#.................................................................................................
# FUNZIONI E PACCHETTI
#.................................................................................................

sqrt(x = c(1,2,3)) # funzione eseguita con sintassi nome_funzione(nome_argomento = valore_argomento)
sqrt(c(1,2,3)) # seguendo il giusto ordine degli argomenti, non è necessario specificarne il nome

# R Help system: ?nome_funzione
?sqrt # per capire cosa fa una funzione, conoscerne gli argomenti e l'ordine di default

# installare e aprire un pacchetto: esempio con il pacchetto car
install.packages("car") # installare un pacchetto
library(car) # apripre un pacchetto per poter accedere alle sue funzioni

#.................................................................................................
# Oggetti, funzioni e workspace
#.................................................................................................

x <- 1 # assegno valore a x
y <- 2 # assegno valore a y

ls() # mostra tutti gli oggetti nel workspace

rm(y) # rimuove l'oggetto y

ls() # y non c'è più

rm(list = ls()) # comando per svuotare il workspace (da inserire all'inizio di ogni script)

head(sleep,4) # alcuni oggetti sono già dentro a R, anche se non si vedono nel workspace
?datasets # pacchetto base di R che contiene diversi dataset (tra cui sleep)

#.................................................................................................
# Caricare oggetti dall’esterno: La working directory
#.................................................................................................

# individuare la working directory
getwd() # funzione per stampare la WD attuale
dir() # stampo i nomi di tutti i file nella WD

# impostare la working directory
setwd("data") # per selezionare una sottocartella nella WD attuale
setwd("C:/Users/mengh/OneDrive/Desktop") # per cambiare completamente percorso (parto con "C:/")
# nota: il percorso della cartella a riga 44 è legato all'organizzazione del mio PC, che sarà sicuramente diversa dalla tua

# esportare e importare un file .RData (formato default di R)
qs <- data.frame(ID=c("s1","s2","s3"),sesso=factor(c("F","F","M")),punteggio=c(100.3,87.4,50.5))
save(qs, file = "questionarioprova.RData") # export (prova a cercare il file nella WD)
rm(list=ls()) # svuoto il workspace
load(file = "data/questionarioStudenti.RData") # import

# file .CSV (comma separated values)
write.csv(x = qs,"data/questionarioStudenti.csv", row.names = FALSE) # export
qs <- read.csv(file = "data/questionarioStudenti.csv") # import

# file .SAV (da SPSS)
library(foreign) # prima va installato il pacchetto con il comando install.packages("foreign")
qs <- read.spss("data/Sara_dataset.sav.sav", to.data.frame=TRUE)

# per cercare manualmente un file nel pc
qs <- read.csv(file = file.choose())


# Hands on ...............................................

# 1) Apri o crea un file .xlsx sul tuo PC, salvalo in formato .csv (comma separated values) e importalo su R
# io ho creato un file con 3 righe e due colonne e l'ho chiamato "Cartel1.xlsx"
a <- read.csv2("Cartel1.csv") # nota: read.csv2 perché il file usa il simbolo ";" come separatore, anziché la virgola

# 2) Prova ad importare direttamente il file .xlsx (se hai dubbi, cerca su Google “how to read xlsx file with R”)
library("readxl") # prima va installato il pacchetto con il comando install.packages("readxl")
b <- read_excel("Cartel1.xlsx")

# 3) In entrambi i casi, osserva la classe e la struttura dell’oggetto importato
class(a) # a è un data.frame
class(b) # b ha più di una classe
str(a) # entrambe le colonne sono integer
str(b) # qui invece sono numeric

# Hands on: Questionario incontri facoltativi ............

# 1. Scarica i file questionarioStudenti.RData e questionarioStudenti.csv da Github: https://github.com/Luca-Menghini/eseRcitazioni/tree/main/data 
#   (Premere su File > Raw > Download o tasto dx > Save as), salva il file in una cartella e imposta quella cartella come working directory.
setwd("data") # cambio la WD per associarla alla cartella "data", in cui ho salvato i file

# 2. Importa entrambi i file su RStudio e confronta i due oggetti. Di che classe sono? E le variabili di che classe sono?
load("questionarioStudenti.RData") # nota: il dataset viene caricato con il nome che gli è stato assegnato quando è stato esportato (qs)
file2 <- read.csv("questionarioStudenti.csv")
class(qs)
class(file2) # entrambi sono data.frame
str(qs)
str(file2) # unica differenza: con csv le colonne Q01-Q05 sono character, mentre con RData sono factor

# 3. Usa la funzione describe() del pacchetto psych (forse va prima installato!) per calcolare
#    le statistiche descrittive della variabile numVar e usa la funzione hist() per visualizzare
#    il grafico di densità ad istogrammi (prova a cambiare il valore dell’argomento breaks).
install.packages("psych") # installo il pacchetto
library(psych) # apro il pacchetto
describe(qs$numVar)
hist(qs$numVar)
hist(qs$numVar,breaks=3)
hist(qs$numVar,breaks=50)

# nota: se calcolo la media a mano dà valore "NA" (not available)
mean(qs$numVar) 
qs$numVar # questo perché il valore 27 è mancante! Di conseguenza, R non riesce a calcolare la media della variabile
mean(qs$numVar, na.rm = TRUE) # con l'argomento na.rm = TRUE diciamo a R di ignorare i valori mancanti

# 4. Usa la funzione table() per produrre la tabella di frequenza della variabile Q02 (“Quale giorno preferiresti per gli incontri facoltativi?”)
table(qs$Q02)

# 5. Ora fai la stessa cosa, ma considerando solo chi ha risposto “Sì” alla domanda Q01 (“Parteciperai a tutti gli incontri?”)
table(qs[qs$Q01=="Sì","Q02"])

# 6. Ora incrocia le frequenze delle variabili Q02 e Q03 (“Riusciresti a partecipare anche se fossero nel giorno che NON hai scelto?”)
table(qs[,c("Q02","Q03")])

# .................................................................................................
# Statistiche descrittive (univariate)
# .................................................................................................

x <- c(1,1,1,2,8,9) # creo vettore numerico
x

# indici di tendenza centrale
mean(x) media
median(x) # mediana
as.numeric(which.max(table(x))) # moda

# indici di variabilità
var(x) # varianza
sd(x) # dev. standard

# quantili e quartili
quantile(x,probs=0.90) # 90° percentile
quantile(x,probs=c(0.25,0.50,0.75,1)) # quartili
round(rank(x)/length(x),2) # ranghi percentili

# frequenze
table(x) # frequenze assolute
round(table(x)/length(x),2) # freq relative
cumsum(table(x)) # freq cumulate assolute
round(cumsum(table(x)/length(x)),2) # freq cumulate relative
               
cumsum(x) # somma cumulata

# .................................................................................................
# Statistiche descrittive (bivariate)
# .................................................................................................

y <- -x - 1 # valori inversamente prop. a x
y
z <- round(rnorm(n=length(x)),1) # valori estratti casualmente dalla distribuzione normale standard
z

(df <- data.frame(x,y,z)) # nuovo dataframe

cov(x,y) # covarianza tra x e y
cor(x,y) # correlazione tra x e y

cov(df) # matrice di covarianza tra x, y e z
cor(df) # matrice di correlazione tra x, y e z

# Esercizio 1.10 ..................................................

# Sara è una ricercatrice che ha rilevato dei dati relativi agli studenti di un corso di Statistica
# della laurea magistrale. I dati sono organizzati in una apposita tabella e salvati su tre file uguali
# (Sara dataset ma con diversi formati: testo (txt), excel (xls) e SPSS (sav).

#   Le informazioni raccolte da Sara sono le seguenti:
#   - genere dei soggetti (gender: 1 = femmine, 2= maschi)
#   - tipo di laurea triennale conseguita (major: 1 = psicologia, 2 = medicina, 3 = biologia, 4 = sociologia, 5 = economia)
#   - condizione sperimentale (cond: 1 = facile, ..., 4 = impossibile)
#   - autovalutazione del timore verso la matematica (phobia) su scala 0-10
#   - numero corsi di matematica frequentati (prevmath)
#   - punteggio ad un pre-test di matematica (mathquiz)
#   - punteggio ad un test di statistica (statquiz)
#   - auto-misurazione del battito cardiaco in condizioni normali (hr base), prima del test (hr pre) e dopo il test (hr post)
#   - punteggi ad un test sull'ansia in condizioni normali (anx base), prima del test (anx pre) e dopo il test (anx post)

# Si prendano in considerazione i tre file, nei tre diversi formati:
# 1. Si importino i file in R assegnando a ciascuno un nome diverso (ad esempio sara_txt, sara_xls e sara_sav).
getwd() # nota: ho prima salvato i file nella cartella "data" (ovvero nella mia working directory)
sara_txt <- read.table("Sara_dataset.txt",header=TRUE) # nota: va messo header = TRUE per considerare la prima riga come 'header' del dataset (cioè con i nomi di colonna)
library(readxl)
sara_xls <- read_xls("Sara_dataset.xls") 
library(foreign)
sara_sav <- read.spss("Sara_dataset.sav",to.data.frame = TRUE)

# 2. Si confrontino le strutture dei tre dataset importati e si valuti se sono uguali.
str(sara_txt) # in questo gender, major e cond sono integer
str(sara_xls) # in questo sono numeric (come tutte le variabili)
str(sara_sav) # in questo sono factor

# 3. Si identifichino le proprietà e il livello di scala delle variabili del dataset.
summary(sara_sav$gender) # gender è una variabile categoriale misurata su scala nominale, con due livelli: Female e Male
summary(sara_sav$major) # major è una variabile categoriale misurata su scala nominale, con 5 livelli
summary(sara_sav$cond) # cond sembra una variabile categoriale misurata su scala ordinale, con 4 livelli ordinati
summary(sara_sav$phobia) # anche se letta come 'numeric', si tratta di una variabile ordinale (es. scala Likert)
# ...

# 4. Si identifichino i valori ottenuti dal 57o soggetto.
sara_sav[57,]

# 5. Si individuino i punteggi del test hr base.
sara_sav$hr_base
sara_sav[57,"hr_base"]

# Esercizio 1.10 ..................................................

# Esercizio 1.11
# In un reparto psichiatrico di un ospedale del nord Italia sono ricoverati 30 pazienti. Per ciascuno
# di essi sono state rilevate le seguenti informazioni: regione di residenza, classe sociale (definita come bassa, media e alta), 
# punteggio su una scala di ansia (0 = poco ansioso, 7 = molto ansioso), età, tipo di disturbo presentato. 
# I dati sono raccolti nel filele pazienti.xls.
# 1. Si importi il file in R.
paz <- read_xls("pazienti.xls")

# 2. Si specifichi il livello di misura di ciascuna variabile del data-set.
str(paz)
# sogg = numero del soggetto (nominale)
# regione = regione di residenza (nominale)
# cl.sociale = classe sociale (ordinale)
# ansia = scala d'ansia, tipicamente assunta su scala intervallo, ma sarebbe ordinale
# eta = età (scala a rapporti)
# disturbo = tipo di disturbo (nominale)

# 3. Si produca la tabella di frequenze di ciascuna variabile del data-set nel modo più opportuno.
table(paz$sogg) # 1 valore per soggetto
table(paz$regione)
table(paz$cl.sociale)
quantile(paz$ansia) # forse per questa variabile ha più senso guardare min, max e i quartili
quantile(paz$eta) # idem
table(paz$disturbo)

# 4. Si produca la tabella di frequenze cumulate (relative) per le variabili per cui abbia senso.
round(cumsum(table(paz$cl.sociale)/nrow(paz)),2)
round(cumsum(table(paz$ansia)/nrow(paz)),2)
round(cumsum(table(paz$eta)/nrow(paz)),2)

# 5. Si producano i grafici a barre o istogrammi delle variabili del data-set.
plot(as.factor(paz$regione)) # oppure barplot(table(paz$regione))
plot(as.factor(paz$cl.sociale))
hist(paz$ansia)
hist(paz$eta)
plot(as.factor(paz$disturbo))

# 6. Si producano i grafici delle cumulate empiriche per le variabili per cui abbia senso.
plot(cumsum(table(paz$cl.sociale)/nrow(paz)))
plot(cumsum(table(paz$ansia)/nrow(paz)))
plot(cumsum(table(paz$eta)/nrow(paz)))


# Esercizio 1.12......................................................

# Utilizzando i dati dell'esercizio 1.11:
# 1. Si determinino i quartili delle variabili cl.sociale, ansia e eta. Le variabili ansia ed eta
# sono numeriche e quindi possiamo utilizzare direttamente la funzione quantile(); nel caso della variabile
# cl.sociale dobbiamo prima trasformarla in fattore ordinato, con il comando ordered, e poi utilizzare un
# diverso tipo di algoritmo (type = 1) per il calcolo, si veda ?quantile.
quantile(ordered(paz$cl.sociale),type=1)
quantile(as.numeric(factor(paz$cl.sociale,levels=c("1","2","3"), labels=c("Alta","Media","Bassa")))) # comando alternativo 

# 2. Si calcoli il rango percentile di 39 anni.
nrow(paz[paz$eta <= 39,])/nrow(paz) # il rango percentile di 39 anni è 27%

# 3. Si determini la moda delle variabili del data-set.
names(which.max(table(paz$regione))) # Piemonte
library(ADati)
moda(paz$regione) # funzione moda() dal pacchetto ADati
moda(paz$cl.sociale) # bassa
moda(paz$ansia) # 3.8
moda(paz$eta) # diverse età sono ugualmente frequenti
moda(paz$disturbo) # nevrosi

# 4. Si determini la mediana delle variabili del data-set per cui ha senso.
median(paz$ansia) # ha senso se la assumiamo come su scala intervalli
median(paz$eta)

# 5. Si determini la media delle variabili del data-set per cui ha senso.
mean(paz$ansia) # ha senso se la assumiamo come su scala intervalli
mean(paz$eta)

# 6. Si determinino deviazione standard, varianza e devianza delle variabili del data-set per cui ha senso.
sd(paz$ansia) # deviazione standard
sd(paz$eta)
var(paz$ansia) # varianza
var(paz$eta)
var(paz$ansia)*nrow(paz) # devianza
var(paz$eta)*nrow(eta)

# Esercizio 1.13......................................................

# Utilizzando i dati dell'esercizio 1.11:
#   1. Si produca il boxplot per le variabili del data-set per cui abbia senso.
boxplot(paz$ansia) # ha senso se la assumiamo come su scala intervalli
boxplot(paz$eta)

# 2. Si determini, con un opportuno metodo grafico, se la distribuzione delle variabili ansia ed eta possa considerarsi approssimativamente normale.
hist(paz$ansia) # non proprio normale...
hist(paz$eta) # abbastanza normale

# 3. Si produca un grafico opportuno per rappresentare le medie delle età dei soggetti (con un indicatore di variabilità associato) 
# in funzione del disturbo diagnosticato.
Medie <- aggregate(paz$eta, list(paz$disturbo), FUN=mean) # calcolo medie per disturbo con la funzione aggregate()
Medie$SD <- aggregate(paz$eta, list(paz$disturbo), FUN=sd)[,2] # aggiungo SD
Medie

# plotto le medie
plot(x = 1:4, # asse x: 1, 2, 3, 4
     y = Medie$x, # asse y: medie
     pch=19) # forma dei punti

# sistemo nomi e testo assi
plot(x = 1:4, y = Medie$x, pch=19,
     xlab="Disturbo", ylab="Età (anni)", # nomi degli assi
     xaxt="n") # tolgo etichette asse x
axis(side=1, at=1:4, labels=Medie$Group.1) # aggiungo manualmente un nuovo asse x

# sistemo i limiti del grafico (in modo che rientrino le deviazioni standard)
plot(x = 1:4, y = Medie$x, pch=19, xlab="Disturbo", ylab="Età (anni)", xaxt="n",
     ylim = c(min(paz$eta), max(paz$eta)))
axis(side=1, at=1:4, labels=Medie$Group.1)

# aggiungo le deviazioni standard al plot
segments(x0 = 1, # orizzontalmente, il segmento parte da x0
         x1 = 1, # e finisce a x1 (in questo caso uguale a x0)
         y0 = Medie[1,"x"] - Medie[1,"SD"], # verticalmente, il segmento parte da y0 (media - SD)
         y1 = Medie[1,"x"] + Medie[1,"SD"]) # e finisce a y1 (media + SD)

# aggiungo gli altri segmenti
segments(x0=2, x1=2, y0=Medie[2,"x"] - Medie[2,"SD"], y1=Medie[2,"x"] + Medie[2,"SD"])
segments(x0=3, x1=3, y0=Medie[3,"x"] - Medie[3,"SD"], y1=Medie[3,"x"] + Medie[3,"SD"]) 
segments(x0=4, x1=4, y0=Medie[4,"x"] - Medie[4,"SD"], y1=Medie[4,"x"] + Medie[4,"SD"])

# Esercizio 1.13......................................................

# Il dataset gambling8, nel pacchetto ADati, contiene i dati relativi ad un campione di 1221studenti
# di età compresa tra 15 e 19 anni selezionati in 42 classi di 5 scuole. A ciascun soggetto viene
# somministrato un questionario che rileva le seguenti misure: 
# - ID: codice identificativo
# - school: scuola frequentata 
# - class: classe frequentata
# - age: età
# - gender: genere 
# - frequency: frequenza di gioco 
# - perc_peers: percezione di quanto giocano i pari 
# - disapproval: grado di disapprovazione 
# - risk: percezione del rischio 
# - par_know: percezione del controllo genitoriale 

# 1. Si importi il file in R.
install.packages("devtools") # installo pacchetto devtools (nota: solo la prima volta che si installa ADati)
library(devtools) # apro pacchetto devtools (nota: solo la prima volta che si installa ADati)
install_github("https://github.com/masspastore/ADati") # installo ADati da github (nota: solo la prima volta che si installa ADati)
library(ADati) # apro pacchetto ADati
data("gambling", package = "ADati") # estraggo il dataset dal pacchetto ADati

# 2. Si valuti se nel data-set ci siano casi mancanti.
which(is.na(gambling)) # non ci sono dati mancanti
which(is.na(gambling$ID)) # per un controllo colonna per colonna

# 3. Si producano i grafici appropriati per le distribuzioni univariate delle variabili del data set.
plot(gambling$school)
plot(gambling$class)
hist(gambling$age, breaks = 20)
plot(gambling$gender)
hist(gambling$frequency)
hist(gambling$perc_peers, breaks = 20)
hist(gambling$disapproval)
hist(gambling$risk)
hist(gambling$par_know)

# 4. Si calcolino media, mediana e i decili della variabile disapproval.
mean(gambling$disapproval)
median(gambling$disapproval)
quantile(gambling$disapproval, probs = seq(from = 0.10, to = 1, by = 0.10))

# 5. Si rappresenti graficamente la distribuzione dei valori di frequency separatamente per
#    maschi e femmine. Qundi si valuti in quale dei due gruppi presenti una frequenza media più alta.
par(mfrow=c(1,2)) # per avere due grafici in un'unica finestra
hist(gambling[gambling$gender=="f","frequency"], main = "Femmine")
hist(gambling[gambling$gender=="m","frequency"], main = "Maschi") # sembra maggiore nei maschi

# 6. Si utilizzi una rappresentazione grafica che permetta di valutare se vi sia una relazione tra
#    la percezione di rischio e l'età dei soggetti. Si interpreti il grafico ottenuto.
par(mfrow=c(1,1)) # per ritornare ad un grafico per finestra
boxplot(risk ~ age, data = gambling) # le distribuzioni della perc di rischio sembrano molto simili tra le varie età (stessa mediana, variabilità simile)
