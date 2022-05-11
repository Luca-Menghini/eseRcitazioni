# Giorno 5: Regressione lineare
#.................................................................................................

# Specificazione modelli con la funzione lm()
# ........................................................

data(gambling,package = "ADati") # apro dataset gambling dal pacchetto ADati

# Modello nullo (m0): il punteggio di `frequency` è predetto soltanto dall'intercetta $b_0$
m0 <- lm(formula = frequency ~ 1, data = gambling) # specifico modello nullo

# coefficienti m0
coefficients(m0) # coefficiente stimato dal modello (intercetta)
mean(gambling$frequency) # media della variabile frequency (sono uguali!)

# Modello di regressione lineare semplice (m1): intercetta $b_0$ e coefficiente $b_1$, che esprime la differenza 'media' `m - f`.
m1 <- lm(formula = frequency ~ gender, data = gambling)

# coefficienti m1
coefficients(m1) 
tapply(gambling$frequency, gambling$gender, mean) # media di frequency per f e m

# Modello di regressione lineare multipla (m2)
m2 <- lm(formula = frequency ~ risk + gender, data = gambling)
coefficients(m2)

# Modello interattivo (m3)
m3 <- lm(formula = frequency ~ risk * gender, data = gambling)
coefficients(m3)

# Valutazione assunti
# .................................................................

# 1. Indipendenza delle osservazioni (non serve R, basta pensare se le osservazioni sono indipendenti)

# 2. Indipendenza dei predittori dall'errore

# 3. Varianza costante (omogeneità delle varianze)

# selezionare i residui di un modello lineare
gambling$RESIDUI <- residuals(m3) # residui del modello con funzione residuals()
gambling$ATTESI <- fitted(m3) # valori di predetti dal modello con funzione fitted()
head(gambling[,c("frequency","ATTESI","RESIDUI")],5) # RESIDUI = OSSERVATI - ATTESI

# visualizzare i residui per valutare gli assunti
par(mfrow=c(1,2),mar=c(5,4,0,2)+0.1)
plot(RESIDUI ~ gender, data=gambling) # RESIDUI vs. gender --> non c'è tendenza (2) ma c'è un po' più variabilità nei maschi (3)
plot(RESIDUI ~ risk, data=gambling, col="gray") # RESIDUI vs risk --> non c'è tendenza (2) e la variabilità è costante (3)
abline(lm(RESIDUI ~ risk, data=gambling)) # aggiungo linea di regressione RESIDUI vs risk

# 4. Linearità: il valore atteso dell’errore per un dato valore di X è zero --> residui simmetrici con media zero

# 5. Normalità: i residui (gli errori) sono distribuiti normalmente --> si può anche usare un test (es. Kolmogorov-Smirnov)

# visualizzare linearità e normalità dei residui
par(mfrow=c(1,3))
hist(gambling$frequency) # normalità variabile frequnecy (di solito, se i residui non sono normali, neanche la Y lo è)
hist(residuals(m3),main=paste("Residuals mean =",round(mean(residuals(m3)),2), # normalità RESIDUI (abbastanza simmetrici, con media zero) (4) e (5)
                              "\nmedian =",round(median(residuals(m3)),2)))
qqnorm(residuals(m3), col="gray", pch=19, ylab = "Quantili distr. residui") # normalità RESIDUI (abbastanza normali) (5)
qqline(residuals(m3), col="red")

# tutti gli assunti in un unico grafico
par(mfrow=c(2,2))
plot(m3) # mostra anche se sono presenti casi influenti (grafico in basso a destra)
par(mfrow=c(1,1)) # ritorno alla visualizzazione di un grafico alla volta

# Confrontare diversi modelli
# .................................................................

# Likelihood ratio test:
library(lmtest) # prima va installato con il comando install.packages("lmtest")
lrtest(m0,m1,m2,m3) # test del rapporto delle verosomiglianze tra i modelli m1 e m0, m2 e m1, m3 e m2

# Akaike information criterion (AIC)
AIC(m0,m1,m2,m3)$AIC # + basso meglio è

# Akaike weights: da 0 (-) a 1 (+)
library(MuMIn) # prima va installato con il comando install.packages("MuMIn")
Weights(AIC(m0,m1,m2,m3)) # Aw

# Bayesian information criterion (BIC)
BIC(m0,m1,m2,m3)$BIC # + basso meglio è

# Bayesian weights: da 0 (-) a 1 (+)
Weights(BIC(m0,m1,m2,m3)) # Bw

# Inferenza sui coefficienti di regressione
# .................................................................

# comando summary() mostra un sommario del modello con i coefficienti, gli errori standard, i t-value associati e il relativo p-value
summary(m3)$coefficients 

# l'oggetto r.squared nel summary del modello è il coefficiente di determinazione (% di varianza spiegata in Y)
summary(m3)$r.squared

# visualizzare gli effetti stimati
library(effects) # prima va installato con il comando install.packages("effects")
plot(allEffects(m1)) # effetti modello m1
plot(allEffects(m2)) # effetti modello m2
plot(allEffects(m3)) # effetti modello m3

# Hands on .....................................................................................................

# Esercizio 4.1 ...............................................................

# Per ciascuna coppia di variabili (x; y) generate come specificato di seguito:

# a. si rappresentino le distribuzioni univariate (con boxplot) e un grafico a dispersione;
# b. si calcoli la correlazione tra esse;
# c. si valuti con un test opportuno se la diffeerenza tra le media sia significativamente diversa da zero.
# Al termine si confrontino i risultati e si traggano le conclusioni.

# 1. Siano: y <- rnorm(50) e x1 <- y.
y <- x1 <- rnorm(50) # creo variabili
# a (distribuzioni univariate)
par(mfrow=c(2,2)) # 4 grafici per finestra
boxplot(y,main="y") # boxplot
boxplot(x1,main="x1")
plot(y,main="y") # grafico a dispersione
plot(x1,main="x1")
# b (correlazione)
cor(y,x1) # r = 1 (correlazione positiva perfetta)
# c (test sulla media)
t.test(x=x1, y=y) # non sono diverse, anzi sono perfettamente uguali (t = 0, p = 1)

# per i prossimi punti, siccome devo eseguire sempre le stesse 3 operazioni, creo una funzione che applicherò di volta in volta
my_function <- function(y,x){ # gli argomenti della funzione sono y e x
  
  # a. distribuzioni univariate
  par(mfrow=c(2,2)) # 4 grafici per finestra
  boxplot(y,main="y") # boxplot
  boxplot(x,main="x")
  plot(y,main="y") # grafico a dispersione
  plot(x,main="x")
  
  # b. correlazione (la funzione cat stampa un messaggio, simile a paste())
  cat("Correlazione =",cor(x,y))
  
  # c. test sulla media (la funzione print() serve per stampare il messaggio quando viene usata la funzione my_function)
  print(t.test(x,y))
}

# 2. Siano: y (generato al punto 1) e x2 <- y+10.
x2 <- y + 10 # creo x2
my_function(y=y, x=x2) # applico la funzione scritta sopra 
# commento: x è maggiore di y, ma hanno la stessa variabilità, correlano a r = 1 (sono uguali, tranne per la costante +10) e la differenza tra le medie è significativa

# 3. Siano: y (generato al punto 1) e x3 <- rnorm(50).
x3 <- rnorm(50)
my_function(y=y, x=x3) # applico la funzione scritta sopra 
# commento: x è simile a y (generate assumendo stessa media = 0 e SD = 1), la correlazione dipende da caso a caso ma dovrebbe essere bassa, mentre il t test è non significativo (di nuovo xk generate assumendo la stessa media)

# 4. Siano: y (generato al punto 1) e x4 <- x3+10.
x4 <- x3 + 10
my_function(y=y, x=x4) # applico la funzione scritta sopra 
# commento: x è maggiore di y, ma hanno la stessa variabilità (generato assumendo SD = 1), correlazione uguale al punto 3 (xk è stata solo aggiunta una costante) e il t test è significativo (perché la media di x è superiore a quella di y)

# 5. Quali conclusioni possiamo trarre, in particolare per quanto riguarda la relazione tra correlazione e differenza tra medie?
# che sono indipendenti: due variabili possono essere perfettamente correlate ma avere media diversa
# oppure possono avere la stessa media ma essere debolmente correlate

# Esercizio 4.6 ...............................................................

# Ad un campione di 50 adolescenti affetti da una sindrome metabolica vengono misurate la con-
# centrazione di globuli bianchi nel sangue (white blood cell count, wbcc) e l'indice di massa corpo-
# rea (body mass index, bmi). I dati sono nel file MetS.dat. Si vuole sapere se la concentrazione
# di globuli bianchi sia predittiva del bmi.

# 1. Si importino i dati in R.
MetS <- read.csv("data/MetS.dat",sep=" ") # MetS.dat è nella cartella data su Github. usando il menu File > Import dataset, ho visto che si apre con read.csv(), usando lo spazio come separatore di colonna

# 2. Si calcolino media, varianza e deviazione standard delle variabili wbcc e bmi.
apply(MetS,2,mean) # media
apply(MetS,2,var) # varianza
apply(MetS,2,sd) # DS

# 3. Si produca un grafico a dispersione che rappresenti la distribuzione congiunta delle due variabili.
par(mfrow=c(1,1)) # torno a visualzzare un grafico per finestra
plot(bmi ~ wbcc, data = MetS,
     xlim = c(min(MetS$wbcc), 8.5)) # imposto 8.5 come limite dell'asse x per visualizzare il punto 6

# 4. Si stimino i parametri del modello di regressione in cui la variabile wbcc è il predittore della variabile bmi.
m <- lm(bmi ~ wbcc, data = MetS) # modello
coefficients(m) # parametri del modello
summary(m)$coefficients # per avere anche l'errore standard, il t e il p-value

# 5. Si aggiunga al grafico ottenuto al punto 3 la retta di regressione attesa.
abline(a = coefficients(m)[1], # intercetta (primo parametro)
       b = coefficients(m)[2]) # coefficiente angolare (secondo parametro)
abline(m,col="red",lty=2) # in alternativa, si può fare abline(modello)

# 6. Si stimi il valore atteso di indice di massa corporea quando la concentrazione di globuli bianchi è 8 e lo si rappresenti graficamente.
bmi8 <- coefficients(m)[1] + 8*coefficients(m)[2] # applico il modello di regressione al valore x = 8
as.numeric(bmi8)
as.numeric(predict(m,data.frame(wbcc=8))) # in alternativa posso usare la funzione predict() in base al modello m su un data.frame con wbcc=8
points(x = 8, y = bmi8, col = "red", pch = 19) # il punto è posizionato sulla retta di regressione

# 7. Si calcoli la dimensione dell'effetto (R2).
summary(m)$r.squared # R2 = 0.08 (wbcc spiega l'8% della varianza in bmi)

# Esercizio 4.14 ...............................................................

# Si vuole studiare l'effetto della deprivazione da sonno in un compito attentivo. Tale compito
# consiste nell' individuazione di un oggetto in movimento su uno schermo radar. 16 soggetti
# vengono suddivisi in quattro gruppi, ciascuno dei quali viene privato del sonno per un certo
# numero di ore (4, 12, 20 e 28; variabile hr). I punteggi dei soggetti nel compito (variabile score)
# sono espressi nel numero di mancate individuazioni durante un periodo di 30 minuti e sono
# riportati nel file anova1.dat (dati Keppel, 1991). Si vuole sapere se esista una differenza tra i
# punteggi nel compito in relazione alle ore di deprivazione da sonno.

# 1. Si importi il file anova1.dat in R.
anova1 <- read.delim("data/anova1.dat") # come sopra, ho selezionato il menu File (in alto a sinistra) > Import dataset > From Text
head(anova1) # visualizzo prime 6 righe

# 2. Si produca un grafico con le medie e le barre di errore dei punteggi per i quattro gruppi
# (a scelta si usi la funzione errbarr del pacchetto Hmisc oppure plotmeans del pacchetto gplots).
library(gplots) # va prima installato con il comando install.packages("gplots")
plotmeans(formula=score~hr,data=anova1)

# 3. Si valuti, con uno o più test opportuni, se le distribuzioni dei punteggi nei quattro gruppi sono normali.
shapiro.test(anova1$score) # così applico il test Shapiro-Wilk a tutti i punteggi di score --> non rifiuto H0 -> score è normale
shapiro.test(anova1[anova1$hr==4,"score"]) # così lo applico al primo gruppo
by(data = anova1$score, INDICES = anova1$hr, FUN = shapiro.test) # così lo applico ad ogni gruppo -> nessun gruppo mostra deviazioni significative dalla normalità
by(data = anova1$score, INDICES = anova1$hr, FUN = function(x) ks.test(x=x,y="pnorm",mean=mean(x),sd=sd(x))) # stesso risultato con test Kolmogorov-Smirnov

# 4. Si valuti con un opportuno test se le varianze dei gruppi sono omogenee.
bartlett.test(score ~ hr, data = anova1) # test di omogeneità delle varianze --> non significativo (non possiamo rifiutare l'ipotesi nulla di omogeneità delle varianze) -> le varianze sono omogenee

# 5. Si stabilisca, con il test opportuno, se le ore di deprivazione da sonno siano predittive dei
# punteggi al compito definendo l'ipotesi H0 relativa al test.
# H0: mu_hr4 = mu_hr12 = mu_hr20 = mu_hr28
m <- lm(score ~ hr, data = anova1)
anova(m) # analisi della varianza (ANOVA) per testare l'ipotesi H0 -> il valore stimato di F è significativo -> rifiuto H0
summary(aov(score ~ hr, data = anova1)) # in alternativa

# 6. Si valuti la bontà del modello lineare valutando gli assunti sui residui.
par(mfrow=c(2,2)) # 4 grafici per finestra
plot(m) # 1. residui indipendenti da fitted, 2. residui abbastanza normali, 3. leggera eteroschedasticità (ma abbiamo "verificato" l'omogeneità delle varianze), 4. non ci sono evidenti casi influenti
par(mfrow=c(1,2)) # in alternativa
boxplot(residuals(m)~anova1$hr,main="independenza e omoscedasticità") # non proprio omoschedastici (ma abbiamo "verificato" l'omogeneità delle varianze)
hist(residuals(m),main="linearità e normalità",breaks=10) # non normalissimi, ma centrati sullo zero (linearità)

# 7. Si stimi la dimensione dell'effetto (con R2 e eta2) e lo si interpreti.
summary(m)$r.squared # R2 (coeff. di determinazione): il modello spiega il 65% della varianza nella variabile score
anova(m)[1,2] / # eta2 = somma quadrati (SS) di hr (riportata nella prima riga e seconda colonna della tabella dell'ANOVA)
  sum(anova(m)[,2]) # diviso SS totale (hr + Residuals) (riporati nella seconda colonna della tabella dell'ANOVA)
# commento: R2 e eta2 sono IDENTICI --> 65% di varianza spiegata da hr
library(lsr) # in alternativa, calcolo l'eta2 con il pacchetto lrs
etaSquared(m)

# 8. Si stimi il Bayes Factor e lo si interpreti in relazione ai risultati ottenuti ai punti 5 e 7.
library(BayesFactor) # va prima installato con il comando install.packages("BayesFactor")
lmBF(score ~ hr, data = anova1) # calcolo Bayes Factor = 10.19
anovaBF(score ~ hr, data = anova1) # in alternativa
# interpretazione: il BF è > 1, suggerendo una maggiore evidenza per H1 (almeno una differenza tra gruppi) rispetto ad H0 (nessuna differenza), in particolare è circa pari a 10, suggerendo un'evidenza moderata/forte a favore di H1


# Esercizio 5.1......................................................................

# Consideriamo un esperimento sulle capacità di apprendimento delle scimmie in relazione al tem-
# po di deprivazione da cibo e l'assunzione di determinati farmaci. 24 animali vengono sottoposti
# ad una serie di problemi di identificazione di oggetti e vengono ricompensati con del cibo quan-
# do rispondono correttamente. La V.D. è rappresentata dal numero di risposte corrette su 20
# tentativi (variabile score). I predittori sono il tipo di farmaco (x, y e nessuno (c); variabile
# drug) e il tempo di deprivazione da cibo (1 e 24 ore; variabile fdep). I dati sono contenuti nel
# dataset monkeys nel pacchetto ADati (Keppel, 1991). Si vuole sapere se esista una differenza
# nel numero di risposte corrette in funzione del farmaco assunto, del periodo di deprivazione da
# cibo e se vi sia un'interazione tra le due variabili.

# 1. Si renda disponibile il data frame monkeys.
data(monkeys,package="ADati") # carico dataset monkeys dal pacchetto ADati
head(monkeys) # prime 6 righe

# 2. Si calcolino media e deviazione standard del numero di risposte corrette in relazione alla
# variabile drug, alla variabile fdep e all'interazione tra le due (si può usare la funzione
# aggregate).
?aggregate # argomenti x, by e FUN
aggregate(x = monkeys$score, by = list(monkeys$drug), FUN = mean)
aggregate(x = monkeys$score, by = list(monkeys$drug), FUN = sd)
by(monkeys$score,monkeys$drug,mean) # in alternativa
by(monkeys$score,monkeys$drug,sd) # in alternativa

# 3. Si rappresentino graficamente le medie delle risposte corrette in funzione del tipo di farmaco
# (drug) e tempo di deprivazione (fdep) inserendo anche le barre di errore (si può utilizzare
# la funzione errbarr(), pacchetto Hmisc).
library(Hmisc) # da installare con il comando install.packages("Hmisc")
par(mfrow=c(1,1))
errbar(x=levels(as.factor(monkeys$drug)), # x = livelli della variabile drug = c("c","x","y")
       y=aggregate(x = monkeys$score, by = list(monkeys$drug), FUN = mean)[,2], # y = medie per gruppo 
       yplus=aggregate(x = monkeys$score, by = list(monkeys$drug), FUN = mean)[,2] +
         aggregate(x = monkeys$score, by = list(monkeys$drug), FUN = sd)[,2], # yplus = medie + 1 SD
       yminus=aggregate(x = monkeys$score, by = list(monkeys$drug), FUN = mean)[,2] -
         aggregate(x = monkeys$score, by = list(monkeys$drug), FUN = sd)[,2]) # yminus = medie - 1 SD
library(gplots)
plotmeans(formula=score~drug,data=monkeys) # in alternativa
plotmeans(formula=score~fdep,data=monkeys) # in alternativa

# 4. Si valuti se le varianze dei 6 gruppi sono omogenee (quali sono i sei gruppi?) formulando
# l'ipotesi nulla del test. Suggerimento: si può utilizzare la funzione leveneTest() del
# pacchetto car.
table(monkeys[,c("drug","fdep")]) # ecco i 6 gruppi, dati dall'incrocio di drug e fdep
monkeys$group <- as.factor(paste(monkeys$drug,monkeys$fdep,sep="_")) # creo variabile group incollando le variabili drug e fdep
summary(monkeys$group) # ecco i 6 gruppi
library(car) # da installare con il comando install.packages("car")
leveneTest(y = monkeys$score, group = monkeys$group) # non è significativo, non posso rifiutare l'ipotesi H0 di omogeneità delle varianze --> sono omogenee

# 5. Si stabilisca, con il modello opportuno, se vi siano differenze nel numero di risposte corrette
# in funzione delle variabili drug, fdep e dell'interazione tra di esse definendo le ipotesi H0
# relative ai vari test inclusi nel modello.
# H0_1: mu_drug1 = mu_drug2 = mu_drug3
# H0_2: mu_fdep1 = mu_fdep2
# H0_3: mu_drug1_fdep1 - mu_drug1_fdep2 = mu_drug2_fdep1 - mu_drug2_fdep2 = mu_drug3_fdep1 - mu_drug3_fdep2
m <- lm(score ~ drug + fdep + drug:fdep, data = monkeys)
m <- lm(score ~ drug*fdep, data = monkeys) # in alternativa uso il simbolo "*"
anova(m) # ANOVA --> soltanto l'interazione mostra una statistica test (F) significativa, quindi possiamo solo rifutare l'ipotesi H0_3
summary(aov(score ~ drug*fdep, data = monkeys)) # in alternativa

# 6. Si stimino le dimensioni degli effetti (eta2 parziali) con la funzione partial.eta2() del
# pacchetto ADati oppure con la funzione etasq() del pacchetto heplots e le si interpreti.
library(ADati)
partial.eta2(SS.eff = c(112, 24, 144), # devianze degli effetti (colonna "Sum Sq" dell'ANOVA table)
             SS.err = 330, # devianze dell'errore (ultima riga colonna Sum sq)
             eff.lab = c("drug","fdep","interaction")) # nomi degli effetti
partial.eta2(SS.eff=anova(m)[1:3,2], SS.err=anova(m)[4,2], eff.lab=rownames(anova(m))[1:3]) # in alternativa
anova(m)[1:3,2]/(anova(m)[1:3,2]+anova(m)[4,2]) # in alternativa si può calcolare a mano

# 7. Si determini, utilizzando il BIC, se il modello con interazione sia meglio di quello senza interazione.
m1 <- lm(score ~ drug + fdep, data = monkeys) # modello additivo (senza interazione)
m2 <- lm(score ~ drug * fdep, data = monkeys) # modello moltiplicativo (con interazione)
BIC(m1,m2) # il BIC è PIù BASSO per il modello m2 (con interazione), che pertanto risulta MEGLIO del modello m1 (senza interazione)


# Esercizio 5.4
# ......................................................................

# In uno studio sull'utilizzo di un particolare servizio sociale disponibile su internet in relazione
# al grado di estroversione sono selezionati 100 studenti, 50 dei quali sono membri di un gruppo
# on-line e 50 no (variabile member). Ai soggetti viene somministrato l'EPQ (Eysenck personality
# questionnaire) e, sulla base dei punteggi ottenuti, essi vengono suddivisi in due gruppi: introversi
# ed estroversi (variabile trait). Il grado di utilizzo del servizio sociale (variabile use) viene
# valutato con un questionario composto di 10 item, il cui punteggio finale varia da 0 (nessun uso
# del servizio) a 7 (massimo uso del servizio). I dati sono disponibili nel file eysenck.dat. Si
# vuole sapere se il fare parte del gruppo ed il grado di estroversione in
# uenzano l'uso del servizio sociale.

# 1. Si importino i dati in R.
eysenck <- read.csv("data/eysenck.dat",sep="") # comando ricavato selezionando File (in alto a sinistra) > Import dataset > From Text
head(eysenck) # primi 6 valori

# 2. Per ciascuno dei quattro gruppi, definiti dall'incrocio tra le variabili trait e member si
# calcolino, medie, deviazioni standard, numerosità ed intervallo di confidenza delle medie
# al 95%.
library(Rmisc)
summarySE(data = eysenck, # data è il dataset
          measurevar = "use", # measurevar è la variabile da analizzare
          groupvars = c("member", "trait")) # groupars sono le variabili che determinano i gruppi
by(data=eysenck$use,INDICES = list(eysenck$trait,eysenck$member), FUN = mean) # in alternativa
aggregate(x=eysenck$use,by=list(eysenck$trait, eysenck$member),FUN=mean) # in alternativa
aggregate(x=eysenck$use,by=list(eysenck$trait, eysenck$member),FUN=sd) # in alternativa
table(eysenck[,c("trait","member")]) # numero di casi
eysenck$group <- as.factor(paste(eysenck$trait,eysenck$member,sep="_")) # in alternativa
summary(eysenck$group)
aggregate(x=eysenck$use,by=list(eysenck$trait, eysenck$member),FUN=mean)[,3] + # upper CI = media(x) +
  aggregate(x=eysenck$use,by=list(eysenck$trait, eysenck$member),FUN=sd)[,3]/  # dev.st(x)/sqrt(N)
  sqrt(table(eysenck$group)) 
aggregate(x=eysenck$use,by=list(eysenck$trait, eysenck$member),FUN=mean)[,3] - # lower CI = media(x) -
  aggregate(x=eysenck$use,by=list(eysenck$trait, eysenck$member),FUN=sd)[,3]/  # dev.st(x)/sqrt(N)
  sqrt(table(eysenck$group))

# 3. Si valuti graficamente se sia ipotizzabile una interazione delle variabili member e trait
# sulla variabile use.
boxplot(use ~ trait * member, data = eysenck)
# commento: è ipotizzabile un interazione, poiché trait=Ex è minore di trait=In solo per member=no, mentre non sembrano esserci differenze sostanziali quando member=yes

# 4. Si definisca il modello appropriato per valutare l'interazione tra le variabili trait e member.
m <- lm(use ~ trait * member, data = eysenck)
anova(m) # ANOVA: effetto principale significativo di trait + effetto interattivo, mentre l'effetto principale di member non è significativo

# 5. Si calcoli la dimensione dell'effetto di interazione usando eta2 parziale.
library(ADati)
partial.eta2(SS.eff=c(1.816,0.008,3.032),SS.err=43.106,eff.lab=c("trait","member","trait:member"))
partial.eta2(SS.eff=anova(m)[1:3,"Sum Sq"],SS.err=anova(m)[4,"Sum Sq"],eff.lab=c("trait","member","trait:member")) # in alternativa

# 6. Si calcoli il Bayes Factor dell'interazione rispetto al modello senza interazione.
library(BayesFactor)
BF1 <- lmBF(use ~ trait + member, data = eysenck) # BF modello additivo vs. modello nullo
BF2 <- lmBF(use ~ trait * member, data = eysenck) # BF modello interattivo vs. modello nullo
BF2 / BF1 # BF = 4.75 (maggiore di 1 quindi a supporto del modello interattivo BF2, ma evidenza moderata)

# 7. Sulla base dei risultati ottenuti si traggano le conclusioni.
# l'ANOVA suggerisce differenze significative tra gruppi solo in relazione al predittore trait e all'interazione tra trait e member
# l'eta quadro conferma che la varianza spiegata dal modello è per lo più attribuibile a trait (circa 4%) e all'interazione (circa 6%), mentre member spiega pochissima varianza (circa 0.02%)
# il Bayes Factor suggerisce, seppur moderatamente, maggiore evidenza per il modello interattivo rispetto a quello additivo
# in conclusione: l'utilizzo del servizio è maggiore tra gli introversi rispetto agli estroversi, ma solo per coloro che sono membri del gruppo on-line, mentre non si rilevano differenze significative tra coloro che sono membri del gruppo on-line