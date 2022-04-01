# ....................................................
# VETTORI

# Esercizio 1.5

# 1. Si crei un vettore con nome PI che contenga 10 valori in sequenza e passo costante da ????? a +?? e si visualizzino i valori ottenuti.
(PI <- seq(from=-pi, to=pi, length.out = 10))
length(PI)

# 2. Si calcolino media e varianza degli elementi contenuti in PI.
mean(PI) # round(mean(PI),17)
var(PI)

# 3. Si calcoli la somma del terzo, quinto e ottavo elemento del vettore PI (risultato = -0.3490659).
ciao <- sum(PI[3,5,8])
PI[3] + PI[5] + PI[8] # in alternativa

# 4. Si calcolino e si visualizzino le radici quadrate degli elementi del vettore PI
sqrt(PI)


#################
?diff
diff(10:1)

# FATTORI
# ..................................

# Esercizio 1.8

# 1. Si crei un vettore numerico X contenente 50 valori estratti a caso dall'insieme {0, 1, 2, 3}.

# 2. Si produca la tabella di frequenze di X.

# 3. Si converta il vettore X in un vettore categoriale, Y, usando il comando factor().

# 4. Si visualizzino i livelli del vettore Y.

# 5. Si converta il vettore X in un vettore categoriale, Z, assegnando ai valori {0, 1, 2, 3} le
# seguenti etichette: 0 = giallo, 1 = verde, 2 = rosso, 3 = blu.

# 6. Si produca la tabella di frequenze di Z.



