def ggt_euklid(x,y):
    # Python Programm zur Berechnung des ggt mittels Euklid-Reihe
    print(x)
    print(y)
    prev1=x # vorletztes
    prev2=y # letztes
    while(prev2!=0):
        prev1,prev2=prev2,max(prev1,prev2)-min(prev1,prev2)
        print(prev2)
    return prev1

if __name__ == '__main__': 
    print(ggt_euklid(int(input("Zahl 1:")),int(input("Zahl 2:"))))


"""
RAM-Programm zur Berechnung des ggt mittels Euklid-Reihe
R0,R1 Eingabe
R0 Ausgabe
R0 vorletztes Euklid-Glied
R1 letztes Euklid-Glied
R2 Cache

0 IF R0 = 0 GOTO 3    // R0 ist 0 somit ist R1 max(R0,R1)
1 IF R1 = 0 GOTO 14   // R1 ist 0 somit ist R0 max(R0,R1), fertig!
2 GOTO 5              // Zum eigentlichen Beginn
3 R0 <- R1            // R1 ist das Ergebnis
4 GOTO 14             // STOP
5 R2 <- R0-R1         // R2 als Vergleichscache
6 IF R2 = 0 GOTO 8    // R1 ist max, R0 ist min (1. Fall)
7 GOTO 10             // R0 ist max, R1 ist min (2. Fall)
8 R2 <- R1 - R0       // R2 auf max-min setzen (1. Fall)
9 GOTO 11
10 R2 <- R0 - R1      // R2 auf max-min setzen (2. Fall)
11 R0 <- R1           // Hier enthält R2 max-min (wieder beide Fälle)
12 R1 <- R2           // 11,12 = 2x "Durchrutschen"
13 IF R1 > 0 GOTO 5   // Noch nicht fertig, nochmal
14 STOP               // Fertig

"""