# While-Programm fur prodZ mit Laufzeit O(n^3)
# Benutzt "Schulmethode"

def prodZ(x,y):
    if(y<0):    # y Soll positiv sein
        y=(0-y) # Negatives Vorzeichen von y entfernen
        x=(0-x) # und auf x uebertragen
    c=0     # Ergebnis
    i=0     # Anzahl Nullen anzuhaengen an x in dieser Schleife
    p=1     # Zwierpozent zum zerlegen von y
    while(p<=y):     # i und p hochzaehlen
        p=(p+p)
        i=(i+1)
    while(y>0):       # Wenn y==0 fertig multipliziert
        if(y>=p):     # Eine 1 in y
            y=(y-p)   # 1 wegsubtrahieren
            x00=x     # Dann x mit passender Zahl Nullen
            k=0       # Zaehler
            while (k<i):      # i Nullen anhaengen
                k=(k+1)
                x00=(x00+x00) # i Mal verdoppeln
            c=(c+x00) # x mit Nullen dazuaddieren
        i=(i-1) # Naechstes mal weniger Nullen
        y=(y+y) # y Weiter nach vorne Scheiben
    return c

