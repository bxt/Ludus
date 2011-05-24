
def prodZ(x,y):    # Multipliziert zwei nat. Zahlen
    c=0
    i=binLength(x) # Maximale Zweierpotenz
    while(x>0):    # x wird beim multiplizieren reduziert
        p=pow2(i)
        if(p<=x):  # In Zweierpotenzschritten multiplizieren
            x=(x-p)
            c=(c+prodZInner(i,y))
        i=(i-1)
    return c

def prodZInner(i,y): # Multipliziert y mit i-ter Zweierpotenz
    for x in range(0,i):
        y=(y+y)
    return y


def code(x): # Kodiert x
    c=0 # Speichert die kodierte Zahl
    i=binLength(x)
    while(i>=0): # von vorne nach hinten druch Binaerrepresentation
        p=pow2(i)
        if(p<=x): # Bei einer Eins
            x=(x-p)
            s=prodZ(p,p) # Stelle verdoppeln
            c=(c+s) # Eine Ziffer setzen
            c=(c+(s+s)) # Zweite Ziffer setzen
        # Nullen werden implizit gesetzt
        i=(i-1)
    return c

# 2^x
def pow2(x):
    r=1
    while (x>0):
        r=(r+r)
        x=(x-1)
    return r

def maxPow(x):
    pow=1
    while(pow<=x):
        pow=(pow+pow)
    return pow

# Zweierlogarithmus
def binLength(x):
    pow=1
    c=0
    while(pow<=x):
        pow=(pow+pow)
        c=(c+1) 
    return c


# Die Leere liste
def ListCreate():
    return 2 # Direkt nach dem Beispiel auf Folie 10

# Anzahl der Elemente in der Liste
def ListGetLength(l):
    len=0
    p=maxPow(l)
    preOne=0 # Ob letztes Element eine 1 war
    even=1 # Bei jeder zweiten Stelle >0
    while(l>0): # Vorne>Hinten druch Binaerrepresentation
        if(l>=p): # Eine Eins
            l=(l-p)
            preOne=1
        else: # Eine Null
            # Trenner-Erkennung:
            if((preOne>0) and(even>0)):
                len=(len+1) # Laenge hochzaehlen
            preOne=0
        l=(l+l)
        # Schleifenvariablen aktualisieren
        if(even>0):
            even=0
        else:
            even=1
    return len
    

# i-tes Listenelement bei p>=1
def ListGetElement(l,pos):
    listpos=0 # Nummer des aktuellen Listeneintrags
    result=0 # Gesuchte Nummer wird hier zusammengesetzt
    p=maxPow(l)
    preOne=0 # wie oben
    even=1
    thisOne=0 # >0 wenn aktuelles Element eine Eins
    while(l>0): # Vorne>Hinten durch Binaerrepresentation
        if(p<=l): # Eins
            l=(l-p)
            thisOne=1
        else: # Null
            thisOne=0
        l=(l+l)
        if(thisOne>0): 
            preOne=1
        else:
            # Trenner-Erkennung
            if((preOne>0) and(even>0)):
                listpos=(listpos+1)
            preOne=0
        if(listpos>pos):
            l=0 # Schleife stoppen
        else:
            if(even>0):
                # Wenn kein Trenner kam, verdoppeln
                result=(result+result)
        if(thisOne>0):
            # Bei einer Eins an zweiter Stelle addieren
            if((listpos==pos) and(even>0)):
                result=(result+1)
        # Schleifenvariablen aktualisieren
        if(even>0):
            even=0
        else:
            even=1
    return result

# Positives e rechts an Liste
def ListAppendElement(l,e):
    bl=binLength(e)
    if(e==0):
        bl=1
    anzahlNullen=((bl+bl)+2) # Doppelte Stellenzahl + 2
    l=prodZ(pow2(anzahlNullen),l) # Nullen fuer Code und Trenner anhaengen
    ce=code(e)
    l=(l+((ce+ce)+(ce+ce))) # Code um 2 vorruecken und ans Ende schreiben
    l=(l+2) # Trenner ans Ende schreiben
    return l

def main():
    a=ListCreate()
    a=ListAppendElement(a,0)
    a=ListAppendElement(a,1)
    a=ListAppendElement(a,2)
    a=ListAppendElement(a,3)
    a=ListAppendElement(a,4)
    a=ListAppendElement(a,5)
    a=ListAppendElement(a,6)
    a=ListAppendElement(a,7)
    a=ListAppendElement(a,8)
    a=ListAppendElement(a,9)
    print(ListGetElement(a,1))
    print(ListGetElement(a,3))
    print(ListGetElement(a,9))
    return a



