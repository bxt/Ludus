
# Von Folie 40 im Script
def prodZ(x,y):
    [i,z] = [0,0]
    if (x < 0):
        x = (0 - x)
        y = (0 - y)
    for i in range(0,x):
        z = (z + y)
    return z

# Berechnung von floor(x/2)
def divtwo(x):
    y=0
    d=2
    while (d<=x):
        y=(y+1)
        d=((y+y)+2)
    return y

# Berechnung von ~ floor(log2(e))+1
# also Anzahl der Stellen in der Binaerrepresentation
def binLength(n):
    x=0
    if (not (n <= 0)):
        half=1337
        while(half>0):
            half=divtwo(n)
            n=(n-(half+half))
            n=half
            x=(x+1)
    else:
        x=1
    return x

# 2^x
def pow2(x):
    r=1
    while (x>0):
        r=(r+r)
        x=(x-1)
    return r

# Kodierung von n
def code(n):
    x=0
    if (not (n <= 0)):
        pow=1 # current power of 4
        half=1337 # Nur um Schleife anzustossen
        while(half>0):
            half=divtwo(n)
            n=(n-(half+half)) # n ist jetzt der Rest von /2
            x=(x+prodZ(n,pow)) # Eine Ziffer
            x=(x+prodZ(n,(pow+pow))) # Zweite Ziffer
            n=half # Fuer naechste Schleife
            pow=((pow+pow)+(pow+pow)) # *4 (Naechste 4er Potenz)
    return x

# Die Leere liste
def ListCreate():
    return 2 # Direkt nach dem Bespiel auf Folie 10

def ListGetLength(l): 
    c=-1 # counter fuer 10er
    while(l>0):
        quater=divtwo(divtwo(l)) # Division durch 4
        rest=(l-((quater+quater)+(quater+quater))) # mod 4
        if(rest == 2): # Rest == 2 <=> 10 am Ende
            c=(c+1) # Folglich diese 10 zaehlen
        l=quater # Fuer naechsten Durchlauf
    return c

# i-tes Listelement bei i>=1
def ListGetElement(l,i):
    len=(ListGetLength(l)+1)
    c=0 # counter fuer 10er
    lese=0 # >0 Wenn im Lesemodus
    ergebins=0
    cpow2=1 # aktuelle Zweierpotenz zum auslesen
    while(l>0):
        half=divtwo(l)
        quater=divtwo(half) # Division durch 4
        rest=(l-((quater+quater)+(quater+quater))) # % 4
        if(rest == 2): # Rest == 2 <=> 10 am Ende
            c=(c+1) # Folglich diese 10 zaehlen
            if(lese>0): # Beim Lesen eine 10
                quater=0 # Schleifenabbruch
            if(c==(len-i)): # An der gewuenschten Stelle angelangt
                lese=1 # Beginnen mit Lesen
        if((rest==3) and (lese>0)): # Wenn beim Lesen eine codierte 1 kommt
            ergebins=(ergebins+cpow2) # Bit Setzen
        if((lese>0) and (rest != 2)):
            cpow2=(cpow2+cpow2) # Naechste Zweierpotenz
        l=quater # Fuer naechsten Durchlauf
    return ergebins

# Positives e rechts an Liste
def ListAppendElement(l,e):
    anzahlNullen=(prodZ(binLength(e),2)+2) # Doppelte Stellenzahl + 2
    l=prodZ(l,pow2(anzahlNullen)) # Nullen fuer code und Trenner anhaengen
    l=(l+prodZ(code(e),4)) # Code um 2 Vorruecken und ans Ende schreiben
    l=(l+2) # Trenner ans Ende schreiben
    return l


def main():
    a=ListCreate()
    a=ListAppendElement(a,0)
    a=ListAppendElement(a,1)
    a=ListAppendElement(a,2)
    #a=ListAppendElement(a,3)
    #a=ListAppendElement(a,4)
    #a=ListAppendElement(a,5)
    #a=ListAppendElement(a,6)
    #a=ListAppendElement(a,7)
    #a=ListAppendElement(a,8)
    #a=ListAppendElement(a,9)
    print(ListGetElement(a,2))
    return a

