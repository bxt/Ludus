
def oddInner(x,myPow):
    myPow=(myPow+myPow)     # Naechste Zweierpotenz
    if(myPow<x):            # Noch zu klein
        x=oddInner(x,myPow) # Rekursiver Aufruf
    if(myPow>x):            # Wenn nicht subtrahiert werden kann
        result=x            # Wird nicht subtrahiert
    else:                   # Ansonsten
        result=(x-myPow)    # Schon
    return result           # Zahl ohne erste log2(myPow) Binaerstellen
    

def odd(x):
    e=oddInner(x,1) # Letzte Stelle der Binaerrepraesentation
    return e
