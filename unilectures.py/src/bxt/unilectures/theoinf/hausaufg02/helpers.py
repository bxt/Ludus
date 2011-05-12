def bin(x):
    print(binInner(x,1))
    return 0

def binInner(x,myPow):
    myPow=(myPow+myPow)     # Nächste Zweierpotenz
    if(myPow<x):            # Noch zu klein
        x=binInner(x,myPow) # Rekursiver Aufruf
    if(myPow>x):            # Wenn nicht subtrahiert werden kann
        print(0)
        result=x            # Wird nicht subtrahiert
    else:                   # Ansonsten
        print(1)
        result=(x-myPow)    # Schon
    return result
    
