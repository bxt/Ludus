
# WHILE-Programm zur Berechnung von floor(x/2)
def divtwo(x):
    y=0
    d=2
    while (d<=x):
        y=(y+1)
        d=2
        d=(y+y)
        d=(d+2)
    return y

# WHILE-Programm zur Ausgabe der Binärrepräsentation
def bin(n):
    if (n <= 0):
        print(0)
    else:
        half=2
        while(half>0):
            half=divtwo(n)
            n=(n-half)
            n=(n-half)
            print(n) # rest
            n=half
    return -1