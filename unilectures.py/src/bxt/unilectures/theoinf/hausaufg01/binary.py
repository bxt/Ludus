
# WHILE-Programm zur Berechnung von floor(x/2)
def divtwo(x):
    y=0
    d=2
    while (d<=x):
        y=(y+1)
        d=((y+y)+2)
    return y

# WHILE-Programm zur Ausgabe der Binärrepräsentation
def bin(n):
    if (n <= 0):
        print(0)
    else:
        half=1337
        while(half>0):
            half=divtwo(n)
            n=(n-(half+half))
            print(n) # rest
            n=half
    return -1