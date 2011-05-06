
# WHILE-Programm zur Berechnung von x*2 
def g(x):
    x=(x+x)
    return x

# WHILE-Programm zur Berechnung von 2^x
def f(x):
    if (x<0):
        result=0
    else:
        if (x==0):
            result=1
        else: 
            result=g(f((x-1)))
    return result