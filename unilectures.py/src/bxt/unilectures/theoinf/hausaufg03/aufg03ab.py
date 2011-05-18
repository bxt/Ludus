def codeZ(z): # Bijektion Z nach N
    n=(z+z) # 2*z
    if(z<0): # für negative z
        n=((0-1)-n) # 0-1-2n
    return n

def codeZinv(n): # Bijektion N nach Z
    z=0
    while(n>=2): # Division durch 2
        z=(z+1)
        n=(n-2)
    if(n==1): # Ungerade, somit negatives z
        z=((0-z)-1)
    return z

def fN(x,y): # Funktion über N
    return codeZ(f(codeZinv(x),codeZinv(y)))

def h(x,y): # Funktion über Z, deren Funktion über N, die Funktion g ist
    return codeZinv(g(codeZ(y),codeZ(y)))
