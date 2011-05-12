
def prodZ(x,y):
    c=0
    i=binLength(x)
    while(x>0):
        p=pow2(i)
        if(p<=x):
            x=(x-p)
            c=(c+prodZInner(i,y))
        i=(i-1)
    return c

def prodZInner(i,y):
    r=y
    if(i>0):
        r2=prodZInner((i-1),y)
        r=(r2+r2)
    return r


def code(x):
    c=0
    i=binLength(x)
    while(i>=0):
        p=pow2(i)
        z=0
        if(p<=x):
            x=(x-p)
            z=1
            s=prodZ(p,p)
            c=(c+prodZ(z,s)) # Eine Ziffer
            c=(c+prodZ(z,(s+s))) # Zweite Ziffer
        i=(i-1)
    return c

# 2^x
def pow2(x):
    r=1
    while (x>0):
        r=(r+r)
        x=(x-1)
    return r


def binLength(x):
    pow=1
    c=0
    while(pow<=x):
        pow=(pow+pow)
        c=(c+1) 
    return c


# Die Leere liste
def ListCreate():
    return 2 # Direkt nach dem Besipiel auf Folie 10

def ListGetLength(l):
    len=0
    i=binLength(l)
    preOne=0
    even=1
    while(l>0):
        p=pow2(i)
        if(p<=l):
            l=(l-p)
            preOne=1
        else:
            if((preOne>0) and(even>0)):
                len=(len+1)
            preOne=0
        i=(i-1)
        if(even>0):
            even=0
        else:
            even=1
    return len
    

# i-tes Listelement bei p>=1
def ListGetElement(l,pos):
    listpos=0
    result=0
    i=binLength(l)
    preOne=0
    even=1
    thisOne=0
    while(l>0):
        p=pow2(i)
        if(p<=l):
            l=(l-p)
            thisOne=1
        else:
            thisOne=0
        if(thisOne>0): 
            preOne=1
        else: 
            if((preOne>0) and(even>0)):
                listpos=(listpos+1)
            preOne=0
        if(listpos>pos):
            l=0
        else:
            if(even>0):
                result=(result+result)
        if(thisOne>0): 
            if((listpos==pos) and(even>0)):
                result=(result+1) 
        if(even>0):
            even=0
        else:
            even=1
        i=(i-1)
    return result

# Positives e rechts an Liste
def ListAppendElement(l,e):
    bl=binLength(e)
    if(e==0):
        bl=1
    anzahlNullen=((bl+bl)+2) # Dopelte Stellenzahl + 2
    l=prodZ(pow2(anzahlNullen),l) # Nullen für code und Trenner anhängen
    ce=code(e)
    l=(l+((ce+ce)+(ce+ce))) # Code um 2 Vorrücken und ans Ende schrieben
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



