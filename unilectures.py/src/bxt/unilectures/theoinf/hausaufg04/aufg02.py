def DyadicRepresentation(x):
    # liefert eine Liste mit den dyadischen Ziffern von x
    # z.B. DyadicRepresentation(20) == [1, 2, 1, 2]
    dya=[];
    while (x>0):
        rem=x//2;
        if(x%2==0):
            rem=rem-1
        dya=[(x-rem*2)]+dya;
        x=rem;
    return dya;

# 2^x
def pow2(x):
    p=1
    while (x>0):
        p=(p+p)
        x=(x-1)
    return p  

def Number(l):
    # liefert den Wert der durch die Liste l dargestellten dyadischen Zahl
    # z.B. Number([1, 2, 1, 2]) == 20
    dez=0
    for i in range(0,len(l)):
        dez+=pow2(i)*l[len(l)-1-i];
    return dez;

def main(x):
    z = 0                                 # Zustand z0
    b1 = DyadicRepresentation(x) + [0]    # dya(x) auf Band 1 schreiben und 0=Leerzeichen anhaengen, wegen dya(0) = leeres Wort                              # 
    b2 = [0]                              # Band 2 ist leer
    h1 = h2 = 0                           # beide Koepfe stehen auf Position 0
    while (z != 1):                       # solange Stoppzustand nicht erreicht
        # hier die Befehle der TM simulieren
        # nach Verlassen der While-Schleife testen,
        # ob eine Konfiguration M(z1,w) vorliegt und
        # ggf. nicht definierten Programmabbruch ausloeen
        # Inhalt von Band 1 mittels Number() in eine Zahl y umwandeln
        d=0 # Spart else-Verschachtelungen
        if(d==0 and z==0 and b1[h1]==1 and b2[h2]==0): # (z0,1,_)->(z0,_,1,R,L)
            b1[h1]=0 # auf Band 1 ein _ schreiben
            b2[h2]=1 # auf Band 2 ein 1 schreiben
            h1=h1+1  # Kopf 1 nach rechts
            h2=h2-1  # Kopf 2 nach links
            d=1
        if(d==0 and z==0 and b1[h1]==2 and b2[h2]==0): # (z0,2,_)->(z0,_,2,R,L)
            b1[h1]=0 # auf Band 1 ein _ schreiben
            b2[h2]=2 # auf Band 2 ein 2 schreiben
            h1=h1+1  # Kopf 1 nach rechts
            h2=h2-1  # Kopf 2 nach links
            d=1
        if(d==0 and z==0 and b1[h1]==0 and b2[h2]==0): # (z0,_,_)->(z2,_,_,R,R)
            b1[h1]=0 # auf Band 1 ein _ schreiben
            b2[h2]=0 # auf Band 2 ein 1 schreiben
            h1=h1+1  # Kopf 1 nach rechts
            h2=h2+1  # Kopf 2 nach rechts
            z=2      # Zustandwechsel zu 2
            d=1
        if(d==0 and z==2 and b1[h1]==0 and b2[h2]==1): # (z2,_,1)->(z2,1,_,R,R)
            b1[h1]=1 # auf Band 1 ein 1 schreiben
            b2[h2]=0 # auf Band 2 ein _ schreiben
            h1=h1+1  # Kopf 1 nach rechts
            h2=h2+1  # Kopf 2 nach rechts
            d=1
        if(d==0 and z==2 and b1[h1]==0 and b2[h2]==2): # (z2,_,2)->(z2,2,_,R,R)
            b1[h1]=2 # auf Band 1 ein 2 schreiben
            b2[h2]=0 # auf Band 2 ein _ schreiben
            h1=h1+1  # Kopf 1 nach rechts
            h2=h2+1  # Kopf 2 nach rechts
            d=1
        if(d==0 and z==2 and b1[h1]==0 and b2[h2]==0): # (z2,_,_)->(z3,_,_,L,O)
            b1[h1]=0 # auf Band 1 ein _ schreiben
            b2[h2]=0 # auf Band 2 ein _ schreiben
            h1=h1-1  # Kopf 1 nach links
            z=3      # Zustandwechsel zu 3
            d=1
        if(d==0 and z==3 and b1[h1]==1 and b2[h2]==0): # (z3,1,_)->(z3,1,_,L,O)
            b1[h1]=1 # auf Band 1 ein 1 schreiben
            b2[h2]=0 # auf Band 2 ein _ schreiben
            h1=h1-1  # Kopf 1 nach links
            d=1
        if(d==0 and z==3 and b1[h1]==2 and b2[h2]==0): # (z3,2,_)->(z3,2,_,L,O)
            b1[h1]=2 # auf Band 1 ein 2 schreiben
            b2[h2]=0 # auf Band 2 ein _ schreiben
            h1=h1-1  # Kopf 1 nach links
            d=1
        if(d==0 and z==3 and b1[h1]==0 and b2[h2]==0): # (z3,_,_)->(z1,_,_,R,O)
            b1[h1]=0 # auf Band 1 ein _ schreiben
            b2[h2]=0 # auf Band 2 ein _ schreiben
            h1=h1+1  # Kopf 1 nach rechts
            z=1      # Zustandwechsel zu 1
            d=1
        if(d==0): # Sollte hier nicht auftreten
            print("Ueberfuehrungsfunktion ist nicht total")
        # Wenn noetig _ an Baender anhaengen:
        while(h1>=len(b1)):
            b1+=[0]
        while(h2>=len(b2)):
            b2+=[0]
        while(h1<0):
            b1=[0]+b1
            h1=h1+1
        while(h2<0):
            b2=[0]+b2
            h2=h2+1
    # Endzustand prüfen:
    block=0
    out=[]
    for x in range(0,len(b2)):
        if(b2[x]!=0): # Etwas anderes als Trenner in der Liste
            b2=otherBandNotEmpty # Undefinierte Var.
    for x in range(0,len(b1)):
        if(b1[x]!=1 and b1[x]!=2): # Nicht in {1,2}, folglich _
            if(block!=0):
                block=(block+1)
        else: 
            if(block>0):
                b2=wrongArityReturned # Undefinierte Var.
            out+=[b1[x]] # Ans Ergebnis hängen
    y=Number(out)
    return y









