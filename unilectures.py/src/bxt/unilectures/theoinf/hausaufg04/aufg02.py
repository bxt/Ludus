def DyadicRepresentation(x):
    # Liefert eine Liste mit den dyadischen Ziffern von x
    # z.B. DyadicRepresentation(20) == [1, 2, 1, 2]
    dya=[];                # Starte mit leerem Wort
    while (x>0):
        newX=x//2;         # Division mit Rest
        if(x%2==0):        # Kein Rest, somit eine 2
            newX=newX-1    # Die 2 ist zu viel gezaehlt
            dya=[2]+dya;
        else:              # Eine 1
            dya=[1]+dya;
        x=newX;
    return dya;

def Number(l):
    # Liefert den Wert der durch die Liste l dargestellten dyadischen Zahl
    # z.B. Number([1, 2, 1, 2]) == 20
    dez=0                           # Baue hier natuerliche Zahl
    p=1                             # Entsprechende Zwierpozenz
    for i in range(0,len(l)):       # Benutze Summenformel
        v=l[len(l)-1-i]             # Die 2 oder 1 an der Stelle
        dez+=p*v;                   # Wert dieser Ziffer addieren
        p=(p+p)                     # Naechste Zwierpozenz
    return dez;

def main(x):
    z = 0                                 # Zustand z0
    b1 = DyadicRepresentation(x) + [0]    # dya(x)_ auf Band 1 schreiben
    b2 = [0]                              # Band 2 ist leer
    h1 = h2 = 0                           # beide Koepfe stehen auf Position 0
    while (z != 1):                       # solange Stoppzustand nicht erreicht
        # Die Befehle der TM simulieren
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
        # Wenn noetig _ hinten an Baender anhaengen:
        while(h1>=len(b1)):
            b1+=[0]
        while(h2>=len(b2)):
            b2+=[0]
        # Wenn noetig _ vorne an Baender anhaengen:
        while(h1<0):
            b1=[0]+b1
            h1=h1+1
        while(h2<0):
            b2=[0]+b2
            h2=h2+1
    # Nach Verlassen der While-Schleife testen,
    # ob eine Konfiguration M(z1,w) vorliegt und
    # ggf. nicht definierten Programmabbruch ausloesen
    block=0 # Bei wievielter Zahl auf Band 1 wir sind
    out=[] # Erste dyadische Zahl auf Band 1
    for x in range(0,len(b2)):
        if(b2[x]!=0): # Etwas anderes als Trenner in der Liste
            b2=otherBandNotEmpty      # Undefinierte Variable
    for x in range(0,len(b1)):
        if(b1[x]!=1 and b1[x]!=2):    # Nicht in {1,2}, folglich _
            if(block!=0):             # Wenn wieder ein Zahl kommt
                block=(block+1)       # ist es die naechste
        else: 
            if(block>0):              # Zu viele Zahlen auf Band 1
                b2=wrongArityReturned # Undefinierte Variable
            out+=[b1[x]]              # Ans Ergebnis haengen
    # Inhalt von Band 1 mittels Number() in eine Zahl y umwandeln
    y=Number(out)
    return y









