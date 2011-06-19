
def ueberlappung(eingelesen, suchmuster): #  Laenge des max. Suffix von e., 
    #                                     #  welches Praefix von s. ist
    result=0          # Maximale bisher gefunden Laenge (0 ist trivial)
    l=len(eingelesen) # |e.|, wird noch oefters verwendet
    for i in range(1,l+1):            # von 1 bis einschl. |e.|
        ein_suffix =eingelesen[l-i:l]     # Das Suffix von e. mit Laenge i
        sum_preafix=suchmuster[0:i]       # Praefix von s. des selben Laenge
        if ein_suffix==sum_preafix:       # Test auf Uebereinstimmung
            result=i                      # Neue max. Laenge gefunden
    return result

def deaDefineSuche(Sigma,v): # DEA entsprechend Def. 3.2 und Bsp. 3.9
    Z = set() # Zustandsmenge, zunaechst leere Menge, dann:
    for z in range(len(v)+1): # |v|+1 viele Zustaende
        Z=Z|{z} # Name jeweils die Anzahl der passenden eingelesenen Zeichen
    delta = {} # Ueberfuehrungsfunktion
    for i in range(len(v)): # Zustaende fuer jedes Zeichen des Suchmusters
        for b in Sigma: # Fuer jedes Zeichen b des Alphabets
            delta[i,b] = ueberlappung(v[0:i]+b,v) #Zum entsprechenden Zust.
    for b in Sigma: # Egal welches Zeichen, nach einmaligen Findens  
        delta[len(v),b] = len(v) # im Endzustand |v| bleiben
    F = {len(v)} # Menge der akzeptierenden Zustaende (Endzustand |v|)
    A = [Sigma, Z, delta, 0, F] # Zum Tupel zusammensetzen
    return A # Feritg

# Funktionen aus dem Loesungshinweis zur Aufgabe 3 des Uebungsblatt 6:
def deaErweiterteUEF(delta, z, w): # erw. Ueberfuehrungsfunktion eines DEA
    for a in w:
        z = delta[z, a]
    return z
def deaRun(A, w): # testet, ob der DEA A das Wort w akzeptiert
    [Sigma, Z, delta, z0, F] = A # A ist ein DEA entsprechend Definition 3.2
    return deaErweiterteUEF(delta, z0, w) in F

def schnelleSuche(Sigma,v,w): # Schnelle suche mittels DEA (v=Muster,w=Wort)
    A=deaDefineSuche(Sigma,v) # DEA erstellen
    return deaRun(A,w)        # Wort einlesen


if __name__ == '__main__': # Tests:
    print(schnelleSuche("abcd","abc","aaddababcaa"))
    print(schnelleSuche("abcd","abc","aaddababaca"))
    print(schnelleSuche("abcd","abcabd","aaadaddacdcadabcabcabdaac"))
    print(schnelleSuche("abcd","","aaddababcaa"))
    print(schnelleSuche("abcd","abc",""))




