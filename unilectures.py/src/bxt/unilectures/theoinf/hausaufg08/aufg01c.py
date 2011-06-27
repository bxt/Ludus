
def powerset(s):                     # liefert die Potenzmenge der Menge s
    ps = {frozenset()}               # Verwendung von frozenset statt set, da Mengen nur
    for z in s:                      # nichtveraenderbare Objekte enthalten duerfen. Beispiel:
        ps |= {a | {z} for a in ps}  #     falsch:  a = { {1,2}, {4,5} }
    return ps                        #     richtig: a = { frozenset({1,2}), frozenset({4,5}) }

def nea2dea(A):                      # liefert aequivalenten DEA (Potenzmengenkonstruktion)
    [Sigma, Z, delta, z0, F] = A     # A ist ein NEA entsprechend Definition 3.11
    Z1 = powerset(Z)
    delta1 = {}                      # Ueberfuehrungsfunktion
    for S in Z1:                     # Verwendung von frozenset statt set, da Mengen nur
        for a in Sigma:              # nichtveraenderbare Objekte enthalten duerfen. 
            delta1[S,a] = frozenset({s for z in S for s in delta[z, a]})
    F1 = frozenset({S for S in Z1 if (S & F) != set()})
    return [Sigma, Z1, delta1, frozenset({z0}), F1]

def tupelize(prefix,oldset):         # Zustaende eindeutig zu machen, indem
    return {(prefix,z) for z in oldset} # sie mit einem Praefix in ein Tupel kommen

def neaKomplement(A):                # liefert NEA, der das Komplement von L(A) akzeptiert,
    [Sigma, Z, delta, z0, F] = A     # wobei A ein NEA entsprechend Definition 3.11 ist
    Adea=nea2dea(A)                  # Einen sprachaequivalenten DEA konstruieren
    deltaDea=Adea[2]                 # Dessen Ueberfuehrungsfunktion
    deltaNea={}                      # Neue NEA-Kompatibele Ueberff.
    for k,v in deltaDea.items():     # Aus der des DEAs gebaut
        deltaNea[k]={v}              # Jedoch mit Einermengen
    C = [Adea[0],Adea[1],deltaNea,Adea[3],(Adea[1]-Adea[4])] # Groestenteils der DEA
    return C

def neaVereinigung(A,B):     # liefert NEA, der die Vereinigung von L(A) und L(B) akzeptiert,
    [ASigma, AZ, Adelta, Az0, AF] = A # wobei A,B NEAs entsprechend Definition 3.11 sind
    [BSigma, BZ, Bdelta, Bz0, BF] = B
    Sigma = ASigma | BSigma
    z0=(2,"z0")                      # Komplett neuer Startzustand
    Z = tupelize(0,AZ)|tupelize(1,BZ)|{z0} # Vereinigung alte Zustaende und z0
    # Zustaende bleiben akzeptierend, z0 akz. wenn epsilon in L(A) oder in L(B) ist
    F = tupelize(0,AF)|tupelize(1,BF)|({z0} if (Az0 in AF) or (Bz0 in BF) else set())
    delta={}                         # Neue Ueberfuehrungsfunktion
    for z in Z:
        if z[0]==0:                  # Ein Zustand aus A
            for a in ASigma:         # Ein Zeichen aus As Alhpabet
                delta[z,a]=tupelize(0,Adelta[z[1],a]) # Pfeil aus A
            for a in BSigma-ASigma:  # A kann mit dem Zeichen nichts anfangen
                delta[z,a]=set()     # Rechenweg endet immer
        if z[0]==1: # from B         # Selbes Spiel fuer Bs Zustaende
            for a in BSigma:
                delta[z,a]=tupelize(1,Bdelta[z[1],a])
            for a in ASigma-BSigma:
                delta[z,a]=set()
        if z[0]==2:                  # Aus dem neuen z0
            for a in Sigma:          # Ersmtal initialisieren
                delta[z,a]=set()     # Mit leeren Mengen
            for a in ASigma:         # Zu den Nachstartzustaenden gehen
                delta[z,a] |= tupelize(0,Adelta[Az0,a])
            for a in BSigma:         # In beiden Automaten gleichzeitig
                delta[z,a] |= tupelize(1,Bdelta[Bz0,a])
    C = [Sigma, Z, delta, z0, F]     # Zusammensetzen
    return C

def neaKonkatenation(A,B):           # liefert NEA, der L(A)L(B) akzeptiert,
    [ASigma, AZ, Adelta, Az0, AF] = A # wobei A,B NEAs entsprechend Definition 3.11 sind
    [BSigma, BZ, Bdelta, Bz0, BF] = B
    Sigma = ASigma | BSigma
    Z = tupelize(0,AZ)|tupelize(1,BZ)# Alte Zustaende disjunktivieren und vereinigen
    z0 = (0,Az0)                     # Uebernehme Startzustand von A
    F = tupelize(1,BF)               # Uebernahme akz. Zustaende von B
    if Bz0 in BF: F |= tupelize(0,AF)# Wenn epsilon in L(B) auch akz. Z. von A nehmen
    delta={}                         # Neue Ueberfuehrungsfunktion
    for z in Z:
        if z[0]==0 and not z[1] in AF: # Nicht-akz. Zustand von A
            for a in ASigma:         # Wie bei A verhalten
                delta[z,a]=tupelize(0,Adelta[z[1],a])
            for a in BSigma-ASigma:
                delta[z,a]=set()
        if z[0]==1:                  # Zustand von B
            for a in BSigma:         # Wie bei B verhalten
                delta[z,a]=tupelize(1,Bdelta[z[1],a])
            for a in ASigma-BSigma:
                delta[z,a]=set()
        if z[0]==0 and z[1] in AF:   # Akz. Zustand von A
            for a in Sigma:          # Zunaechst initialisieren
                delta[z,a]=set()
            for a in ASigma:         # Ganz normal in A weiter
                delta[z,a] |= tupelize(0,Adelta[z[1],a])
            for a in BSigma:         # Aber auch nach B springen
                delta[z,a] |= tupelize(1,Bdelta[Bz0,a])
    C = [Sigma, Z, delta, z0, F]     # Zusammensetzen
    return C


