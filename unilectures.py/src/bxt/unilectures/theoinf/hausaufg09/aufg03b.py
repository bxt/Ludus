# gibt alle Paare von unterscheidbaren Zustaenden in A zurueck
def deaUnterscheidbareZustaende(A):
    [Sigma, Z, delta, z0, F] = A
    
    # Bilde Liste aller Zweierkombinatinen von Zustaenden
    # Liste aus Tupeln, da Liste/Tupel besser iterierbar/extrahierbar als Menge/Menge
    # Mit i<=j bleiben die Tupel aequivalent zu Mengen
    U = [(i,j) for i in range(1,max(Z)+1) for j in range(1,max(Z)+1) if i<=j]
    # Bilde Menge (bzw. Liste) aller bisher markierter Kombinationen
    markiert = [{i,j} for i in range(1,max(Z)+1) for j in range(1,max(Z)+1) 
                if i<=j and ((i in F) ==(not (j in F)))  ] # Vormarkierungsbedingung
    
    einesWurdeMarkeirt=True                     # Praedikat, ob im Durchlauf markiert wurde
    while einesWurdeMarkeirt:                   # Stoppen, wenn keines markiert wurde,
        einesWurdeMarkeirt=False                # eigentlich eine do-while-Schleife
        for s in U:                             # Alle Zustaende, 
            if not ({s[0],s[1]} in markiert):   # die nicht markiert sind,
                for a in Sigma:                 # falls es ein a in Sigma gibt,
                    tmp={delta[s[0],a],delta[s[1],a]} # (erreichte Zustaende)
                    if tmp in markiert:         # fuer das die {d(i,s),d(j,a)} markiert ist:
                        einesWurdeMarkeirt=True # Praedikat setzen
                        markiert+=[{s[0],s[1]}] # Markieren
                        break                   # Andere Buchstaben dann egal
    
    return markiert          # Liste der markierten Mengen zurueckgeben

def deasAequivalent(A, B):   # prueft, ob DEAs A und B die gleiche Sprache akzeptieren
    [ASigma, AZ, Adelta, Az0, AF] = A
    [BSigma, BZ, Bdelta, Bz0, BF] = B
    Sigma = ASigma & BSigma  # Annahme: ASigma = BSigma
    
    Z = AZ | BZ              # Annahme: AZ, BZ disjunkt
    Adelta.update(Bdelta)
    delta = Adelta
    F = AF | BF
    
    C = [Sigma, Z, delta, Az0, F]
    return {Az0, Bz0} not in deaUnterscheidbareZustaende(C)

