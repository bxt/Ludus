def deaDefine():                    # definiert einen DEA A entsprechend Definition 3.2
    Sigma = {'a','b','c'}           # Alphabet
    Z = {0,1,2,3,4,5,6,7}           # Zustandsmenge
    delta = {}                      # Ueberfuehrungsfunktion
    delta[0,'a'] = 1
    delta[0,'b'] = 0
    delta[0,'c'] = 0
    delta[1,'a'] = 1
    delta[1,'b'] = 0
    delta[1,'c'] = 2
    delta[2,'a'] = 1
    delta[2,'b'] = 3
    delta[2,'c'] = 0
    delta[3,'a'] = 4
    delta[3,'b'] = 0
    delta[3,'c'] = 0
    delta[4,'a'] = 1
    delta[4,'b'] = 0
    delta[4,'c'] = 5
    delta[5,'a'] = 6
    delta[5,'b'] = 3
    delta[5,'c'] = 0
    delta[6,'a'] = 1
    delta[6,'b'] = 0
    delta[6,'c'] = 7
    delta[7,'a'] = 7
    delta[7,'b'] = 7
    delta[7,'c'] = 7
    F = {7}                         # Menge der akzeptierenden Zustaende
    A = [Sigma, Z, delta, 0, F]
    return A

def deaErweiterteUEF(delta, z, w):  # erweiterte Ueberfuehrungsfunktion eines DEA
    for a in w:
        z = delta[z, a]
    return z

def deaRun(A, w):                   # testet, ob der DEA A das Wort w akzeptiert
    [Sigma, Z, delta, z0, F] = A    # A ist ein DEA entsprechend Definition 3.2
    return deaErweiterteUEF(delta, z0, w) in F

