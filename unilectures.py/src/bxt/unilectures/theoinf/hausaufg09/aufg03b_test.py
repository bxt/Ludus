from aufg03b import deasAequivalent


def deaDefineAufg03aA1():                    # definiert einen DEA A entsprechend Definition 3.2
    Sigma = {'a','b','c'}           # Alphabet
    Z = {1,2,3}           # Zustandsmenge
    delta = {}                      # Ueberfuehrungsfunktion
    delta[1,'a'] = 2
    delta[1,'b'] = 2
    delta[1,'c'] = 2
    delta[2,'a'] = 3
    delta[2,'b'] = 3
    delta[2,'c'] = 3
    delta[3,'a'] = 1
    delta[3,'b'] = 1
    delta[3,'c'] = 1
    F = {1}                         # Menge der akzeptierenden Zustaende
    A = [Sigma, Z, delta, 1, F]
    return A

def deaDefineAufg03aA2():                    # definiert einen DEA A entsprechend Definition 3.2
    Sigma = {'a','b','c'}           # Alphabet
    Z = {4,5,6,7,8}           # Zustandsmenge
    delta = {}                      # Ueberfuehrungsfunktion
    delta[4,'a'] = 7
    delta[4,'b'] = 6
    delta[4,'c'] = 7
    delta[5,'a'] = 4
    delta[5,'b'] = 4
    delta[5,'c'] = 4
    delta[6,'a'] = 5
    delta[6,'b'] = 8
    delta[6,'c'] = 5
    delta[7,'a'] = 8
    delta[7,'b'] = 8
    delta[7,'c'] = 5
    delta[8,'a'] = 4
    delta[8,'b'] = 4
    delta[8,'c'] = 4
    F = {4}                         # Menge der akzeptierenden Zustaende
    A = [Sigma, Z, delta, 4, F]
    return A


def main():
    dea1=deaDefineAufg03aA1()
    dea2=deaDefineAufg03aA2()
    print(deasAequivalent(dea1,dea2))


main()