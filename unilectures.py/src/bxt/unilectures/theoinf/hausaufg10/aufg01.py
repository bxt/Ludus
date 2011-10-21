from shared import deaUnterscheidbareZustaende

def deaDefineAufg01():
    Sigma = {'a','b'}           # Alphabet
    Z = {0,1,2,3,4,5}           # Zustandsmenge
    delta = {}                      # Ueberfuehrungsfunktion
    delta[0,'a'] = 2
    delta[0,'b'] = 1
    delta[1,'a'] = 4
    delta[1,'b'] = 1
    delta[2,'a'] = 5
    delta[2,'b'] = 3
    delta[3,'a'] = 4
    delta[3,'b'] = 1
    delta[4,'a'] = 4
    delta[4,'b'] = 4
    delta[5,'a'] = 2
    delta[5,'b'] = 1
    F = {4}                         # Menge der akzeptierenden Zustaende
    A = [Sigma, Z, delta, 1, F]
    return A

def algoLatexTable(crosset,dea):
    (_,Z,_,_,_)=dea
    Z=list(Z)
    out="\n\[\n\\begin{array}{"+"c"*len(Z)+"}\n"
    for i in range(len(Z)):
        for k in range(i):
            if({Z[i],Z[k]} in crosset):
                out+=" \\times &"
            else:
                out+="        &"
        out+="  "+str(Z[i])+"  \\\\\n"
    out+="\\end{array}\n\\]\n"
    return out

def main():
    dea=deaDefineAufg01()
    print(algoLatexTable(deaUnterscheidbareZustaende(dea),dea))

main();


