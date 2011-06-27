from pprint import pprint
from aufg01c import nea2dea,neaKomplement,neaVereinigung,neaKonkatenation

def deaErweiterteUEF(delta, z, w):  # erweiterte Ueberfuehrungsfunktion eines DEA
    for a in w:
        z = delta[z, a]
    return z

def deaRun(A, w):                   # testet, ob der DEA A das Wort w akzeptiert
    [Sigma, Z, delta, z0, F] = A    # A ist ein DEA entsprechend Definition 3.2
    return deaErweiterteUEF(delta, z0, w) in F

def testNeaA(a):
    Sigma = {a}           # Alphabet
    z0 = a+"0"
    z1 = a+"1"
    Z = {z0,z1}           # Zustandsmenge
    delta = {}                      # Ueberfuehrungsfunktion
    delta[z0,a] = {z1}
    delta[z1,a] = set()
    F = {z1}                         # Menge der akzeptierenden Zustaende
    A = [Sigma, Z, delta, z0, F]
    return A

def main():
    neaA=testNeaA('a')
    neaB=testNeaA('b')
    neaC=testNeaA('c')
    neaAB=neaKonkatenation(neaA,neaB)
    neaAvB=neaVereinigung(neaA,neaB)
    neaAvB_C=neaKonkatenation(neaAvB,neaC)
    neaNotA=neaKomplement(neaA)
    neaAvB_CnoA=neaKonkatenation(neaAvB_C,neaNotA)
    neaAvBnoA=neaKonkatenation(neaAvB,neaNotA)
    neaAnoA=neaKonkatenation(neaA,neaNotA)
    neanoAnoA=neaKonkatenation(neaNotA,neaNotA)
    
    dut=neaAvBnoA
    dea=nea2dea(dut)
    pprint(dut)
    #pprint(dea)
    print(deaRun(dea,"ab")) ## WHY FALSE!?

main();