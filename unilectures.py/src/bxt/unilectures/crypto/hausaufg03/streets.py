from fractions import Fraction

n = list(range(1, 7))
M = [(a,b,c,d,e) for a in n for b in n for c in n for d in n for e in n]

if len(M) != 6**5: print("Fehler 01")

K = {m for m in M if set([1,2,3,4]) <= set(m) or set([2,3,4,5]) <= set(m) or set([3,4,5,6]) <= set(m)}
print("|K| = " + str(len(K)))
print("P(K) = " + str(Fraction(len(K), len(M))) + " = " + str(float(Fraction(len(K), len(M)))))

G = {m for m in M if set([1,2,3,4,5]) <= set(m) or set([2,3,4,5,6]) <= set(m)}
print("|G| = " + str(len(G)))
print("P(G) = " + str(Fraction(len(G), len(M))) + " = " + str(float(Fraction(len(G), len(M)))))

print("P(G|K) = P(G cap K) / P(K) = " + str(Fraction(len(G & K), len(K))) + " = " + str(float(Fraction(len(G & K), len(K)))))

