# Lese- und Schreibfunktionen von Folie 77

def read(u,v,a):            # liefert den Inhalt von Ra
    i = 0
    while (i < len(u) and u[i] != a): # Index a suchen
        i = i + 1
    if (i == len(u)):                 # Listen erweitern
        u += [a]
        v += [0]
    return v[i]                       # Inhalt von Ra zurueck

def write(u,v,a,b):         # schreibt b in Ra
    i = 0
    while (i < len(u) and u[i] != a): # Index a suchen
        i = i + 1
    if (i == len(u)):                 # Listen erweitern
        u += [a]
        v += [0]
    v[i] = b                          # schreibt b in Ra


def phi(x1,x2):# Aritaet 2 vorgegeben
    u = [0,1]
    v = [x1,x2]
    br = 0
    while (br < 7): # 7 ist STOP
        if (br == 0): # R3 <- RR2
            i = read(u,v,2)
            j = read(u,v,i)
            write(u,v,3,j)
            br = br + 1
        if (br == 1): # R3 <- R3 + R2
            i = read(u,v,3) + read(u,v,2)
            write(u,v,3,i)
            br = br + 1
        if (br == 2): # R2 <- R2 + R1
            i = read(u,v,2) + read(u,v,1)
            write(u,v,2,i)
            br = br + 1
        if (br == 3): # RR2 <- R3
            i = read(u,v,3)
            j = read(u,v,2)
            write(u,v,j,i)
            br = br + 1
        if (br == 4): # R0 <- R0 - R1
            i = read(u,v,0) - read(u,v,1)
            if (i < 0):
                i = 0
            write(u,v,0,i)
            br = br + 1
        if (br == 5): # IF R0 > 0 GOTO 0
            if (read(u,v,0) > 0):
                br = 0
            else:
                br = br + 1
        if (br == 6): # R0 <- RR2
            i = read(u,v,2)
            j = read(u,v,i)
            write(u,v,0,j)
            br = br + 1
    return v[0]

# print(phi(79846313,657684)) # 4934211917
