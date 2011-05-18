def ggt_euklid(x,y):
    # Python Programm zur Berechnung des ggt mittels Euklid-Reihe
    print(x)
    print(y)
    prev1=x # vorletztes
    prev2=y # letztes
    while(prev2!=0):
        prev1,prev2=prev2,max(prev1,prev2)-min(prev1,prev2)
        print(prev2)
    return prev1

if __name__ == '__main__': 
    print(ggt_euklid(int(input("Zahl 1:")),int(input("Zahl 2:"))))