from aufg02 import main,DyadicRepresentation


# Relevante Script-Seiten: 120, 70

print(main(0)==0)
print(main(1)==1)
print(main(2)==2)
print(main(3)==3)
print(main(4)==5)
print(main(5)==4)
print(main(9)==9)
print(main(99)==72)
print(main(10)==13)
print(main(1000)==814)
print(main(72)==99)
print(main(13)==10)
print(main(814)==1000)

def printDyadicPalindromes(till):
    for x in range(0,till):
        if(main(x)==x):
            print(x,DyadicRepresentation(x))

printDyadicPalindromes(200)