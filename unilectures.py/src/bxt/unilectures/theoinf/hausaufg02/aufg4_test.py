


from aufg4 import odd
from time import time
from sys import setrecursionlimit

#setrecursionlimit(300000000)

pow10=1
i=0
try:
    #while(i<100000000):
    while(1):
        start=time()
        try:
            odd(pow10)
        except RuntimeError:
            print("Probably reached recursion limit at 10^",i)
            break
        end=time()
        if(i%100==0): 
            print(i)
        if(end-start>60): 
            print("Reached 1 minute limit at 10^",i)
            break
        i=i+6
        pow10=pow10*1000000
finally:
    print("Ending at 10^",i)