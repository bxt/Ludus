
from aufg5_reviewed import ListAppendElement,ListCreate,ListGetElement,ListGetLength
from time import time

n=1
while(1):
    start=time()
    
    a=ListCreate()
    for x in range(1,n):
        a=ListAppendElement(a,x)
    
    for x in range(1,ListGetLength(a)):
        if not (x==ListGetElement(a,x)):
            print("uarg! n x fx",n,x,ListGetElement(a,x))
    
    end=time()
    #if(n%1000==0): 
    print(n)
    if(end-start>60): 
        print("Reached 1 minute limit at ",n) # about 1000 !
        break
    n=n+10

