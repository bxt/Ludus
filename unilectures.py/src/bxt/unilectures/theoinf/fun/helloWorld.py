'''
Created on 02.05.2011

@author: Burny
'''

def helloWorld():
    print('Hello World!')

def simpleCalculation():
    i=4
    i=i+3
    print("The result is: " +str(i))

def multiplicationTable():
    print("Wunderschöne Multiplikationstabelle: ")
    for i in range(10):
        for k in range(10):
            if(k>=i or k==0):
                print(repr((i+1)*(k+1)).rjust(4), end="")
            else:
                print("  ..", end="")
        print();

class Message:
    '''Prints stuff...'''
    def __init__(self,msg):
        self.string=msg
        self.count=0
    def print(self):
        self.count+=1
        print(self.string, self.count)

def cubes(*args): #lol, no, not a pointer at all :)
    def cube(x): return x*x*x
    for x in map(cube, range(1, args[0])):
        print(x, end=", ")
    print()
    for x in [cube(x) for x in range(1, args[0]) if 3<x<7]:
        print(x, end=", ")

def main():
    helloWorld()
    simpleCalculation()
    multiplicationTable()
    mess=11,
    cuba=Message("Wooha "+("abc"[:2]+"x")[-2:]+"t!")
    mess, cuba=cuba, mess[0]
    mess.print()
    Message.print(*[mess])
    pass
    cubes(cuba)

def log(name):
    def decorator(func):
        def decoration(n):
            print(name)
            return func(n)
        return decoration
    return decorator

if __name__ == '__main__': 
    main()
