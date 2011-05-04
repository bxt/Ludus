'''
Sudoku game
@date 2011-05-04
@author: Burny
'''
from itertools import chain

class LoadTypeError(TypeError):
    """Error thrown by load on invalid string length"""
    pass

def _comparators():
    """Generator for generators for indices to check for duplicates"""
    # Well, about 18 generators in this line :P
    return chain(
    ((i+x for i in range(9)) for x in range(0,9*9,9)),
    (range(x,9*9,9) for x in range(9)),
    (chain(*[range(x+l,x+l+3) for l in range(0,3*9,9)]) 
        for x in chain(*[range(k,k+9,3) for k in range(0,9*9,3*9)])) )

class Field:
    """9x9 Sudoku field"""
    def __init__(self):
        self.fields={}
        for i in range(9*9):
            self.fields[i]=0
    def check(self):
        """Check if filled values do not have duplicates"""
        for co in _comparators():
            values=[self.fields[i] for i in co if self.fields[i]>0]
            if(len(values)!=len(set(values))): return 0
        return 1
    def solved(self):
        """Check if all values are filled"""
        for i in range(9*9):
            if not self.fields[i]>0: return 0
        return 1
    def load(self,string):
        """Input string data for whole field's entries"""
        if not len(string)==9*9: raise LoadTypeError
        self.fields=[int(i) for i in string]
    def set(self,field,value):
        """Set one single entry"""
        self.fields[field]=value
    def print(self):
        """Printout field"""
        print()
        for i in range(81):
            if i%9==0: print()
            try:
                print(self.fields[i], end="")
            except KeyError:
                print("?", end="") 

def _comparatorsPrint():
    """Printout fileds of _comparators' results"""
    for set in _comparators():
        f=Field()
        for pos in set:
            f.set(pos,1)
        f.print()


if __name__ == '__main__': 
    #_comparatorsPrint()
    f=Field()
    f.load("001000000000000000023000000000000000000000004560000000000000000000000000000000000")
    f.print();
    print()
    print("good!" if f.check() else "bad?")