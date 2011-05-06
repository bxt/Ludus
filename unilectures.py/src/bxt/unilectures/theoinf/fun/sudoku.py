'''
Sudoku game
@date 2011-05-04
@author: Burny
'''
from itertools import chain
from time import clock

class LoadTypeError(TypeError):
    """Error thrown by load on invalid string length"""
    pass

class UnsolvableSudoku(TypeError):
    """Error thrown by load on invalid string length"""
    pass

class FieldRangeError(TypeError):
    """Error thrown when setting a field to a number not in range"""
    pass

s=9
s_i=3


def _comparatorsGenerator():
    """Generator for generators for indices to check for duplicates"""
    # Well, about 18 generators in this line :P
    return chain(
    ((i+x for i in range(s)) for x in range(0,s*s,s)),
    (range(x,s*s,s) for x in range(s)),
    (chain(*[range(x+l,x+l+s_i) for l in range(0,s_i*s,s)]) 
        for x in chain(*[range(k,k+s,s_i) for k in range(0,s*s,s_i*s)])) )

_comparatorsCached=[list(x) for x in _comparatorsGenerator()]

def _comparators():
    """Sacrifice some memory for CPU not using generators"""
    return _comparatorsCached


class Field:
    """9x9 Sudoku field"""
    def __init__(self):
        self.fields={}
        for i in range(s*s):
            self.set(i,0)
    def check(self):
        """Check if filled values do not have duplicates"""
        for co in _comparators():
            values=[self.fields[i] for i in co if self.fields[i]>0]
            if(len(values)!=len(set(values))): return 0
        return 1
    def solved(self):
        """Check if all values are filled"""
        for i in range(s*s):
            if not self.fields[i]>0: return 0
        return 1
    def filled(self):
        """Indices of filled cells"""
        return set([i for i in range(s*s) if self.fields[i]>0])
    def load(self,string):
        """Input string data for whole field's entries"""
        if not len(string)==s*s: raise LoadTypeError
        self.fields=[int(i) for i in string]
    def set(self,field,value):
        """Set one single entry"""
        if not 0<=value<=s: raise FieldRangeError
        self.fields[field]=value
    def get(self,field):
        """Get one single entry"""
        return self.fields[field]
    def inc(self,field):
        """Increment one single entry"""
        self.set(field,self.fields[field]+1)
    def copy(self):
        """Make an independent clone"""
        f=Field();
        for i in range(s*s):
            f.set(i,self.fields[i])
        return f
    def print(self):
        """Printout field"""
        print()
        for i in range(s*s):
            if i%s==0: print()
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

def solve(field):
    """A very basic brute force solver using some backtracking only"""
    print("Brute forcing...")
    t=clock()
    if(not field.check()): raise UnsolvableSudoku
    prefilled=field.filled()
    solution=field.copy()
    i=0
    while 0<=i<s*s:
        if i in prefilled:
            i=i+1
            continue
        try:
            solution.inc(i)
            while not solution.check():
                solution.inc(i)
            i=i+1
        except FieldRangeError:
            solution.set(i,0)
            i=i-1
            while i in prefilled:
                i=i-1
    if i<0: raise UnsolvableSudoku
    print ('Dude, took me %.3f s' % (clock()-t))
    return solution

if __name__ == '__main__': 
    #_comparatorsPrint()
    f=Field()
    # hard one
    f.load("100007090030020008009600500005300900010080002600004000300000010040000007007000300")
    # rather empty
    # f.load("001000000000000000023000000000000000000000004560000000000000000000000000000000000")
    # empty one
    #f.load("000000000000000000000000000000000000000000000000000000000000000000000000000000000")
    # takes "forever"
    #f.load("000000000000003085001020000000507000004000100090000000500000073002010000000040009")
    f.print();
    print()
    print("good!" if f.check() else "bad?")
    solve(f).print()
