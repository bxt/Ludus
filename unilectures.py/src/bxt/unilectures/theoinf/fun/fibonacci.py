"""
Algorithm to calculate a fibunacci number recursively. 

@date 2011-05-03
@author: Burny
"""

from functools import wraps

def memoize(func):
    saved = {}
    @wraps(func)
    def call(*args):
        try:
            return saved[args]
        except KeyError:
            res = func(*args)
            saved[args] = res
            return res
        except TypeError:
            # Unhashable argument
            return func(*args)
    call.__name__ = func.__name__
    return call

@memoize
def fib(n):
    """Calculate n-th Fibonacci number recursively.
    
    >>> (fib(-3),fib(0),fib(1),fib(2),fib(3),fib(4),fib(5),fib(6))
    (-3, 0, 1, 1, 2, 3, 5, 8)
    
    """
    if n<=1: return n
    return fib(n-1)+fib(n-2)

def main():
    """Prints out fibonacci sequence. """
    for i in range(int(input("Wie viele Fibonacci-Zahlen?"))):
        print( "Die %d-te Fibonacci-Zahl lautet: %d" % (i, fib(i)) )

if __name__ == '__main__':
    import doctest
    doctest.testmod()#verbose=True) 
    main()
