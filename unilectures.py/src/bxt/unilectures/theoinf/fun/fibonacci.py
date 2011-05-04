'''
Algorithm to calculate a fibunacci number recursively. 

@date 2011-05-03
@author: Burny
'''

def memoize(func):
    saved = {}
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
    """Recursive method to calculate a Fibonacci number."""
    if n<=1: return n
    return fib(n-1)+fib(n-2)

def main():
    """Prints out fibonacci sequence. """
    for i in range(15):
        print( "Die %d-te Fibonacci-Zahl lautet: %d" % (i, fib(i)) )

if __name__ == '__main__': 
    main()
