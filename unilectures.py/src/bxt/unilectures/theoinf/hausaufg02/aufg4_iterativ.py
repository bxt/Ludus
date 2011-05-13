

# 2^x
def pow2(x):
    r=1
    while (x>0):
        r=(r+r)
        x=(x-1)
    return r

# Zweierlogarithmus
def binLength(x):
    pow=1
    c=0
    while(pow<=x):
        pow=(pow+pow)
        c=(c+1) 
    return c


def odd(x):
    i=binLength(x)
    while(x>0):
        p=pow2(i)
        if(p<=x):
            x=(x-p)
        i=(i-1)
    if(i>=0):
        i=0
    if(i==-1):
        i=1
    return i
