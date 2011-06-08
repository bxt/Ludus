def c(z):
    [x,y,r,p]=[0,0,0,0]
    for x in range(0,z):
        for y in range(0,z):
            p=(prim(x)+prim(y))
            if(p==z):
                r=1
    return r