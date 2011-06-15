vs=[False,False,False]
good=True
def pB(b):
    if(b):
        return "1"
    else:
        return "0"
for x in range(2**len(vs)):
    a=vs[0]
    b=vs[1]
    c=vs[2]
    f1=(  ((not a) and c) or (a and b)  )
    #f2=(  (a and (b^c)) ^ c  )
    f2=(  ((not a or not b or c) and (not a or b or not c) and c) or (a and b and not c) )
    print("i:",pB(a),pB(b),pB(c))
    print("o:",pB(f1),pB(f2),pB(f1==f2))
    good=good and (f1==f2)
    for y in range(len(vs)):
        if(x%(2**y)==(2**y)-1):
            vs[y]=not vs[y]
print("good:",pB(good))
