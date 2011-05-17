def f6(x):
    y=0
    while (x>0):
        x = (((x-y)-y)-1)
        y = (y+1)
    return y

if __name__ == '__main__':
    for x in range(20):
        print(str(x),str(f6(x)))