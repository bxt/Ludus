

def karnaugh(name,ones,redundant,varnames):
    varseqence=[0b00,0b01,0b11,0b10] # karnaugh sequence
    #varseqence=[0b00,0b01,0b10,0b11] # veitsch sequence
    pad=sum([len(x) for x in varnames[2:]])+2
    def just(str):
        return ('{:>%d}'%pad).format(str)
    def calc(cur):
        return "*" if cur in redundant else "1" if cur in ones else "0"
    v3pad=" "*(len(varnames[3]))
    print("\n".join(["",name+": ",]+
                    ["".join([just(varnames[l]),"|"]+["{:02b}".format(i)[l] for i in varseqence]) for l in range(2)]+
                    ["".join([just(" ".join([varnames[i+2] for i in range(2)])),"|","    "]),"-"*(pad+1+4),
                     "".join(sum([
                              [just(v3pad.join(["{:04b}".format(i)[x+2] for x in range(2)])),"|"]+
                              [calc(i+k)+"" for k in [x<<2 for x in varseqence]]+["\n"]
                              for i in varseqence
                              ],[]))
                     ]))

def rechenanlagenUE7Karnaughs():
    varnames=["a%d"%i for i in range(4)]
    redundant=[0b1100,0b1101,0b1110,0b1111]
    diagrams=[("l0",[1,2,3]),
              ("l1",[4,5,6]),
              ("l2",[7,8,9]),
              ("l3",[0,0b1010,0b1011]),
              ("h0",[1,4,7,0b1010]),
              ("h1",[2,5,8,0]),
              ("h2",[3,6,9,0b1011])  ]
    for x in diagrams: karnaugh(*list(x)+[redundant,varnames])

if __name__ == '__main__':
    rechenanlagenUE7Karnaughs()





