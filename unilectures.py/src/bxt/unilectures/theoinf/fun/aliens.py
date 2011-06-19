'''
Created: 2011-06-19
Aauthor: Bernhard Häussner

Alien Brain Teaser
'''

from random import randint

COLORNAMES={0:'purple',1:'green'}
ANZ=10
DEBUG=True

class Participant:
    def __init__(self):
        self.id=0
        self.name="NN"
    def _debug(self,msg):
        if DEBUG:
            print(" "*9*(self.id+1) + " [%s] %s" % (self.name,msg) )

class Human(Participant):
    def __init__(self,id):
        self.id=id
        self.alive=True
        self.knowledge={}
        self.heard={}
        self.name="Human %d"%id
    def kill(self):
        self._debug("been killed")
        self.alive=False
    def tell(self,position,hatcolor):
        self._debug("knows: Human %d has %s" % (position,COLORNAMES[hatcolor]))
        if(position!=ANZ-1):
            self.knowledge[position]=hatcolor
    def hear(self,position,hatcolor,killed):
        self._debug( "heears: Human %d said \"%s!\" and has%s been killed " %
                     (position,COLORNAMES[hatcolor],"" if killed else " not") )
        self.heard[position]=hatcolor
        self.tell(position,(hatcolor+killed)%2)
    def ask(self):
        result=0
        if(self.id!=ANZ-1):
            result=0 if len([1 for v in self.knowledge.values() if v]
                            ) %2 == self.heard[ANZ-1] else 1
        else:
            result=len([1 for v in self.knowledge.values() if v]) %2
        self._debug("figures to have %s" % COLORNAMES[result])
        return result

class Alien(Participant):
    def __init__(self):
        self.id=-1
        self.name="Alien"
        self.mood=0
    def hear(self,position,hatcolor,killed):
        self._debug( "heears: Human %d said \"%s!\" and has%s been killed " %
                     (position,COLORNAMES[hatcolor],"" if killed else " not") )
        if(killed):
            self.cheer()
        else:
            self.miffed()
    def cheer(self):
        self.mood+=1
        if(self.mood>0):
            self._debug("cheering!")
        else:
            self._debug("feels to itchy to be happy")
    def miffed(self):
        self.mood+=-1
        if(self.mood<-3):
            self._debug("is pretty "+"effing "*(-self.mood-3)+"miffed")
        elif(self.mood<0):
            self._debug("is miffed. ")
        else:
            self._debug("rages a little. ")
    def invade(self):
        self._debug("beams up 10 human representatives")
        humanz=[Human(i) for i in range(ANZ)]
        self._debug("distributes 10 silly looking hats curely among them")
        hats=[randint(0,1) for i in range(ANZ)]
        self._debug("arranges humans in a row and lets them see the hats")
        for i in range(ANZ):
            for k in range(i):
                humanz[i].tell(k,hats[k])
        self._debug("Asks humans one by one bloodthirstily")
        for i in reversed(range(ANZ)):
            said=humanz[i].ask()
            if said!=hats[i]:
                humanz[i].kill()
            for x in [self]+[humanz[k] for k in range(i)]:
                x.hear(i,said,said!=hats[i])

if __name__ == '__main__':
    Alien().invade()
