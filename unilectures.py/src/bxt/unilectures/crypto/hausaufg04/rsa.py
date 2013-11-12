import random

def IsPrime(n, s = 50): # Miller-Rabin-Primzahltest
  # Aufruf: IsPrime(n,s) mit natuerlichen Zahlen n,s
  # Ausgabe: True oder False
  #     n prim => Ausgabe True mit Wkt. 1
  #     n nicht prim => Ausgabe True mit Wkt. <= 1/(2**s)
  # Laufzeit bei Eingabe (n,s): O(s * |n|**2)
  """
  A simple and fast prime test
  
  >>> IsPrime(1)
  False
  >>> IsPrime(5)
  True
  >>> IsPrime(1327)
  True
  >>> IsPrime(1337)
  False
  >>> IsPrime(1361)
  True
  """
  if n < 2: return False
  for j in range(1, s + 1):
    a = random.randint(1, n - 1)
    i = n - 1
    b = []
    while (i > 0):
      b.append(i % 2)
      i = i // 2
    d = 1
    for i in range(len(b) - 1, -1, -1):
      x = d
      d = (d * d) % n
      if d == 1 and x != 1 and x != n - 1:
        return False
      if b[i] == 1:
        d = (d * a) % n
    if d != 1:
      return False
  return True

def egcd(ld,d): # erweiterter Euklidischer Algorithmus
  # Aufruf: egcd(a,b) mit natuerlichen Zahlen a,b>0
  # Ausgabe: (d,x,y) mit:
  #     d ist groesster gemeinsamer Teiler von a und b
  #     x,y sind ganze Zahlen mit d = x*a + y*b
  """
  Extended Euclidean algorithm
  
  (d,x,y) = egcd(a,b)
  d = gcd(a,b) = x*a + y*b
  
  >>> egcd(99, 78)
  (3, -11, 14)
  >>> egcd(10, 5)
  (5, 0, 1)
  >>> egcd(1327,1337)[0] == 1327*egcd(1327,1337)[1] + 1337*egcd(1327,1337)[2]
  True
  """
  (lx,x) = (1,0)
  (ly,y) = (0,1)
  while d != 0:
    q = ld//d
    (d,ld) = (ld%d,d)
    (x,lx) = (lx-q*x,x)
    (y,ly) = (ly-q*y,y)       
  return (ld,lx,ly)

def ModInv(e,n): # Inverses mod n
  # Aufruf: ModInv(e,n) mit natuerlichen Zahlen e,n>0 und ggT(e,n)=1
  # Ausgabe: d aus {1,...,n-1} mit (d*e)%n = 1
  """
  Invert a number in mod n 
  
  >>> ModInv(4,7)
  2
  >>> ModInv(456, 1327)
  259
  >>> all([1 == (i * ModInv(i, p)) % p for i in range(1,100) for p in [3547, 6121, 7451]])
  True
  """
  return egcd(e,n)[1]

def ModExp(x,y,n): # Exponentialfunktion mod n
  # Aufruf: ModExp(x,y,n) mit natuerlichen Zahlen x,y,n und n>=2
  # Ausgabe: z = (x**y)%n
  """
  Pow in mod n 
  
  >>> [ModExp(3, i, 7) for i in range(7)] # cyclic!
  [1, 3, 2, 6, 4, 5, 1]
  >>> ModExp(37, 87, 127)
  19
  >>> ModExp(22, 11, 551)
  238
  """
  z = 1
  pot = x
  while y > 0:
    y,r = divmod(y,2)
    if r > 0:
      z = (z * pot) % n
    pot = (pot * pot) % n
  return z

def RSAKeyGen(r=1024):
  # Aufruf: RSAKeyGen(r) mit natuerlicher Zahl r>=5
  # Ausgabe: ((n,e),(n,d)) mit
  #     n = p*q fuer zufaellige Primzahlen p,q mit p!=q und
  #         p,q aus [2**l,2**(l+1)), wobei l = (r+2)//2
  #     e = zufaellige Zahl mit 1<e<phi(n) und ggT(e,phi(n))=1
  #     d = Inverses von e modulo phi(n),
  #         d.h. 1<d<phi(n) mit der Eigenschaft (e*d) % phi(n) = 1
  #
  # Hinweis: Der Befehl random.randint(a,b) liefert eine
  #          gleichverteilt zufaellige Zahl aus {a,a+1,...,b}.
  l = (r+2)//2
  a = 2**l
  b = 2**(l+1) - 1
  
  p = 1
  while not IsPrime(p):
    p = random.randint(a,b)
  
  q = 1
  while q==p or not IsPrime(q):
    q = random.randint(a,b)
  n = p * q
  phi = (p-1) * (q-1)
  
  while True:
    e = random.randint(1,phi-1)
    (ggT,x,y) = egcd(e,phi)
    if ggT == 1:
      break
  d = x
    
  return ((n,e),(n,d))

def RSAEncrypt(pk,m):
  # Aufruf: RSAEncrypt(pk,m) mit public key pk und Klartext m
  # Ausgabe: Chiffretext c
  (n,e)=pk
  return ModExp(m,e,n)

def RSADecrypt(sk,c):
  # Aufruf: RSADecrypt(sk,c) mit secure key sk und Chiffretext c
  # Ausgabe: dechiffrierte Nachricht m
  (n,d)=sk
  return ModExp(c,d,n)

def str2int(s): # codiert einen String als Zahl (zum Testen von RSA)
  # Aufruf: str2int('Das ist ein Test.')
  # Ausgabe: 23268733837745479405720608239248647353390
  """
  Integer value of the ASCII encoding of a string
  
  >>> str2int('Das ist ein Test.')
  23268733837745479405720608239248647353390L
  """
  x = 0
  for i in range(0,len(s)):
    x = (x<<8) + ord(s[i])
  return x

def int2str(x): # codiert eine Zahl als String (zum Testen von RSA)
  # Aufruf: int2str(23268733837745479405720608239248647353390)
  # Ausgabe: 'Das ist ein Test.'
  """
  Integer value of the ASCII encoding of a string
  
  >>> int2str(23268733837745479405720608239248647353390)
  'Das ist ein Test.'
  """
  s = ''
  while(x>0):
    s = chr(x&255) + s
    x = x >> 8
  return s

def RSATest():
  ms = 'NSA aus USA hasst RSA.'
  m=str2int(ms)
  print("Klartext als String:      "+ms)
  print("Klartext als Zahl:        "+str(m))
  r = len(bin(m))-2                 # Laenge des Klartexts bestimmen
  (pk,sk)=RSAKeyGen(r)
  (n,e)=pk
  (n,d)=sk
  print("n =                       "+str(n))
  print("e =                       "+str(e))
  print("d =                       "+str(d))
  c=RSAEncrypt(pk,m)
  cs=int2str(c)
  print("Chiffretext als Zahl:     "+str(c))
  b=RSADecrypt(sk,c)
  bs=int2str(b) 
  print("entschl. Text als String: "+bs)
  print("entschl. Text als Zahl:   "+str(b))
  return

if __name__ == '__main__':
  import doctest
  doctest.testmod()
  
  RSATest()

