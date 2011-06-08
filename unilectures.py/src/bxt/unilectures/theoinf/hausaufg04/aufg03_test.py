from aufg03 import prodZ

# Null
print(prodZ(0,0)==0)
print(prodZ(1,0)==0)
print(prodZ(0,-1)==0)

# Vorzeichen
print(prodZ(1,1)==1)
print(prodZ(1,-1)==-1)
print(prodZ(-1,1)==-1)
print(prodZ(-1,-1)==1)

# Mit Zahlenwert
print(prodZ(56,54)==3024)
print(prodZ(56,-54)==-3024)
print(prodZ(-56,54)==-3024)
print(prodZ(-56,-54)==3024)

# Die Koenigsdisziplin
print(prodZ(9**20,8**20)==(9**20)*(8**20))
