#!/usr/bin/env python
# -*- coding: UTF-8 -*-

"""
Implements 𝔽₂₅₆

Uses the irreducible univariate polynomial
x⁸+x⁴+x³+x+1 over the finite field 𝔽₂. 

@date 2013-10-20
@author: Bernhard Häussner
"""

MODUL = 283

def F256Add(x,y):
  """
  Returns the sum of two elements in 𝔽₂₅₆
  
  >>> F256Add(123,45)
  86
  """
  return x^y

def F256Mul(x,y):
  """
  Returns the product of two elements in 𝔽₂₅₆
  
  >>> F256Mul(123,45)
  128
  """
  z = 0
  for i in range(8):
      z ^= (1 & y>>i) * (x<<i)
  for i in reversed(range(8,16)):
      z ^= (1 & z>>i) * (MODUL << i-8)
  return z

if __name__ == '__main__':
  import doctest
  doctest.testmod()