#!/usr/bin/env python
# -*- coding: UTF-8 -*-
"""
Answest a twitter interview question.
See: http://qandwhat.apps.runkite.com/i-failed-a-twitter-interview/

@date 2013-10-31
@author: Bernhard Häussner
"""

def puddle_sizes(wall_sizes):
  """
  Calculates the water between the walls.
  
  Consider this picture, where ~ is water and # are walls:
  
        77
        ##6
   5~~~~###
   #~~~4###
   #~~3####
  2#~2#####
  ##1######
  ---------
  012345678 (indices)
  
  We have a list of wall heights and want to know how much water would
  accumulate at any index in case of rain. The water on the sides spills out.
  For the above picture we would get:
  
  >>> puddle_sizes([2, 5, 1, 2, 3, 4, 7, 7, 6])
  [0, 0, 4, 3, 2, 1, 0, 0, 0]
  
  Let's consider another picture:
  
         77
         ##6
   5~~~~~###
   #~~~~~###
   #~3~~~###
  2#~#~2~###
  ##1#1#1###
  ----------
  0123456789 (indices)
  
  This is the accompanying calculation:
  
  >>> puddle_sizes([2, 5, 1, 3, 1, 2, 1, 7, 7, 6])
  [0, 0, 4, 2, 4, 3, 4, 0, 0, 0]
  
  There can be more than one puddle though. Consider this picture:
  
         7~7
         #~#6
   5~~~~~#~##
   #~~~~~#4##
   #~3~~~####
  2#~#~2~####
  ##1#1#1####
  -----------
  0123456789A (hex indices)
  
  This is the accompanying calculation:
  
  >>> puddle_sizes([2, 5, 1, 3, 1, 2, 1, 7, 4, 7, 6])
  [0, 0, 4, 2, 4, 3, 4, 0, 3, 0, 0]
  
  There can be no puddle at all. Consider this picture:
  
   3
  2#
  ##1
  ---
  012 (indices)
  
  This is the accompanying calculation:
  
  >>> puddle_sizes([2, 3, 1])
  [0, 0, 0]
  
  Also consider this picture:
  
   3
  2#~2
  ##1#1
  -----
  01234 (indices)
  
  This is the accompanying calculation:
  
  >>> puddle_sizes([2, 3, 1, 2, 1])
  [0, 0, 1, 0, 0]
  
  """
  pass


if __name__ == '__main__':
  import doctest
  doctest.testmod()
 
 
 
