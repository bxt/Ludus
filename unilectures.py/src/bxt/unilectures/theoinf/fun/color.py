'''24 Bit Color Module. 
@date 2011-06-22
@author Burny

Zusammenfassung:

>>> 0xF48610
16025104
>>> c2rgb(0xF48610)
[244, 134, 16]
>>> rgb2c([244, 134, 16])
16025104
>>> c2hexstring(16025104)
'#F48610'
>>> hexstring2c('#F48610')
16025104
>>> c2hexstring(darken(16025104, 0.5))
'#7A4308'
>>> c2hexstring(lighten(16025104, 0.5))
'#FFFF20'

Integrationstests:

>>> c2hexstring(rgb2c(c2rgb(hexstring2c('#0AFFE0'))))
'#0AFFE0'
>>> all([hexstring2c(c2hexstring(i))==i for i in range(0,0xFFFFFF+1,2710)])
True
>>> all([rgb2c(c2rgb(i))==i for i in range(0,0xFFFFFF+1,2690)])
True

'''


def c2rgb(color):
    """Extract RGB value list from color. 
    
    >>> c2rgb(0xFF8800)
    [255, 136, 0]
    >>> c2rgb(10697505)
    [163, 59, 33]

    """
    return [(color & (255<<i*8))>>8*i for i in reversed(range(3))]

def rgb2c(rgb):
    """Extract color code from RGB value list. 
    
    >>> rgb2c([255, 136, 0])
    16746496
    >>> rgb2c([0xFE, 0x93,0x2])
    16683778
    
    """
    return sum([c<<8*(2-i) for i,c in zip(range(3),rgb)])

def c2hexstring(color):
    """Extract Web-Hex-Colorstring from color. 
    
    >>> c2hexstring(0b111111101001001100000010)
    '#FE9302'
    >>> c2hexstring(0)
    '#000000'
    
    """
    return ('#'+'{:02X}'*3).format(*c2rgb(color))

def hexstring2c(string):
    """Extract a color code from a hex string. 
    
    >>> hexstring2c('#FE9302')
    16683778
    >>> hexstring2c('#FF8800')
    16746496
    
    """
    #return sum([int(string[0+i*2:2+i*2],16)<<8*(2-i) for i in range(3)])
    return sum([int(string[1:][i],16)<<4*(5-i) for i in range(6)])

def darken(color, amount):
    """Darken a color to a certain percentage. 
    
    >>> c2hexstring(darken(0xFFFFFF, 0.5))
    '#808080'
    >>> c2hexstring(darken(0x3AF04B, 0.5))
    '#1D7826'
    
    """
    # Round Channel-wise to avoid hue-shift
    return rgb2c([round(c*amount) for c in c2rgb(color)])

def lighten(color, amount):
    """Lighten a color by a certain percentage. 
    
    >>> c2hexstring(lighten(0x808080, 0.8))
    '#FFFFFF'
    >>> c2hexstring(lighten(0x122448, 0.5))
    '#244890'
    >>> c2hexstring(lighten(0x3AF04B, 0.5))
    '#74FF96'
    
    """
    # Round Channel-wise to avoid hue-shift
    return rgb2c([min(round(c/(1-amount)),0xFF) for c in c2rgb(color)])

if __name__ == '__main__':
    import doctest
    doctest.testmod()#verbose=True)

