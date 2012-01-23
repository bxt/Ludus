
# source('noten.m');

function jingleBells = aufgabe2()

  # Frequenzen:
  A = 440.00
  H = 493.88
  C = 523.25
  D = 587.33
  E = 659.26
  F = 698.46
  G = 783.99

  # Abtastrate:
  br = 44000

  chorus =[note(H,1/4,br) note(H,1/4,br) note(H,1/2,br) note(H,1/4,br) note(H,1/4,br) note(H,1/2,br) note(H,1/4,br) note(D,1/4,br) note(G/2,1/4,br) note(A,1/4,br) note(H,1,br)];
  verse = [note(C,1/4,br) note(C,1/4,br) note(C,1/4,br) note(C,1/4,br) note(C,1/4,br) note(H,1/4,br) note(H,1/4,br) note(H,1/8,br) note(H,1/8,br) ];
  brige = [note(H,1/4,br) note(A,1/4,br) note(A,1/4,br) note(H,1/4,br)  note(A,1/2,br) note(D,1/2,br) ];
  outro = [note(D,1/4,br) note(D,1/4,br) note(C,1/4,br) note(A,1/4,br) note(G/2,1/1,br) ] ;
  
  jingleBells = [chorus verse brige chorus verse outro]';
  
  wavwrite(jingleBells,44000,"jinglebells.wav")
end
