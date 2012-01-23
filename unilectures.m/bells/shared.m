
1;

function vec = note(frequenz,notenlaenge,abtastrate)
  # laenge des vektors ist abtastrate*notenlaenge
  vec = [1:(abtastrate*notenlaenge)];
  # fuktionswerte
  vec=sin(vec.*(2*pi*frequenz/abtastrate));
  # stille (hinten mit nullen ueberschreiben)
  vec((abtastrate*(notenlaenge-0.08)):(abtastrate*notenlaenge))=0;
end

function bells = bells(bellpermut,abtastrate)

  # Frequenzen:
  f=[ 440.00 493.88 523.25 587.33 659.26 698.46 783.99 880.00 ];
  
  
  flist = f(bellpermut);
  
  bells = [];
  
  for k = 1:length(flist)
    bells = [bells note(flist(k),0.2,abtastrate)];
  end
  
end
