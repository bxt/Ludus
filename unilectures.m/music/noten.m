

function vec = note(frequenz,notenlaenge,abtastrate)
  # laenge des vektors ist abtastrate*notenlaenge
  vec = [1:(abtastrate*notenlaenge)];
  # fuktionswerte
  vec=sin(vec.*(2*pi*frequenz/abtastrate));
  # stille (hinten mit nullen ueberschreiben)
  vec((abtastrate*(notenlaenge-0.05)):(abtastrate*notenlaenge))=0;
end

# # zum testen kann ein 0.08 sekunden lange quinte geplottet werden:
# a=note(440,0.08,44000)+note(659.26,0.08,44000);
# x=[1:length(a)];
# 
# plot(x,a)
# print -dpng testnote.png 

