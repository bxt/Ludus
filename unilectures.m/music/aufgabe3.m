

function aufgabe3()
  
  [audiodaten,abtastrate,bits] = wavread("silentnight-mit-glocken.wav");
  
  y=fft(audiodaten);
  l=length(y)
  x=[1:l];
  
  #plot(x,y)
  #print -dpng spec.png
  l1=length(y)
  l2=length(audiodaten)
  
end

aufgabe3();