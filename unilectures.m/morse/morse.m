

function code = morseOne(letter)
  dah1=[1 1 1];
  dit1=[1];
  dah0=[0 0 0];
  dit0=[0];
  if letter=="a"
    code = [dit1 dit0 dah1];
  elseif letter=="b"
    code = [dah1 dit0 dit1 dit0 dit1 dit0 dit1];
  elseif letter=="c"
    code = [dah1 dit0 dit1 dit0 dah1 dit0 dit1 ];
  elseif letter=="d"
    code = [dah1 dit0 dit1 dit0 dit1 ];
  elseif letter=="e"
    code = [dit1];
  elseif letter=="f"
    code = [dit1 dit0 dit1 dit0 dah1 dit0 dit1 ];
  elseif letter=="g"
    code = [dah1 dit0 dah1 dit0 dit1 ];
  elseif letter=="h"
    code = [dit1 dit0 dit1 dit0 dit1 dit0 dit1];
  elseif letter=="i"
    code = [dit1 dit0 dit1];
  elseif letter=="j"
    code = [dit1 dit0 dah1 dit0 dah1 dit0 dah1 ];
  elseif letter=="k"
    code = [dah1 dit0 dit1 dit0 dah1 ];
  elseif letter=="l"
    code = [dit1 dit0 dah1 dit0 dit1 dit0 dit1 ];
  elseif letter=="m"
    code = [dah1 dit0 dah1 ];
  elseif letter=="n"
    code = [dah1 dit0 dit1 ];
  elseif letter=="o"
    code = [dah1 dit0 dah1 dit0 dah1 ];
  elseif letter=="p"
    code = [dit1 dit0 dah1 dit0 dah1 dit0 dit1 ];
  elseif letter=="q"
    code = [dah1 dit0 dah1 dit0 dit1 dit0 dah1 ];
  elseif letter=="r"
    code = [dit1 dit0 dah1 dit0 dit1 ];
  elseif letter=="s"
    code = [dit1 dit0 dit1 dit0 dit1];
  elseif letter=="t"
    code = [dah1 ];
  elseif letter=="u"
    code = [dit1 dit0 dit1 dit0 dah1 ];
  elseif letter=="v"
    code = [dit1 dit0 dit1 dit0 dit1 dit0 dah1 ];
  elseif letter=="w"
    code = [dit1 dit0 dah1 dit0 dah1 ];
  elseif letter=="x"
    code = [dah1 dit0 dit1 dit0 dit1 dit0 dah1 ];
  elseif letter=="y"
    code = [dah1 dit0 dit1 dit0 dah1 dit0 dah1 ];
  elseif letter=="z"
    code = [dah1 dit0 dah1 dit0 dit1 dit0 dit1 ];
  else
    code = [dit0]; # space
  end
  code = [code dah0]
end



function vec = morse(letters)
  vec=[];
  for L=letters
    vec=[vec morseOne(L)];
  end
end

function morseTon(dits)
  ditlenMs = 0.120# 120 ms  =  10 WPM
  bitrate = 24000 # bps
  frequency = 660 # Hz
  frequencyBad = 880 # Hz
  
  ditlen = bitrate * ditlenMs # len of 1 dit in bits
  bitlen = ditlen * length(dits) # len of final bitvec
  
  sine = sin([1:bitlen].*(2*pi*frequency/bitrate))*0.03;
  sine2 = sin([1:bitlen].*(2*pi*frequencyBad/bitrate))*0.97;
  factors = reshape(repmat(dits,ditlen,1), 1, []); # duplicate ditlen times
  
  data = (sine+sine2).*factors ;
  
  dist=500
  data2= conv(data,ones(1,dist)/dist)(1:length(data)).*5;
  
  wavwrite(data2',bitrate,"morse.wav");
  
end


string="ka oh ja ein ganz toller funkspruch kommt hier rein der dich ueber nichts informiert also cya ar"
#string="morse code"

morseTon(morse(string))



