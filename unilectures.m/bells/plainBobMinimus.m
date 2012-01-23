
abtastrate=44000;

source("shared.m");

function belltones = plainBobMinimus()
  belltones=[];
  start=[1 3 5 7];

  perm_a=[2 1 4 3];
  perm_b=[1 3 2 4];
  perm_c=[1 2 4 3];

  i=start;
  for k = 1:12
    belltones=[belltones i];
    i=i(perm_a);
    belltones=[belltones i];
    if mod(k,4)==0
      i=i(perm_c);
    else
      i=i(perm_b);
    end
  end
  belltones=[belltones i]
end

wavwrite(bells(plainBobMinimus(),abtastrate)',abtastrate,"plainBobMinimus.wav")
# v = bells([1 2 3 4 1 2 3 4 2 1 4 3 1 2 3 4],abtastrate)
