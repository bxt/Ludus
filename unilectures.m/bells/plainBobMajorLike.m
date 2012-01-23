
abtastrate=44000;

source("shared.m");

function extend = plainBobExtend(n)
  if n==1
    extend=[1];
  else
    
    preExtend = plainBobExtend(n-1);
    extend=[];
    forward=0;
    
    for i = [1:size(preExtend)(1)]
      for k = [1:n]
        if forward
          extend=[extend ; preExtend(i,1:k-1) n preExtend(i,k:n-1) ];
        else
          extend=[extend ; preExtend(i,1:n-k) n preExtend(i,n-k+1:n-1) ];
        end
      end
      forward=!forward;
    end
    
  end
end

n=8
belltones = reshape ([ plainBobExtend(n) ; [1:n] ]',[],1);

save plainPobMajorLike.mat belltones

wavwrite(bells(belltones,abtastrate)',abtastrate,"plainBobMajorLike.wav")
# v = bells([1 2 3 4 1 2 3 4 2 1 4 3 1 2 3 4],abtastrate)
