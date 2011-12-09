% Some methods for Z2 polynomials and CRC codes

function sr2 = shiftSR (sr,mu)
    % Es wurde keine for-Schelife priogrammiert, sondern
    % sofort die folgende Lösung:
    sr2=( xor([sr 0],mu*sr(1,1)) ) (:,2:length(sr)+1);
    % Die Register werden um 1 veschoben und je nach
    % erstem Register mit mu ge-xor-t. Dann wird die
    % erste Stelle abgeschnitten. Das Programm 
    % benötigt 31ms. 
end


function doSomeShifts
  tic % Zeit ab hier zählen
  polynom=[1,0,1,1]; % M(u)
  initialzustand=[0,0,1];
  register=initialzustand;
  
  disp "Registerverlauf:";
  for i=0:16 % Die ersten 17 Takte
    % Ausgabe des Registerinhalts
    disp(strcat("Takt",sprintf(" %02d",i),", Register: ",num2str(register)))
    % Ein Takt auf den Registern ausführen:
    register=shiftSR(register,polynom);
  end
  toc
end

doSomeShifts


function randoms
  polynom=[1 1 zeros(1,29) 1]; % u^31+u^30+1
  initialzustand=[zeros(1,30) 1];
  register=initialzustand;
  
  disp "Zufallszahlen:";
  % 5000x verschieben
  for i=1:5000
    register=shiftSR(register,polynom);
  end
  % Die zahn Zufallszahlen ausgeben:
  for k=1:10
  achtBitZufallszahl=zeros(1,8); # hier bauen
  % Jeweils die letzten Stellen hinein schreiben:
  for i=1:8
    achtBitZufallszahl(1,i)=register(1,length(register));
    % und weiter verschieben:
    register=shiftSR(register,polynom);
  end
  % Zufallszahl ausgeben:
  achtBitZufallszahl
  end
end


randoms



function polytext = polyprint(poly1)
  polytext=""; % Mit leerem String starten
  for i=1:length(poly1) % Koeffizienten durchgehen
    if(poly1(1,i)) % Wenn Koeffizient nicht null
    
      if(poly1(1,i)==1) % Wenn K. = 1
        % Ausgabe ohne Koeffizient
        if(i==length(poly1)) % Bei u^0 nur K.
          polytext=[polytext,sprintf("%d",poly1(1,i))];
        elseif(i==length(poly1)-1) % Bei u^1 nur u
          polytext=[polytext,sprintf("u")];
        else # Sonst u^(r-i)
          polytext=[polytext,sprintf("u^%d",length(poly1)-i)];
        end
      else # Sonst mit Koeffizient ausgeben
        if(i==length(poly1))
          polytext=[polytext,sprintf("%d",poly1(1,i))];
        elseif(i==length(poly1)-1)
          polytext=[polytext,sprintf("%du",poly1(1,i))];
        else 
          polytext=[polytext,sprintf("%du^%d",poly1(1,i),length(poly1)-i)];
        end
      end
      
      % Wenn noch weitere Koeffizienten!=0 sind, plus anghängen
      if(any(poly1(1,i+1:length(poly1))))
        polytext=[polytext,"+"];
       end
    end
  end
end

polyprint([12,1,14,0,0,1,1,1])

function [poly2, deg] = polyshorten(poly1)
  poly2=poly1;
  while poly2(1,1)==0 % solange führende Null
    poly2=poly2(:,2:length(poly2)); # null abschneiden
  end
  deg=length(poly2)-1; # Grad ist nun Länge-1
end

[p,d] = polyshorten([0,0,2,3,1,0])

function produkt = polymult(poly1,poly2)
  produkt=zeros(1,length(poly1)+length(poly2)-1);
  for i=1:length(poly2)
    produkt = produkt.+ ([zeros(1,i-1) poly1 zeros(1,length(poly2)-i)]*poly2(1,i));
  end
end



a=mod(polymult([1,0,1],[1,1]),2)

a=polymult([3,2],[2,1,3])

function [quo,rest] = polydiv(poly1,poly2) % poly1/poly2
  rest=poly1;
  quo=[];
  for i=1:length(poly1)-length(poly2)+1
    faktor=(rest(1,i)/poly2(1,1));
    rest=rest- [zeros(1,i-1) poly2 zeros(1,length(poly1)-length(poly2)-i+1)]*faktor;
    quo=[quo faktor];
  end
  [rest,_]=polyshorten(rest);
end


[x,r]=polydiv([1 1 0 1],[1 0 1]);
quo=mod(x,2)
rest=mod(r,2)

