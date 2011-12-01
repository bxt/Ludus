% Aufgabe  6.1.4

% Sortieren absteigend und Merken der Umsortierung
function [list,permutation] = sort2 (list)
  permutation=[1:length(list)]
  for i=1:length(list)
    max=list(i)
    for k=i:length(list)
      if max <= list(k)
        max=list(k)
        list(k)=list(i)
        list(i)=max
        tmp=permutation(k)
        permutation(k)=permutation(i)
        permutation(i)=tmp
      end
    end
  end
end

% wird rekursiv fuer jede binaerstelle aufgerufen
function codierung = codeShannonFanoInnen (vorcodierung, whrs, offs)
  codierung = vorcodierung;
  if length(whrs)>1 % bei nur einem element muss nichts getan werden
    
    % Herausfinden, wo geteilt werden muss:
    summe=0
    gesamtsumme= sum(whrs)
    prediff=5 % Vorherige Differenz zur optimalteilung
    i=0
    while prediff > abs(summe/gesamtsumme-0.5)
      if i>=length(whrs)-1
        break; % Teilung am Ende
      end
      prediff = abs(summe/gesamtsumme-0.5)
      summe=summe+whrs(i+1)
      i=i+1
    end
    if i>1 % Korrektur des Index
      i=i-1
    end

    % Binaerstellen in die Codierungstabelle schreiben:
    for j=1:length(whrs)
      if j<=i % vor der Trennung Nullen
        codierung{j+offs}=[ codierung{j+offs} '0' ];
      else % Nach der Trennung Einsen
        codierung{j+offs}=[ codierung{j+offs} '1' ];
      end
    end
    
    % Rekursiver Aufruf fuer die beiden Teile
    % Offs speichert die Position in der Codetabelle
    codierung = codeShannonFanoInnen(codierung,whrs(1:i),offs);
    codierung = codeShannonFanoInnen(codierung,whrs(i+1:length(whrs)),offs+i);
  end
end

% Rahmenfunktion zum Aufruf der rekursiven Funktion
function codierung = codeShannonFano (whrs)
  c(1:length(whrs))={''} # Leere Kodierungstabelle
  [whrsSorted,perm]=sort2(whrs) # Absteigend Sortieren
  codierung=codeShannonFanoInnen(c,whrsSorted, 0) # Aufruf
  for i=1:length(whrs) # Kodierungstabelle ausgeben
    disp(['x' num2str(perm(i)) ': ' codierung{i}])
  end
end

% Kodierungstabelle aus Aufgabe 6.1.2.a)
codierung = codeShannonFano ([1/8 1/20 1/15 1/12 1/60 1/30 3/40 1/6 2/15 1/4]);
