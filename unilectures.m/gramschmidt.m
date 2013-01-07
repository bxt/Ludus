function [ onb ] = gramschmidt( base, prod )

% Wenn kein Skalarprodukt gegeben, Standard verwenden
if nargin < 2 || isempty(prod)
    prod = @(x,y) x'*y;
end

% Norm ist wie folgt definiert durch das Skalarprodukt
norm = @(v) v .* (1/ sqrt(prod(v,v)) );

% Zunächst 0-Matrix, enthält zuletzt die ONB
onb = base.*0;

% Initialisieren, ersten Vektor normieren
onb(:,1) = norm(base(:,1));

% Rekursion
for i = 2:size(base,2)
    % Projizieren
    projected = zeros(size(base,1),1);
    for k = 1:(i-1)
       projected = projected +  prod(onb(:,k),base(:,i)).*onb(:,k);
    end
    % Senkrechtmachen
    onb(:,i) = base(:,i) - projected;
    % Normieren
    onb(:,i) = norm (onb(:,i));
end


end

