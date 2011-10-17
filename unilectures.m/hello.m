% this file creates a small image in c:\pic.ppm
% PS: this is actually Octave code...

n=7;
path='c:\pic.ppm';
pad=5;
z=zeros(pad,5);
z2=zeros(4+n+pad*2,pad);
r=mod(1:5,2);
l=~r;
b=~mod(0:4,4);
s=r-b;
p=~[z2 [z;s;l(ones(n)(1,:),:);b;r;l;z] z2];
saveimage(path,p*65,'ppm');
