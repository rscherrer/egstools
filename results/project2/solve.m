global a b m h s d x xm

a = 100;
b = 30;
m = 0.01;
h = 1;
s = 1.2;
d = 0.2;

tol = 0.0000001;

for xval = -1:0.1:1
  
  wlow = round(W(xval, xval - 0.0001) - W(xval, xval) / tol) * tol;
  whigh = round(W(xval, xval + 0.0001) - W(xval, xval) / tol) * tol;
  [xval, wlow, whigh]
  
endfor
