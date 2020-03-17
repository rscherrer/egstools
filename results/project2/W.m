# Fitness function
function lambda = W(x_, xm_)
global x xm;
x = x_;
xm = xm_;
[Y, ~, ~, ~, J] = fsolve(@F, [1000, 1000, 0, 0]);
lambda = max(eig(J(3:4, 3:4)));

# Population dynamics
function dN = F(N)
global a b m h s d x xm
dN(1) = ((1 - m) * N(1) + m * N(2)) * (w1(x, x, N(1), N(2), a, b, m, h, s) - d) - N(1);
dN(2) = ((1 - m) * N(2) + m * N(1)) * (w2(x, x, N(1), N(2), a, b, m, h, s) - d) - N(2);
dN(3) = ((1 - m) * N(3) + m * N(4)) * (w1(xm, x, N(1), N(2), a, b, m, h, s) - d) - N(3);
dN(4) = ((1 - m) * N(4) + m * N(3)) * (w2(xm, x, N(1), N(2), a, b, m, h, s) - d) - N(4);

# Fitness on each resource
function f = w1(xm, x, n1, n2, a, b, m, h, s)
f = e1(xm, s) * r11(x, n1, n2, a, b, m, s) + e2(xm, s) * r21(x, n1, n2, a, b, m, h, s);

function f = w2(xm, x, n1, n2, a, b, m, h, s)
f = e1(xm, s) * r12(x, n1, n2, a, b, m, h, s) + e2(xm, s) * r22(x, n1, n2, a, b, m, s);

# Equilibrium resource concentrations
function r = r11(x, n1, n2, a, b, m, s)
r = a / (b + e1(x, s) * (n1 * (1 - m) + m * n2));

function r = r21(x, n1, n2, a, b, m, h, s)
r = h * a / (b + e2(x, s) * (n1 * (1 - m) + m * n2));

function r = r12(x, n1, n2, a, b, m, h, s)
r = h * a / (b + e1(x, s) * (n2 * (1 - m) + m * n1));

function r = r22(x, n1, n2, a, b, m, s)
r = a / (b + e2(x, s) * (n2 * (1 - m) + m * n1));

# Attack rates
function e = e1(x, s)
e = exp(-s * (x + 1)^2);

function e = e2(x, s)
e = exp(-s * (x - 1)^2);