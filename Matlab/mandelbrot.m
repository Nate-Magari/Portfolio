function mandelbrot(n, n2, x0, y0, g)

x1 = x0 - g;   x2 = x0 + g;
y1 = y0 - g; y2 = y0 + g;

[x,y] = meshgrid(linspace(x1, x2, n), linspace(y1, y2, n));

c = x + 1i * y;
z = zeros(size(c));
k = zeros(size(c));

for ii = 1:n2
    z   = z.^2 + c;
    k(abs(z) > 2 & k == 0) = n2 - ii;
end

figure,
imagesc(k),
colormap hot
axis square
end