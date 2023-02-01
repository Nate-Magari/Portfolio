function [x,y]= trajectory(v)
v0 = v;
g=9.81;
hold on
    for angle = 0:5:95
    tges=(2*v0.*sin(angle.*(pi/180)))/g;
    t =linspace(0,tges);
    vx = v0.*cos(angle.*(pi/180));
    vy = v0.*sin(angle.*(pi/180));
    x = vx.*t;
    y=vy.*t-0.5*g.*t.^2;
    plot(x,y);
    end
hold off
end

