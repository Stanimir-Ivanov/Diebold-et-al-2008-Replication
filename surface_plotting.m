t1 = datetime(1995,4,1,0,0,0);
t2 = datetime(2017,12,31,0,0,0);
times = t1:calmonths(1):t2;
maturities = [6:3:24, 30, 36:12:120];

us = csvread('./Export/us.csv', 1, 1);
ca = csvread('./Export/ca.csv', 1, 1);
jp = csvread('./Export/jp.csv', 1, 1);
de = csvread('./Export/de.csv', 1, 1);
uk = csvread('./Export/uk.csv', 1, 1);

[dT, dM]  = meshgrid(datenum(times), maturities);

%plot settings
position = [154, 10];
zlimit = [-.5, 10];
ylimit = [0, 120];
fsize = 20;


% US
surf(dT', dM', us)
dateaxis('x', 10);
xlabel('Time');
zlim(zlimit);
ylabel('Maturity (months)')
ylim(ylimit);
zlabel('Yield (percent)')
title('US yield curve')
view(position)
set(findall(gcf,'-property','FontSize'),'FontSize',fsize)
print('./Plots/us_curve','-depsc')


% CA
surf(dT', dM', ca)
dateaxis('x', 10);
xlabel('Time');
zlim(zlimit);
ylabel('Maturity (months)')
ylim(ylimit);
zlabel('Yield (percent)')
title('Canada yield curve')
view(position)
set(findall(gcf,'-property','FontSize'),'FontSize',fsize)
print('./Plots/ca_curve','-depsc')

% JP
surf(dT', dM', jp)
dateaxis('x', 10);
xlabel('Time');
zlim(zlimit);
ylabel('Maturity (months)')
ylim(ylimit);
zlabel('Yield (percent)')
title('Japan yield curve')
view(position)
set(findall(gcf,'-property','FontSize'),'FontSize',fsize)
print('./Plots/jp_curve','-depsc')

% DE
surf(dT', dM', de)
dateaxis('x', 10);
xlabel('Time');
zlim(zlimit);
ylabel('Maturity (months)')
ylim(ylimit);
zlabel('Yield (percent)')
title('Germany yield curve')
view(position)
set(findall(gcf,'-property','FontSize'),'FontSize',fsize)
print('./Plots/de_curve','-depsc')

% UK
surf(dT', dM', uk)
dateaxis('x', 10);
xlabel('Time');
zlim(zlimit);
ylabel('Maturity (months)')
ylim(ylimit);
zlabel('Yield (percent)')
title('UK yield curve')
view(position)
set(findall(gcf,'-property','FontSize'),'FontSize',fsize)
print('./Plots/uk_curve','-depsc')