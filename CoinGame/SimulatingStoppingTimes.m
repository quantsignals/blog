% simulating path length
% number of simulations m
m = 100000;
n_bins_total = 25;
w = 1/n_bins_total;
p_head_fair = 1/2;
p_tail_fair = 1-p_head_fair;
p_head_biased = 11/36;
p_tail_biased = 1-p_head_biased;
stopping_time = zeros(1,m);
weights_biased = zeros(1,m);
weights_fair = zeros(1,m);

for i = 1:m
  number_of_tails = unidrnd(n_bins_total) - 1;
  weights_biased(i) = 1/w*p_head_biased*p_tail_biased^number_of_tails;
  weights_fair(i) = 1/w*p_head_fair*(p_tail_fair)^(number_of_tails);
  stopping_time(i)=number_of_tails+1;
end

t_stop_fair = mean(stopping_time.*weights_fair);
t_stop_biased = mean(stopping_time.*weights_biased);
pwin_fair = sum(weights_fair(mod(stopping_time,2)==1)) /length(stopping_time);
pwin_biased = sum(weights_biased(mod(stopping_time,2)==1)) /length(stopping_time);

fprintf( "Stopping time for fair coin flip: simulated: %f, calculated %f   \n" , t_stop_fair, 2);
fprintf( "Stopping time for biased coin:    simulated: %f, calculated %f   \n", t_stop_biased, 36/11);
fprintf( "First player win for fair coin:   simulated: %f, calculated %f   \n", pwin_fair,   1/(2-18/36));
fprintf( "First player win for biased coin: simulated: %f, calculated %f   \n", pwin_biased,   1/(2-11/36));

plot( (1:length(stopping_time) )(mod(stopping_time,2)==1)  , cumsum(weights_biased(mod(stopping_time,2)==1))./(   (1:length(stopping_time) )(mod(stopping_time,2)==1)  ) , (1:length(stopping_time) )(mod(stopping_time,2)==1) , 1/(2-11/36)+0*(1:length(stopping_time) )(mod(stopping_time,2)==1) ,
(1:length(stopping_time) )(mod(stopping_time,2)==1)  , cumsum(weights_fair(mod(stopping_time,2)==1))./(   (1:length(stopping_time) )(mod(stopping_time,2)==1)  ) , (1:length(stopping_time) )(mod(stopping_time,2)==1) , 1/(2-18/36)+0*(1:length(stopping_time) )(mod(stopping_time,2)==1) )
title("Simulated probability of winning vs. number of simulations")
xlabel("Number of simulations")
ylabel("Probability of winning")
axis([1 m .55 .7])
