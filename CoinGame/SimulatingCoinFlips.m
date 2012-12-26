% number of simulations m
m = 100000;
n_bins_total = 36;
n_bins_head = 11;
w_head =n_bins_head/n_bins_total;
w_tail = 1-w_head;
p_head_biased = 11/36;
p_tail_biased = 1-p_head_biased;
p_head_fair = 0.5;
p_tail_fair = 1-p_head_fair;
stopping_time = zeros(1,m);
weights = zeros(1,m);
weights_fair = zeros(1,m);

for i = 1:m
    step = 1;
    weight_biased = 1;
    weight_fair = 1;

    while (unidrnd(n_bins_total)> n_bins_head)
          # coin flip resulted in tail -> game continues
          step = step + 1;
          weight_biased= weight_biased*p_tail_biased/w_tail;
          weight_fair = weight_fair*p_tail_fair/w_tail;
     end

     weights_biased(i) = weight_biased*p_head_biased/w_head;
     weights_fair(i) = weight_fair*p_head_fair/w_head;
     stopping_time(i)=step;
end

t_stop_fair = mean(stopping_time.*weights_fair);
t_stop_biased = mean(stopping_time.*weights_biased);
pwin_fair = sum(weights_fair(mod(stopping_time,2)==1)) /length(stopping_time);
pwin_biased = sum(weights_biased(mod(stopping_time,2)==1)) /length(stopping_time);

fprintf( "Stopping time for fair coin:      simulated: %f, calculated %f \n" , t_stop_fair, 2);
fprintf( "Stopping time for biased coin:    simulated: %f, calculated %f \n", t_stop_biased, 36/11);
fprintf( "First player win for fair coin:   simulated: %f, calculated %f \n", pwin_fair, 1/(2-0.5) );
fprintf( "First player win for biased coin: simulated: %f, calculated %f \n", pwin_biased,1/(2-11/36) );

plot( (1:length(stopping_time) )(mod(stopping_time,2)==1)  , cumsum(weights_biased(mod(stopping_time,2)==1))./(   (1:length(stopping_time) )(mod(stopping_time,2)==1)  ) , (1:length(stopping_time) )(mod(stopping_time,2)==1) , 1/(2-11/36)+0*(1:length(stopping_time) )(mod(stopping_time,2)==1) ,
(1:length(stopping_time) )(mod(stopping_time,2)==1)  , cumsum(weights_fair(mod(stopping_time,2)==1))./(   (1:length(stopping_time) )(mod(stopping_time,2)==1)  ) , (1:length(stopping_time) )(mod(stopping_time,2)==1) , 1/(2-18/36)+0*(1:length(stopping_time) )(mod(stopping_time,2)==1) )
title("Simulated probability of winning vs. number of simulations")
xlabel("Number of simulations")
ylabel("Probability of winning")
axis([1 m .55 .7])
