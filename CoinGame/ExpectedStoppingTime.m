plot(
1:25,36/11+0*(1:25),
1:25,11/36*cumsum((1:25).*(25/36).^((1:25)-1)),
1:25,2+0*(1:25),
1:25,0.5*cumsum((1:25).*0.5.^((1:25)-1))
)
title("Theoretical and approximated stopping times vs. number of steps included in estimation.")
xlabel("Number of  steps in summation")
ylabel("Expected number of flips (stopping time)")
axis([1 25 0 3.5])
