plot(
2*(1:13)-1,36/61+0*(2*(1:13)-1),
2*(1:13)-1,11/36*cumsum( (25/36).^(2*(1:13)-2)),
2*(1:13)-1,2/3+0*(2*(1:13)-1),
2*(1:13)-1,0.5*cumsum( (0.5).^(2*(1:13)-2))
)
title("Theoretical and approximate expectation of winning vs. number of steps included in estimation.")
xlabel("Number of  steps in summation")
ylabel("Probability of winning")
axis([1 25 0.3 0.7])
