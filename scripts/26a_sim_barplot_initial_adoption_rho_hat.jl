# Expect exactly two args: output dirs for figures and numbers
if length(ARGS) < 2
    error("Usage: julia your_script.jl <figures_dir> <numbers_dir>")
end
fig_dir = ARGS[1]
num_dir = ARGS[2]

using Memoize, ThreadSafeDicts, DelimitedFiles

# Cached function that iteratively calcuates the expected value of delays.
# Assume uniform distribution c over [mc-hc,mc+hc].
@memoize ThreadSafeDict function ev_iteration(
    beta_hat, rho_hat, y, delta, alpha;
    TT = 8, # The maximum day of deadline.
)
    N = 500 # Size of grids for costs.
    mc = 0.5 # Distribution parameters of c.
    hc = 0.1 # Distribution parameters of c.
    c = LinRange(mc-hc, mc+hc, N) # Generate a linear range.
    wt = ones((N,))/N # Uniform distribution.
    F = x -> min(1, max(0, ((x-1/2)/hc+1)/2)) # Cumulative distributive function.

    T = 184 # Days of no deadline group.
    ev = zeros((T+1,)) # Store the results of E[V_{t}] in a reverse order.
    for t = 1:T
        ct = beta_hat*delta*(alpha*y-rho_hat*ev[t]) # Thresholds.
        vc = @. ifelse(c<ct, delta*alpha*y-c, delta*rho_hat*ev[t]) # Continuation value.
        ev[t+1] = sum(vc.*wt) # Calculate expectation.
    end
  
    return ev[1:TT], ev[end], F # Return a segment of a necessarily length.
end

# Function that calculates the initial adoption rate of the deadline group.
# Assume uniform distribution c over [mc-hc,mc+hc].
function initial_adoption_rate(;
    rho_hat = 0.995,
    beta = 0.8,
    beta_hat = 1,
    delta = 0.99,
    alpha = 1,
    y = 1,
)
    T = 8 # Day of deadline.

    # Iterate to obtain EV's.
    ev, evend, F = ev_iteration(beta_hat, rho_hat, y, delta, alpha; TT=T)

    cad = F(beta*delta*(alpha*y-rho_hat*ev[T])) # Deadline group.
    nad = F(beta*delta*(alpha*y-rho_hat*evend)) # No deadline group.

    return cad, nad # Initial adoption rate for both groups.
end

# Function that tests whether initial adoption rates decrease with rho_hat.
# Note that there is no rho_hat in arguments.
# Return: 1= Prediction holds; 0= no valid cases; -1= Prediction does not hold.
function check_mono_rho_hat(;
    beta = 0.8,
    beta_hat = 1,
    delta = 0.99,
    alpha = 1,
    y = 1,
    mc = 0.5,
    hc = 0.1,
    deadline = true,
)
    pr = -Inf # Previous adoption difference.
    for rho_hat = LinRange(1, 0, 101) # Loop over y in a reverse order.
        cr = initial_adoption_rate(;
            rho_hat=rho_hat, beta_hat=beta_hat, delta=delta,
            alpha=alpha, y=y, beta=beta
        )[deadline ? 1 : 2] # Obtain the current adoption difference.
        #println(rho_hat, '=', cr)
        if cr<pr-1e-14 # If rate decreases compared to the previous case.
            return -1 # Return -1: Prediction does not hold.
        else # If rate increases compared to the previous case.
            pr = cr # Update the previous difference and continue the loop.
        end
    end
    return 1 # Return 1: Prediction holds.
end

M = 100 # Number of grids.
res_d = zeros(M+1) # Store results.

for i = 0:M # i for beta_hat.
    cnt1 = 0 # Count the number of tuples such that Prediction holds.
    cntm1 = 0 # Count the number of tuples such that Predition does not hold.
    beta_hat = i/M 
    println(beta_hat)
    for beta = LinRange(0, beta_hat, M)
        cr = check_mono_rho_hat(
            beta=beta, beta_hat=beta_hat
        ) # Check the current parameters.
        if cr==1 # If Prediction holds.
            cnt1 += 1 # Increment cnt1.
        elseif cr==-1 # If Prediction does not hold.
            cntm1 += 1 # Increment cntm1.
        end
    end
    res_d[i+1] = cnt1/(cnt1+cntm1+1e-10) # Return ratios (1e-10 in case of both being zeros).    
end

# Export the results as CSV into the figures folder
writedlm(joinpath(fig_dir, "data_barplot_initial_adoption_rho_hat.csv"), res_d)

