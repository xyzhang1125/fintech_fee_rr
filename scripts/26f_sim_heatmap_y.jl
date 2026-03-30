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
    TT = 10, # The maximum day of deadline.
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
        ct = beta_hat*delta*(alpha*y-rho_hat*ev[t]) # Thresholds
        vc = @. ifelse(c<ct, delta*alpha*y-c, delta*rho_hat*ev[t]) # Continuation value.
        ev[t+1] = sum(vc.*wt) # Calculate expectation.
    end
  
    return ev[1:TT], ev[end], F # Return a segment of a necessarily length.
end

# Function that calculates the difference of cumulative adoption rates.
function adoption_difference(;
    rho = 0.995,
    rho_hat = 0.995,
    beta = 0.95,
    beta_hat = 1,
    delta = 0.99,
    alpha = 1,
    y = 1,
    TT = 8 # Deadline day.
)
    # Iterate to obtain EV's.
    ev, evend, F = ev_iteration(beta_hat, rho_hat, y, delta, alpha)
    
    lad = F(beta*delta*(alpha*y-rho_hat*evend)) # Conditional adoption rate of no deadline.
    if lad<1e-14 # If zero. (Float precision.)
        return -Inf # Return -inf to indicate invalid cases.
    end

    rem = 1.0 # Mass of remaining agents.
    ad = 0.0 # Adoption rate.
    for t = 1:TT # Deadline group.
        cad = F(beta*delta*(alpha*y-rho_hat*ev[TT+1-t])) # Conditional adoption rate.
        ad = ad+rem*cad # Update adoption rate.
        rem = rem*(1-cad)*rho # Update mass of remaining agents.
    end

    rem = 1 # Reset mass of remaining agents.
    for _ = 1:TT # No deadline group. Condition adoption rate is lad.
        ad = ad-rem*lad # Update adoption rate.
        rem = rem*(1-lad)*rho # Update mass of remaining agents.
    end

    # Note that ad here is actually the difference of adoption rates.

    # if ad<-1e-14 # Only cases with a positive difference is valid.
    #     return -Inf
    # end

    return ad 
end

# Function that tests whether Prediction holds.
# Note that there is no y in arguments.
# Return: 1= Pred holds; 0= no valid cases; -1= Pred does not hold.
function check_mono_y(;
    rho = 0.995,
    rho_hat = 0.995,
    beta = 0.8,
    beta_hat = 1,
    delta = 0.99,
    alpha = 1,
    yl = 0.5,
    yh = 1.5,
    TT = 8
)
    pr = -Inf # Previous adoption difference.
    for y = LinRange(yh, yl, 201) # Loop over y in a reverse order.
        cr = adoption_difference(;
            rho=rho, rho_hat=rho_hat, beta_hat=beta_hat, delta=delta,
            alpha=alpha, y=y, beta=beta, TT=TT
        ) # Obtain the current adoption difference.
        #println(y, '=', cr)
        if cr==-Inf # If this case is invalid.
            if pr==-Inf # If there is no previous case.
                return 0 # Return 0: No valid cases.
            else 
                return 1 # Return 1: Prediction holds.
            end
        elseif cr<pr-1e-14 # If difference narrows compared to the previous case.
            return -1 # Return -1: Prediction does not hold.
        else # If difference widens compared to the previous case.
            pr = cr # Update the previous difference and continue the loop.
        end
    end
    return 0 # Return 0: No valid cases.
end

# Function that calculates the ratio of (beta, rho_hat)'s such that Prediction holds for a 
#     given pair of (rho, beta_hat).
function get_ratio(rho, beta_hat; TT=8) 
    cnt1 = 0 # Count the number of rho_hat's such that Prediction holds.
    cntm1 = 0 # Count the number of rho_hat's such that Predition does not hold.
    M = 100 # Number of grids.
    for beta = (0:M)/M # Loop over beta.
        if beta>beta_hat+1e-10 # Require that beta<=beta_hat.
            break
        end
        for rho_hat = (0:M)/M # Loop over rho_hat.
            cr = check_mono_y(
                rho_hat=rho_hat, 
                rho=rho, 
                beta_hat=beta_hat,
                beta=beta,
                TT=TT
            ) # Check the current parameters.
            if cr==1 # If Pred holds.
                cnt1 += 1 # Increment cnt1.
            elseif cr==-1 # If Pred does not hold.
                cntm1 += 1 # Increment cntm1.
            end
        end
    end
    println(rho, ' ', beta_hat, ' ', cnt1, ' ', cntm1, ' ', cnt1/(cnt1+cntm1+1e-10))
    return cnt1/(cnt1+cntm1+1e-10) # Return ratios (1e-10 in case of both being zeros).
end

for TT = [1, 2, 6, 8] # Different deadline days.

    M = 100 # Number of grids.
    res = zeros(M+1, M+1) # Store results.

    Threads.@threads for (j,i) in collect(Iterators.product(0:M, 0:M)) # i for rho, j for beta_hat.
        println(i, j) # To track progress.
        res[i+1,j+1] = get_ratio(i/M, j/M; TT=TT)
    end


        # Export the matrix as CSV into your figures folder:
    writedlm(
        joinpath(fig_dir, "data_heatmap_y_t$(TT).csv"),
        res
    )

end
