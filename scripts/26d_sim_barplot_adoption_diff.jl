# Expect exactly two args: output dirs for figures and numbers
if length(ARGS) < 2
    error("Usage: julia your_script.jl <figures_dir> <numbers_dir>")
end
fig_dir = ARGS[1]
num_dir = ARGS[2]

using Memoize, ThreadSafeDicts, DelimitedFiles

# Cached function that iteratively calculates the expected value of delays.
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

# Function that checks the positive deadline effect for every date before the deadline.
# Assume uniform distribution c over [mc-hc,mc+hc].
# Return: 1= Pred holds; 0= no valid cases; -1= Pred does not hold.
function check_adoption_effect(;
    rho = 0.995,
    rho_hat = 0.995,
    beta = 0.95,
    beta_hat = 1,
    delta = 0.99,
    alpha = 1,
    y = 1,
)
    T = 8 # Day of deadline.

    # Iterate to obtain EV's.
    ev, evend, F = ev_iteration(beta_hat, rho_hat, y, delta, alpha; TT=T)

    rem_d = 1.0 # Mass of remaining agents for the deadline group.
    rem_n = 1.0 # Mass of remaining agents for the no-deadline group.
    ad = 0.0 # Adoption rate.

    for t = 1:T 
        # Conditional adoption rate.
        cad_d = F(beta*delta*(alpha*y-rho_hat*ev[T+1-t])) # Deadline group.
        cad_n = F(beta*delta*(alpha*y-rho_hat*evend)) # No-deadline group.
        # Update adoption rate.
        ad = ad+rem_d*cad_d-rem_n*cad_n 
        if ad<-1e-10
            return -1
        end
        # Update mass of remaining agents.
        rem_d = rem_d*(1-cad_d)*rho 
        rem_n = rem_n*(1-cad_n)*rho 
    end 

    return 1
end

function single_get_ratio(iter, beta_hat)
    cnt1 = 0 # Count the number of tuples such that Prediction holds.
    cntm1 = 0 # Count the number of tuples such that Prediction does not hold.
    for (beta, rho, rho_hat) in iter
        cr = check_adoption_effect(
            rho_hat=rho_hat, 
            rho=rho, 
            beta_hat=beta_hat,
            beta=beta,
        ) # Check the current parameters.
        if cr==1 # If Prediction holds.
            cnt1 += 1 # Increment cnt1.
        elseif cr==-1 # If Prediction does not hold.
            cntm1 += 1 # Increment cntm1.
        end
    end
    return (cnt1, cntm1)
end

# Function that calculates the ratio of (rho,rho_hat,beta)'s such that Prediction
#     holds for a given beta_hat.
function get_ratio(beta_hat) 
    cnt1 = 0 # Count the number of tuples such that Prediction holds.
    cntm1 = 0 # Count the number of tuples such that Predition does not hold.
    M = 100 # Number of grids.
    # Parallel computing.
    params = collect(Iterators.product(0:(1/M):beta_hat, (0:M)/M, (0:M)/M)) 
    chunks = Iterators.partition(params, length(params) ÷ Threads.nthreads())
    tasks = map(chunks) do chunk
        Threads.@spawn single_get_ratio(chunk, beta_hat)
    end
    chunk_res = fetch.(tasks)
    # Summarize results.
    for (cc1, ccm1) in chunk_res 
        cnt1 += cc1
        cntm1 += ccm1
    end
    println(beta_hat, ' ', cnt1, ' ', cntm1, ' ', cnt1/(cnt1+cntm1+1e-10))
    return cnt1/(cnt1+cntm1+1e-10) # Return ratios (1e-10 in case of both being zeros).
end

M = 100 # Number of grids.
res = zeros(M+1) # Store results.

for i = 0:M # i for rho, j for beta_hat.
    res[i+1] = get_ratio(i/M)
end

# Export the matrix as CSV into the figures folder
writedlm(
    joinpath(fig_dir, "data_barplot_adoption_diff.csv"),
    res
)
