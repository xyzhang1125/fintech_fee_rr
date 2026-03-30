# Expect exactly two args: output dirs for figures and numbers
if length(ARGS) < 2
    error("Usage: julia your_script.jl <figures_dir> <numbers_dir>")
end
fig_dir = ARGS[1]
num_dir = ARGS[2]

using CairoMakie, LaTeXStrings, DelimitedFiles, Printf
CairoMakie.activate!(; pt_per_unit=1) 

for plot_name = ["beta", "y"]
    M = 100

    # Combine the heatmaps for T=1,2,6,8.
    fig = Figure(
        fontsize=12,
        fonts=(; regular="Times New Roman"),
        size=(450, 525),
        colormap=:OrRd,
    )
    gu = GridLayout(
        fig[1, 1],
    )
    for (T, subfig, ch) = zip(
        [1, 2, 6, 8],
        [gu[1, 1], gu[1, 2], gu[2, 1], gu[2, 2]],
        ['a', 'b', 'c', 'd']
    )   
        ax = Axis(
            subfig,
            xlabel=L"\hat\beta", ylabel=L"\rho",
            xticks=LinRange(0, 1, 6), 
            yticks=LinRange(0, 1, 6), 
            backgroundcolor=:grey80,
            xticksvisible=false,
            yticksvisible=false,
            rightspinevisible=false,
            topspinevisible=false,
            xgridvisible=false,
            ygridvisible=false,
            aspect=1,
            subtitle=latexstring("($(ch))\$T=$(T)\$"),
        )
        # Read the CSV from the figures folder
        res = readdlm(joinpath(fig_dir, "data_heatmap_$(plot_name)_t$(T).csv"))

        # Export values.
        if T == 8
            βrange = ((0:M)/M)[sum(res, dims=1)[1,:] .< 1e-6]
            open(joinpath(num_dir, "heatmap_$(plot_name)_t$(T)_max_beta_hat.tex"), "w") do io
                @printf(io, "%.2f%%", max(βrange...))
            end
        end

        replace!(res, 0.0=>NaN)
        heatmap!(
            ax,
            (0:M)/M, (0:M)/M, res'
        )
    end
    rowgap!(gu, 20)
    colgap!(gu, 20)
    gd = GridLayout(
        fig[2, 1],
    )
    Label(
        gd[1, 1], 
        "Proportion of Simulations where Prediction Holds",
        alignmode=Outside(10)
    )
    Colorbar(
        gd[1, 2],
        ticks=LinRange(0, 1, 3),
        ticksvisible=false,
        height=30,
        alignmode=Outside(10)
    )
    Box(
        gd[1, 1:2],
        color=:transparent,
    )
    colgap!(gd, -10)
    rowgap!(fig.layout, 10)
    colsize!(fig.layout, 1, Aspect(1, 1))
    resize_to_layout!(fig)

    # Save the combined heatmap into the figures folder
    save(joinpath(fig_dir, "heatmap_$(plot_name)_combined.eps"), fig)

end
