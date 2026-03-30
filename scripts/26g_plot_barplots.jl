# Expect exactly two args: output dirs for figures and numbers
if length(ARGS) < 2
    error("Usage: julia your_script.jl <figures_dir> <numbers_dir>")
end
fig_dir = ARGS[1]
num_dir = ARGS[2]

using CairoMakie, LaTeXStrings, DelimitedFiles
CairoMakie.activate!(; pt_per_unit=1) 

M = 100
plot_names = [ 
    "final_adoption_y",
    "initial_adoption_rho_hat",
    "final_adoption_alpha",
    "adoption_diff"
]

fig = Figure(
    fontsize=12,
    fonts=(; regular="Times New Roman"),
    size=(450, 525),
    colormap=:OrRd,
)
gu = GridLayout(
    fig[1, 1],
)
for (plot_name, subfig, ch, pred) = zip(
    plot_names,
    [gu[1, 1], gu[1, 2], gu[2, 1], gu[2, 2]],
    ['a', 'b', 'c', 'd'],
    [1, 3, 4, 6]
)   
    ax = Axis(
        subfig,
        xlabel=L"\hat\beta",
        ylabel="Proportion of Simulations where Prediction Holds",
        xticks=LinRange(0, 1, 6), 
        yticks=LinRange(0, 1, 6), 
        xticksvisible=false,
        yticksvisible=false,
        rightspinevisible=false,
        topspinevisible=false,
        xgridvisible=false,
        ygridvisible=false,
        limits=(-0.5/M, 1+0.5/M+5e-4, 0, 1+5e-4),
        subtitle="($(ch)) Prediction $(pred)",
    )
        # Read the CSV from the figures folder
    res = readdlm(joinpath(fig_dir, "data_barplot_$(plot_name).csv"))[:,1]
    barplot!(
        ax,
        (0:M)/M, res,
        gap=0, strokewidth=0,
        strokecolor=Makie.wong_colors()[1],
    )

    # Write out .tex snippets into the numbers folder
    beta_hat_range = ((0:M)/M)[res .< 1-1e-6]
    open(joinpath(num_dir, "barplot_$(plot_name)_min_beta_hat.tex"), "w") do io
        print(io, min(beta_hat_range...), "%")
    end
    open(joinpath(num_dir, "barplot_$(plot_name)_max_beta_hat.tex"), "w") do io
        print(io, max(beta_hat_range...), "%")
    end
    open(joinpath(num_dir, "barplot_$(plot_name)_argmin_ratio.tex"), "w") do io
        print(io, ((0:M)/M)[argmin(res)], "%")
    end
    open(joinpath(num_dir, "barplot_$(plot_name)_min_ratio.tex"), "w") do io
        print(io, round(min(res...)*100, digits=1), "%")
    end
end
rowgap!(gu, 20)
colgap!(gu, 20)
rowgap!(fig.layout, 10)
colsize!(fig.layout, 1, Aspect(1, 1))
resize_to_layout!(fig)

# Save the combined barplot into the figures folder
save(joinpath(fig_dir, "barplot_combined.eps"), fig)
