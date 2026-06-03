import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns


CONTROL_CHART_CONSTANTS = pd.DataFrame(
    {
        "n": list(range(2, 26)),
        "A2": [
            1.880,
            1.023,
            0.729,
            0.577,
            0.483,
            0.419,
            0.373,
            0.337,
            0.308,
            0.285,
            0.266,
            0.249,
            0.235,
            0.223,
            0.212,
            0.203,
            0.194,
            0.187,
            0.180,
            0.173,
            0.167,
            0.162,
            0.157,
            0.153,
        ],
        "D3": [
            0.000,
            0.000,
            0.000,
            0.000,
            0.000,
            0.076,
            0.136,
            0.184,
            0.223,
            0.256,
            0.283,
            0.307,
            0.328,
            0.347,
            0.363,
            0.378,
            0.391,
            0.403,
            0.415,
            0.425,
            0.434,
            0.443,
            0.451,
            0.459,
        ],
        "D4": [
            3.267,
            2.574,
            2.282,
            2.114,
            2.004,
            1.924,
            1.864,
            1.816,
            1.777,
            1.744,
            1.717,
            1.693,
            1.672,
            1.653,
            1.637,
            1.622,
            1.608,
            1.597,
            1.585,
            1.575,
            1.566,
            1.557,
            1.548,
            1.541,
        ],
    }
).set_index("n")


def _ordered_data(data, time_col=None):
    """Return a clean copy of data, sorted by time_col if it is supplied."""
    chart_data = data.copy()
    if time_col is not None:
        chart_data = chart_data.sort_values(time_col)
    return chart_data.reset_index(drop=True)


def _x_positions(labels):
    return np.arange(1, len(labels) + 1)


def _plot_chart(
    ax,
    x,
    y,
    center,
    ucl,
    lcl,
    title,
    ylabel,
    xlabel="Observation",
    line_color="#1f77b4",
    point_size=55,
):
    sns.lineplot(x=x, y=y, marker="o", color=line_color, linewidth=1.5, ax=ax)
    ax.scatter(x, y, s=point_size, color=line_color, edgecolor="white", linewidth=0.8, zorder=3)

    ax.plot(x, center, color="black", linewidth=1.4, linestyle="-", label="Center line")
    ax.plot(x, ucl, color="red", linewidth=1.4, linestyle="--", label="UCL")
    ax.plot(x, lcl, color="red", linewidth=1.4, linestyle="--", label="LCL")

    ax.set_title(title, fontsize=14, weight="bold")
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    ax.grid(True, alpha=0.25)
    ax.legend(loc="best", frameon=True)


def _constant_or_array(value, length):
    if np.isscalar(value):
        return np.repeat(value, length)
    return np.asarray(value)


def xbar_r_chart(
    data,
    value_col,
    group_col,
    time_col=None,
    figsize=(12, 8),
    point_size=55,
):
    """
    Construct X-bar and R control charts for subgrouped continuous data.

    Parameters
    ----------
    data : pandas.DataFrame
        Data frame containing the observations.
    value_col : str
        Column with the continuous measurements.
    group_col : str
        Column identifying each subgroup/sample.
    time_col : str, optional
        Column used to order the subgroups.
    figsize : tuple
        Size of the matplotlib figure.
    point_size : int
        Size of plotted data points.

    Returns
    -------
    dict
        Dictionary with the figure, axes, and a summary data frame.
    """
    chart_data = _ordered_data(data, time_col)
    summary = (
        chart_data.groupby(group_col, sort=False)[value_col]
        .agg(xbar="mean", r=lambda x: x.max() - x.min(), n="count")
        .reset_index()
    )

    if summary["n"].min() < 2:
        raise ValueError("Each subgroup must contain at least 2 observations for an X-bar/R chart.")
    if summary["n"].max() > CONTROL_CHART_CONSTANTS.index.max():
        raise ValueError("This simple function supports subgroup sizes from 2 to 25.")

    constants = CONTROL_CHART_CONSTANTS.loc[summary["n"]]
    xbarbar = summary["xbar"].mean()
    rbar = summary["r"].mean()

    summary["xbar_center"] = xbarbar
    summary["xbar_ucl"] = xbarbar + constants["A2"].to_numpy() * rbar
    summary["xbar_lcl"] = xbarbar - constants["A2"].to_numpy() * rbar
    summary["r_center"] = rbar
    summary["r_ucl"] = constants["D4"].to_numpy() * rbar
    summary["r_lcl"] = constants["D3"].to_numpy() * rbar

    sns.set_theme(style="whitegrid")
    fig, axes = plt.subplots(2, 1, figsize=figsize, sharex=True)
    x = _x_positions(summary[group_col])

    _plot_chart(
        axes[0],
        x,
        summary["xbar"],
        summary["xbar_center"],
        summary["xbar_ucl"],
        summary["xbar_lcl"],
        title="X-bar Chart",
        ylabel="Subgroup Mean",
        point_size=point_size,
    )
    _plot_chart(
        axes[1],
        x,
        summary["r"],
        summary["r_center"],
        summary["r_ucl"],
        summary["r_lcl"],
        title="R Chart",
        ylabel="Subgroup Range",
        xlabel=group_col,
        point_size=point_size,
    )

    axes[1].set_xticks(x)
    axes[1].set_xticklabels(summary[group_col], rotation=45, ha="right")
    plt.tight_layout()

    return {"fig": fig, "axes": axes, "summary": summary}


def imr_chart(
    data,
    value_col,
    time_col=None,
    figsize=(12, 8),
    point_size=55,
):
    """
    Construct Individuals and Moving Range charts for continuous observations.

    The moving range uses consecutive observations, so ordering matters.
    Use time_col when the data frame is not already in the desired order.
    """
    chart_data = _ordered_data(data, time_col)
    values = chart_data[value_col].dropna().reset_index(drop=True)

    if len(values) < 2:
        raise ValueError("At least 2 observations are needed for an I-MR chart.")

    moving_range = values.diff().abs()
    mrbar = moving_range.dropna().mean()
    xbar = values.mean()

    chart_data = pd.DataFrame(
        {
            "observation": np.arange(1, len(values) + 1),
            value_col: values,
            "moving_range": moving_range,
        }
    )
    chart_data["i_center"] = xbar
    chart_data["i_ucl"] = xbar + 2.66 * mrbar
    chart_data["i_lcl"] = xbar - 2.66 * mrbar
    chart_data["mr_center"] = mrbar
    chart_data["mr_ucl"] = 3.267 * mrbar
    chart_data["mr_lcl"] = 0

    sns.set_theme(style="whitegrid")
    fig, axes = plt.subplots(2, 1, figsize=figsize, sharex=True)
    x = chart_data["observation"]

    _plot_chart(
        axes[0],
        x,
        chart_data[value_col],
        chart_data["i_center"],
        chart_data["i_ucl"],
        chart_data["i_lcl"],
        title="Individuals Chart",
        ylabel=value_col,
        point_size=point_size,
    )
    _plot_chart(
        axes[1],
        x,
        chart_data["moving_range"],
        chart_data["mr_center"],
        chart_data["mr_ucl"],
        chart_data["mr_lcl"],
        title="Moving Range Chart",
        ylabel="Moving Range",
        point_size=point_size,
    )

    axes[1].set_xlabel("Observation")
    plt.tight_layout()

    return {"fig": fig, "axes": axes, "summary": chart_data}


def p_chart(
    data,
    defectives_col,
    n_col,
    group_col=None,
    time_col=None,
    figsize=(12, 5),
    point_size=55,
):
    """
    Construct a p-chart for the proportion defective.

    defectives_col should contain the number of defective/nonconforming items.
    n_col should contain the number of items inspected/tested.
    """
    chart_data = _ordered_data(data, time_col)
    chart_data = chart_data[[c for c in [group_col, defectives_col, n_col] if c is not None]].copy()
    chart_data = chart_data.dropna(subset=[defectives_col, n_col]).reset_index(drop=True)

    if (chart_data[n_col] <= 0).any():
        raise ValueError("All sample sizes must be positive.")
    if (chart_data[defectives_col] > chart_data[n_col]).any():
        raise ValueError("The number of defectives cannot be larger than the sample size.")

    chart_data["p"] = chart_data[defectives_col] / chart_data[n_col]
    pbar = chart_data[defectives_col].sum() / chart_data[n_col].sum()
    se = np.sqrt(pbar * (1 - pbar) / chart_data[n_col])

    chart_data["center"] = pbar
    chart_data["ucl"] = np.minimum(1, pbar + 3 * se)
    chart_data["lcl"] = np.maximum(0, pbar - 3 * se)

    sns.set_theme(style="whitegrid")
    fig, ax = plt.subplots(figsize=figsize)
    x = _x_positions(chart_data.index)
    xlabels = chart_data[group_col] if group_col is not None else x

    _plot_chart(
        ax,
        x,
        chart_data["p"],
        chart_data["center"],
        chart_data["ucl"],
        chart_data["lcl"],
        title="p-Chart",
        ylabel="Proportion Defective",
        xlabel=group_col or "Sample",
        point_size=point_size,
    )
    ax.set_ylim(bottom=0, top=max(1, chart_data["ucl"].max() * 1.05))
    ax.set_xticks(x)
    ax.set_xticklabels(xlabels, rotation=45, ha="right")
    plt.tight_layout()

    return {"fig": fig, "axes": ax, "summary": chart_data}


def np_chart(
    data,
    defectives_col,
    n_col,
    group_col=None,
    time_col=None,
    figsize=(12, 5),
    point_size=55,
):
    """
    Construct an np-chart for the number defective.

    The classical np-chart is usually taught with constant sample size, but
    this function also draws varying limits when sample sizes differ.
    """
    chart_data = _ordered_data(data, time_col)
    chart_data = chart_data[[c for c in [group_col, defectives_col, n_col] if c is not None]].copy()
    chart_data = chart_data.dropna(subset=[defectives_col, n_col]).reset_index(drop=True)

    if (chart_data[n_col] <= 0).any():
        raise ValueError("All sample sizes must be positive.")
    if (chart_data[defectives_col] > chart_data[n_col]).any():
        raise ValueError("The number of defectives cannot be larger than the sample size.")

    pbar = chart_data[defectives_col].sum() / chart_data[n_col].sum()
    center = chart_data[n_col] * pbar
    se = np.sqrt(chart_data[n_col] * pbar * (1 - pbar))

    chart_data["center"] = center
    chart_data["ucl"] = center + 3 * se
    chart_data["lcl"] = np.maximum(0, center - 3 * se)

    sns.set_theme(style="whitegrid")
    fig, ax = plt.subplots(figsize=figsize)
    x = _x_positions(chart_data.index)
    xlabels = chart_data[group_col] if group_col is not None else x

    _plot_chart(
        ax,
        x,
        chart_data[defectives_col],
        chart_data["center"],
        chart_data["ucl"],
        chart_data["lcl"],
        title="np-Chart",
        ylabel="Number Defective",
        xlabel=group_col or "Sample",
        point_size=point_size,
    )
    ax.set_ylim(bottom=0)
    ax.set_xticks(x)
    ax.set_xticklabels(xlabels, rotation=45, ha="right")
    plt.tight_layout()

    return {"fig": fig, "axes": ax, "summary": chart_data}
