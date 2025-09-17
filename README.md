# UCL_Match_Quality_Score
Creation of two metrics to analyze the performances in the UEFA Champions League from the 2020-21 to the 2024-25 season using Football Reference's data.

There are four steps to create these two metrics, as follows:

## 1 - Selection of match statistics

We select the match statistics that are relevant to compute the quality of the performance of a team in a football match. We decided to select the following statistics that comes from the [Football Reference website](https://fbref.com/en/): 

**npxG E** (Non Penalty Expected Goals), **SoT** (Number of Shots on Target), **xAG E** (Expected Assisted Goals), **SCA** (Number of Shot-Creating Actions), **GCA** (Number of Goal-Creating Actions), **PrgP** (Number of Progressive Passes), **PrgC** (Number of Progressive Carries), **STO**(Number of Successful Takes-Ons), **Tkl** (Number of Tackles), **Int** (Number of Interceptions), **Blocks** (Number of Blocks).

## 2 - Compute the Empirical Cumulative Distribution for each statistic of a team $t$ in match $m$

For each statistic, we compute the Empirical Cumulative Distribution based on the values of the same statistic from all the UEFA Champions League performances between the 2020-21 and 2024-25 season. We multiply the value obtained by 100 to have a more understandable score between 0 and 100.

Here is the formula that we use for each statistic:

$$
M(v_{s, t, m}) = \frac{1}{n} \sum_{i=1}^n \mathbf{1}_{\{x_{s, i} \leq v_{s, t, m}\}} \times 100
$$

Where:

- $v_{s, t, m}$ is the value of the statistic $s$ for team $t$ in match $m$.
- $x_{s, i}$ is the value of the statistic $s$ for the team performance $i$.
- $n$ is the number of team performances from the 2020-21 to 2024-25 Champions Leagues seasons.
- $\mathbf{1}_{\{X_{s, i} \leq v_{s, t, m}\}}$ is an indicator function that equals 1 if $x_{s, i} \leq v_{s, t, m}$
else 0

## 3 - Compute the Team Match Quality Score ($TMQS$) for a team $t$ and a match $m$

For all the statistics presented in the first step, we apply the $M$ function from step 2 and we compute the mean of all those values. This mean corresponds to the $TMQS$ metric. Here is the formula of it for a team $t$ and a match $m$:

$$
TMQS_{t, m} = \frac{M(v_{\text{npxG E}, t, m}) + M(v_{\text{SoT}, t, m}) + M(v_{\text{xAG E}, t, m}) + M(v_{\text{SCA}, t, m}) + M(v_{\text{GCA}, t, m}) + M(v_{\text{PrgP}, t, m}) + M(v_{\text{PrgC}, t, m}) + M(v_{\text{STO}, t, m}) + M(v_{\text{Tkl}, t, m}) + M(v_{\text{Int}, t, m}) + M(v_{\text{Blocks}, t, m})}{11}
$$

## 4 - Compute the Match Quality Score ($MQS$) for a match $m$:

When the $TMQS$ metric is computed for the two teams of a match, we can compute the Match Quality Score ($MQS$) by computing the mean of the two team's $TMQS$. Here is the formula of this new metric:

$$
MQS = \frac{TMQS_{\text{Home team}, m} + TMQS_{\text{Away team}, m}}{2}
$$
