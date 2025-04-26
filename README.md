# MASC-simulation-studies
Monte-Carlo simulation studies testing parameter recovery of the Meta-Analytic Stability &amp; Change (MASC) model.

> **What’s MASC?**  A non‑linear curve that fits test–retest correlations to estimate **reliability**, **change**, and **stability‑of‑change** (Anusic & Schimmack 2016).

**What MASC is for**

The Meta-Analytic Stability & Change (MASC) model is a meta-analytic tool that turns a pile of test-retest correlations from many longitudinal studies into three interpretable numbers:

1. **Reliability (rel)** – the share of variance that is genuine signal rather than measurement error.  
2. **Change (change)** – the portion of that reliable variance that actually shifts over time.  
3. **Stability-of-change (stabch)** – the speed at which that change decays toward an asymptote.

Because it fits all studies simultaneously, MASC lets researchers ask questions such as:

- “How stable is risk-taking across the lifespan compared with affect or self-control?”  
- “Does a measure’s stability peak in mid-life and drop again in old age?”  
- “Are health-domain items intrinsically less reliable than financial-domain items?”


Policy-makers and developmental psychologists use these answers to judge when early-life traits can (or cannot) predict later outcomes, to benchmark new instruments, and to decide how long a longitudinal panel needs to run.  In short, MASC translates scattered longitudinal evidence into a clear, three-parameter portrait of temporal stability versus change.

> **Why this matters.**
> Our simulations show that MASC is rock-solid in simple (intercept-only) applications with as few as ~1 000 correlations, but the change and stability-of-change parameters become fragile once age × domain moderators are added—requiring datasets ≥10 k and ≥10-year spans, and still plateauing for the most complex interactions. These benchmarks give researchers a concrete sample-size rule of thumb and highlight when informative priors or alternative models are needed. In short, the work turns MASC from a promising idea into a calibrated tool with clear operating limits.
