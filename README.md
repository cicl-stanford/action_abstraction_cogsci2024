# *Without his cookies, he's just a monster*: <br>A counterfactual simulation model of social explanation

**Erik Brockbank\*, Justin Yang\*, Mishika Govil, Judith E. Fan, Tobias Gerstenberg**

Presented at the 46th Annual Meeting of the Cognitive Science Society (2024; Rotterdam, Netherlands).

[Link to paper](https://github.com/cicl-stanford/action_abstraction_cogsci2024/blob/master/writeup/CogSci2024-final.pdf)

```
@inproceedings{brockbank2024without,
  title = {Without his cookies, he's just a monster: A counterfactual simulation model of social explanation},
  booktitle = {Proceedings of the 46th {Annual} {Conference} of the {Cognitive} {Science} {Society}},
  author = {Brockbank*, Erik and Yang*, Justin and Fan, Judith E. and Gerstenberg, Tobias},
  year = {2024},
}
```


**Contents:**

* [Overview](#overview)
* [Experiment pre-registration & demo](#experiment-pre-registration--demo)
* [Repository structure](#repository-structure)
* [Experiments](#experiments)
* [Empirical analyses](#empirical-analyses)
* [Models](#models)
* [CRediT author statement](#credit-author-statement)




## Overview


<p align="center" style="font-size: smaller">
  <img width="50%" src="figures/Schemas/schema_fig.png"></img><br/>
  Our approach
</p>


Everyday reasoning about others involves accounting for why they act the way they do.
With many explanations for someone's behavior, how do observers choose the best one?
A large body of work in social psychology suggests that people's explanations rely heavily on traits rather than external factors.
Recent results have called this into question, arguing that people balance traits, mental states, and situation to make sense of others' actions.
*How might they achieve this?*
In the current work, we hypothesize that people rely on *counterfactual simulation* to weigh different explanations for others' behavior.
We propose a computational model of this process that makes concrete predictions about when people will prefer to explain events based on the actor's *traits* or their *situation*.
We test the predictions of this model in an experimental paradigm in which trait and situation each guide behavior to varying degrees.
Our model predicts people's causal judgments well overall but is less accurate for trait explanations than situational explanations.
In a comparison with simpler causal heuristics, a majority of participants were better predicted by the counterfactual model.
These results point the way toward a more comprehensive understanding of how social reasoning is performed within the context of domain-general causal inference.




## Experiment pre-registration & demo

The experiment reported in these results was pre-registered on the [Open Science Framework](https://help.osf.io/article/158-create-a-preregistration).
Our pre-registration can be found [here](https://osf.io/d48jk).

A demo of the experiment can be found [here](https://justintheyang.github.io/experiment_demos/action_abstraction/index.html).



## Repository structure

```
├── code
│   ├── analyses
│   ├── experiments
│   └── models
├── data
│   ├── data_processed
│   ├── experiment_data_unprocessed
│   └── model_predictions_unprocessed
├── figures
│   ├── results
│   └── schemas
└── writeup
```

* `/code`: code for experiments, as well as models and analyses reported in the paper.
    * `/analyses`: this folder contains R code for all behavioral and modeling results reported in the paper (see [Empirical analyses](#empirical-analyses) section below).
    * `/experiments`: this folder contains web code to run demos of the experiment itself. Each of the five between-subjects conditions has a separate folder within this directory (see [Experiments](#experiments) section below).
    * `/models`: this folder contains python code for running the two models used in the paper (see [Counterfactual simulation model](#counterfactual-simulation-model) and [Causal heuristic model](#causal-heuristic-model) sections below).

* `/data`: contains all behavioral and model data used in our analyses.
    * `/data_processed`: this folder stores `.RData` files with structured and organized experiment and model data.
    * `/experiment_data_unprocessed`: this folder has CSV files with raw experiment data.
    * `/model_predictions_unprocessed`: this folder has CSV files with model predictions.

* `/figures`: folder containing all figures (and accompanying files) presented in the paper.
    * `/results`: results figures generated during analysis.
    * `/schemas`: overview figures illustrating the task and our model.
* `/writeup`: the paper can be accessed here.



## Experiments

<p align="center" style="font-size: smaller">
  <img width="50%" src="figures/Schemas/expt_overview.png"></img><br/>
  Experiment overview
</p>


Our experiment examined how people use counterfactual simulation to weigh different explanations for others' behavior.
A detailed explanation of the experimental design procedure is documented in the [study preregistration](https://osf.io/d48jk).

Participants were shown a series of $10x10$ grid worlds whose cells were populated with $10$ berry trees at randomly selected locations.
Some of these berry trees were _mystery trees_ which contained an unknown number of berries.
One of two _farmer agents_ harvested as many berries as possible in $10$ steps, either succeeding or failing depending on whether they collected at least $20$ berries.
The agents differed in their level of _optimism_, operationalized as their expectation about the number of berries in each tree.

In a between-subjects manipulation, participants observed the farmer agents collect berries along the path and were prompted with a slider scale that asked one of the following questions:
- whether the agent would have succeeded if they had been in a different starting location (***counterfactual situation***)
- whether the agent would have succeeded if they had been as optimistic or pessimistic as the other agent (***counterfactual trait***)
- whether the agent succeeded because of where they started (***causal situation***)
- whether the agent succeeded because of their optimism or pessimism (***causal trait***)
- whether the agent's success was attributable more to their starting location or their optimism (***causal tradeoff***)

Code for each experimental condition can be found in `code/experiments/s1_*`.

A demo of the experiments is available [here](https://justintheyang.github.io/experiment_demos/action_abstraction/index.html).


<p align="center" style="font-size: smaller">
  <img width="75%" src="https://github.com/cicl-stanford/action_abstraction_cogsci2024/blob/master/code/experiments/s1_causal_selection/assets/img/result.gif?raw=true"></img><br/>
  Example stimulus for experiment
</p>




## Empirical analyses

**Overview**

Our empirical analyses explore the degree to which participants' causal judgments can be predicted by their counterfactual evaluations, and whether these causal judgments are strong predictors of their causal *tradeoff* judgments.
In addition, we explore whether the counterfactual judgments themselves can be predicted by our counterfactual simulation model (see [Counterfactual simulation model](#counterfactual-simulation-model) below). Finally, our *model comparison* explores whether causal judgments are better predicted by simple heuristics (see [Causal heuristic model](#causal-heuristic-model) below) rather than counterfactual responses.


**Execution**

The empirical analyses, conducted in R, can be found in `/code/analyses`.
They require *R Studio* or another environment for running R code.
The R Project `R.Rproj` is the simplest way to run the analysis files below, but they can also be run independently.

Below is an overview of each script in the order in which results appear in the paper:

* `data_processing.R`: reads in unprocessed experiment responses stored in `/data/experiment_data_unprocessed` and unprocessed model predictions (`/data/model_predictions_unprocessed`) copied from the `model_output` directory of the models (see relevant model sections below). It processes the behavioral and model data and stores them to separate RData files in `/data/data_processed`.
* `experiment_analyses.R`: reads in the processed experimental data in `/data/data_processed` and analyzes the relationship between participants' causal and counterfactual judgments.
* `model_analyses.R`: reads in the processed experimental data and the *counterfactual simulation model* predictions in `/data/data_processed` and analyzes the relationship between the simulation model's predicted counterfactual judgments and participants' counterfactual responses.
* `model_comparison.R`: Estimates regression parameters for the relationship between participants' counterfactual judgments and their causal judgments. This fit is compared to a similar regression predicting causal judgments using the outputs of the [Causal heuristic model](#causal-heuristic-model). These regressions are fit using `brms`; files encoding the model fit are saved to the `/brms_fits` directory.

Figures generated by these scripts are saved to the `/figures` directory.


## Models

Models were coded in python and require a basic python environment.
To run the modeling code, we recommend setting up a `Miniforge` conda environment with dependencies for the models.

To do this, navigate to the `/code/models` directory and follow the instructions below (one-time only):

1. install miniforge from `https://github.com/conda-forge/miniforge` (eg `Miniforge3-MacOSX-arm64`)
2. execute the Miniforge installer: `bash Miniforge3-MacOSX-arm64.sh`
3. in this same directory, create a conda environment to run the code: `conda create --name name-of-my-env python==3.10`
4. activate the new conda environment: `conda activate name-of-my-env`
5. install necessary dependencies in the `requirements.txt` file: `pip install -e .`

After completing the above, you should be able to run all code for models described in our results.

These files are locating in `/code/models/src/action_abstraction_models`.

Below is an overview of the two models used in our results, the ***counterfactual simulation model*** and the ***causal heuristic model***.


### Counterfactual simulation model

**Overview**

The counterfactual simulation model estimates the situation and trait counterfactual judgments made by participants on each trial: how likely it was that the farmer's outcome would have changed if they had started on the other start location or had the other farmer's level of optimism.
The model forms these estimates by noisily sampling paths for the relevant counterfactual (i.e., those taken by the same farmer from the other start location, or those taken by the other farmer) and evaluating the farmer's outcome in each sampled path; this process incorporates additional noise in the number of berries on visible trees and the number of steps taken by the farmer.
The model's predicted counterfactual judgment is simply the proportion of samples in which the farmer's outcome differs from the one observed in the trial.

The model uses parameter values obtained during a separate fitting procedure that applied a grid search to identify optimal values for the model's three free parameters: the softmax temperature with which paths are sampled and the variance around representations of the number of berries on visible trees and the number of steps taken by the farmer.
The number of sampled paths used to estimate the counterfactual ($1000$) is fixed and was not fit to participant responses.

**Execution**

The counterfactual simulation model code is contained in `counterfactual_simulation_model.py`.

* The model first reads JSON files for each of the experiment trials contained in the `gridworld_json` directory (note this folder contains a full set of $1000$ simulated grid world environments from which our experimental trials were originally selected).
* The model then simulates agent behavior in each of the trials using a combination of gridworld classes defined in `gridworld.py`, functions for initializing and testing gridworld classes in `gridworld_io.py`, util functions defined in `utils.py`, and global variables stored in `gridworld_globals.py`.
* On completion, the model writes output to a CSV in the `/model_output` directory (`cf_model_default_samples_1000_berry_noise_0.112_step_noise_0.03_softmax_0.59.csv`).


### Causal heuristic model

**Overview**

The heuristic model estimates how much the farmer's trait or situation *caused* their outcome using simple heuristic strategies in place of counterfactuals.
The model assumes that the causal role of a farmer's *trait* in their outcome is merely a function of covariation between each agent and the two possible outcomes.
Meanwhile, the causal role of the farmer's *situation* in their outcome is taken to be a function of the average *distance-weighted expected reward* between the farmer and the trees in each trial (this measure incorporates information about the rewards in the environment as well as their proximity to the farmer).
To evaluate the model, we estimate regression weights for each of these heuristics (agent-outcome combination and distance-weighted expected reward) as predictors of participants' causal judgments.
We compare the regression fits to those obtained using participants' counterfactual responses as predictors instead.

This model does not contain any free parameters except for the discount factor $\gamma$ applied to tree rewards based on their distance: a tree's *distance-weighted* expected reward is $\gamma ^ d \mathbb{E}(r)$ where $d$ is the Manhattan distance to the tree from the farmer's start location, $\mathbb{E}(r)$ is the tree's expected reward (either its visible reward $r$ or $5$, the expected reward of mystery trees), and $\gamma$ is a discount factor in $(0, 1)$.
We set $\gamma=0.9$.


**Execution**

The causal heuristic model code is located in `heuristic_model.py`.

* The model first reads in the summary CSV file `/gridworld_json/gridworld_summary.csv`, which stores relevant information about each trial used to produce the trial stimuli themselves (e.g., the agent's path).
* The model then evaluates heuristic summary information about each trial (agent, outcome, distance-weighted expected reward) using functions defined in `heuristic_model.py`.
* On completion, the model writes output to a csv in the `/model_output` directory (`heuristic_model_trial_summary.csv`). This file contains columns corresponding to the trial, agent (optimist vs. pessimist), start location, outcome (binary), the true reward obtained by the agent, and the distance-weighted expected reward.



## CRediT author statement

*What is a [CRediT author statement](https://www.elsevier.com/authors/policies-and-guidelines/credit-author-statement)?*

- **Erik Brockbank\*:** Conceptualization, Methodology, Software, Validation, Formal analysis, Investigation, Resources, Data Curation, Writing - Original Draft, Writing - Review & Editing, Visualization, Supervision, Project administration
- **Justin Yang\*:** Methodology, Software, Validation, Formal analysis, Investigation, Resources, Data Curation, Writing - Review & Editing, Visualization
- **Mishika Govil:** Software, Formal analysis, Resources, Data Curation, Writing - Review & Editing
- **Judith E. Fan:** Writing - Review & Editing, Supervision, Project administration, Funding acquisition
- **Tobias Gerstenberg:** Conceptualization, Methodology, Writing - Review & Editing, Supervision, Project administration, Funding acquisition


