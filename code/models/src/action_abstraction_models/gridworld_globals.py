"""
Global variables for the gridworld models.
"""

MIN_REWARD = 1
MAX_REWARD = 9
TARGET_REWARD_TOTAL = 20
AGENTS = ["optimist", "pessimist"]
STEPS = 10
NUM_TREES = 10
AGENT_MYSTERY_TREE_EXPECTED_REWARD = {
    "optimist": 8,
    "pessimist": 2,
}
TREE_INVISIBILITY_PRIOR = 0.25