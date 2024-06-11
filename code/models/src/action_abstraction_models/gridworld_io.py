"""
Pared down IO library for handling grid world configuration files
"""

import json

from gridworld import GridWorld, GridWorldAgent
from gridworld_globals import AGENTS, MAX_REWARD, MIN_REWARD, STEPS






"""
Function for running basic sanity check on grid config variables
"""
def test_grid_config(name, rows, cols, agent_type, agent_start_position, tree_positions, tree_rewards, tree_visibility):
    # Basic checks
    assert name, "Must provide a name for the grid world"
    assert rows, "Must provide a number of rows"
    assert cols, "Must provide a number of columns"
    # Compatibility checks
    assert rows > 0, "Number of rows must be greater than 0"
    assert cols > 0, "Number of columns must be greater than 0"
    assert rows == cols, "Number of rows and columns must be equal" # TODO is this necessary?
    if agent_type:
        assert agent_type in AGENTS, "Agent must be one of {}".format(AGENTS)
    if agent_start_position:
        assert len(agent_start_position) == 2, "Agent start position must be a tuple formatted as (x, y)"
        assert agent_start_position[0] > 0 and agent_start_position[0] <= rows, "Agent start row must be between 1 and {}, inclusive".format(rows)
        assert agent_start_position[1] > 0 and agent_start_position[1] <= cols, "Agent start column must be between 1 and {}, inclusive".format(cols)
    if tree_positions and tree_rewards and tree_visibility:
        assert len(tree_positions) == len(tree_rewards) == len(tree_visibility), "Tree vectors must all be the same length"
    if tree_positions:
        assert [elem[0] > 0 and elem[0] < rows for elem in tree_positions if elem], "Tree row positions must be between 1 and {}".format(rows)
        assert [elem[1] > 0 and elem[1] < cols for elem in tree_positions], "Tree column positions must be between 1 and {}".format(cols)
    if tree_rewards:
        assert [elem >= MIN_REWARD for elem in tree_rewards], "Tree rewards must be greater than {}".format(MIN_REWARD)
        assert [elem <= MAX_REWARD for elem in tree_rewards], "Tree rewards must be less than {}".format(MAX_REWARD)
    if tree_visibility:
        assert [elem == 0 or elem == 1 for elem in tree_visibility], "Tree visibility must be binary (0 or 1)"





def initialize_grid_world(filepath, steps=STEPS):
    # Read in file
    with open(filepath, "r") as file:
        grid = json.load(file)
    # Run basic tests on grid config
    test_grid_config(grid["name"], grid["rows"], grid["cols"], grid["agent_type"], grid["agent_start_position"], grid["tree_positions"], grid["tree_rewards"], grid["tree_visibility"])
    # Initialize GridWorld object with provided parameters
    return GridWorld(name=grid["name"],
                     rows=grid["rows"],
                     cols=grid["cols"],
                     agent=GridWorldAgent(grid["agent_type"]),
                     agent_start_position=grid["agent_start_position"],
                     tree_positions=grid["tree_positions"],
                     tree_rewards=grid["tree_rewards"],
                     tree_visibility=grid["tree_visibility"],
                     steps=steps)