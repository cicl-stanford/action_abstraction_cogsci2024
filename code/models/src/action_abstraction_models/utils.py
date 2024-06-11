"""
Util functions used across libraries
"""


def get_agent_start_positions(rows, cols):
    """
    Returns a list of all possible agent start positions for a grid of size (rows, cols)
    """
    return [[1, 1], [rows, cols]]