# CMPE260 Project 1 - Farm Simulator in Prolog

## Program Overview

This project is a farm simulator implemented in Prolog. The farm consists of grid of cells. Each cell can be empty, contain a plant, or contain an animal. The farm can be visualized as follows:

```
.....
CG..C
....G
```

Here, `C` represents a chicken, `G` represents a grass, and `.` represents an empty cell. The farm is a 5x5 grid in this example. There are other types of animals and plants in the farm as well. The animals can move around the farm and eat the plants. If they eat enough foods they can reproduce. Also there is an animal type `W` (represents a wolf) which can eat other animals.

Animals are stored in a dictionary called `Agents` and plants are stored in a dictionary called `Objects`. The current state of the farm is stored in a list which is structured as follows:

```
[Agents, Objects, Time, TurnOrder]
```

Time and TurnOrder is not directly related to the code and beyond the scope of this document.

## Predicates

The program mainly consists of 9 predicates:

- `agents_distance(+Agent1, +Agent2, -Distance)`: Calculates the Manhattan distance between two agents.
- `number_of_agents(+State, -NumberOfAgents)`: Counts the number of agents in the farm.
- `value_of_farm(+State, -NumberOfAgents)`: Calculates the value of the farm given the value of each agent and object in the `farm.pro` file.
- `find_food_coordinates(+State, +AgentId -Coordinates)`: Finds the coordinates of the foods that the agent can eat and instantiates the `Coordinates` variable with the list of coordinates.
- `find_nearest_agent(+State, +AgentId, -Coordinates, -NearestAgent)`: Finds the nearest agent to the given agent.
- `find_nearest_food(+State, +AgentId, -Coordinates, -FoodType, -Distance)`: Finds the nearest food to the given agent. In this predicate it doesn't matter if the food is reachable or not.
- `move_to_coordinate(+State, +AgentId, +X, +Y, -ActionList, +DepthLimit)`: Moves the agent to the given coordinate if possible. Other agents act as obstacles. (Wolf can move through other agents except other wolves).
- `move to nearest food(+State, +AgentId, -ActionList, +DepthLimit)`: Moves the agent to the nearest food if possible.
- `consume all(+State, +AgentId, -NumberOfMovements, -Value, -NumberOfChildren, +DepthLimit)`: Consumes all the foods that the agent can eat. Tries the nearest food first. If at any point the food is not reachable, tries to move to the next nearest food. If the agent has enough food, it reproduces. Implemented breadth-first search algorithm to find the shortest path to the food. The algorithm is limited by the `DepthLimit` parameter. If the food is not reachable within the `DepthLimit`, the agent doesn't move.

## Running the Program

To run the program, you need to have SWI-Prolog installed on your machine. After installing SWI-Prolog, you can run the program by executing the following command in the terminal:

```bash
swipl main.pro
```

After running the command, you can run the queries in the following format to test the predicates:

```prolog
?- state(Agents, Objects, Time, TurnOrder), State=[Agents, Objects, Time, TurnOrder], number_of_agents(State, NumberOfAgents).
```
