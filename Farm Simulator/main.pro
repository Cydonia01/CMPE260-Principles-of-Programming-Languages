% mehmet ali ozdemir
% 2021400000
% compiling: yes
% complete: yes


:- ['cmpefarm.pro'].
:- init_from_map.


% 1 - This predicate finds the manhattan distance between two agents.
agents_distance(Agent1, Agent2, Distance):-
    X1 is Agent1.x, Y1 is Agent1.y,
    X2 is Agent2.x, Y2 is Agent2.y,
    Distance is abs(X1 - X2) + abs(Y1 - Y2).



% 2 - These predicates evaluates the total number of agents.
% number_of_pairs predicate is used to find the length of a list.
number_of_pairs([], 0).
number_of_pairs([_|T], N):-
    number_of_pairs(T, N1), 
    N is N1 + 1. 

number_of_agents(State, NumberOfAgents):-
    State = [Agents, _, _, _], % Decompose state
    dict_pairs(Agents, _, Pairs), % List of pairs
    number_of_pairs(Pairs, NumberOfAgents). % find the number of pairs



% 3 - These predicates evaluates the total value of the farm.
% helper predicates to calculate the total value of agents and objects.
total_value([], Mid, Mid).
total_value([H|T], Mid, Value):-
    _-Entity = H, % get the object from the pair
    Subtype = Entity.subtype, 
    (   Subtype = wolf
    ->  (   SubtypeValue is 0, % if the subtype of the entity is wolf, then value is 0
            Mid1 is Mid + SubtypeValue % add the value of the object to the total value
        )
    ;   (   value(Subtype, SubtypeValue), % else, get the value of the entity
            Mid1 is Mid + SubtypeValue % add the value of the object to the total value
        )
    ),
    total_value(T, Mid1, Value).

value_of_farm(State, Value):-
    State = [Agents, Objects, _, _], 
    dict_pairs(Agents, _, AgentList), % get the list of agents
    dict_pairs(Objects, _, ObjectList), % get the list of objects
    total_value(AgentList, 0, AgentValue), 
    total_value(ObjectList, 0, ObjectValue), 
    Value is AgentValue + ObjectValue.



% 4 - These predicates finds the coordinates of the consumable foods by the specific agent.
% helper predicates to find the coordinates of the consumable foods.
consumable_helper([], [], _, List, List).
consumable_helper([HA|TA], [H|T], SubtypeAgent, List, LastList):-
    (   SubtypeAgent = wolf % check if the agent is wolf
    ->  (   _-FoodAgent = HA, 
            SubtypeFoodAgent = FoodAgent.subtype,
            (   can_eat(SubtypeAgent, SubtypeFoodAgent) % check if the agent can eat the food
            ->  (   X = FoodAgent.x, Y = FoodAgent.y,
                    consumable_helper(TA, _, SubtypeAgent, [[X,Y]|List], LastList) % call the helper predicate with new coordinates
                )
            ;   consumable_helper(TA, _, SubtypeAgent, List, LastList) 
            )
        )
    ;   (   _-Object = H,
            SubtypeObject = Object.subtype,
            (   can_eat(SubtypeAgent, SubtypeObject)
            ->  (   X = Object.x, Y = Object.y,
                    consumable_helper(_, T, SubtypeAgent, [[X,Y]|List], LastList) % call the helper predicate with new coordinates
                )
                ;   consumable_helper(_, T, SubtypeAgent, List, LastList)
            )
        )
    ).

find_food_coordinates(State, AgentId, Coordinates):-
    State = [Agents, Objects, _, _],
    Agent = Agents.AgentId,
    dict_pairs(Agents, _, AgentList), % get the list of agents
    dict_pairs(Objects, _, ObjectList), % get the list of objects
    consumable_helper(AgentList, ObjectList, Agent.subtype, [], Coordinates),
    \+ Coordinates = []. % If coordinates is empty, return false



% 5 - These predicates finds the coordinates of the nearest agent to the specific agent.
% helper predicates to find the nearest food to the agent.
nearest_agent_helper([], _, _, List, List).
nearest_agent_helper([H|T], Agent, NearestAgentId, List, LastList):-
    NearestAgentId-NearestAgent = H, % Decompose the agent
    agents_distance(Agent, NearestAgent, Dist), % find the distance between the agent and the other agent
    nearest_agent_helper(T, Agent, _, [[Dist, NearestAgentId]|List], LastList).

% helper predicate to find the minimum distance in the list of distance-agent pairs.
% Dist is Distance, MinDist is Minimum Distance, TMinDist is Temporary Minimum Distance
% ID is Agent ID, MinID is Minimum Agent ID, TMinID is Temporary Minimum Agent ID
find_min([[Dist, ID]], [Dist, ID]).

find_min([[Dist, ID]|T], [MinDist, MinID]):-
    find_min(T, [TMinDist, _]), 
    Dist < TMinDist, % check if the distance is less than the temporary minimum distance
    MinDist = Dist, % assign the distance to the minimum distance
    MinID = ID. % assign the id to the minimum id

find_min([[Dist, _]|T], [MinDist, MinID]):-
    find_min(T, [TMinDist, TMinID]),
    Dist >= TMinDist, % check if the distance is greater than or equal to the temporary minimum distance
    MinDist = TMinDist,
    MinID = TMinID.

find_nearest_agent(State, AgentId, Coordinates, NearestAgent):-
    State = [Agents, _, _, _],
    Agent = Agents.AgentId,
    del_dict(AgentId, Agents, _, NewAgents), % delete the agent from the agents
    dict_pairs(NewAgents, _, AgentList),
    nearest_agent_helper(AgentList, Agent, _, [], LastList),
    find_min(LastList, [_, MinID]), % find the minimum distance in the list
    NearestAgent = NewAgents.MinID, 
    Coordinates = [NearestAgent.x, NearestAgent.y].



% 6 - These predicates finds the coordinates of the nearest food to the specific agent.
% helper predicate for merge sort doing the split operation.
split([], [], []).
split([X], [X], []).
split([X,Y|T], [X|Left], [Y|Right]):-
    split(T, Left, Right).

% helper predicate for merge sort doing the merge operation.
merge([], Right, Right).
merge(Left, [], Left).

merge([[Agent1, Dist1]|L1], [[Agent2, Dist2]|L2], [[Agent1, Dist1]|Merged]):-
    Dist1 =< Dist2, % check if the distance of the first agent is less than or equal to the distance of the second agent
    merge(L1, [[Agent2,Dist2]|L2], Merged).

merge([[Agent1, Dist1]|L1], [[Agent2, Dist2]|L2], [[Agent2, Dist2]|Merged]):-
    Dist1 > Dist2, % check if the distance of the first agent is greater than the distance of the second agent
    merge([[Agent1,Dist1]|L1], L2, Merged).

% helper predicates for find_nearest_food predicate doing merge sort.
merge_sort([], []).
merge_sort([X], [X]).

merge_sort(List, Sorted):-
    split(List, Left, Right), % split the list
    merge_sort(Left, SortedLeft), % sort the left part
    merge_sort(Right, SortedRight), % sort the right part
    merge(SortedLeft, SortedRight, Sorted). % merge the sorted left and right parts

% helper predicate to find the nearest food to the agent.
% helper predicate for wolf agent.
nearest_food_helper_wolf([], _, _, List, List).
nearest_food_helper_wolf([HA|TA], _, Agent, List, FinalList):-
    _-FoodAgent = HA,
    SubtypeFoodAgent = FoodAgent.subtype,
    (   can_eat(wolf, SubtypeFoodAgent) % check if the agent can eat the food
    ->  (   agents_distance(Agent, FoodAgent, D), % find the distance between the entities
            nearest_food_helper_wolf(TA, _, Agent, [[FoodAgent, D]|List], FinalList)
        )
    ;   nearest_food_helper_wolf(TA, _, Agent, List, FinalList)
    ).

% helper predicate for other agents.
nearest_food_helper_others(_, [], _, List, List).
nearest_food_helper_others(_, [H|T], Agent, List, FinalList):-
    SubtypeAgent = Agent.subtype,
    _-Object = H,
    SubtypeObject = Object.subtype,
    (   can_eat(SubtypeAgent, SubtypeObject) % check if the agent can eat the food
    ->  (   agents_distance(Agent, Object, D), % find the distance between the entities
            nearest_food_helper_others(_, T, Agent, [[Object, D]|List], FinalList)
        )
    ;   nearest_food_helper_others(_, T, Agent, List, FinalList)
    ).

find_nearest_food(State, AgentId, Coordinates, FoodType, Distance):-
    State = [Agents, Objects, _, _],
    Agent = Agents.AgentId, 
    dict_pairs(Objects, _, ObjectList), % get the list of objects
    dict_pairs(Agents, _, AgentList), % get the list of agents
    (   Agent.subtype = wolf
    ->  nearest_food_helper_wolf(AgentList, _, Agent, [], FinalList)
    ;   nearest_food_helper_others(_, ObjectList, Agent, [], FinalList)
    ),
    merge_sort(FinalList, SortedList), % sort the list
    !,
    member([NearestFood, MinDistance], SortedList), % get the nearest food and the distance
    Coordinates = [NearestFood.x, NearestFood.y],
    FoodType = NearestFood.subtype,
    Distance = MinDistance.



% 7 - These predicates moves the agent to the specific coordinates.
move_to_coordinate(State, AgentId, X, Y, ActionList, DepthLimit):-
    move_helper(State, AgentId, X, Y, ActionList, DepthLimit). % call the helper predicate to move the agent to the coordinates

move_helper(State, AgentId, X, Y, ActionList, _):-
    State = [Agents, _, _, _],
    Agents.AgentId.x = X, Agents.AgentId.y = Y, % check if the agent is at the coordinates
    ActionList = []. % return an empty list

move_helper(State, AgentId, X, Y, ActionList, DepthLimit):-
    DepthLimit > 0,
    DepthLimit1 is DepthLimit - 1,
    State = [Agents, _, _, _],
    can_move(Agents.AgentId.subtype, Action), % check if the agent can move
    move(State, AgentId, Action, NewState), % move the agent
    ActionList = [Action|T],
    move_helper(NewState, AgentId, X, Y, T, DepthLimit1).
    


% 8 - These predicates moves the agent to the nearest food.
% It uses the find_nearest_food and move_to_coordinate predicates to move the agent to the nearest food.
move_to_nearest_food(State, AgentId, ActionList, DepthLimit):-
    find_nearest_food(State, AgentId, [X,Y], _, _), % find the coordinates of the nearest food
    move_to_coordinate(State, AgentId, X, Y, ActionList, DepthLimit). % move to the coordinates of the nearest food



% 9 - These predicates implements that one specific agent consumes all the consumable foods in the farm.
% It uses the find_nearest_food, and add_list predicates to consume all the consumable foods.
% helper predicate to append items to a list.
add_list([], List, List).
add_list([H|T], List, [H|NewList]) :-
    add_list(T, List, NewList).

% helper predicates to find the minimum distance to the nearest food. It uses breaedth first search algorithm.
bfs([(State, Distance)|_], AgentId, Goal, _, Distance, NewState, _) :-
    State = [Agents, _, _, _],
    Goal = [Agents.AgentId.x, Agents.AgentId.y], % check if the agent is at the goal
    eat(State, AgentId, NewState). % consume the food

bfs([(State, Dist) | T], AgentId, Goal, Visited, Distance, NewState, DepthLimit) :-
    findall((NewState, Dist1), 
        (
            State = [Agents, _, _, _],
            can_move(Agents.AgentId.subtype, Action), % check if the agent can move
            move(State, AgentId, Action, NewState),
            \+ member(NewState, Visited), % check if the state is visited
            Dist1 is Dist + 1,
            Dist1 =< DepthLimit % check if the depth limit is reached
        ),
    AdjacentStates),
    add_list(T, AdjacentStates, NewList), % add the adjacent states to the list
    findall(S, member((S, _), AdjacentStates), NewVisitedStates), % find the visited states
    add_list(NewVisitedStates, Visited, NewVisited), % add the visited states to the list
    bfs(NewList, AgentId, Goal, NewVisited, Distance, NewState, DepthLimit). % call the breadth first search algorithm

consume_all(State, AgentId, NumberOfMoves, Value, NumberOfChildren, DepthLimit) :-
    find_nearest_food(State, AgentId, _, _, _), % check if there is no food in the farm
    consume_all_helper(State, AgentId, 0, NumberOfMoves, Value, NumberOfChildren, DepthLimit), % call the helper predicate to consume all the consumable foods
    NumberOfMoves > 0.
    
consume_all_helper(State, AgentId, MidNumberOfMoves, NumberOfMoves, Value, NumberOfChildren, DepthLimit) :-
    find_nearest_food(State, AgentId, [X, Y], _, _), % find the coordinates of the nearest food
    State = [Agents, _, _, _],
    bfs([(State, 0)], AgentId, [X,Y], [[Agents.AgentId.x, Agents.AgentId.y]], Distance, NewState, DepthLimit), % find the minimum distance to the nearest food
    !,
    MidNumberOfMoves1 is MidNumberOfMoves + Distance, % increase the number of moves
    consume_all_helper(NewState, AgentId, MidNumberOfMoves1, NumberOfMoves, Value, NumberOfChildren, DepthLimit). % call the helper predicate to consume all the consumable foods

consume_all_helper(State, AgentId, MidNumberOfMoves, MidNumberOfMoves, Value, NumberOfChildren, _) :-
    State = [Agents, _, _, _],
    value_of_farm(State, Value), % find the value of the farm
    NumberOfChildren = Agents.AgentId.children. % find the number of children of the agent
