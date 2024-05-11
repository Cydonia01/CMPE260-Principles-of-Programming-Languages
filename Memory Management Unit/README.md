# CMPE 230 – Project 1

This project can be split up into two parts: parsing the sentences and executing them. We parsed the input in the main.c file. We have the inventory management functionality in inventory.h file and we have another file to sort of link them called execute.h. We created those additional files as header files and statically linked them to the program.

## General Program Flow

The program works in the terminal and executes one line of input each time just like the Python interpreter. It first parses the input, then executes it according to the conditions. After that, it expects a new line of input. If the input is valid, it prints “OK” and executes accordingly. If the input is a question, it parses it like others, then prints the answer. If the input is invalid it prints “INVALID” and does nothing, then continues normally. If the input is “exit”, it terminates the program.

## Parsing

We have an operations array which consists of operation structs. In operation struct, we have fields: type, name, subjects, items, subject, item, location. Type can be ‘a’, ‘c’ and ‘q’ which represent action, condition, question respectively. Name can be “go to”, “sell”, “sell to”, “has”, “where” etc. Every field is not required for every operation. When executing the operations, we only use the meaningful fields.

We first tokenized the input by spaces. Then we looped through the tokens. We parsed sentences into operation structs using a state variable and considering the next token. For example, if the state is “buy”, the program expects a numeric value as the next token then a valid item name. If these conditions are not satisfied, state is changed to “invalid” and break the loop. When we encounter “and” keyword, the behavior is determined according to the state. For example, if the state is “subject”, the program expects a valid subject name after the “and”.

After terminal keywords like “from” (which indicate the ending of a sentence), current operation struct is complete, so it creates a new operation and starts filling that one. This also change the state to “finished” and it expects a new operation now. In sentences like “a buy 1 b and 2 c and …”, to determine where the current sentence ends, it checks if the next token is a number or a name. If it is a number, then there will be another item after the “and”. If it is not, finish the sentence and start a new one.

## Execution

After parsing the input, we loop through the operations array to determine which operations to execute depending on the conditions. We used a two-pointer approach for this purpose. Right pointer traverses the array until it reaches a condition. Then it evaluates the condition, if it is false, it moves the right and left pointers to the next operation. If it is true, it continues to the next operation in the array. If the next operation is an action, it marks previous actions (those coming after the left pointer) to be executed. If the next operation is a condition, then it evaluates that condition and repeats the same process. After marking the operations to be executed or not, we call appropriate functions from inventory management system to execute them. Lastly, after executing one line of input, we cleared all the memory associated with that operations array to avoid memory leaks.

## Inventory Management

We have an inventories array which consists of inventory structs. An inventory struct holds the information about an individual subject and has fields: name, items, quantities, location. When a subject is first mentioned in a sentence, it creates an inventory for that subject with appropriate initial values. Then executes given operations using those inventories and the operation struct. For conditions, we have functions to evaluate condition operations and return 0 or 1.

In “buy from”, “sell” and “sell to” operations, it first checks if all the subjects selling the item has enough items. If they have, it makes necessary transactions. Otherwise, it does nothing. When printing items of a new subject or printing its location, it creates their inventory then prints the initial values accordingly, for example “NOWHERE” for location.

## Testing

You can test the program with python.  
First compile the program with this command:

```console
make
```

Then use one of the following command according to your system:

```console
make grade
```

```console
python3 test/grader.py ./ringmaster test-cases
```

```console
python test/grader.py ./ringmaster test-cases
```

```console
py test/grader.py ./ringmaster test-cases
```
