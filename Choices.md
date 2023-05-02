# Reasoning of choices

### TicTacToe GameState and storage
In the TicTacToe project, the State monad is used to keep track of the game state. This choice is a good one because it allows us to encapsulate the state of the game and simplify the logic of the functions. By using the State monad, we can write our functions as pure functions that operate on a GameState, rather than as impure functions that modify a mutable state. This makes it easier to reason about the behavior of our functions and to test them in isolation.

Using the State monad also allows us to sequence the actions that modify the game state. For example, when we make a move, we first check if the cell is empty, and if it is, we update the board and change the current player. By sequencing these actions in the State monad, we can ensure that they happen in the correct order and that the state is always consistent.

Overall, using the State monad to store the game state is a good choice because it allows us to write simpler, more modular code that is easier to reason about and test.

### Parser for userinput
Using a parser to parse user input is generally a good choice because it provides a structured way to extract meaningful data from a string. Parsers can handle complex input formats and validate the input against a set of rules, which helps to prevent errors and ensure consistency in the data. 

In this particular case, the parser is used to extract commands from a string, which can be used to control the behavior of a Discord bot. Parsers are well-suited for this task because they can handle a variety of input formats, such as commands with or without arguments, and can return structured data that can be easily processed by the bot.
 
Additionally, parsers can be easily composed and combined to handle more complex input scenarios, which makes them a flexible and powerful tool for handling user input. Although its not used that way in this case. Overall, using a parser to parse user input is a good choice in many scenarios where structured data needs to be extracted from input strings.

### TVar for storing gamestate
In this implementation, TVars are used to ensure thread-safe access to shared data structures that store game state information. By using TVars, we can safely modify the shared game state data structures from multiple threads without the risk of data races, deadlocks, or other synchronization errors.

Another advantage of using TVars is that they allow us to implement a functional style of programming, where state changes are modeled as a series of atomic transactions that can be composed and manipulated using higher-order functions. This makes it easier to reason about the behavior of the program and to test it in isolation.

Overall, using TVars is a good choice for managing shared state in a concurrent environment like this one because it simplifies synchronization, promotes consistency and correctness, and facilitates functional programming paradigms.