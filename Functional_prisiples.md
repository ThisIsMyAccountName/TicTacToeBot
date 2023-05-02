### Here is a list of advanced functional techniques used in the whole project:

#### Language Extensions:
##### LambdaCase: 
- Allows pattern matching on lambda expressions.
##### OverloadedStrings: 
- Allows string literals to be overloaded.
##### RecordWildCards: 
- Allows the use of wildcards in record patterns.
##### ViewPatterns: 
- Enables pattern matching on the result of a function.

### Higher-order functions:

#### Monads and monadic functions:
##### DiscordHandler: 
- A monad for Discord-related operations.
##### MonadIO and liftIO: 
- To lift IO actions into the DiscordHandler monad.
Control.Monad functions like 'when' and 'void' are used for control flow and discarding results.

#### Software Transactional Memory (STM):
##### TVar: 
- A transactional variable used for shared mutable state.
##### newTVarIO, readTVarIO, modifyTVar': 
- Functions to create, read, and modify transactional variables.
##### atomically: 
- To perform STM actions atomically.

##### Function composition:
The use of (.) for function composition is present throughout the code.

##### Pattern matching:
Extensive use of pattern matching on data constructors for control flow.

##### Custom data types and type aliases:
Custom data types like GameState, Command, and Board, and type aliases like Text are used to enhance code readability and maintainability.
These techniques contribute to the expressiveness, conciseness, and readability of the code.




### Tech used in TicTacToe:

The TicTacToe code employs various techniques to enhance its functionality and clarity.

To represent the game state, board, and cell status, algebraic data types are utilized. Custom data types are defined for Cell, Player, Result, Board, and GameState in the DataTypes module. This provides a sturdy foundation for the game logic and makes the code easy to comprehend. For instance, Cell is defined as either Empty or Taken by a Player, and Player is defined as X or O.

To update the game state, the State monad is employed in the code. They manage side effects and update game state in a clean and functional way, with functions such as makeMove and playMove using the State monad.

The minimax function performs a depth-first search of the game tree, which is a computationally expensive operation. However, the function is deterministic and has no side effects, so there is no need for lazy evaluation.

List comprehension is used in the availableMoves function to generate the list of all legal moves. This expressive technique allows for concise and efficient code in functional programming.

Finally, higher-order functions such as map, maximumBy, minimumBy, and any are used throughout the code. These functions take other functions as arguments, which is an important feature of functional programming in Haskell, enabling concise, expressive, and maintainable code.


### Parser tech

Parser Combinators: The code makes use of parser combinators provided by the Megaparsec library, such as (<|>), try, string, space1, and L.decimal. Parser combinators allow you to build complex parsers by combining smaller, simpler parsers in a modular and composable way. This makes the code more maintainable and easier to expand for additional commands.

Monadic Parsing: The do notation is used in the playUserParser function to sequence parsing actions monadically. Monadic parsing provides a clean and expressive way to handle complex parsing logic, such as consuming input, managing state, and handling errors.

Backtracking: The try combinator is used in the commandParser function to enable backtracking. If a parsing branch fails, the parser can backtrack and try another branch. This is particularly useful when dealing with ambiguous syntax, as it allows the parser to explore multiple possible interpretations of the input before settling on the correct one.

Alternative Combinator: The <|> combinator is used in the commandParser function to specify alternative parsing branches. This combinator allows you to define a parser that attempts to match any one of several possible inputs. In this case, the commandParser tries to parse user commands (using playUserParser), bot commands (using playBotParser), or the help command (using helpParser).

Modularity: The code is organized into modular functions, each responsible for parsing a specific command. This makes it easy to extend the parser with new commands in the future, simply by defining new parsing functions and combining them with the existing ones.


### type tech

The code for the Tic Tac Toe game employs various techniques to make it more expressive and easy to understand.

Algebraic data types are used to represent the game's core entities, such as Player, Result, and Cell. For instance, Player is defined as either X or O, and Cell is defined as either Empty or Taken by a Player. This makes the code more expressive and easier to comprehend.

Type synonyms are used to provide more descriptive names for certain data types. Board is defined as Matrix Cell, and GameState is defined as (Board, Player), making the code more readable and helping to clarify the purpose of each data type.

Type aliases are used to simplify the type signature of command parser functions. The CommandParser type alias is defined as Parsec Void String, which enhances code readability and ease of maintenance.

External types such as Matrix type from the Data.Matrix module and the UserId type from the Discord.Types module are imported to simplify the Tic Tac Toe game's implementation. The external types allow the code to leverage existing libraries and make it more manageable.

The Command data type is defined as an algebraic data type with three constructors: PlayBot, PlayUser UserId, and Help. It is used to represent the different commands recognized by the command parser. The CommandParser type alias simplifies the type signature for command parser functions.

### TVar tech
The program declares gameStatesVar and twoPlayerGameStatesVar as TVars of type TVar a, where "a" is the game state data structure. These variables are mutable variables for concurrent settings with atomic read and write operations.

They hold the current state of Tic-Tac-Toe games for both single and two-player modes. The newTVarIO function initializes new TVars with the starting game state.

The readTVarIO function reads the current value of the TVar atomically, while modifyTVar' modifies the value atomically inside the atomically function.

The gameStatesVar TVar holds the current state of the game for single-player mode. Whenever the user clicks a button, the playMove function updates the game state with the user's move and stores the new state in gameStatesVar. isWinner, isDraw, and playBotMove functions are used to determine if the game is over and to display a message to the user accordingly.

The twoPlayerGameStatesVar TVar holds the current state of the game for two-player mode. Whenever a user clicks a button, playMove updates the game state with the user's move and stores the new state in twoPlayerGameStatesVar. getTwoPlayerGameState and deleteTwoPlayerMapState functions are used to manipulate the state of the game for two-player mode.