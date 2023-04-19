Here is a list of advanced functional techniques used in the whole project:

Language Extensions:
LambdaCase: Allows pattern matching on lambda expressions.
OverloadedStrings: Allows string literals to be overloaded.
RecordWildCards: Allows the use of wildcards in record patterns.
ViewPatterns: Enables pattern matching on the result of a function.

Higher-order functions:
map, foldl, any, and other standard higher-order functions from Prelude are used.

Monads and monadic functions:
DiscordHandler: A monad for Discord-related operations.
MonadIO and liftIO: To lift IO actions into the DiscordHandler monad.
Control.Monad functions like 'when' and 'void' are used for control flow and discarding results.

Software Transactional Memory (STM):
TVar: A transactional variable used for shared mutable state.
newTVarIO, readTVarIO, modifyTVar': Functions to create, read, and modify transactional variables.
atomically: To perform STM actions atomically.

Function composition:
The use of (.) for function composition is present throughout the code.

Partial function application:
Partial function application is used in various places in the code, allowing for more concise expressions.

Pattern matching and guards:
Extensive use of pattern matching on data constructors and guards for control flow.

Custom data types and type aliases:
Custom data types like GameState, Command, and Board, and type aliases like Text are used to enhance code readability and maintainability.
These techniques contribute to the expressiveness, conciseness, and readability of the code, leveraging the power of Haskell as a functional programming language.




Tech used in TicTacToe:
Algebraic Data Types: The code makes use of algebraic data types to represent the board, game state, and cell status. In the DataTypes module, custom data types for Cell, Player, Result, Board, and GameState are defined, providing a strong foundation for the game logic. For example, Cell is defined as Empty or Taken Player, and Player is defined as X or O. These algebraic data types make the code easier to read and understand.

Monads: The code uses the State monad and the StateT monad transformer to handle game state updates. The State monad is used in functions like makeMove and playMove to update the game state and manage side effects in a clean and functional way.

Lazy Evaluation: Lazy evaluation is a core feature of Haskell, and it is utilized throughout the code. For example, in the minimax function, Haskell's lazy evaluation ensures that the entire tree of possible game states is not fully explored, but only as needed. This leads to performance improvements and ensures that the code runs efficiently.

List Comprehensions: In the availableMoves function, a list comprehension is used to generate the list of all legal moves available in the current game state. This demonstrates the expressive power of Haskell and functional programming, allowing for concise and efficient code.

Higher-order functions: Functions like map, maximumBy, minimumBy, and any are used throughout the code. These higher-order functions take other functions as arguments and are an important part of functional programming in Haskell. They allow for more concise, expressive, and maintainable code.


Parser tech

Parser Combinators: The code makes use of parser combinators provided by the Megaparsec library, such as (<|>), try, string, space1, and L.decimal. Parser combinators allow you to build complex parsers by combining smaller, simpler parsers in a modular and composable way. This makes the code more maintainable and easier to expand for additional commands.

Monadic Parsing: The do notation is used in the playUserParser function to sequence parsing actions monadically. Monadic parsing provides a clean and expressive way to handle complex parsing logic, such as consuming input, managing state, and handling errors.

Backtracking: The try combinator is used in the commandParser function to enable backtracking. If a parsing branch fails, the parser can backtrack and try another branch. This is particularly useful when dealing with ambiguous syntax, as it allows the parser to explore multiple possible interpretations of the input before settling on the correct one.

Alternative Combinator: The <|> combinator is used in the commandParser function to specify alternative parsing branches. This combinator allows you to define a parser that attempts to match any one of several possible inputs. In this case, the commandParser tries to parse user commands (using playUserParser), bot commands (using playBotParser), or the help command (using helpParser).

Modularity: The code is organized into modular functions, each responsible for parsing a specific command. This makes it easy to extend the parser with new commands in the future, simply by defining new parsing functions and combining them with the existing ones.


type tech

Algebraic Data Types: The code makes use of algebraic data types to represent the game's core entities, such as Player, Result, and Cell. For example, Player is defined as X | O, and Cell is defined as Empty | Taken Player. These algebraic data types make the code more expressive and easier to understand.

Type Synonyms: The code uses type synonyms to provide more descriptive names for certain data types. For example, Board is defined as Matrix Cell, and GameState is defined as (Board, Player). Type synonyms improve code readability and help clarify the purpose of each data type.

Type Aliases: The CommandParser type alias is used to simplify the type signature of the command parser functions. By defining CommandParser as Parsec Void String, the code becomes more readable and easier to maintain.

Importing External Types: The code imports the Matrix type from the Data.Matrix module and the UserId type from the Discord.Types module. These external types are used in the definitions of Board and PlayUser, respectively. Importing external types allows the code to leverage existing libraries and simplifies the implementation of the Tic Tac Toe game.

Parsing-related Data Types: The Command data type is defined as an algebraic data type with three constructors: PlayBot, PlayUser UserId, and Help. This data type is used to represent the different commands that the command parser can recognize. The CommandParser type alias, as mentioned earlier, simplifies the type signature for command parser functions.