# TicTacToeDiscordBot

This program is a Discord bot that allows users to play Tic Tac Toe against each other or against the bot. The bot uses a minimax algorithm to make its moves. The program is written in Haskell and uses the Discord API to interact with Discord servers, with help from the `haskell Discord` library.

Here is a step by step guide to setup the bot for your own server:


#### Clone the repository
To get started, you'll need to clone the project's repository onto your local machine. You can do this using the git clone command in your terminal:


`git clone https://github.com/ThisIsMyAccountName/TicTacToeBot.git`


#### Edit the bot key
The next step is to edit the bot key in the Tokens.hs file. You should have received a bot key from the Discord Developer Portal when you registered your bot. Open the Tokens.hs file in your preferred code editor and find the following line:

`getToken = TIO.readFile "Path_To_A_.Secret_file" `

#### Change the ready channel
The bot is currently set up to send a message in a channel called "ready". If you'd like to change the channel where the bot sends the message or disable the ready message, you'll need to modify the readyChannel variable in the Main.hs file. You can do this by replacing `readyChannel = 1084744206238621747` with the ID of the channel you want to use or by changing `sendOnReady` to False.


#### Change the emote
The bot sends a message with a custom emote at the end. If you'd like to change the emote to something else, you'll need to modify the emote variable in the Main.hs file. You can do this by replacing ":robot:" with the emote you want to use:


`emote = ":robot:"`

#### Run the project
With your changes saved, you can now run the project. You can do this using either Cabal or Stack. In your terminal, navigate to the root directory of the project and enter one of the following commands:


`cabal run`
or
`stack run`

This will compile the project and start the bot. The first time this can take some time as you might need to install alot of librarys and depadencies. The bot will then be online and listening for commands in the Discord server you added it to.



### Usage
##### `!play`
By typing `!play` in any chat the bot has access to it will start a game of tictactoe, The user will always start, and the bot will never lose.
##### `!play @user`
by typing `!play @user` you will start a game against the user you tag, only you and that player can make moves on that game.
##### `!help`
By typing `!help` you will be given the two other command in a message in the channel you sendt it in.