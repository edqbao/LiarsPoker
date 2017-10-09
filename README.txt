Tristan Stone
Michelle Ip
James McManus
Ed Bao

README:
To start game, simply type 'make play' into terminal.
Once game is started, enter number of players (2 to 9 inclusive) and difficulty (1 to 3 inclusive, with 1 as easiest)
For game itself, type 'Raise [pokerhand]' or 'BS' as a move. This is not case sensitive, and number of spaces does not matter.
Type 'numcard' to display how many cards are in play
Type 'help' for display of valid commands

Valid ways to write poker hands are as follows:
FOUR OF A KIND (four) followed by the rank. 
Ex: four 4
FULL HOUSE (fh) followed by the rank of the three of a kind and the rank of the pair 
Ex: fh 3 aces
STRAIGHT followed by the highest card in the straight
Ex: Straight 9 is 5, 6, 7, 8, 9.
THREE OF A KIND (three) followed by the rank
TWO PAIR (tp) followed by the rank of the first pair, then the rank of the second pair
PAIR followed by the rank
HIGH CARD (hc) followed by the rank
Valid ranks include the numbers 2-14(s), jack(s), queen(s), king(s) and ace(s)
Valid ranks for a STRAIGHT do not include the numbers 2-5, because
the lowest possible straight is: 2,3,4,5,6

As before, inputted pokerhands are case insensitive, and the number of spaces does not matter. In parentheses are shortcuts for each hand.
In addition, input will accept hands such as 'raise pair 2s' or 'raise pair 2'