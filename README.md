# Alchemists tools
This project is a collection of tools to aid players of the game [Alchemists](https://boardgamegeek.com/boardgame/161970/alchemists). So far, it can completely automate the deduction grid (the grid players use to keep track of which ingredients might be assigned to which alchemicals). Players can record the events that have happened in the game, and these tools will let them query useful information about the remaining assignment possibilities.

This project is designed to be used from the Haskell repl. To start the repl, run the following in the root directory:

```
cabal repl
```

Let's work through a sample game.

During the first round of the game, you try combining the bird's foot and the toad, and discover they create a potion of wisdom.

```
*Tools.Game.Alchemists.Cli> let a1 = filter (createsPotion FOOT TOAD BLUE_PLUS) allAssignments
```

You also discover that combining a toad and a flower produces a poison.

```
*Tools.Game.Alchemists.Cli> let a2 = filter (createsPotion TOAD FLOWER RED_MINUS) a1
```

During the second round, you have the opportunity to sell a healing potion to an adventurer. You're not sure how to make one, but you consult your deduction grid.

```
*Tools.Game.Alchemists.Cli> printPotionProbs RED_PLUS a2
         MUSHROOM SPROUT TOAD   FOOT FLOWER   ROOT SCORPION FEATHER
MUSHROOM           20.0%       40.0%         20.0%    20.0%   20.0%
  SPROUT    20.0%              40.0%         20.0%    20.0%   20.0%
    TOAD
    FOOT    40.0%  40.0%                     40.0%    40.0%   40.0%
  FLOWER
    ROOT    20.0%  20.0%       40.0%                  20.0%   20.0%
SCORPION    20.0%  20.0%       40.0%         20.0%            20.0%
 FEATHER    20.0%  20.0%       40.0%         20.0%    20.0%
 ```
 
 Hmm, the odds aren't great. But you do have a bird's foot and a scorpion in your hand, and the odds of that combination working are tied for the best. You try it and ... fail. You got one gold, which means you didn't even get the sign right.
 
 ```
 *Tools.Game.Alchemists.Cli> let a3 = filter (salesResult FOOT SCORPION RED_PLUS 1) a2
 ```
 
 Let's see what we've discovered so far:
 
 ```
 *Tools.Game.Alchemists.Cli> printAlchemicalProbs a3
     MUSHROOM SPROUT   TOAD   FOOT FLOWER   ROOT SCORPION FEATHER
AL_1    17.9%  17.9%                28.6%  17.9%            17.9%
AL_2                        100.0%
AL_3    10.7%  10.7%                       10.7%    57.1%   10.7%
AL_4     7.1%   7.1%  42.9%         28.6%   7.1%             7.1%
AL_5     3.6%   3.6%  57.1%         28.6%   3.6%             3.6%
AL_6    25.0%  25.0%                       25.0%            25.0%
AL_7    10.7%  10.7%                14.3%  10.7%    42.9%   10.7%
AL_8    25.0%  25.0%                       25.0%            25.0%
```

Great! It looks like we have a 100% match on the bird's foot, and we're zeroing in on toad and scorpion.

Now we want to try a potion on ourselves. Let's check the odds that each combination will produce a negative potion:

```
*Tools.Game.Alchemists.Cli> printSignProbs MINUS a3
         MUSHROOM SPROUT   TOAD   FOOT FLOWER   ROOT SCORPION FEATHER
MUSHROOM           33.3%  35.7%  21.4%  42.9%  33.3%    57.1%   33.3%
  SPROUT    33.3%         35.7%  21.4%  42.9%  33.3%    57.1%   33.3%
    TOAD    35.7%  35.7%               100.0%  35.7%    71.4%   35.7%
    FOOT    21.4%  21.4%                14.3%  21.4%   100.0%   21.4%
  FLOWER    42.9%  42.9% 100.0%  14.3%         42.9%    85.7%   42.9%
    ROOT    33.3%  33.3%  35.7%  21.4%  42.9%           57.1%   33.3%
SCORPION    57.1%  57.1%  71.4% 100.0%  85.7%  57.1%            57.1%
 FEATHER    33.3%  33.3%  35.7%  21.4%  42.9%  33.3%    57.1%
 ```
 
 We're looking for a low probability. Obviously we don't want to pick the bird's foot and the toad, because we've already tried that. Let's go with the bird's foot and the flower, that has only a 14% chance of being negative.
 
 And we did it! We got a potion of wisdom.
 
 ```
*Tools.Game.Alchemists.Cli> let a4 = filter (createsPotion FOOT FLOWER BLUE_PLUS) a3
```
 It's now turn 3, and someone has just debunked a theory! The theory about toads was incorrect; toads have a green plus, not a green minus. Let's record that fact.
 
 ```
*Tools.Game.Alchemists.Cli> let a5 = filter (debunkResult TOAD GREEN_MINUS) a4
```

What does our deduction grid look like now?

```
*Tools.Game.Alchemists.Cli> printAlchemicalProbs a5
     MUSHROOM SPROUT   TOAD   FOOT FLOWER   ROOT SCORPION FEATHER
AL_1    25.0%  25.0%                       25.0%            25.0%
AL_2                        100.0%
AL_3    12.5%  12.5%                       12.5%    50.0%   12.5%
AL_4                               100.0%
AL_5                 100.0%
AL_6    25.0%  25.0%                       25.0%            25.0%
AL_7    12.5%  12.5%                       12.5%    50.0%   12.5%
AL_8    25.0%  25.0%                       25.0%            25.0%
```

It's looking fantastic! We know 3 assignments for certain, and we're on our way to finding a few more.
