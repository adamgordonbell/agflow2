DESCRIPTION
-----------

A Fruit machine has 3 reels (rolls). Each reel contains `n` numbers from 1 to `n` (precisely it is a permutation of 1..n sequence ).
On each play the Fruit Machine rotates its reels, each one independently. You win if there is a winning row.
The winning row is a state on the machine where all numbers in a row are the same.
Bajtek wants to play the Fruit Machine, but he is worried he might lose too much money.
Help him to figure out what his chance of winning is.

Given a fruti machine instance:
1) How many winning rows are there?

2) Write an algorithm which computes the maximum number of subsequent winning rows which are possible to achieve for the Fruit Machine in a single game. Your algorithm should work in a reasonable time.

INPUT
-----

First line specifies `n`, the size of reel
Next 3 lines each gives `n` numbers, a description of a reel

Constrains:

* 1 <= n <= 300000.


OUTPUT
------

Output one number - answer for question 2)


EXAMPLE
-------

Input:
5
1 5 4 3 2
1 3 2 4 5
2 1 5 4 3

Output:
3

Explanation:
You can reach 3 subsequent winning rows by rotating:
* first reel 3 positions up
* second reel 1 position up
* third reel 1 position down.

Using encoding from task it will look like:
3 2 1 5 4
3 2 4 5 1
3 2 1 5 4


LIMITS:
-------

Time limit:     10s
Source limit:   5KB
Memory limit: 128MB
Deadline:      26h
