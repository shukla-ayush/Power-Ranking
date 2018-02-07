# Power-Ranking
A ranking system that takes as input various matches between competitors
Provides the Ranking following these rules

-Given a list of outcomes, a competitor A outranks a competitor B if any of the following are true:

 - One of the outcomes shows that A has defeated B.
 - One of the outcomes shows that A and B have tied.
 - There is a competitor C that outranks B according to the list of outcomes, and there is an outcome that shows A has defeated or tied C.

Implemented using Racket, a functional programming language.

