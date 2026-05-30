---
id: 0001-fix-two-tests-that-fail-on-slower-lisps
type: change
status: done
title: Fix two tests that fail on slower Lisps
---

# Fix two tests that fail on slower Lisps

In clamiga (a Commoin Lisp implementation) two tests fail, see below.
Probably it is due to some timings.
Please check source and relax the timings.


Failing tests:
Did 535 checks.
    Pass: 533 (99%)
    Skip: 0 ( 0%)
    Fail: 2 ( 0%)

 Failure Details:
 --------------------------------
 ACTOR-OF--LIMIT-QUEUE-SIZE in ACTOR-TESTS [Tests that actor's queue size can be limited.]: 
      
(LENGTH (FILTER (LAMBDA (X) (IF X X)) TELLS))

 evaluated to 

2

 which is not 

=

 to 

1


 --------------------------------
 --------------------------------
 ACTOR-OF--LIMIT-QUEUE-SIZE in ACTOR-TESTS [Tests that actor's queue size can be limited.]: 
      
(LENGTH (FILTER (FUNCTION NULL) TELLS))

 evaluated to 

8

 which is not 

=

 to 

9
