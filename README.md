# Racket_project



# Open questions:

1. 
Ans:

Types in SOL language:

  [Set  SET] - set
  [Smult Number SOL] - mult every element of the set with the number
  [Inter SOL SOL] - intersection
  [Union SOL SOL] - union
  [Id    Symbol]- identifier symbol
  [Fun   Symbol Symbol SOL] - function
  [CallS SOL SOL SOL] - static call
  [CallD SOL SOL SOL] - dynamic call

2.
Ans:
For overcome the number of parameters I exchanged the 'with statement with Call-Static and Fun
 and I used static calling to keep the envarioment error free from exchanging the symbols.
I will explain, 
 I choose to use "CallS" for several reasons when parsing with expressions :
    1) Because of the CALL STATIC definition.
       as I want to connect a name to experation, and also I needed that the connection will be to the value
       in that moment.
       On static model the function ran in the environment in which it was defined
       and all the variables recieve their values on definition and not on run time.
    2) The tests given to me the use of CALLS was quite clear so the guidence of the tests let me decide to use Stativlly.
    



3.
Ans:
 Tail Reqursion:
 
 * set-sorted-set
 * set-union
 * set-intersectiuon

 Just Reqursion:
 * ismember?
 * remove-duplicates

regular reqursion is a function that keep calling itself until a return statement is applied and then the function re-calling itself back.
which leads to use of growing memory in dynamic form. which can lead to stack overflow as the stack keep on filled with each reqursive call.

Tail reqursion on the other hand make use of another function which will do the reqursive calls.
that make the the function to return the value of the reqursion as soon as we hit the return statement - less memory consumption. 

4.
Ans:
In the GlobalEnv, I only used CALLS in all of them(In SECOND, FIRST and CONS).
But it is actually possible to combine and use CALLD as well but only within CONS.
First and second else use only CALLS.
This is because I wanted the evel of the body to be familiar with x and y thay inside what we recived from 'cons', and
also to prenvent the user from changing their behavior.
and CONS can use also CALLS or CALLD it depends on whether I want to give the user the option to change the CONS setting or not.

5.
Ans: 
In CONS it does not matter if we write CALLS or CALLD it will work one way or the other.
Regarding others it will also not cause any change because in our environment we have already set FIRST and SECOND with CALLS
so they already have preset values that will not change so there will be no difference.
