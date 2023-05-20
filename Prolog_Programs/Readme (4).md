Instructions for running the Assignment 5 (4 simple prolog problems). We will start by installing prolog and then opening a terminal
in the directory where all the 4 files are present. Then we will type swipl in the terminal which will open the interface for the prolog
programs and after that we will just consult files one by one using consult("name.P"). command. 

Problem 1 :  In this problem, first just consult the subseq.P file in the prolog opened interface using consult("subseq.P"). and then we can give inputs of the form of subsequence(S,L). where S is the list of elements occuring in that order in L. After that our program will return true or false depending on the values of S and L.


Program 2 : In this problem, first just consult the triplicates.P file in the prolog opened interface using consult("triplicates.P"). and then we can give inputs of the form of has_no_triplicates(L). where L is the list of elements in which we have to tell whether there are elements occuring thrice or not. After that our program will return true or false depending on the values of L.


Program 3 : In this problem, first just consult the arith.P file in the prolog opened interface using consult("arith.P"). file and then we can give inputs of the form of arith(L). where L is the list of elements in which we have to add symbols i.e. +, -, = to make it a correct equation. In this I have assumed that -- is a valid operator and it will be equivalent to +. Similarly +- and -+ are also valid and are equivalent to -. The above are possible only when there are already negative numbers present in the list we are given. Finally we will get one of the equation printed if there exists a solution for it otherwise it will be evaluated to false. After that our program will return true or false depending on the possibility of correct equation.
NOTE : We had to print only one of the valid equation and therefore i have just printed one of the equation if there exists one.
If you want more solutions just press ; after one of the solution is printed. If you press enter it will end there only.


Program 4: In this problem, first just consult the ABCD.P file in the prolog opened interface using consult("ABCD.P"). file and then we can give inputs of the form of abcd(Crossings). where Crossings is just the name of the variable and we have to put it directly in abcd. After that solutions of the problem statement will be printed. There are total 6 types of solution possible and in terminal it will print only one. I have tried printing all the solutions in the online terminal and all of them were possible. 
NOTE : We had to print valid solutions and therefore i have not restricted the number of solutions to 1 only and all solutions are printed.
If you want to print all solutions just press ; after one solution is printed. if you press enter it will end there only.


Ways to run the queries with an example:

Problem 1:
?- subsequence([1,2], [1,2,5,6,8]).

Problem 2:
?- has_no_triplicates([1,1,4,6]).

Problem 3: 
?- arith([1,1,2]).

Problem 4:
?- abcd(Crossings).



