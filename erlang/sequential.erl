-module(sequential).
-export([is_palindrome/1, factor/1]).

% Palindrome.
% Checks if the string can be read the same way in either directions, in spite 
% of spaces, punctual and letter cases. Uses a regular expresion to filter.
is_palindrome(Str) ->
    FilteredStr = re:replace(Str, "[^A-Za-z]", "", [global,{return, list}]),
    LowerCaseStr = string:to_lower(FilteredStr,)
    string:equal(LowerCaseStr, lists:reverse(LowerCaseStr)).

% Factors
% Given a number, computes its prime factors.
factor(N) when N>0 -> factor(N,[],2).

factor(N,R,I) when (I*I > N) -> [N|R];                          
factor(N,R,I) when ((N rem I) =:= 0) -> factor(N div I, [I|R], I);
factor(N,R,I) -> factor(N,R,I+1).


