-module(sequential).
-export([is_palindrome/1]).

% Palindrome.
% Checks if the string can be read the same way in either directions, in spite 
% of spaces, punctual and letter cases. Uses a regular expresion to filter.
is_palindrome(Str) ->
    FilteredStr = re:replace(Str, "[^A-Za-z]", "", [global,{return, list}]),
    LowerCaseStr = string:to_lower(FilteredStr),
    string:equal(LowerCaseStr, lists:reverse(LowerCaseStr)).
        
