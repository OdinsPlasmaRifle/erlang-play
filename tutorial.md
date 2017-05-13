# Erlang

Notes on the *Learn You Some Erlang for great good!* [book](http://learnyousomeerlang.com/content).

## The Shell

```
erl
```
End commands with `.`.


### Hotkeys

^A - Beginning of line
^E - End of line

^G - Switch command
    q - exit
    h - help


## Syntax

Line ends are also `.`.

Floating points and numbers can be dealt with in arithmetic.


### Operators

`div` - integer to integer division
`rem` - integer to integer modulo
`#` - base value (eg `2#101010.` outputs `42`)
`and` - and (evaluates both sides always)
`or` - or (evaluates both side ALWAYS)
`andalso` - and (evaluates only right if necessary)
`orelse` - or (evaluates only right if necessary)
`xor` - xor, outputs true when input differs.
`=:=` - exact equal comparison
`=/=` - exact inequality (not equal) comparison
`==` - not exact equality
`/=` - not exact inequality
`<` - less than
`>` - greater than
`>=` - greater than or equal to
`=<` - less than or equal to (wrong way around to normal)


### Variables

Variables can get assigned values once. If value matches original assignment, can pretend to assign.

`=` compares and assigns values.

Variable names must begin with capital letters. Can use `_` as first letter , but only if value is one you don't care about.

Variables with the name just `_` don't store any value. No value gets bound.

`f()` clears all variables (In shell only). `f(Variable)` clears specific variable.


#### Types

**Atoms**

Atoms are literals, constants with their own name as their value:

`atom.` outputs `atom`.

or:

`'Atoms can be cheated!'` outputs `Atoms can be cheated!`

**Tuples**

Group terms when you know how many there are. Fixed length list.

```
X = 10, Y = 4.
Point = {X,Y}.
```

Assign tuple to variables in a backwards manner.

```
Point = {4,5}.
{X,Y} = Point.
X.
```

Final line will output 4.

**Lists**

Grouping of any number of any terms.

```
[1, 2, 3, 4, {5, 5}, atom, {atom, {1, 2}}]
```

Strings are also lists, which means weird things can happen: `[97, 98, 99].` outputs `abc`.

`++` is used to glue lists together.

`--` is used to remove elements.

`hd()` gets the head.

`tl()` gets the tail, everything besides the head.

To add items to the head:

```
List = [2,3,4].
newList = [1|List].
```

`|` is the cons operator (constructor).

```
[a, b, c, d]
[a, b, c, d | []]
[a, b | [c, d]]
[a, b | [c | [d]]]
[a | [b | [c | [d]]]]
[a | [b | [c | [d | [] ]]]]|]
```
Are all the same list, using different constructors.

**List Comprehensions**

Ways to build or modify lists. Based of set notation. Can act in the manner of a filter.

In maths: `{x : x > 0}.`  this means `x > 0.`

or in Erlang: `[2*N || N <- [1,2,3,4]].` means multiply each value of the list by 2.

Examples:

```
[X || X <- [1,2,3,4,5,6,7,8,9,10], X rem 2 =:= 0].
```

Prints out all even numbers. Using constraints that return boolean values.

```
[X-Y || X <- [1,2], Y <- [2,3]].
```

Prints out `[3,4,4,5]`

```
Weather = [{toronto, rain}, {montreal, storms}, {london, fog}, {paris, sun}, {boston, fog}, {vancouver, snow}].
FoggyPlaces = [X || {X, fog} <- Weather].
```

Recipes:

`NewList = [Expression || Pattern <- List, Condition1, Condition2, ... ConditionN].`

or:

`NewList = [Expression || GeneratorExp1, GeneratorExp2,... GeneratorExpN, Condition1, Condition2, ... ConditionM].`


## Modules

Modules are a bunch of functions regrouped into a single file. All functions in Erlang should be defined in a module.

A BIF (Built in function) belong to the `erlang` module. For example `hd()`  or `tl()`. BIFs in the `erlang` module differ from others in that they are automatically imported.

Functions about similar things should be placed within a single module. Example Erlang modules are:

`lists` - manipulates lists
`io` - input output
`erlang` - assortment of generic functions


### Calling Modules

**Syntax**

```
Module:Function(Arguments).
```

Example usage: `erlang:element(2, {a,b,c}).`


### Module Declaration

When writing a module *functions* and* attributes* can be declared.

**Attributes**

Attributes are metadata describing the module itself. Attributes follow this form:

```
-name(Attribute).
```

The only required module atribute is:

```
-module(Name).
```

This attribute defines the name by which this module can be called.

**Functions**

The act of defining which functions are available to a module is called exporting. The syntax is:

```
-export([Function1/Arity, Function2/Arity,..., FunctionN/Arity]).
```

Exported functions represent a module;s interface. Only what is strictly required for usage should be revealed when defining this interface.

An example of an exported function is:

```
-module(useless).
-export([add/2]).

add(A,B) ->
    A + B.
```

Function syntax follows the following rules: `Name(Args) -> Body.`. `Name` is an atom and `Body` contains one or more Erlang expression separated by a comma.

Functions in Erlang have no 'Return' keyword. The last executed logical expression has its value returned instead. However, functions must return something, so when no return expression exists an atom `ok` will be returned.

**Imports**

To import a module rather than always explicitly call it you can use:

```
-import(Module, [Function1/Arity, ..., FunctionN/Arity]).
```

Using `import` is generally discouraged as it reduces readability.

**Macros**

Used to define short functions and constants. Simple expressions represented by text that will be replaced before the code is compiled for the VM.

Macros are used to avoid having magic values floating around your modules. Defined as:

```
-define(MACRO, some_values).
```

Usage in module functions:

```
?MACRO
```

Example macro:

```
-define(sub(X,Y), X-Y).
```

And used as:

```
?sub(23,47).
```

**Metadata**

user `moduleName:module_info()` to see a modules metadata (Once compiled).

Adding extra metadata:

```
-author("En Erlang Boss").
```

## Compiling

Erlang is compiled to bytecode. The compiler can be called from any place with `$ erlc flags file.erl` or in the shell `compile:file(FileName)`

Example:

1) Open shell
2) Navigate to folder: `cd("/path/to/where/you/saved/the-module/").`
3) `c(moduleName).` or `c(useless, [debug_info]).`

Use:

```
moduleName:function(args).
```

It is also possible to place compile flags within the module itself, this is done via:

```
-compile([debug_info, export_all]).
```

Which will run with debug info and the export all flag passed when a normal compile is run.

**Note**

Can also compile code natively using the `hipe` module:

```
hipe:c(Module,OptionsList).
```

or:

```
c(Module,[native]).
```

or in the module itself:

```
-compile([native]).
```

This is up to 20% faster than bytecode.


## Syntax in Functions


### Pattern Matching

Pattern matching helps reduce boilerplate code. Pattern matching can be used to define both what parts of a function should be used and bind the values we need at the same time.

An example of pattern matching within a function is:

```
function(X) ->
    Expression;
function(Y) ->
    Expression;
function(_) ->
    Expression.
```

Each of the above function declarations is called a `function clause`. Function clauses are separated with `;` and together from a function declaration.

Pattern matching can be applied in the same manner to list comprehensions.

```
head([H|_]) -> H.
```

The concept of unbound and bound variables still applies in function declarations:

```
same(X, X) ->
    true;
same(_,_) ->
    false.
```

If a value is the same, the variable can be rebound, but otherwise it cannot change the value. This is known as Invariable Variables.

An advanced example of pattern matching would be:

```
valid_time({Date = {Y,M,D}, Time= {H,Min,S}}) ->
    io:format("The date tuple (~p) says today is: ~p/~p/~p,~n", [Date,Y,M,D]),
    io:format("The time tuple (~p) indicates: ~p:~p:~p.~n", [Time,H,Min,S]);
valid_time(_) ->
    io:format("Stop feeding me wrong data!~n").
```

Looking at the above you can see that any value can be accepted as long as the tuple has the correct format. This is a short-falling of pattern matching and can be solved with `guards`.


### Guards

Guards are additional clauses that make pattern matching more expressive.

Guards are placed in a function's head.

An example of a concept that pattern matching can't solve is counting. For instance whether someone is old enough for something:

```
old_enough(X) when X >= 16 ->
    true;
old_enough(_) ->
    false.
```

The first and most basic rule of guards is that their expression must return `true` to succeed. If it return `false` or throws an exception the guard will fail.

Multiple guards can be added to a function clause:

```
right_age(X) when X >= 16, X =< 104 ->
    true;
right_age(_) ->
    false.
```

The `,` above acts in a similar way to `andalso`. Both guard expressions must evaluate to `true` for the full guard to pass.

A `;` can also be used to represent something similar to an `orelse`:

```
wrong_age(X) X < 16, X > 104 ->
    true;
wrong_age(_) ->
    false.
```

One of the above guard expressions must succeed in order for the guard to pass.

A whole range of functions and comparisons may be used in guards including math and data type operations.

**Note**: one negative point about guards as that they will not accept user defined functions. Erlang doesn't trust you for this.

**Note**: the `,` and `;` are not really the same as `andalso` or `orelse` The former catch exceptions automatically. The latter will skip the whole expression if exception is thrown. `andalso` and `orelse` may also be nested within guards like `(A orelse B) andalso C`

### If Conditionals

`If`s act in in a similar way to guards, but outside of the function clause's head. An `if` clause is called a *Guard Pattern*.

**Note**: `if`s in Erlang are different to other languages `if`s. Leave all other concepts of `if`s at the door when entering Erlang territory.

In Erlang everything has to return something and `if` expressions are no exception to this rule.

So, in Erlang `if`s there must be a catch all branch that will succeed no matter what, similar to an `else`.

`if`s take this form:

```
oh_god(N) ->
    if N =:= 2 -> might_succeed;
        true -> always_does %% this is Erlang's if's `else!`
    end.
```

A more complex example would be the following:

```
help_me(Animal) ->
    Talk = if Animal == cat -> "meow";
              Animal == beef -> "mooo";
              Animal == dog -> "bark";
              Animal == tree -> "bark";
              true -> "safasdass"
           end,
    {Animal, "says " ++ Talk ++ "!"}.
```

*The above is meant as an example and would have been better done using pattern matching.*

Instead of relying on `true` one can cover all bases with the `if` statement. So, something like this:

```
if X > Y -> a();
   X < Y -> b();
   X == Y -> c()
end
```
As you can see no `true` is required and the `if` statement will evaluate every situation.

Covering all logical ends rather than relying on a "catch all" clause is a is a way to avoid the confusing `true` or `else` branches.

### Case ... of Conditionals

`Case ... of` is a conditional that can have both complex pattern matching and guards at the same time.

Example syntax:

```
insert(X,[]) ->
    [X];
insert(X,Set) ->
    case lists:member(X,Set) of
        true -> Set;
        false -> [X|Set]
    end.
```

or a more complex example:

```
beach(Temperature) ->
    case temperature of
        {celsius, N} when N >= 20, N =< 45 ->
            'favorable';
        {kelvin, N} when N >= 293, N =< 318 ->
            'scientifically favorable';
        {fahrenheit, N} when N >= 68, N =< 113 ->
            'favorable in the US';
        _ ->
            'avoid beach'
    end.
```

The whole of the above could be done with function clauses like this:

```
beachf({celsius, N}) when N >= 20, N =< 45 ->
    'favorable';
...
beachf(_) ->
    'avoid beach'.
```

**Note**: `case...of` and function clauses have the same overhead in terms of performance. Whether you use `if`, `function heads` or `case...of` is more of a personal preference.

## Types

Erlang is **dynamically** typed, which means errors get caught at runtime.

Most languages attempt to make a program error free. Erlang on the other hand assumes that errors will happen and that all these cases are covered.

Meaning, Erlang's dynamic type is not a barrier to reliability and safety of programs.

Erlang is also **strongly** typed. Strongly typed languages cannot do implicit type conversions.

### Type Conversions

Because Erlang is strongly typed type conversions are used regularly.

The process of converting types is called "casting". In Erlang these type conversion functions are all implemented in Built In Functions.

The syntax for a type casting function is always:

```
<type>_<type>
```

And they are implemented in the `erlang` module.

```
erlang:list_to_integer("54").
erlang:integer_to_list(54).
erlang:list_to_float("54.32").
erlang:atom_to_list(true).
erlang:list_to_bitstring("hi there").
erlang:bitstring_to_list(<<"hi there">>).
```

a full list of conversion functions can be found below:

```
atom_to_binary/2, atom_to_list/1, binary_to_atom/2, binary_to_existing_atom/2, binary_to_list/1, bitstring_to_list/1, binary_to_term/1, float_to_list/1, fun_to_list/1, integer_to_list/1, integer_to_list/2, iolist_to_binary/1, iolist_to_atom/1, list_to_atom/1, list_to_binary/1, list_to_bitstring/1, list_to_existing_atom/1, list_to_float/1, list_to_integer/2, list_to_pid/1, list_to_tuple/1, pid_to_list/1, port_to_list/1, ref_to_list/1, term_to_binary/1, term_to_binary/2 and tuple_to_list/1.
```

### Data Type Guards

In order to add guards for data types there are a list of BIFs that can be used. These functions take a single argument and return true if the true is right, otherwise they return false.

Built | In | Functions
------|----|----------
is_atom/1 | is_binary/1 |
is_bitstring/1 | is_boolean/1 | is_builtin/3
is_float/1 | is_function/1 | is_function/2
is_integer/1 | is_list/1 | is_number/1
is_pid/1 | is_port/1 | is_record/2
is_record/3 | is_reference/1 | is_tuple/1

These can be used like any other guard expression, wherever guard expressions are allowed.

**Note**: There are several additional BIFs (not dedicated to indicating data types) that can be used in guard expression:

```
abs(Number), bit_size(Bitstring), byte_size(Bitstring), element(N, Tuple), float(Term), hd(List), length(List), node(), node(Pid:call <SNR>116_align()
aRef|Port), round(Number), self(), size(Tuple|Bitstring), tl(List), trunc(Number), tuple_size(Tuple).|)
```

## Recursion

Functional languages like Erlang usually do not have offer looping constructs like `for` and `while`.

Instead of looping constructs Erlang has a concept called *Recursion*.

A 'factorial of a value' is a basic concept that illustrates recursion:

```
3! = 3 * 2 * 1 = 6
```

The above concept can be illustrated using the following Erlang function:

```
fac(0) ->
    1;
fac(N) when N > 0 ->
    N*fac(N-1).
```

Essentially the definition of recursion could be stated as: 'A function that calls itself'.

It is important to have a stopping condition like the example, which stops when N is 0.

### Length

Mini tutorial to count how many elements a list contains.

1) A base case:

Start with a base case first, it is the easiest places to start: what is the simplest input we can have to find a length from.

The simplest list would be `[]` with a length of `0` and the next simplest would be  `[_]` with a length of `1`

```erlang
len([]) ->
    0;
len([_]) ->
    1.
```

The above will calculate a list as long as it only has a length of 1 or 0.

```erlang
len([]) -> 0;
len([_|T]) -> 1 + len(T).
```
By using the [H|T] pattern we can figure out whether a list has more elements, and then recurse through each one counting them.

### Tail Recursion

The above list example has a major problem when it comes to dealing with a list of millions of records as each number has to be stored in memory.

A less wasteful way of doing it is using *tail recursion*. Tail transforms a linear process (grows for each element) into an iterative one (where no growth occurs)

What this means in practice is that a recursion should be 'alone'. So, the recursion cannot depend on the evaluation of other parts.

The answer of `1 + len(Rest)` needs the answer of `len(Rest)` to be found, and so it continues to stack until the last one is found (based on which the final result gets calculated).

Tail recursion aims to eliminate the stacking of operation by reducing them as they happen.

Tail recursion is achieved by holding an extra temporary variable as a parameter in a function. This variable is called an `accumulator` and basically stores the results of computations as they happen:

```erlang
tail_fac(N) ->
    tail_fac(N,1).

tail_fac(0, Acc) ->
    Acc;
tail_fac(N, Acc) when N > 0 ->
    tail_fac(N-1, N*Acc).
```

`tail_fac/1` acts as an abstraction over the tail recursive `tail_fac/2` function. This allows as to never hold more than 2 terms in memory. Essentially, the amount of memory consumed by `4!` is now almost equivalent to the amount of memory used by `1000000!`.

Tail recursion applied to the `len/1` would work as follows:

```erlang
tail_len(L) ->
    tail_len(L, 0).

tail_len([], Acc) ->
    Acc;
tail_len([_|T], Acc) ->
    tail_len(T, Acc+1).
```

### More Recursion

Full examples in tail recursion format:

Duplicate Term N times:

```erlang
tail_duplicate(N,Term) ->
    tail_duplicate(N,Term,[]).

tail_duplicate(0,_,List) ->
    List;
tail_duplicate(N,Term,List) when N > 0 ->
    tail_duplicate(N-1, Term, [Term|List]).
```

List reverse function:

```erlang
tail_reverse(L) ->
    tail_reverse(L,[]).

tail_reverse([],Acc) ->
    Acc;
tail_reverse([H|T],Acc) ->
    tail_reverse(T, [H|Acc]).
```

Get N first element(s) of a List:

```erlang
tail_sublist(L, N) ->
    reverse(tail_sublist(L, N, [])).

tail_sublist(_, 0, SubList) ->
    SubList;
tail_sublist([], _, SubList) ->
    SubList;
tail_sublist([H|T], N, SubList) when N > 0 ->
    tail_sublist(T, N-1, [H|SubList]).
```

The above function makes use of the previously created `reverse(L)` in order to list the results in the correct order.

**Note**: Do not actually use the above `reverse` as there is an existing BIF: `lists:reverse/1`. This BIF is faster and more efficient as it is written in C.

Recursive zip:

```erlang
tail_zip(X, Y) ->
    lists:reverse(tail_zip(X, Y, [])).

tail_zip([], [], Acc) ->
    Acc;
tail_zip([X|Xs], [Y|Ys], Acc) ->
    tail_zip(Xs, Ys, [{X,Y}|Acc]).
```

Lenient recursive zip:

```erlang
tail_lenient_zip(X, Y) ->
    lists:reverse(tail_lenient_zip(X, Y, [])).

tail_lenient_zip([], _, Acc) ->
    Acc;
tail_lenient_zip(_, [], Acc) ->
    Acc;
tail_lenient_zip([X,Xs], [Y,Ys], Acc) ->
    tail_lenient_zip(Xs, Ys, [{X,Y}|Acc])
```

The above 2 zipping examples use the `lists:reverse/1` BIF.

### Quick Sort

A naive implementation of quicksort works by taking the first element of a list, the *pivot*, and then putting all the elements smaller or equal to the pivot in a new list, and those larger in another list.  We then take each of these lists and do the same thing on them until each list gets smaller and smaller.This goes on until there is nothing but an empty list to sort (the base case).

This is referred to as *naive* because smarter versions of quicksort will try to pick optimal pivots to be faster.

This tut will make use of 2 functions in order to implement a quicksort:

1) function to partition the list into smaller and larger parts.
2) function to apply the partition function on each of the new lists and glue them together.

```erlang
quicksort([]) ->
    [];
quicksort([Pivot|Rest]) ->
    {Smaller, Larger} = partition(Pivot, Rest, [], []),
    quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

partition(_, [], Smaller, Larger) ->
    {Smaller, Larger}.
partition(Pivot, [H|T], Smaller, Larger) ->
    if H =< Pivot -> partition(Pivot, T, [H|Smaller], Larger);
       H > Pivot -> partition(Pivot, T, Smaller, [H|Larger])
    end.
```

Again, as in the case of the `reverse/1`, do not use quicksort like above, use the `lists:sort/1` instead.


### Recursion On More Than Lists

Recursion can also be done on other data structures such as binary trees.

We shall create a very basic tree implementation in order to build  and read data from binary trees.

Tuples are an appropriate data structure for representing nodes (in a tree of nodes). A tree is a node containing nodes, each of which contains nodes, which in turn also contain nodes.

A tuple definition of a node could be `{node, {Key, Value, Smaller, Larger}}`, where smaller and larger can be another similar or empty node (`{node, nil}`).

```erlang
-module(tree).
-export([empty/0, insert/3, lookup/2]).

empty() -> {node, 'nil'}.
```

insert:

```erlang
insert(Key, Val, {node, 'nil'}) ->
    {node, {Key, Val, {node, 'nil'}, {node, 'nil'}}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey < Key ->
    {node, {Key, Val, insert(NewKey, NewVal, Smaller), Larger}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey > Key ->
    {node, {Key, Val, Smaller, insert(NewKey, NewVal, Larger)}};
insert(Key, Val, {node, {Key, _, Smaller, Larger}}) ->
    {node, {Key, Val, Smaller, Larger}}.
```

lookup:

```erlang
lookup(_, {node, 'nil'}) ->
    undefined;
lookup(Key, {node, {Key, Val, _, _}}) ->
    {ok, Val};
lookup(Key, {node, {NodeKey, _, Smaller, _}}) when Key < NodeKey ->
    lookup(Key, Smaller);
lookup(Key, {node, {_, _, _, Larger}}) ->
    lookup(Key, Larger).
```

Once again, this approach is naive and should not be used. Instead, us the `gb_trees` module.

### Thinking Recursively

Recursive definitions are different from their imperative counterpart in approach. The recursive approach is more declarative ("If you get this input, do that, this otherwise") whereas the imperative approach is more step-by-step ("do this, then that then this, then you're done").

Pattern matching in function heads helps make the "declarative" aspect more obvious.

By subdividing each part of a problem into separate functions until they can no longer be simplified, the algorithm becomes nothing but assembling a bunch of correct answers coming from short routines.


## Higher Order Functions

An important part of functional programming is the ability to take a function you defined and then pass it as a parameter to another function. This in turn binds that function parameter to a variable which can be used like any other variable within the function.

A function that can accept other functions transported around like this is called a *higher order function*. Higher order functions are a powerful means of abstraction.

This falls into the domain of *lambda calculus* where everything is a function (even numbers). And because everything is a function, functions must accept other functions as parameters and can operate on them with even more functions.

```erlang
-module(hhfuns).
-compile(export_all).

one() -> 1.
two() -> 2.

add(X, Y) ->
    X() + Y().
```

this will then be called like:

`hhfuns:add(fun hhfuns:one/0, fun hhfuns:two/0).`

The general function notation when passing functions is `fun Module:Function/Arity`.

A practical example of higher order functions:

```erlang
map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F, T)].

incr(X) -> X + 1.
decr(X) -> X -1.
```

These map functions can then be accessed by:

`hhfuns:map(fun hhfuns:incr/1, List).`

or

`hhfuns:map(fun hhfuns:decr/1, List).`

### Anonymous Functions

Anonymous functions can do everything normal functions can do, except call themselves recursively.

They follow the following syntax:

```erlang
fun(Args1) ->
    Expression1, Exp2, ..., ExpN;
    (Args2) ->
    Expression1, Exp2, ..., ExpN;
    (Args3) ->
    Expression1, Exp2, ..., ExpN
end
```

used like:

```erlang
Fn = fun() -> a end.

hhfuns:map(fun(X) -> X + 1 end, L).

hhfuns:map(fun(X) -> X - 1 end, L).
```

Anonymous functions allow you to make abstractions on a very low level of code. Basic concepts such as looping can be ignored, letting you focus on what is done rather than how to do it.

Anonymous functions allow you to do complex function calls using closures:

```erlang
PrepareAlarm = fun(Room) ->
    io:format("Alarm set in ~s.~n", [Room]),
    fun() ->
        io:formmat("Alarm tripped in ~s! Call Batman!~n", [Room]) 
    end
end

AlarmReady = PrepareAlarm("bathroom").

AlarmReady().
```

The functions are only called when they get explictly called, not when they get assigned.

The variable **Room** is taken from the parent function `PrepareAlarm`. This is the concept of `closures`.

Closures work because scope is inherited by anonymous functions.

Like here:

```erlang
base(A) ->
    B = A + 1,
    F = fun() -> A * B end,
    F().
```

However, parent scope has no access. For instance, the following function will get an 'unbound variable' error on `C` because it is accessing a variable in teh child's scope.

```erlang
base(A) ->
    B = A + 1,
    F = fun() -> C = A * B end,
    F(),
    C.
```

An error that can commonly occur in Anonymous function is redefning of variables. A variable cannot be redefined as such:

```erlang
# This will fail
base() ->
    A = 1,
    (fun() -> A = 2 end)().
```

However you can shadow variables:

```erlang
# This will pass but with a warning
base() ->
    A = 1,
    (fun(A) -> A = 2 end)(2).
```

This will throw a warning and should be avoided as it can cause confusion.

Since version 17.0 of Erlang , 'named' anoymous functions can be used. This makes it possible to define anonymous recursive functions.

```erlang
# clearing function calls above
f(PrepareAlarm), f(AlarmReady).

PrepareAlarm = fun(Room) ->
    io:format("Alarm set in ~s.~n", [Room]),
    fun Loop() ->
        io:format("Alarm tripped in ~s! Call Batman!~n", [Room]),
        timer:sleep(500),
        Loop
    end
end.

AlarmReady = PrepareAlarm("bathroom").

AlarmReady().
```

The `Loop` variable refers to the anonymous function itself.

### Maps, filters, folds and more

Maps were addresses at the beginning of this chapter and are a useful way to abstract list mapping functions.

Here is a map function again:

```erlang
map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F, T)].
```

This can be used like:

```
hhfuns:map(fun(H) -> H + 1 end, lists:seq(1, 6)).
```

There are many abstractions that can be built for commonly occurring recursive functions.

For instance a function can be built that handles any sort of basic filtering:

```erlang
filter(Pred, L) -> lists:reverse(filter(Pred, L, [])).

filter(_, [], Acc) -> Acc;
filter(Pred, [H|T], Acc) ->
    case Pred(H) of
        true -> filter(Pred, T, [H|Acc]);
        false -> filter(pred, T, Acc)
    end.
```

The `Pred` is what is called a *predicate* and is basically a test that keeps elements if they pass a certain test.

The function could then be use like this:

```erlang
Numbers = lists:seq(1, 10).
hhfuns:filter(fun(X) -> X rem 2 == 0 end, Numbers).

People = [{male,45},{female,67},{male,66},{female,12},{unknown,174},{male,74}].
hhfuns:filter(fun({Gender, Age}) -> Gender == male andalso Age > 60 end, People).
```

This allows the programmer to only worry about producing a *predicate* and a list. Thus eliminating the need to think of the act of cycling through the list and discarding unwanted elements.

This is an important element of code abstraction: try to get of what is always the same and let the programmer supply the changeable parts.

Another thing that can be done recursively is a *fold*. A *fold* is when you look at every element of a list one after another and reduce them to a single answer.

A fold can be expressed as:

```erlang
fold(_, Start, []) -> Start;
fold(F, Start, [H|T]) -> fold(F, )
```

Used like this:

```erlang
[H|T] = [1, 7, 3, 5, 9, 0, 2, 3].
hhfuns:fold(fun(A, B) when A > B -> A; (_, B) -> B end, H, T).
hhfuns:fold(fun(A,B) when A < B -> A; (_,B) -> B end, H, T).
hhfuns:fold(fun(A,B) -> A + B end, 0, lists:seq(1,6)).
```

Basically any function that reduces a list to 1 element can be expressed as a fold.

Again.... As in every previous chapter, these functions can all be implemented using the standard library:

`lists:map/2`, `lists:filter/2`, `lists:foldl/3` (traverse left to right) and `lists:foldr/3` (traverse right to left)

There are other functions such as:

`lists:all/2` : test if all elements return true based on a predicate.
`lists:any/2` : test if at least 1 element returns true on a predicate.
`lists:dropwhile/2` : ignores elements in a list until it finds one that fit s a predicate.
`lists:takewhile/2` : keeps all elements until it finds one that matches a predicate
`lists:partition/2` : separates a list into 2 based on a predicate (those that match and those that don't).
`lists:flatten/1`
`lists:flatlength/2`
`lists:flatmap/2`
`lists:merge/1`
`lists:nth/2`
`lists:nthtail/2`
`lists:split/2`.

In addition there are function such as zippers and combinations of maps and fold. Check the `lists` documentation for more info:

[Lists Documentation](http://erldocs.com/18.0/stdlib/lists.html)

In the end, you will rarely have to write recursive functions due to the already abstracted standard library functions.


## Errors and Exceptions

Erlang has two main paradigms: functional and concurrent.

The functional subset deals with referential transparency, recursion, higher order functions etc.

Whereas the concurrent subset deals with actors, concurrent processes, supervision trees etc.

This section will explain error handling within the functional subset of Erlang.

**Note:** although there are ways to handle errors in functional code, it is generally best to let a program crash, this is achieved via the concurrent part of Erlang.

## Compile-time Errors

There are several types of errors in Erlang: compile-time, logical, run-time and generated errors.

Compile-time errors are often syntactic mistakes. Compile time errors are normally fixed by checking function names, language tokens (brackets, parentheses, periods, commas etc) and function arity.

It is good to fix compile errors in the order they were reported. This helps solve the problem from its cause rather than dealing with the resulting errors.

## Logic Errors 

Logic errors are almost always errors coming from the programmer.

These errors do not make programs crash or prevent compilation but can end up providing bad data or cause a program to function in an unintended manner.

These issues have to be solved by the programmer alone, however there are some tools available for analysis:

TypEr - debugger
Dialyzer - tracing module

The best way to deal with these errors is by creating test cases.

## Run-time Errors

Run-time errors are errors that crash a program during run time.

Here are several examples of runtime errors:

**functionclause**

```erlang
lists:sort([3,2,1]).
lists:sort(fffff).
#** exception error: no function clause mathcing lists:sort(fffff)
```

All the Guard clauses of a function failed, or none of the function clauses' patterns matched.

**caseclause**

```erlang
case "Unexpected Value" of
    expected_value -> ok;
    other_expected_value -> 'also ok'
end.
#** exception error: no case clause matching "Unexpected Value"
```

No specific pattern in the `case`, no catch-all clause, or the wrong kind of data.

**ifclause**

```erlang
if 2 > 4 -> ok;
    0 > 1 -> ok
end.
#** exception error: no true branch found when evaluating an if expression
```

No branch in the conditional evaluates to true.

**badmatch**

```erlang
[X,Y] = {4,5}.
#** exception error: no match of right hand side value {4,5}
```

The pattern match failed because the pattern match is impossible.

**badarg**

```erlang
erlang:binary_to_list("he, already a list").
#** exception error: bad argument
    in function  binary_to_list/1
        called as binary_to_list("heh, already a list")
```

The incorrect argument was used for a function.

**undef**

```elrang
lists:random([1,2,3]).
#** exception error: undefined function lists:random/1
```

The function does not exist. Ensure a function is exported with the correct arity.

**badarith**

```erlang
5 + llama.
#** exception error: bad argument in arithmetic expression
    in operator +/2
        called as 5 + llama
```

This occurs when arithmetic that doesn't exist is done.

**badfun**

```erlang
hhfuns:add(one, two).
#** exception error: bad function one
in function hhfuns:add/2
```

Occurs when variables are used as functions.

**badarity**

```erlang
F = func(_) -> ok end.
F(a,b).
#** exception error: interpreted function with arity 1 called with two arguments
```

Using a higher order function with the incorrect arity.

**systemlimit**

Too many processes, atoms that are too log, too many arguments in function, number of atoms too large, too many nodes connected etc.

Some of the `system_limit` errors are serious enough to crash the whole VM.

## Raising Exceptions

It is a good idea to provoke run-time errors when monitoring and protecting against errors. This allows for problems to be spotted early on.

There are 3 types of exceptions in Erlang: *errors, throws and exits*

### Errors

`erlang:error(Reason)` will end execution in the current process and include a stack trace of the least function called with their arguments when you catch it.

This is a way to provoke a run-time error. These should be used when you want execution stopped.

Custom errors can be defined as well.

```erlang
# Bad aritmetic error
erlang:error(badarith).
#** exception error: bad argument in arithmetic expression
# Custom error
erlang:error(custom_error).
#** exception error: custom error
```

The `custom_error` is not recognized by the Erlang shell but it is usable in the same way and can be handled by a programmer in an identical manner.

### Exits

There are 2 types of exits: 'internal' and 'external'.

Internal exits are triggered by calling `exit/1` and they make the current process stop its execution.

External exits are called with `exit/2` and have to do with multiple processes in the concurrent aspect of Erlang. This chapter will deal with internal errors primarily.

Internal errors are roughly the same as errors, and historically they were. To understand their usage and when to use them we will look briefly at processes and actors.

**Processes**

Processes can send each other messages. A process can also listen for messages, and wait for messages. 

You can also choose what messages to listen to, discard some, ignore others, give up listening after a certain time etc.

These concepts let the implementers of Erlang use a special kind of message to communicate exceptions between processes. They act a bit like a processes "last breath". They are sent right before a process dies and the code it contains stops executing.

This allows other processes that were listening for that specific kind of message to do whatever they please with it. Like: logging, restarting the process etc.

So, the real difference between an error and an exit is the intent. You can decide whether what occurs is simply an error or a condition worthy of killing the current process.

Errors also return a stack trace, while exits do not.

### Throws

A throw is a class of exceptions used for cases that the programmer can be expected to handle.

Throws do not carry any "crash that process!" intent but are rather a way to control flow.

Throws should be documented within the module they are used within.

A throws syntax is:

```erlang
throw(permission_denied).
#** exception throw: permission_denied
```

Throws can also be used for non-local returns when in deep recursion. This lets an implementer only write for the successful cases and have one function deal with the exception on top of it all.

An example of that is the `ssl` module which uses throw/1 as a way to push `{error, Reason}` tuples back to a top-level function. This function then simply returns that tuple to the user.

As a rule of thumb, try to limit the use of throws for non-local returns to a single module in order to make it easier to debug your code. In addition this will allow you to change the innards of a module without changing the interface.

### Dealing with Exceptions

Throws, errors and exits can be handled via a `try...catch`.

A `try...catch` is a way to evaluate an expression while letting you handle the successful case as well as the errors encountered.

```erlang
try Expression of
    SuccessfulPatten1 [Guards] ->
        Expression1;
    SuccessfulPattern2 [Guards] ->
        Expression2
catch
    TypeOfError:ExceptionPattern1 ->
        Expression3;
    TypeOfError:ExceptionPattern2 ->
        Expression4
end.
```

The *Expression* in between try and of is said to be *protected*: Any exception happening within it will be caught.

Patterns in between the `try...of` and `catch` behave in exactly the same manner as a `case...of`.

Within the `catch` *TypeOfError* can be replaced by `error`, `throw` or `exit`. If no type is provided a `throw` is assumed.

Simple example:

```erlang
-module(exceptions).
-compile(export_all).

throws(F) ->
    try F() of
        _ -> ok
    catch
        Throw -> {throw, caught, Throw}
    end.
```

Used as:

```erlang
exceptions:throws(fun() -> throw(thrown) end).
# {throw,caught,thrown}

exceptions:throws(fun() -> erlang:error(pang) end).
# ** exception error: pang
```

This `try...catch` only receives throws.

Errors and exits can be aught like this:

```erlang
errors(F) ->
    try F() of
        _ -> ok
    catch
        error:Error -> {error, caught, Error}
    end.

exits(F) ->
    try F() of
        _ -> ok
    catch
        exit:Exit -> {exit, caught, Exit}
    end.
```

Usage:

```erlang
exceptions:errors(fun() -> erlang:error("Die!") end).
# {error,caught,"Die!"}

exceptions:exits(fun() -> exit(goodbye) end).
# {exit,caught,goodbye}
```

A more complex example taking all these exceptions into play:

```erlang
sword(1) -> throw(slice);
sword(2) -> erlang:error(cut_arm);
sword(3) -> exit(cut_leg);
sword(4) -> throw(punch);
sword(5) -> exit(cross_bridge).

black_knight(Attack) when is_function(Attack, 0) ->
    try Attack() of
        _ -> "None shall pass."
    catch
        throw:slice -> "It is but a scratch.";
        error:cut_arm -> "I've had worse.";
        exit:cut_leg -> "Come on you pansy!";
        _:_ -> "Just a flesh wound."
    end.
```

`is_function` is a BIF that makes sure the *Attack* variable is a function of arity 0.

Sample usage:

```erlang
exceptions:black_knight(fun() -> exceptions:sword(1) end).
```

`_:_` is used as a catch all for exceptions. This should be used sparingly. Try to protect code from what you can handle, but not any more than that.

In Erlang there is also a 'finally' block for `try...catch`'s that always gets executed:

```erlang
try Expr of
    Pattern -> Expr1
catch
    Type:Exception -> Expr2
after % this always gets executed
    Expr3
end.
```

A return value can not be used from the `after` construct. Therefore `after` is mostly used to run code with side effects. An example of this: closing files after reading them, regardless of an exception or not.

Multiple expressions can be contained between the `try` and `of`:

```erlang
whoa() ->
    try
        talk(),
        _Knight = "None shall Pass!",
        _Doubles = [N*2 || N <- lists:seq(1,100)],
        throw(up),
        _WillReturnThis = tequila
    catch
        Exception:Reason -> {caught, Exception, Reason}
    end.
```

**Note**: You should try to use `try...of...catch` as often as possible. Running any recursive function between the protected `try...of` rather than the `of...catch` can result in memory issues.

### Catch

Erlang has another error handling structure called a `catch`. It captures all types of exceptions on top of the good results.

```erlang
catch throw(whoa).

catch exit(die).

catch 1/0.

catch 2+2.
```

Catch will often be written in this manner:

```erlang
catcher(X,Y) ->
    case catch X/Y of
        {'EXIT', {badarith, _}} -> "uh oh";
        N -> N
    end.
```

There are several problems with `catch`: operator precedence, it is hard to see the difference between an underlying representation of an exception and a real exception, and the difference between an error and an exit.

Finally, you never know if a `catch` returned an exception or an actual value.


## Functionally Solving Problems

Programming...

## Data Structures

### Records

Records are useful whenever you have a small data structure that needs its elements accessed by name directly.

Records are declared as module attributes:

```erlang
-module(records).
-compile(export_all).
-record(robot, {name,
                type=industrial,
                hobbies,
                details=[]}).
```

The record has 4 fields: name, type, hobbies and details. The `type` and `detals` have default values attached.

To declare a record in the module, do this:

```erlang
first_robot() ->
    #robot{name="Mechatron",
            type=handmade,
            details=["Moved by a small man inside"]}.
```

Values that are not assigned in a record are automatically assigned an `undefined` value.

There is an Erlang function `rr()` that can be used to load record definitions from a module.

There are 2 ways to extract information from a record in Erlang.

Dot syntax:

```erlang
Crusher = #robot{name="Crusher", hobbies=["Crushing people", "pettin cats"]}.
Crusher#robot.hobbies.
```

This ugly syntax is due to the nature of tuples. Because they are some kind of compiler trick, you have to keep keywords that define what record goes with what variable.

A nested example would be:

```erlang
NestedBot = #robot{details=#robot{name="erNest"}}.
(NestedBot#robot.details)#robot.name.
```

It is now also possible to write the above as:

```erlang
NestedBot#robot.details#robot.name.
```

One helpful aspect of records is that they can be used on function heads and pattern matching.

```erlang
-record(user, {id, name, group, age}).

%% use pattern matching to filter
admin_panel(#user{name=Name, group=admin}) ->
    Name ++ " is allowed!";
    admin_panel(#user{name=Name}) ->
    Name ++ " is not allowed".

%% can extend user without problem
adult_section(U = #user{}) when U#user.age >= 18 ->
    %% Show stuff that can't be written in such a text
    allowed;
adult_section(_) ->
    %% redirect to sesame street site
    forbidden.
```

You can assign a whole record to a variable by going `SomeVar = #some_record{}`.

Used like:

```erlang
records:admin_panel(#user{id=1, name="ferd", group=admin, age=96}).
records:admin_panel(#user{id=2, name="you", group=users, age=66}).
records:adult_section(#user{id=21, name="Bill", group=users, age=72}).
records:adult_section(#user{id=22, name="Noah", group=users, age=13}).
```

It is not necessary to match on all elements of the record. It is also not necessary to even know how many elements are in the tuple.

Record can also be updated like this:

```erlang
repairman(Rob) ->
    Details = Rob#robot.details,
    NewRob = Rob#robot{details=["Repaired by repairman"|Details]},
    {repaired, NewRob}.
```

Erlang records are frequently shared across modules with *header files*.

Header files have a `.hrl` extension. For example:

```erlang
-record(included, {some_field,
                    some_default = "yeah!",
                    unimaginative_name}).
```

To include a header file in a module add:

```erlang
-include("records.hrl").
```

**Note**: It is strongly recommended that you keep all recorded definitions local, within one module. It is better to write access functions and keep the modules details as private as possible.

### Key-Value Stores

There are 2 data structures for handling small amounts of data: 

**Proplists**

The first one is a *proplist*. A proplist is any list of tuples of the form [{Key, value}]. That is the only rule.

There is a `proplists` module that can be used for manipulating and working with proplists. Example functions are:

`proplists:delete/2`
`proplists:get_value/2`
`proplists:get_values/2`
`proplists:lookup/2`
`proplists:lookup_all/2`

Proplists are so loosely defined that there is no `update` or `add` function in the *proplists* module. Functions like `lists:keyreplace/4` can be used.

**Orddicts**

The second type of key/value store for small amount of data is a `orddct`. The *orddict* module can be used.

Orddicts are "ordered Dictionaries"  and are essentially formally defined proplists. There are existing functions for:

`orddict:store/3`
`orddict:find/2`
`orddict:fetch/2`
`orddict:erase/2`

Orddicts are a good compromise between complexity and efficiency when no larger than 75 elements. After 75, different key value stores should be used.

For large amounts of data there are:

**dicts**

Dictionaries have the same interface as orddicts and include additional functions like `dict:map/2` and `dict/fold/2`. They are a good way of scaling orddicts up when needed

**gbtrees**

General Balanced Trees, give a lot more control over structure. There are 2 modes for `gb_trees`: 1) Known structures (in and out, ie. smart mode) and 2) unknown structures (ie. naive mode).

Naive functions are:

`gb_trees:enter/3`
`gb_trees:lookup/2`
`gb_trees:delete_any/2`

Smart functions are:

`gb_trees:insert/3`
`gb_trees:get/2`
`gb_trees:update/3`
`gb_trees:delete/2`

In addition there is `gb_trees:map/2`.

The disadvantages of the naive approach are slower speeds due to the tree balancing itself out. In the smart mode, the result and processing are faster due to the functions all assuming there is a key present in the tree.

In general `dicts` have the fastest read speed while `gb_trees` are faster in other respects.

`dicts` also have a fold function while `gb_trees` don't: instead `gb_trees` use an iterator, meaning recursive functions have to be written for `gb_trees` instead of using a fold.

`gb_tees` can however access the smallest and largest elements of a structure via `gb_trees:smallest/1` and `gb_trees:largest/1`.

**Notes** : Erlang now has `maps` which should be treated as the de-facto replacement for `dicts`.

### Maps

Postscript: maps ...read this before finishing the book. Essentially use maps instead of dicts.

[Maps](http://learnyousomeerlang.com/maps)


### Arrays

Erlang also has arrays for storing numerical indices and folding over the whole structure.

In general, arrays are rarely used in Erlang, and they can be slow.

When doing matrix manipulations and other heavy array manipulation, Erlang programmers use a concept called "Ports" to let other languages to the heavy lifting.

### Sets

Sets are groups of unique elements that you can compare an operate on: find which elements are in 2 groups, in none of them, only in one or the other, etc.

There are 4 main modules to deal with sets in Erlang: `ordsets`, `sets`, `gb_sets`, `sofs` (sets of sets).

**ordsets**

Ordsets are implemented as a sorted list. They're mainly useful for small sets, are the slowest kind of set, but they have the simplest and most readable representation of all sets.

**sets**

Sets are implemented on top of a structure very similar to `dicts`. They implement the same interface as ordsets but scale better. They are especially useful for read intensive manipulations.

**gbsets**

`gb_sets` are constructed above a  general balanced tree structure similar to the one used in `gb_trees`. Similar to `gb_trees` they are faster when doing non read operations. Gb sets implement the same interface as sets and ordsets just with more more functions.

**sofs**

Sets of sets are implemented with sorted lists, stuck inside a tuple with some metadata.They're the module to use if you want full control over relationships between sets, families, enforce set types etc.

sofs are useful for mathematical concepts rather than just groupings of unique elements.

**Note:** the different sets modules implement `==` and `=:=` differently, meaning they cant just be swapped out easily.

**Note:** It is recommended that you use gb_sets for most cases.


### Directed Graphs

Directed graphs are implemented as two modules in Erlang: `digraph` and `digraph_utils`. The digraph module allows the construction and modification of directed graphs. `digraph_utils` allows you to navigate a graph, test for cycles, arborescences, finding neighbours, etc.

Directed graphs are closely related to set theory so the `sofs` module contains functions for converting families to digraphs and digraphs to families.

### Queues

The Erlang queue module implements a double-ended FIFO (First in, First Out) queue:

They are implemented as to lists (stacks) that allow to both append a prepend elements rapidly.

The queue module has different functions separated into 3 interfaces of varying complexity:

**Original API**

The original api contains the base functions for the queue: `new/0` creates empty queues, `in/2` for inserting new elements, `out/1` for removing elements. There are also functions to convert lists, reverse the queue, look if a value is part of it etc.

**Extended API**

The extended API mainly adds introspection and flexibility: Can look at the front of the queue without removing elements(`get/1` and `peek/1`), removing elements without caring about them (`drop/1`) etc.

**Okasaki API**

The Okasaki API is derived from Chris Okasaki *Purely Functional Data Structures*. Don't use without knowing the principles of Okasaki.

Queues should be used when you need to ensure the first item ordered is indeed the first one processed. In cases where you can't just do all the reversing at once and elements are frequently added, the queue module is what you want.


## Concurrency

Erlang's concurrency was based on message passing and the actor model.

There is a difference between *concurrency* and *parallelism* in Erlang:

Concurrency refers to having many actors running independently, but not necessarily at the same time.

Parallelism is having actors run exactly at the same time.

Nowadays, multi-core systems allow for parallelism on a single computer and Erlang takes full advantage of this possibility.

### Concepts of Concurrency

**Scalability**

In order to handle scaling and concurrency it was decided to forbid processes from sharing memory.

Shared memory can leave things in an inconsistent state after some crashes. Instead of shared memory, processes communicate by sending messages with all the data copied. Which risks being slower...but safer.

**Fault Tolerance**

In Erlang the idea was to find good ways of handling errors and problems rather than trying to prevent them all.

The ideal solution in Erlang is to kill processes as fast as possible to avoid data corruption and transient bugs.

Another method of ensuring fault tolerance is through distribution (software spread over multiple computers).

Asynchronous messages (across distributed systems) allow for safe remote function calls because there is no assumption about what will happen.

**Implementation**

It was decided that lightweight processes with asynchronous message passing were the approach to take for Erlang.

This was achieved through the following means:

1) The operating system can't be trusted, so Erlang harnesses a VM where Erlang implementers can keep control of optimization and reliability.

2) The VM creates 1 thread per machine core. This acts as a scheduler.

3) Each scheduler has a *run queue* of Erlang processes. When one scheduler has too many tasks it migrates them to other schedulers.

Basically, all the hard stuff in concurrency and parallelism in Erlang is managed for you.

### Scaling

The difficulty of obtaining linear scaling is primarily related to the problem being solved rather than the language.

Problems that scale very well are referred to as *embarrassingly parallel*.

Erlang does not handle data crunching or numerical algorithms well, and unfortunately the majority of *embarrassingly parallel* problems require this sort of power.

Erlang can handle a specific set of problems at close to linear scaling. These problems include any application where work done can be represented as independent logical entities.

A parallel program only goes as fast as its slowest sequential part.

**Note**: Parallelism is not them answer to everything, In purely sequential programs it can actual slow things down and should be avoided.

### Concurrency Primitives

There are 3 concurrency primitives in Erlang: Spawning new processes, sending messages, receiving messages.

In Erlang a process is simply a function.

To start a new process, Erlang provides the function `spawn/1`:

```erlang
F = fun() -> 2 + 2 end.
spawn(F).
```

Processes do not return a result. A simple way of seeing the output is to print it out:

```erlang
spawn(fun() -> io:format("~p~n", [2+2]) end).
```

This isn't practical for a real problem but is useful for experimenting here.

```erlang
G = fun(X) -> timer:sleep(10), io:format("~p~n", [X]) end.
[spawn(fun() -> G(X) end) || X <- lists:seq(1, 10))].
```

Starts 10 processes and pauses each of them for a while. The output will not be in the expected order because the Erlang VM tries to optimize and ensure every process gets a good share of time.

The shell itself is a regular process as well, this can be seen by running:

```erlang
self().
exit(self()).
self().
```

Which will output a PID, delete the process, and then you will see a new PID.

The next primitive used to pass messages is `!` (the *bang* symbol). It takes a pid on the left side and a Erlang term on the right. The term is then sent to the process represented by the PID:

```erlang
self() ! hello
```

This sends the message to the process' mailbox, but it doesn't get read yet. It also returns the term, which means it is possible to send the same message to many processes:

```erlang
self() ! self() ! double.
```

Messages are kept in the process mailbox in the order they were received. When a message is read it is taken out of the mailbox. Flush can be used to see the contents of the current mailbox:

```erlang
flush().
```

Which brings us to the `receive` statement, which is written like:

```erlang
receive
    Pattern1 when Guard1 -> Expr1;
    Pattern2 when Guard2 -> Expr2;
    Pattern3 -> Expr3
end
```

Or in a more full example:

```erlang
-module(dolphins).
-compile(export_all).

dolphin1() ->
    receive
        do_a_flip ->
            io:format("How about no?~n");
        fish ->
            io:format("So long and thanks for all the fish!~n");
        _ ->
            io:format("Heh, we're smarter than you humans.~n")
    end.
```

The pattern is very similar to the `case...of`. The only difference is that the `receive` binds variables coming from messages.

As can be seen in the example syntax, guards can also be used.

The dolphins module can be used like:

```erlang
Dolphin = spawn(dolphins, dolphin1, []).
Dolphin ! "oh, hello dolphin!".
Dolphin ! fish.
```

This uses a spawn function with an arity of 3. It takes a module, a function and any function arguments.

In Erlang there is a way to send a message back to the process that originally called it (of course there is). This is sent as a reply, rather than a return:

```erlang
dolphin2() ->
    receive
        {From, do_a_flip} ->
            From ! "How about no?";
        {From, fish} ->
            From ! "So long and thanks for all the fish!";
        _ ->
            io:format("Heh, we're smarter than you humans.~n")
    end.
```

The reply address is packaged as a pid in a tuple: `{Pid, Message}`.

Used like:

```erlang
Dolphin2 = spawn(dolphins, dolphin2, []).
Dolphin2 ! {self(), do_a_flip}.
flush().
```

In order to create a function that is always available to receive, you can use recursion:

```erlang
dolphin3() ->
    receive
        {From, do_a_flip} ->
            From ! "How about no?",
            dolphin3();
        {From, fish} ->
            From ! "So long and thanks for all the fish!";
        _ ->
            io:format("Heh, we're smarter than you humans.~n"),
            dolphin3()
    end.
```

This function will not blow the stack because it is tail recursive.

Used like:

```erlang
Dolphin3 = spawn(dolphins, dolphin3, []).
Dolphin3 ! Dolphin3 ! {self(), do_a_flip}.
flush().
Dolphin3 ! {self(), unknown_message}.
Dolphin3 ! Dolphin3 ! {self(), fish}.
flush().
```

## Multiprocessing

The receive construct allows for a timeout to be explicitly indicated:

```erlang
receive
    Match -> Expression1
after Delay ->
    Expression2
end
```

This is important when dealing with asynchronous operations as you need a way to recover should a function not receive in data.

The `after` part of the receive construct will trigger if the time given in the `Delay` is reached without a matching message getting received.


## Errors and Processes

### Links

A link is a relationship between 2 processes. A link can be setup so that when 1 process dies, it causes the other to die as well.

This is useful in order to fail as soon as possible. Rather than dealing with missing dependencies when a process fails, the alternative is to stop all processes and restart them in a group.

Erlang has a primitive function to set up links: `link/1`. The function takes a Pid as an argument. It creates a link between the current process and the one identified by the Pid.

`unlink/1` can be used to destroy a link to another process.

When a linked process crashes a special type of message is sent (no message is sent if a process dies naturally).

```erlang
myproc() ->
    timer:sleep(5000),
    exit(reason).
```

Used as:

```erlang
spawn(fun linkmon:myproc/0).
link(spawn(fun linkmon:myproc/0)).
```

Links are used to establish larger groups of processes that should all die together.

```erlang
chain(0) ->
    receive
        _ -> ok
    after 2000 ->
        exit("chain dies here")
    end;
chain(N) ->
    Pid = spawn(fun() -> chain(N-1) end),
    link(Pid),
    receive
    _ -> ok
    end.
```

Used as:

```erlang
link(spawn(linkmon, chain, [3])).
```

**Note**: links cannot be stacked, only a single link will ever exist between processes, and only a single call to `unlink/1` is ever required.

The linking using `link(spawn(Function))` or `link(spawn(M, F, A))` occurs in more then 1 step, so it is possible for a process to die before the link has been set up and then provoke unexpected behaviour. For this reason an addition `spawn_link/1-3` function has been added.

The `spawn_link/1-3` creates a process and links it in the same manner as `link/1` except it all occurs in an atomic operation... meaning the operation either fails or succeeds and nothing else.


### Traps

Error propagation across processes is done through a process similar to message passing, but with a spacial type of message called signals.

Exit signals are 'secret' signals that automatically act on processes, killing them in the action.

In order to be reliable applications need to be able to kill and restart a process quickly. In order to restart a process we need to know that it died first. This can be done via a layer on top of `link` called *system processes*.

System processes are normal processes except that they can convert exit signals to regular messages. This is done by calling `process_flag(trap_exit, true)` in a running process:

```erlang
process_flag(trap_exit, true).
spawn_link(fun() -> linkmon:chain(3) end).
receive C -> X end.
```

This is an easy way to create a process whose only role is to check if something dies and then restart it if it fails.

The Erlang `exception` already addressed in a previous chapter behave around processes that trap exits.

`exit/2` is the process equivalent of a gun. It allows a process to kill another one from a distance, safely. This function takes a PID and a reason.

The `kill` reason acts as a special signal that can't be trapped. Therefore it needs to be changed to a `killed` reason when other processes receive the message.Otherwise every other linked process would also die for the same reason.

### Monitors

Monitors are unidirectional and stackable links. Monitors are used when you want to know what is going on with another process but neither process is vital to each other.

Alternatively, monitors can be used to stack the references. If you have 2-3 libraries that all need to know whether a process is alive or not the monitors should be used as they are stackable and wont result in an 'unlink' cascade when one s process dies.

Monitors can be removed individually and due to their unidirectional nature no other processes need to be aware of their existence. 

A monitor looks like:

```erlang
erlang:monitor(process, spawn(fun() -> timer:sleep(500) end)).
flush().
```

Every time a monitored process goes down a message will be received. The message will be `{'DOWN', MonitorReference, process, Pid, Reason}`. The reference is there to allow the process to be de-monitored. There is an atomic function for spawning a process and monitoring it at the same time:

```erlang
spawn_monitor/1-3.
```

or:

```erlang
{Pid, Ref} = spawn_monitor(fun() -> receive _ -> exit(boom) end end).
erlang:demonitor(Ref).
Pid ! die.
flush().
```

In the above case, the process is demonitored before it crashes and thus there is no trace of it dying. `demonitor/2` exists in order to give a little more information. The second parameter can be a list of options. Only 2 exist `info` and `flush`:

```erlang
f().
{Pid, Ref} = spawn_monitor(fun() -> receive _ -> exit(boom) end end).
Pid ! die.
erlang:demonitor(Ref, [flush, info]).
flush().
```

The `info` options tells whether a monitor existed or not when it tries to remove it. The `flush` option removed the `DOWN` message from the mailbox if it existed.

### Naming Processes

The act of giving a name to a process allows you to replace the unpredictable pid by an atom.

Instead of using a Pid the atom can be used to identify a process.

In order to give a process a name, use the function `register/2`. A process loses its name when it dies but it can also lose it by using `unregister/1`. Getting a list of registered processes can be done using `registered/0` (there is also a shell command that gives a more detailed list of processes `regs()`.

Named processes can be useful to restart stopped/crashed processes in order to ensure other processes don't fail as well.

**Notes:** Because named processes use atoms, they should never be dynamically named. This means that named processes should only be used for important services unique to the VM and that should run for teh duration of an application.


## Designing a Concurrent Application








