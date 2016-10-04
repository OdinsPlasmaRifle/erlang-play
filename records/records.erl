-module(records).
-include("records.hrl").
-export([first_robot/0, car_factory/1, admin_panel/1, adult_section/1, repairman/1, included/0]).

-record(robot, {name,
               type=industrial,
               hobbies,
               details=[]}).

-record(user, {id, name, group, age}).

first_robot() ->
    #robot{name="Mechatron",
          type=handmade,
          details=["Moved by a small man inside"]}.

car_factory(CorpName) ->
    #robot{name=CorpName, hobbies="building cars"}.

%% Use pattern matching
admin_panel(#user{name=Name, group=admin}) ->
    Name ++ " is allowed!";
admin_panel(#user{name=Name}) ->
    Name ++ " is not allowed".

%% Use guards
adult_section(U = #user{}) when U#user.age >=18 ->
    allowed;
adult_section(_) ->
    forbidden.

repairman(Rob) ->
    Details = Rob#robot.details,
    NewRob = Rob#robot{details=["Repaired by repairman"|Details]},
    {repaired, NewRob}.

included() -> #included{some_field="Some value"}.
