%% Author: Jean-Lou Dupont
%% Created: 2009-08-03
%% Description: TODO: Add description to tools
-module(twitter_tools).

-compile(export_all).

%%
%% Exported Functions
%%
-export([
		 is_running/1,
		 extract_host/0,
		 extract_host/1,
		 make_node/1,
		 make_node/2,
		 to_string/1,
		 kfind/2,
		 kfind/3
		 ]).

-export([
		 gen_auth_header/2,
		 gen_auth/2
		 ]).

%% From YAWS
-export([
		 code_to_phrase/1,
		 url_encode/1,
		 integer_to_hex/1,
		 
		 encode_list/0,
		 encode_list/1,
		 
		 encode_tuple/2,
		 
		 format_encoded_list/1,
		 
		 extract/2
		 ]).

-export([
		 getvar/1, getvar/2, getvar/3,
		 add_to_var_list/2,
		 
		 concat_atoms/2,
		 to_atom/1,
		 
		 head/1,
		 head_pair/2, head_pair/3,
		 
		 make_atom_from_list/1,  make_atom_from_list/2
		 ,vsize/1
		 ,is_alive/1, is_alive/2
		 ,extract_head/1, extract_head/2, extract_head2/1
		 ,add_to_tuple_list/3
		 ]).

%%
%% API Functions
%%
is_running(Node) ->
	Regs = erlang:registered(),
	lists:member(Node, Regs).



extract_host() ->
	extract_host(node()).

extract_host(Node) when is_atom(Node) ->
	extract_host(atom_to_list(Node));

extract_host(Node) when is_list(Node) ->
	Tokens = string:tokens(Node, "@"),
	lists:last(Tokens).
	


make_node(Name) ->
	make_node(Name, node()).

make_node(Name, Node) when is_atom(Name) ->
	make_node(erlang:atom_to_list(Name), Node);

make_node(Name , Node) when is_list(Name) ->
	Host=tools:extract_host(Node),
	PartialName=string:concat(Name, "@"),
	CompleteName=string:concat(PartialName, Host),
	erlang:list_to_atom(CompleteName).
	



to_string(StringTerm) ->
	Binary=erlang:iolist_to_binary(StringTerm),
	erlang:binary_to_list(Binary).
	
	


gen_auth_header(Username, Password) ->
	"Basic "++gen_auth(Username, Password).
	
gen_auth(Username, Password) ->
	StringToEncode=Username++":"++Password,
	base64:encode_to_string(StringToEncode).
	
	

%% From YAWS
url_encode([H|T]) ->
    if
        H >= $a, $z >= H ->
            [H|url_encode(T)];
        H >= $A, $Z >= H ->
            [H|url_encode(T)];
        H >= $0, $9 >= H ->
            [H|url_encode(T)];
        H == $_; H == $.; H == $-; H == $/; H == $: -> % FIXME: more..
            [H|url_encode(T)];
        true ->
            case tools:integer_to_hex(H) of
                [X, Y] ->
                    [$%, X, Y | url_encode(T)];
                [X] ->
                    [$%, $0, X | url_encode(T)]
            end
     end;

url_encode([]) ->
    [].


%% From YAWS
integer_to_hex(I) ->
    case catch erlang:integer_to_list(I, 16) of
        {'EXIT', _} ->
            old_integer_to_hex(I);
        Int ->
            Int
    end.

old_integer_to_hex(I) when I<10 ->
    integer_to_list(I);

old_integer_to_hex(I) when I<16 ->
    [I-10+$A];

old_integer_to_hex(I) when I>=16 ->
    N = trunc(I/16),
    old_integer_to_hex(N) ++ old_integer_to_hex(I rem 16).



%% from yaws_api
%%
code_to_phrase(100) -> "Continue";
code_to_phrase(101) -> "Switching Protocols ";
code_to_phrase(200) -> "OK";
code_to_phrase(201) -> "Created";
code_to_phrase(202) -> "Accepted";
code_to_phrase(203) -> "Non-Authoritative Information";
code_to_phrase(204) -> "No Content";
code_to_phrase(205) -> "Reset Content";
code_to_phrase(206) -> "Partial Content";
code_to_phrase(207) -> "Multi Status";
code_to_phrase(300) -> "Multiple Choices";
code_to_phrase(301) -> "Moved Permanently";
code_to_phrase(302) -> "Found";
code_to_phrase(303) -> "See Other";
code_to_phrase(304) -> "Not Modified";
code_to_phrase(305) -> "Use Proxy";
code_to_phrase(306) -> "(Unused)";
code_to_phrase(307) -> "Temporary Redirect";
code_to_phrase(400) -> "Bad Request";
code_to_phrase(401) -> "Unauthorized";
code_to_phrase(402) -> "Payment Required";
code_to_phrase(403) -> "Forbidden";
code_to_phrase(404) -> "Not Found";
code_to_phrase(405) -> "Method Not Allowed";
code_to_phrase(406) -> "Not Acceptable";
code_to_phrase(407) -> "Proxy Authentication Required";
code_to_phrase(408) -> "Request Timeout";
code_to_phrase(409) -> "Conflict";
code_to_phrase(410) -> "Gone";
code_to_phrase(411) -> "Length Required";
code_to_phrase(412) -> "Precondition Failed";
code_to_phrase(413) -> "Request Entity Too Large";
code_to_phrase(414) -> "Request-URI Too Long";
code_to_phrase(415) -> "Unsupported Media Type";
code_to_phrase(416) -> "Requested Range Not Satisfiable";
code_to_phrase(417) -> "Expectation Failed";
code_to_phrase(500) -> "Internal Server Error";
code_to_phrase(501) -> "Not Implemented";
code_to_phrase(502) -> "Bad Gateway";
code_to_phrase(503) -> "Service Unavailable";
code_to_phrase(504) -> "Gateway Timeout";
code_to_phrase(505) -> "HTTP Version Not Supported".


%% For tests
%% Result: param1&param2&param%3F&key=value
%%
encode_list() ->
	Test=["param1", "param2", "param?", {"key", "value"}],
	encode_list(Test).

encode_list([]) ->
	"";

encode_list([{Key, Value}]) ->
	LKey=force_list(Key),
	url_encode(LKey) ++ "=" ++ url_encode(Value);
	
encode_list([H]) ->
	url_encode(H);

encode_list([{Key, Value}|T]) ->
	LKey=force_list(Key),
	url_encode(LKey)++"="++url_encode(Value)++"&"++ encode_list(T);

encode_list([H|T]) ->
	url_encode(H) ++ "&" ++ encode_list(T);

encode_list(E) ->
	url_encode(E).



encode_tuple(Key, Value) ->
	LKey=force_list(Key),
	url_encode(LKey)++"="++url_encode(Value).



format_encoded_list("") ->
	"";

format_encoded_list(Liste) ->
	"?"++Liste.


kfind(_Key, []) ->	{};

%% @doc Searches through a tuple list for Key
%%
%% @spec kfind(Key, List) -> {} | {Key, Value}
%% where
%%	Key = atom()
%%	List = [tuple()]
%%	Value = term()
kfind(Key, List) ->
	case is_list(List) of
		true  -> TheList=List;
		false -> TheList=[List]
	end,
	
	case erlang:is_builtin(lists, keyfind, 3) of
		true  ->
			case lists:keyfind(Key,1,TheList) of
				false -> {};
				Tuple -> Tuple
			end;

		false ->
			case lists:keysearch(Key,1,TheList) of
				{value, Value} -> Value;
				_              -> {}
			end
	end.

kfind(Key, [], Default) ->
	{Key, Default};

%% @doc Returns {Key, Default} if not found or {Key, Value} otherwise
%%
%% @spec kfind(Key, List, Default) -> {Key, Value} | {}
%% where
%%	Key = atom()
%%	List = [tuple()]
%%	Value = term()
%%	Default = term()
kfind(Key, List, Default) ->
	case kfind(Key, List) of
		false        -> {Key, Default};
		{Key, Value} ->	{Key, Value}
	end.



force_list(Key) when is_atom(Key) ->
	erlang:atom_to_list(Key);

force_list(Key) ->
	Key.


%% Result = {{HttpVersion, HttpCode, HttpResponseCode}, [Headers], ResponseBody}
%% HttpVersion = string()         (eg. "HTTP/1.1")
%% HttpCode = integer()           (eg. "200")
%% HttpResponseCode = string()    (eg. "OK")
%% Headers = {key, value}, {key, value} ...
%% ResponseBody = string()


extract(Result, headers) ->
	{{_Version, _Code, _CodeText}, Headers, _Body} = Result,
	Headers;

extract(Result, body) ->
	{{_Version, _Code, _CodeText}, _Headers, Body} = Result,
	Body;

extract(Result, http.code) ->
	{{_Version, Code, _CodeText}, _Headers, _Body} = Result,
	Code;

extract(Result, http.code.text) ->
	{{_Version, _Code, CodeText}, _Headers, _Body} = Result,
	CodeText.
	


getvar(VarName) ->
	getvar(VarName, []).


%% @doc Retrieves a Value from a named variable in the process dictionary or
%%      returns a Default value in the event it is 'undefined'.
%%
%% @spec getvar(VarName, Default) -> Value | Default
%%
%% Value = term()
%% Default = term()
%%
getvar(VarName, Default) ->
	VarValue=get(VarName),
	getvar(VarName, VarValue, Default).

getvar(_VarName, undefined, Default) ->
	Default;

getvar(_VarName, VarValue, _Default) ->
	VarValue.

%% @doc Adds an item to an existing/new list
%%      whilst filtering duplicates.
%%
%% @spec add_to_var_list(VarName, Value) -> list()
%%
add_to_var_list(VarName, Value) when is_list(Value) ->
	List=getvar(VarName,[]),
	FilteredList=List--Value,
	NewList=FilteredList++Value,
	put(VarName, NewList),
	NewList;

add_to_var_list(VarName, Value) ->
	List=getvar(VarName,[]),
	FilteredList=List--[Value],
	NewList=FilteredList++[Value],
	put(VarName, NewList),
	NewList.


%% @doc Concatenates two atoms
%%
%% @spec concat_atoms(A1, A2) -> atom()
%%
concat_atoms(A1, A2) when is_atom(A1), is_atom(A2) ->
	L1=erlang:atom_to_list(A1),
	L2=erlang:atom_to_list(A2),
	erlang:list_to_atom(L1++L2);

concat_atoms(V1, V2) ->
	A1=to_atom(V1),
	A2=to_atom(V2),
	concat_atoms(A1, A2).


to_atom(V) when is_atom(V) -> V;
to_atom(V) when is_list(V) -> erlang:list_to_atom(V);
to_atom(V) when is_integer(V) ->
	L=erlang:integer_to_list(V),
	erlang:list_to_atom(L).

	
head([]) ->	[];
head(Liste) when is_list(Liste) -> erlang:hd(Liste);
head(_) -> [].


%% @doc Retrieves the first two elements of a list
%%
%% @spec head_pair(List, DefaultSecond) -> {Pair, Rest}
%% where
%%	List=list()
%%  DefaultSecond=atom() | string() | integer()
%%	Pair=list()
%%	Rest=list()
%%
head_pair([], _DefaultSecond) ->
	{[],[]};

head_pair(Liste, DefaultSecond) when is_list(Liste) ->
	[First|Rest] = Liste,
	head_pair(First, Rest, DefaultSecond).

head_pair(First, [], DefaultSecond) ->
	{[First, DefaultSecond], []};

head_pair(First, [Second|Rest], _DefaultSecond) ->
	{[First, Second], Rest}.
	


%% @doc Concatenates elements from the list
%%		into one atom.
%%
%% @spec make_atom_from_list(List) -> Result
%% where
%%	List = [atom() | string()]
%%  Result = list()
%%
make_atom_from_list(List) when is_list(List) ->
	{HeadPair, Rest}=head_pair(List, ''),
	make_atom_from_list(HeadPair, Rest).


make_atom_from_list([First,Second], []) ->
	concat_atoms(First, Second);


make_atom_from_list([First, Second], [Third|Rest]) ->
	Partial =concat_atoms(First, Second),
	Partial2=concat_atoms(Partial, Third),
	make_atom_from_list([Partial2|Rest]).


	
	
vsize(Atom) when is_atom(Atom) ->
	erlang:length(erlang:atom_to_list(Atom));

vsize(List) when is_list(List) ->
	erlang:length(List);

vsize(Tuple) when is_tuple(Tuple) ->
	size(Tuple);

vsize(_) ->
	undefined.


is_alive(Name) ->
	Pid=erlang:whereis(Name),
	is_alive(Name, Pid).

is_alive(_Name, undefined) ->
	false;

is_alive(_Name, Pid) when is_pid(Pid) ->
	erlang:is_process_alive(Pid);

is_alive(_,_) ->
	false.



extract_head(Atom) ->
	extract_head(Atom, ".").

%% @doc Extracts the 'head' of the atom
%%		
%% @spec extract_head(Atom, SeparatorList) -> atom()
%%
extract_head(Atom, SeparatorList) when is_atom(Atom) ->
	String=erlang:atom_to_list(Atom),
	Tokens=string:tokens(String, SeparatorList),
	extract_head2(Tokens).

extract_head2([]) ->
	'';

extract_head2([Head|_Rest]) ->
	erlang:list_to_atom(Head).
	
	
%% @doc Adds (respecting uniqueness) an Element
%%		to a specific tuple in a List
%%
%% @spec add_to_tuple_list(List, TupleName, Element) -> TupleList
%% where
%%	List = list().  Tuple list
%%	TupleName= atom()
%%	Element = term()
%%	@type TupleList = [{ItemName, ItemValue}]
%%	@type ItemName = atom()
%%	@type ItemValue = term()
%%
add_to_tuple_list(List, TupleName, Element) when is_atom(TupleName), is_list(List) ->
	do_add_to_tuple_list(List, TupleName, Element);

add_to_tuple_list(_,_,_) ->	{error, invalid_params}.


%% Trivial case... also makes an example for target return format
do_add_to_tuple_list([], TupleName, Element) when is_list(Element)->
	[{TupleName, Element}];

do_add_to_tuple_list([], TupleName, Element) when is_atom(Element)->
	[{TupleName, [Element]}];

do_add_to_tuple_list(List, TupleName, Element) ->
	case is_list(Element) of
		true  -> E=Element;
		false -> E=[Element]
	end,
	
	Tuple=kfind(TupleName, List),
	case Tuple of
		{} ->
			List++[{TupleName, E}];
		
		{_, Value} ->
			FNewList=List--[Tuple],
			FNewValue=Value--E,
			NewValue=FNewValue++E,
			FNewList++[{TupleName,NewValue}]
	end.




%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  TESTS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------
t1() ->
	L1=[{a1, []}, {a2, [a21]}],
	A1=[a11, a12, a13],
	add_to_tuple_list(L1, a1, A1).

t2() ->
	L1=[{a1, []}, {a2, [a21]}],
	A2=[a22, a23, a24],
	add_to_tuple_list(L1, a2, A2).


