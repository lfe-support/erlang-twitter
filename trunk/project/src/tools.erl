%% Author: Jean-Lou Dupont
%% Created: 2009-08-03
%% Description: TODO: Add description to tools
-module(tools).

%%
%% Include files
%%

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



kfind(_Key, []) ->
	{};

%% Searches through a list for a Key
%% Returns {} if not found or {Key, Value} otherwise
kfind(Key, List) ->
	case erlang:is_builtin(lists, keyfind, 3) of
		true  ->
			case lists:keyfind(Key,1,List) of
				false ->
					{};
				Tuple ->
					Tuple
			end;

		false ->
			case lists:keysearch(Key,1,List) of
				{value, Value} ->
					Value;
				_ ->
					{}
			end
	end.

%% Returns {Key, Default} if not found or {Key, Value} otherwise
kfind(Key, [], Default) ->
	{Key, Default};

kfind(Key, List, Default) ->
	Ret=base:kfind(Key, List),
	case Ret of
		false ->
			{Key, Default};
		{Key, Value} ->
			{Key, Value}
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
	
