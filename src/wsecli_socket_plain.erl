%% @hidden
-module(wsecli_socket_plain).
-include("wsecli.hrl").

-export([start_link/5]).
-export([init/5]).
-export([loop/1]).


%%========================================
%% Data
%%========================================
-record(state, {
    client :: pid(),
    socket :: socket()
  }).

%%========================================
%% API
%%========================================
-spec start_link(
  Host    :: string(),
  Port    :: inet:port_number(),
  Client  :: pid(),
  Options :: list(gen_tcp:connect_option()),
  Timeout :: timeout()
) -> {ok, socket()} | {error, term()}.
start_link(Host, Port, Client, Options, Timeout) ->
  proc_lib:start_link(?MODULE, init, [Host, Port, Client, Options, Timeout]).

-spec init(
  Host    :: string(),
  Port    :: inet:port_number(),
  Client  :: pid(),
  Options :: list(gen_tcp:connect_option()),
  Timeout :: timeout()
  ) -> ok.
init(Host, Port, Client, Options, Timeout) ->
  case gen_tcp:connect(Host, Port, Options, Timeout) of
    {ok, Socket} ->
      State        = #state{ client = Client, socket = Socket },
      proc_lib:init_ack({ok, self()}),
      loop(State);
    {error, _} = Error ->
      proc_lib:init_ack(Error)
  end.

%%========================================
%% Internal
%%========================================
loop(State) ->
  receive
    {socket, send, Data}   ->
      case gen_tcp:send(State#state.socket, Data) of
        ok ->
          ok;
        {error, _} = Error ->
          wsecli_socket:notify_client(Error, State#state.client)
      end,
      loop(State);
    {socket, close}        ->
      gen_tcp:close(State#state.socket);
    {tcp, _, Data}         -> % Received data
      wsecli_socket:notify_client({data, Data}, State#state.client),
      loop(State);
    {tcp_closed, _}        -> % Close
      wsecli_socket:notify_client(close, State#state.client);
    {tcp_error, _, Reason} -> % Error
      wsecli_socket:notify_client({error, Reason}, State#state.client)
  end.
