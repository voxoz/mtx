-module(mtx).
-copyright('Synrc Research Center s.r.o.').
-description("Etsy StatsD protocol for DataDog dashboards").
-behaviour(gen_server).
-compile(export_all).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {socket, host, port}).

start_link(Opts) when is_list(Opts) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

increment(K,V,R) -> gen_server:cast(?MODULE, {sample, {increment,K,V,R}}).
decrement(K,V,R) -> gen_server:cast(?MODULE, {sample, {decrement,K,V,R}}).
timing(K,V,R)    -> gen_server:cast(?MODULE, {sample, {timing,K,V,R}}).
histogram(K,V,R) -> gen_server:cast(?MODULE, {sample, {histogram,K,V,R}}).
meter(K,V,R)     -> gen_server:cast(?MODULE, {sample, {meter,K,V,R}}).
gauge(K,V,R)     -> gen_server:cast(?MODULE, {sample, {gauge,K,V,R}}).

init([Host, Port]) ->
    process_flag(trap_exit, true),
    {ok, Socket} = gen_udp:open(0),
    {ok, #state{socket=Socket, host=Host, port=Port}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast({sample, {Operation, Key, Value, SampleRate}}, State) ->
    case random:uniform() =< SampleRate of
        true -> handle_cast({Operation, Key, Value, SampleRate}, State);
        false -> {noreply, State} end;

handle_cast({increment, K, V, R}, State) -> send([K,":",V,"|c|@",R],  State);
handle_cast({decrement, K, V, R}, State) -> send([K,":-",V,"|c|@",R], State);
handle_cast({timing,    K, V, R}, State) -> send([K,":",V,"|ms|@",R], State);
handle_cast({histogram, K, V, R}, State) -> send([K,":",V,"|h|@",R],  State);
handle_cast({meter,     K, V, R}, State) -> send([K,":",V,"|m|@",R],  State);
handle_cast({gauge,     K, V, R}, State) -> send([K,":",V,"|g|@",R],  State).

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

send(Stats,#state{socket=Socket,host=Host,port=Port}=State) ->
    gen_udp:send(Socket,Host,Port,lists:concat(Stats)), {noreply,State}.
