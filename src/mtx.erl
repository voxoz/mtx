-module(mtx).
-copyright('Synrc Research Center s.r.o.').
-description("Etsy StatsD protocol for DataDog dashboards").
-behaviour(gen_server).
-compile(export_all).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {socket, host, port}).

start_link(Opts) when is_list(Opts) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

increment(Key, Magnitude, SampleRate) -> gen_server:cast(?MODULE, {sample, {increment, Key, Magnitude, SampleRate}}).
decrement(Key, Magnitude, SampleRate) -> gen_server:cast(?MODULE, {sample, {decrement, Key, Magnitude, SampleRate}}).
timing(Key, Value, SampleRate)        -> gen_server:cast(?MODULE, {sample, {timing, Key, Value, SampleRate}}).
gauge(Key, Value, SampleRate)         -> gen_server:cast(?MODULE, {sample, {gauge, Key, Value, SampleRate}}).

init([Host, Port]) ->
    process_flag(trap_exit, true),
    {ok, Socket} = gen_udp:open(0),
    {ok, #state{socket=Socket, host=Host, port=Port}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast({sample, {Operation, Key, Value, SampleRate}}, State) ->
    case random:uniform() =< SampleRate of
        true -> handle_cast({Operation, Key, Value, SampleRate}, State);
        false -> {noreply, State} end;

handle_cast({increment, Key, Magnitude, SampleRate}, State) -> send(io_lib:format("~s:~B|c|@~f", [Key, Magnitude, SampleRate]),State);
handle_cast({decrement, Key, Magnitude, SampleRate}, State) -> send(io_lib:format("~s:-~B|c|@~f", [Key, Magnitude, SampleRate]), State);
handle_cast({timing, Key, Value, SampleRate}, State)        -> send(io_lib:format("~s:~B|ms|@~f", [Key, Value, SampleRate]), State);
handle_cast({histogram, Key, Value, SampleRate}, State)     -> send(io_lib:format("~s:~B|h|@~f", [Key, Value, SampleRate]), State).
handle_cast({meter, Key, Value, SampleRate}, State)         -> send(io_lib:format("~s:~B|m|@~f", [Key, Value, SampleRate]), State).
handle_cast({gauge, Key, Value, SampleRate}, State)         -> send(io_lib:format("~s:~B|g|@~f", [Key, Value, SampleRate]), State).

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

send(Stats,#state{socket=Socket,host=Host,port=Port}) -> gen_udp:send(Socket,Host,Port,Stats).
