-module(wx_cfg).
%%%=======================STATEMENT====================
-description("wx_cfg").
-copyright('').
-author("wmh, SuperMuscleMan@outlook.com").
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([set/3, set/4, set/5, get/2, get/1]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).
-define(Default_Opt, [named_table, {read_concurrency, true}]). %% 配置ets默认选项

-record(state, {ets = [], ets_opts = ?Default_Opt}).
-record(cfg_info, {
	name :: atom(),
	file_path :: string(),
	line :: integer(),
	time :: integer()
}).

%%%===================================================================
%%% API
%%%===================================================================
%% -----------------------------------------------------------------
%% Func:
%% Description:存储配置
%% Returns:
%% -----------------------------------------------------------------
set(FilePath, Line, Table, KvList) ->
	set(FilePath, Line, Table, KvList, ?Default_Opt).
set(FilePath, Line, Table, KvList, Opt) when is_list(KvList), length(KvList) > 0 ->
	Opt1 = check_opt(Opt),
	gen_server:call(?SERVER, {set, FilePath, Line, Table, KvList, Opt1}).
%% -----------------------------------------------------------------
%% Func:
%% Description:存儲配置 非配置文件途徑
%% Returns:
%% -----------------------------------------------------------------
set(Table, Key, Value) ->
	gen_server:call(?SERVER, {set, Table, Key, Value}).
%% -----------------------------------------------------------------
%% Func:
%% Description:根据key获取配置
%% Returns:
%% -----------------------------------------------------------------
get(Table, Key) ->
	case ets:info(Table, size) of
		undefined ->
			none;
		_ ->
			case ets:lookup(Table, Key) of
				[{_, V}] -> V;
				[] -> none;
				[V] -> V
			end
	end.
%% -----------------------------------------------------------------
%% Func:
%% Description:根据table获取所有
%% Returns:
%% -----------------------------------------------------------------
get(Table) ->
	case ets:info(Table, size) of
		undefined ->
			none;
		_ ->
			ets:tab2list(Table)
	end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
	{ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init(_) ->
	Ets = ets:new(?MODULE, [named_table, {keypos, #cfg_info.name}, set, protected]),
	{ok, #state{ets = Ets}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
		State :: #state{}) ->
	{reply, Reply :: term(), NewState :: #state{}} |
	{reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_call({set, Table, Key, Value}, _From, #state{ets = Ets,
	ets_opts = Opts} = State) ->
	try
		Time = wx_time:now_second(),
		create_table(Ets, Table, Opts),
		ets:insert(Ets, #cfg_info{name = Table, file_path = [], line = 0, time = Time}),
		Old = get(Table, Key),
		ets:insert(Table, {Key, Value}),
		{reply, {ok, Old}, State}
	catch
		E1:E2:E3 ->
			{reply, {err, {E1, E2, E3}}, State}
	end;
handle_call({set, FilePath, Line, Table, KvList, Opt}, _From,
		#state{ets = Ets, ets_opts = Opts} = State) ->
	Time = wx_time:now_second(),
	create_table(Ets, Table, lists:append(Opt, Opts)),
	ets:insert(Ets, #cfg_info{name = Table, file_path = FilePath, line = Line, time = Time}),
	ets:insert(Table, KvList),
	{reply, ok, State};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
		State :: #state{}) -> term()).
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
		Extra :: term()) ->
	{ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% -----------------------------------------------------------------
%% Func:
%% Description:检测
%% Returns:
%% -----------------------------------------------------------------
check_opt(Opt) ->
	OptList = wx_lib:get_values(Opt, [{keypos, 1}, {named_table, none}]),
	check_opt_(OptList, []).
check_opt_([{named_table, none} | T], Result) ->
	check_opt_(T, [named_table | Result]);
check_opt_([H | T], Result) ->
	check_opt_(T, [H | Result]);
check_opt_([], Result) ->
	Result.
%% -----------------------------------------------------------------
%% Func:
%% Description:创建table
%% Returns:
%% -----------------------------------------------------------------
create_table(Ets, Table, Opt) ->
	case ets:member(Ets, Table) of
		false ->
			ets:new(Table, Opt);
		_ ->
			ok
	end.