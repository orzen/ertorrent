-record(reply, {id, func}).

-export([l_reply/0,
         l_reply/1,
         g_reply/0,
         g_reply/1,
         c_reply/2,
         reply/2]).

l_reply() ->
    ID = self(),
    #reply{id = ID,
           func = fun(Msg) ->
                      ID ! Msg
                  end}.

l_reply(ID) ->
    #reply{id = ID,
           func = fun(Msg) ->
                      ID ! Msg
                  end}.

g_reply() ->
    ID = self(),
    #reply{id = ID,
           func = fun(Msg) ->
                      ID ! Msg
                  end}.

g_reply(ID) ->
    #reply{id = ID,
           func = fun(Msg) ->
                      ID ! Msg
                  end}.

c_reply(ID, Func) ->
    #reply{id = ID,
           func = Func}.

reply(#reply{func = Func}, Msg) ->
    Func(Msg).
