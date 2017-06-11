-define(ERROR(X), error_logger:error_msg("[~s,~s,~s (~p)]: ~w~n",
                                         [?MODULE,
                                          ?FUNCTION_NAME,
                                          ?LINE,
                                          self(),
                                          X])).

-define(WARNING(X), error_logger:warning_msg("[~s,~s,~s (~p)]: ~w~n",
                                             [?MODULE,
                                              ?FUNCTION_NAME,
                                              ?LINE,
                                              self(),
                                              X])).

-define(INFO(X), error_logger:info_msg("[~s,~s,~s (~p)]: ~w~n",
                                       [?MODULE,
                                        ?FUNCTION_NAME,
                                        ?LINE,
                                        self(),
                                        X])).

% Macro for debug prints
-ifdef(debug).
-define(DEBUG(X), io:format("[~s,~s,~s (~p)]: ~w~n",
                            [?MODULE,
                             ?FUNCTION_NAME,
                             ?LINE,
                             self(),
                             X])).
-else.
-define(DEBUG(X), void).
-endif.
