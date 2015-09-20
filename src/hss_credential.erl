-module(hss_credential).
-include("hss.hrl").
-compile(export_all).

-spec new(username(), password()) -> #credential{}.
new(Username, Password) ->
    #credential{username = Username, password = Password}.

-spec get_username(#credential{}) -> username() | undefined.
get_username(Credential) ->
    Credential#credential.username.

-spec get_password(#credential{}) -> password() | undefined.
get_password(Credential) ->
    Credential#credential.password.
