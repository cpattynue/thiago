{application, thiago,
 [
  {description, "Thiago erlang"},
  {vsn, "1.0"},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { thiago_app, []}},
  {env, [{path, "/Users/PattyNunez/Desktop" }]},
  {modules,[thiago, thiago_app, thiago_sup, thiago_code]}
 ]}.
