{application, thiago,
 [
  {description, "Thiago erlang"},
  {vsn, "1.0"},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib,
		  thiago
                 ]},
  {mod, { thiago_app, []}},
  {modules,[thiago, thiago_app, thiago_sup, thiago_code]}
 ]}.
