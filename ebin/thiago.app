{application, thiago,
 [
  {description, "Thiago erlang"},
  {vsn, "1.0"},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib,
		  gnuart
                 ]},
  {mod, { thiago_app, []}},
  {modules,[thiago, thiago_app, thiago_sup]}
 ]}.