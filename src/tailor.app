{application, tailor,
 [{description, "tailor"},
  {vsn, "0.01"},
  {modules, [
    tailor,
    tailor_app,
    tailor_sup,
    tailor_web,
    tailor_deps
  ]},
  {registered, []},
  {mod, {tailor_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
