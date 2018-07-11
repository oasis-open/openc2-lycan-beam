{erl_opts, [debug_info]}.
{deps, []}.

{relx, [{release, { haha, "0.1.0" },
         [haha,
          sasl]},

        {sys_config, "./config/sys.config"},
        {vm_args, "./config/vm.args"},

        {dev_mode, true},
        {include_erts, false},

        {extended_start_script, true}]
}.

{profiles, [ {}
           , { test
             , [ { deps, [ shotgun ] } ]
             }
           , { prod
             , [ { relx
                 , [ {dev_mode, false}
                   , {include_erts, true}
                   ]
                 }
               ]
             }
           ]
}.
