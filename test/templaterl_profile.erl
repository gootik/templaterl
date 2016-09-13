-module(templaterl_profile).

-compile(export_all).

run() ->
    fprofx:start(),
    {ok, Tracer} = fprofx:profile(start),
    fprofx:trace([start, {procs, new}, {tracer, Tracer}]),

    timing:function(
        fun() ->
            templaterl:compile(<<"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque mollis",
                                 "nunc non turpis scelerisque {{{vestibulum}}}. Vestibulum ac nisl sit amet quam",
                                 "{{{convallis}}} pretium vitae ut diam. Nunc at nisl consequat, {{{feugiat}}} arcu vel,",
                                 "eleifend magna. Nulla in justo nibh. Morbi ac leo eget purus ornare sodales",
                                 "quis non orci. Sed et {{{lorem}}} vel est efficitur tincidunt non a ante.",
                                 "Duis lobortis ipsum eu suscipit posuere. Interdum et malesuada fames ac ante",
                                 "ipsum primis in faucibus. Morbi {{{efficitur}}} lorem ut est {{{congue}}} rhoncus. Aenean",
                                 "sodales lobortis erat, eu facilisis massa varius at. In rhoncus lacus ut turpis",
                                 "pretium, vel convallis augue consequat. Vestibulum hendrerit ultrices est sit",
                                 "amet {{{pellentesque}}}. Sed et turpis vitae dui ultrices pellentesque nec ut justo.",
                                 "Nullam vitae sem congue, molestie neque a, hendrerit eros. Pellentesque",
                                 "{{{sollicitudin}}} eu lorem a malesuada">>,
                               #{<<"vestibulum">> => <<"REPLACE1">>,
                                 <<"convallis">> => <<"REPLACE2">>,
                                 <<"feugiat">> => <<"REPLACE3">>,
                                 <<"lorem">> => <<"REPLACE4">>,
                                 <<"efficitur">> => <<"REPLACE5">>,
                                 <<"congue">> => <<"REPLACE6">>,
                                 <<"pellentesque">> => <<"REPLACE7">>,
                                 <<"sollicitudin">> => <<"REPLACE8">>})
        end, 1000, 20),

    fprofx:trace(stop),
    fprofx:analyse([totals, {dest, ""}]),
    fprofx:stop().