bloom
=====

A Simple Pure Erlang Bloom filter


Build
-----

    $ rebar3 compile

Usage
-----

    Bloom = bloom:new(EstimatedSetSize, FailureOdds).
    NewBloom = bloom:add(Bloom, any_term).
    false = bloom:exists(NewBloom, other_term).

