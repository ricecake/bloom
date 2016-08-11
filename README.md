[![Build Status](https://travis-ci.org/ricecake/bloom.svg?branch=master)](https://travis-ci.org/ricecake/bloom)

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
    
    Bloom2 = bloom:new(EstimatedSetSize, FailureOdds).
    OtherBloom = bloom:add(Bloom, any_term).
    
    UnionBloom = bloom:union(Bloom, OtherBloom).
    DiffBloom = bloom:difference(Bloom, OtherBloom).
    IntersectBloom = bloom:intersection(Bloom, OtherBloom).

