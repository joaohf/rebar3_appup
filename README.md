rebar3_appup
============

An rebar3 plugin


Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_appup, {git, "https://github.com/joaohf/rebar3_appup.git", {branch, "master"}}}
    ]}.

Then just call your plugin directly in an existing application:

    $ rebar3 appup compile

To have it invoked automatically when running `rebar3 compile` add it as a `provider_hooks`:

```
{provider_hooks, [
        {post, [{compile, {appup, compile}}]}
    ]}.
```

References:

* [Application upgrade file]http://erlang.org/doc/man/appup.html
* [Original rebar2 appup support] https://github.com/rebar/rebar/pull/449/files

Thanks
------

@lrascao who point me out his original version done in rebar2.

