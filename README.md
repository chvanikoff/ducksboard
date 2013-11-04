# Ducksboard push API wrapper

## About
The library is a wrapper around [ducksboard](http://ducksboard.com) [push API](http://dev.ducksboard.com/apidoc/push-api/).

## Usage:

```bash
%% Start the ducksboard application
ok = ducksboard:start(),
%% Set your API key
ok = ducksboard:set_api_key("k3hJeduHe9cwDs34kF42Ap1ndrl7bs85yHdgf8aS"),
%% Push your data to ducksboard
ok = ducksboard:push("current_answer", 42)
```
