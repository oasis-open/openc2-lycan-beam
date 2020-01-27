this directory is for an elixir implementation of haha.

will use phoenix webserver
(vs cowboy in haha erlang, note phoenix is built on top of cowboy)

this is a baby step towards making OpenC2 interfaces for IoT devices

this readme needs work

The current phoenix configuration installs static and html support including javascript and node.
If you only want an OpenC2 API,
this is overkill, makes for larger code base/executable, as well as potentially introducing security vulnerabilities.
For now it helps with more informative error messages.
It should be removed before being incorporated in any production code.

# HaHa

This requires elixir having been installed on your system.
See https://elixir-lang.org/install.html

Clone this directory and contents onto your system.

To start your Phoenix server:

  * Install dependencies with `mix deps.get`
  * Install Node.js dependencies with `cd assets && npm install`
  * Start Phoenix endpoint with `mix phx.server`

Now you can visit [`localhost:4000`](http://localhost:4000) from your browser.

Ready to run in production? Please [check our deployment guides](http://www.phoenixframework.org/docs/deployment).

## Learn more

  * Official website: http://www.phoenixframework.org/
  * Guides: http://phoenixframework.org/docs/overview
  * Docs: https://hexdocs.pm/phoenix
  * Mailing list: http://groups.google.com/group/phoenix-talk
  * Source: https://github.com/phoenixframework/phoenix
