Https Api Globalwelcome Actuator (HAGA)
is a simple actuator that is conformant with
the OpenC2 Language Specification but
does no actual security functions.
It is slightly more complex than the HAHA "Hello World" actuator
in that the "Global Welcome" actuator can respond
to a variety of commands including all 32 actions, all 25 targets,
and ? args.
Note it does actually perform the actions but it does accept
the command and pretend to perform.
It also does some rudimentary error handling (eg illegal json,
an action is not
in the Language Specification, etc).
It does NOT do semantic checking on validity of command -
for this actuator only certain set combos are valid
(some that make no sense).

It has a Custom Actuator Profile (CAP)
and will respond to the query openc2 command
with it's profile and schema.
It also responds to a custom query extension
"query hello" with "global welcome".

this readme needs work
