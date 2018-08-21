Https Api Helloworld Actuator (HAHA)
is a very simple actuator that is conformant with
the OpenC2 Language Specification but
does no actual security functions.
It has a Custom Actuator Profile (CAP)
and will respond to the query openc2 command
with it's profile and schema.
It also responds to a custom query extension
"query Hello World" with "Hello World" - but that is all it does.

There are several implementations of haha.
The intent is to have erlang, elixir, and nerves implementations.
At the time of this writing, only erlang has been implemented. Two different erlang implementations
were done to show a distinction in error handling for discussion by the OpenC2 Technical Committee.
The first (put hotlink here) is what is probably the
typical implementation. The second (put hotlink here) uses much fewer codes for error responses and is implemented 1/3 the code.
