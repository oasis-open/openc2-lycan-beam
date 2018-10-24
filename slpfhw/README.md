StateLess Packet Filter Hello World (slpf_hw)
is a very simple actuator that is conformant with
the OpenC2 Language Specification and the slpf Actuator Profile but
does no actual security functions.
It has a Custom Actuator Profile (CAP) - put link here -
and will respond to the query openc2 command
with it's profile and schema.
It responds to the SLPF commands with responses that seem it preformed
the function, but it didn't.
It also responds to a custom query extension
"query Hello World" with "Hello World" - but that is all it does.
This code can be used as a stub to start development of actual SLPF lycans (ie transform OpenC2 to a proprietary API).

There are several implementations of slpf_hw.
The intent is to have erlang, elixir, and nerves implementations.
At the time of this writing, only erlang has been implemented.

As of this commit, the software is based on an
earlier OpenC2 Language Specification
and an earlier OpenC2 SLPF Actuator Profile Specification.
Issue 13 (https://github.com/oasis-open/openc2-lycan-beam/issues/13) is to bring this software up to conformance with
OpenC2 Language Specification CSDPRxx (fillin link).
and with
OpenC2 SLPF Actuator Profile Specification CSDPR (fillin link).
