# hahax1

This is a copy of haha for experimenting.
X1 (the first experiment) is to reduce the codes size by only returning one
error (ie internal server error on bad commands).

Both haha and hahax1 pass the same test suite - given the difference
in status codes (hahax1 is all 500's) and error messages (hahax1 has no
  error messages in the body of the response).

Stripping out comments and debugging, haha is 168 lines of erlang
and hahax1 is 58 lines of erlang.
So the added precision in error messages causes the code to be 2.9 times
as long!
