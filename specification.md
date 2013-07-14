# Libertree Specification

## Introduction

This document describes the behaviour required of servers that would be members
of a Libertree network.

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD",
"SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this document are to be
interpreted as described in RFC 2119.

## Connectivity

Servers MUST listen on TCP port 14404. Connections made from one server to
another MUST be made to this port.

## Request Protocol

For any connection, the server that initiates the connection is the requester,
and the server that receives the connection is the responder.  Messages sent
from a requester to a responder are called requests.  Messages sent from a
responder to a requester are called responses.

All messages sent through a connection, in either direction, whether requests
or responses, MUST be terminated with a newline character (ASCII 13). All
messages MUST NOT contain any newline characters.  Messages MUST be encoded in
UTF-8. All message contents, including structures (arrays and hashes) MUST be
valid JSON structures.

This communication protocol is called the Libertree Protocol, abbreviated LTP.
LTP may be used as a noun and as an adjective.

## Server Authentication

Every server MUST have a pair of encryption keys: one private and one public.
A server's public key constitutes its unique identity.  Every connection made
from a requester to a responder MUST begin with an INTRODUCE request.  A
responder MUST NOT process any requests until the requester has successfully
authenticated via the INTRODUCE and/or AUTHENTICATE requests.  A responder MUST
respond with a "NOT AUTHENTICATED" code to any requests (other than INTRODUCE
and AUTHENTICATE) from a requester that is not authenticated.  A responder MAY
close the connection after responding with a "NOT AUTHENTICATED" code.

## Requests

In the following descriptions, required components are indicated with angle
brackets (<>). Optional components are indicated with double square brackets
([[]]). Multiple possibilities are indicated by separating them with a pipe
character (|). Repetitions of an element are indicated with ellipses (...).
Otherwise, the structures given are to be considered normative literal JSON
data.  Conformant servers MUST issue the strings described in the case shown
(the protocol is case sensitive).

Requests are sent through the socket in this format:

    <request command> <data>

For example:

    INTRODUCE { "public_key": "public key data here" }

### Invalid Requests

Given any data sent by a requester through the connection which does not
conform to this format, responders MUST respond with a "BAD REQUEST" code:

    { "code": "BAD REQUEST" }

Given request data that conforms to the required format, but which contains
a command not specified in this protocol, servers MUST respond with an
"UNKNOWN COMMAND" code:

    { "code": "UNKNOWN COMMAND" }

Given request data that conforms to the required format, contains a valid
command, but whose parameter data (a) is not valid JSON, (b) is missing
required parameters, or (c) has parameters of the wrong data type, responders
MUST respond with a "BAD PARAMETER" code.  Unless otherwise specified, a
parameter which is present but blank MUST be considered missing.  Servers MAY
provide a message element to assist request debugging.  Response structure:

    {
      "code": "BAD PARAMETER",
      [[ "message": <explanatory text> ]]
    }

Given a request sent from a server that is not in any forest of which the
receiver is a member, servers MUST respond with an "UNRECOGNIZED SERVER"
code.


### INTRODUCE

Request Parameters:

    {
      "public_key": <public key>
    }

Response Structure:

    { "code": "OK" }
    |
    {
      "code": "OK",
      "challenge": <challenge>
    }

The first request that MUST be sent through every connection is the INTRODUCE
request.

If the responder has no record of a server having the given public key, the
responder MUST store the public key and the requester's IP address.  The
responder responds with an OK code, and MUST consider the requester
authenticated for the remainder of the connection's lifetime.

If the responder has a record of the given public key, the responder generates
a random string which MUST be at least 16 characters long, and MUST consist
only of ASCII characters.  The responder then encrypts it using the public key,
and responds with an OK code together with the encrypted string.  The requester
SHOULD then issue an AUTHENTICATE request.

The random string used for the challenge SHOULD NOT be the same as one used in
any previous connection, whether with the same requester or a different one.

### AUTHENTICATE

Request Parameters:

    {
      "response": <challenge response>,
      [[ "name": <human-readable name of requester> ]]
    }

Response Structure:

    { "code": "OK" }
    |
    {
      "code": "ERROR",
      [[ "message": <explanatory text> ]]
    }

The requester, after receiving an INTRODUCE challenge from the responder, MUST
decrypt the challenge string using its (the requester's) private key, and issue
an AUTHENTICATE request with the decrypted text.  If AUTHENTICATE is issued
without INTRODUCE being given previously in the same connection, the responder MUST
respond with an ERROR code.

If the challenge response does not match the original challenge string, then
the responder MUST respond with an ERROR code.  The responder MAY provide a
"message" element containing text that would assist a human in debugging.
The responder MAY close the connection after the failed challenge.

If the challenge response matches the original (unencrypted) challenge string
generated by the responder, then the responder MUST respond with an OK code,
and consider the requester authenticated for the remainder of the connection's
lifetime.  On successful authentication, the responder should update its
records with the current IP of the requester.

The requester MAY provide a name for itself.  This name is intended to be used
for display purposes.  It MUST be 32 characters in length or shorter.  It MUST
NOT be empty.  Each of its characters MUST be a letter, a number, a period, or
a hyphen.  If provided by the requester, the responder SHOULD update its records
for the server with the new name.

### FOREST

Request Parameters:

    {
      "id": <forest id on requester>,
      "name": <forest name>,
      "trees": [
        { "domain": <domain of tree> },
        [[ { "domain": <domain of tree> }, ... ]]
      ]
    }

Response Structure:

    { "code": "OK" }
    |
    {
      "code": "REJECTED",
      [[ "message": <explanatory message> ]]
    }

A requester would issue a FOREST comment to inform another server about a
a forest that originated at the requester, including its membership.

### MEMBER

Request Parameters:

    {
      "username": <member username>,
      [[ "avatar_url": <URL to image file>, ]]
      [[ "profile": {
        "name_display": <member's profile's display name>,
        "description": <member's profile's description>
      } ]]
    }

Response Structure:

    { "code": "OK" }
    |
    {
      "code": "ERROR",
      [[ "message": <explanatory message> ]]
    }

A requester would use the MEMBER command to share with a remote server
information about a member at the requester server.

If the responder responds with ERROR, it MAY provide a "message" element
containing text that would assist a human in debugging.

### POST

Request Parameters:

    {
      "username": <member username>,
      "id": <post id on requester>,
      "visibility": <"internet" | "forest">,
      "text": <post text>,
    }

Response Structure:

    { "code": "OK" }
    |
    {
      "code": "NOT FOUND",
      "missing": ["MEMBER"]
    }
    |
    {
      "code": "REJECTED",
      [[ "message": <explanatory message> ]]
    }

A requester would use the POST command to share with a remote server a new
post created at the requester, or an update of a post on the requester.

Responders SHOULD return a "NOT FOUND" code to indicate that it has no record
of the given member.  Requesters SHOULD send a request matching the "missing"
parameter of the "NOT FOUND" response.

Responders MAY respond with a REJECTED code for any reason, and such a response
MAY be accompanied by an explanatory message.  When a POST request is rejected,
the requester SHOULD retry the POST request at a future time, but MAY elect
not to after several rejections.

If the responder has a record of a post with the given remote id (on the requester)
by the given remote member, it MUST update its local record with the given text
and public values.  Responders SHOULD create a post revision for the post's values
prior to performing the local update.

### POST-DELETE

Request Parameters:

    {
      "id": <post id on requester>,
    }

Response Structure:

    { "code": "OK" }
    |
    { "code": "NOT FOUND" }

A requester would use the POST-DELETE command to request that a remote server
delete its copy of a post that originated at the requester.

Responders MAY return a "NOT FOUND" code to indicate that it has no record
of the given post.

### COMMENT

Request Parameters:

    {
      "id": <comment id on requester>,
      "post_id": <post id on post origin>,
      "origin": <domain of post origin>,
      "username": <member username of comment author on requester>,
      "text": <comment text>,
    }

Response Structure:

    { "code": "OK" }
    |
    {
      "code": "NOT FOUND",
      "missing": ["MEMBER"]
    }
    |
    {
      "code": "NOT FOUND",
      "missing": ["POST"]
    }
    |
    {
      "code": "NOT FOUND",
      "missing": ["MEMBER", "POST"]
    }
    |
    {
      "code": "REJECTED",
      [[ "message": <explanatory message> ]]
    }

A requester would use the COMMENT command to share with a remote server a new
comment created at the requester.

Responders SHOULD return a "NOT FOUND" code to indicate that it has no record
of the comment author.  Responders SHOULD return a "NOT FOUND" code to indicate
that it has no record of the given post.  Requesters SHOULD send a request
matching the "missing" parameter of the "NOT FOUND" response.

Responders MAY respond with a REJECTED code for any reason, and such a response
MAY be accompanied by an explanatory message.  When a COMMENT request is rejected,
the requester SHOULD retry the COMMENT request at a future time, but MAY elect
not to after several rejections.

### COMMENT-DELETE

Request Parameters:

    {
      "id": <comment id on requester>,
    }

Response Structure:

    { "code": "OK" }
    |
    { "code": "NOT FOUND" }

A requester would use the COMMENT-DELETE command to request that a remote server
delete its copy of a comment that originated at the requester.

Responders MAY return a "NOT FOUND" code to indicate that it has no record
of the given comment.

### POST-LIKE

Request Parameters:

    {
      "id": <like id on requester>,
      "post_id": <post id on post origin>,
      "origin": <domain of post origin>,
      "username": <member username of liker on requester>,
    }

Response Structure:

    { "code": "OK" }
    |
    {
      "code": "NOT FOUND",
      "missing": ["MEMBER"]
    }
    |
    {
      "code": "NOT FOUND",
      "missing": ["POST"]
    }
    |
    {
      "code": "NOT FOUND",
      "missing": ["MEMBER", "POST"]
    }
    |
    {
      "code": "REJECTED",
      [[ "message": <explanatory message> ]]
    }

A requester would use the POST-LIKE command to share with a remote server a new
Like created at the requester.

Responders SHOULD return a "NOT FOUND" code to indicate that it has no record
of the Liker.  Responders SHOULD return a "NOT FOUND" code to indicate that it
has no record of the given post.  Requesters SHOULD send a request matching the
"missing" parameter of the "NOT FOUND" response.

Responders MAY respond with a REJECTED code for any reason, and such a response
MAY be accompanied by an explanatory message.  When a POST-LIKE request is rejected,
the requester SHOULD retry the POST-LIKE request at a future time, but MAY elect
not to after several rejections.

### POST-LIKE-DELETE

Request Parameters:

    {
      "id": <like id on requester>,
    }

Response Structure:

    { "code": "OK" }
    |
    { "code": "NOT FOUND" }

A requester would use the POST-LIKE-DELETE command to request that a remote server
delete its copy of a Like that originated at the requester.  i.e. Unlike
the post.

Responders MAY return a "NOT FOUND" code to indicate that it has no record
of the given Like.

### COMMENT-LIKE

Request Parameters:

    {
      "id": <like id on requester>,
      "comment_id": <comment id on comment origin>,
      "origin": <domain of comment origin>,
      "username": <member username of liker on requester>,
    }

Response Structure:

    { "code": "OK" }
    |
    {
      "code": "NOT FOUND",
      "missing": ["MEMBER"]
    }
    |
    {
      "code": "NOT FOUND",
      "missing": ["COMMENT"]
    }
    |
    {
      "code": "NOT FOUND",
      "missing": ["MEMBER", "COMMENT"]
    }
    |
    {
      "code": "REJECTED",
      [[ "message": <explanatory message> ]]
    }

A requester would use the COMMENT-LIKE command to share with a remote server a new
Like created at the requester.

Responders SHOULD return a "NOT FOUND" code to indicate that it has no record
of the Liker.  Responders SHOULD return a "NOT FOUND" code to indicate that it
has no record of the given comment.  Requesters SHOULD send a request matching
the "missing" parameter of the "NOT FOUND" response.

Responders MAY respond with a REJECTED code for any reason, and such a response
MAY be accompanied by an explanatory message.  When a COMMENT-LIKE request is rejected,
the requester SHOULD retry the COMMENT-LIKE request at a future time, but MAY elect
not to after several rejections.

### COMMENT-LIKE-DELETE

Request Parameters:

    {
      "id": <like id on requester>,
    }

Response Structure:

    { "code": "OK" }
    |
    { "code": "NOT FOUND" }

A requester would use the COMMENT-LIKE-DELETE command to request that a remote server
delete its copy of a Like that originated at the requester.  i.e. Unlike
the comment.

Responders MAY return a "NOT FOUND" code to indicate that it has no record
of the given Like.

### MESSAGE

Request Parameters:

    {
      "username": <username of message author on requester>,
      "recipients": [
        {
          "username": <recipient's username> },
          "origin": <domain of recipient's server>,
        }
        [[ ... ]]
      ],
      "text": <message text>,
    }

Response Structure:

    { "code": "OK" }
    |
    {
      "code": "NOT FOUND",
      "missing": ["MEMBER"]
    }
    |
    { "code": "NOT FOUND" }
    |
    {
      "code": "REJECTED",
      [[ "message": <explanatory message> ]]
    }

A requester would use the MESSAGE command to share with a remote server a new
direct message created at the requester having one or more recipients.
Requesters SHOULD NOT send more than one copy of the same message and recipient
set to any given server.

Responders SHOULD return a "NOT FOUND" code to indicate that it has no record
of the message author.  Requesters SHOULD send a request for the missing MEMBER
if a NOT FOUND with (missing: MEMBER) is sent.

A responder MAY respond with a "NOT FOUND" code if it has no record of one or
more of the recipients.

Responders MAY respond with a REJECTED code for any reason, and such a response
MAY be accompanied by an explanatory message.  When a MESSAGE request is rejected,
the requester SHOULD retry the MESSAGE request at a future time, but MAY elect
not to after several rejections.

### CHAT

Request Parameters:

    {
      "username": <username of sender on requester>,
      "recipient_username": <recipient's username on responder>,
      "text": <chat message text>,
    }

Response Structure:

    { "code": "OK" }
    |
    {
      "code": "NOT FOUND",
      "missing": ["MEMBER"]
    }
    |
    { "code": "NOT FOUND" }
    |
    {
      "code": "REJECTED",
      [[ "message": <explanatory message> ]]
    }

A requester would use the CHAT command to send a chat message created at the requester
to a recipient on the responder.

Responders SHOULD return a "NOT FOUND" code to indicate that it has no record
of the message sender.  Requesters SHOULD send a request for the missing MEMBER
if a NOT FOUND with (missing: MEMBER) is sent.

A responder MAY respond with a "NOT FOUND" code if it has no record of the recipient.

Responders MAY respond with a REJECTED code for any reason, and such a response
MAY be accompanied by an explanatory message.  When a CHAT request is rejected,
the requester SHOULD retry the CHAT request at a future time, but MAY elect
not to after several rejections.

### POOL

Request Parameters:

    {
      "username": <member username>,
      "id": <pool id on requester>,
      "name": <pool name on requester>,
    }

Response Structure:

    { "code": "OK" }
    |
    {
      "code": "NOT FOUND",
      "missing": ["MEMBER"]
    }
    |
    {
      "code": "REJECTED",
      [[ "message": <explanatory message> ]]
    }

A requester would use the POOL command to share with a remote server a pool
that was sprung at the requester, or updates made to a sprung pool originating
at the requester.

Responders SHOULD return a "NOT FOUND" code to indicate that it has no record
of the given member.  Requesters SHOULD send a request matching the "missing"
parameter of the "NOT FOUND" response.

Responders MAY respond with a REJECTED code for any reason, and such a response
MAY be accompanied by an explanatory message.  When a POOL request is rejected,
the requester SHOULD retry the POOL request at a future time, but MAY elect
not to after several rejections.

If the responder has a record of a pool with the given remote id (on the
requester) by the given remote member, it MUST update its local record with the
given name.


### POOL-DELETE

Request Parameters:

    {
      "username": <member username>,
      "id": <pool id on requester>,
    }

Response Structure:

    { "code": "OK" }
    |
    { "code": "NOT FOUND" }

A requester would use the POOL-DELETE command to request that a remote server
delete its copy of a pool that originated at the requester.  This could be sent
either when a pool is actually deleted, or when it is merely unsprung.

Responders MAY return a "NOT FOUND" code to indicate that it has no record
of the given pool.


### POOL-POST

Request Parameters:

    {
      "pool_id": <pool id on requester>,
      "post_id": <post id on post origin>,
      "origin": <domain of post origin>,
      "username": <member username of pool owner on requester>,
    }

Response Structure:

    { "code": "OK" }
    |
    {
      "code": "NOT FOUND",
      "missing": ["POOL"] | ["POST"] | ["MEMBER"] | ["POOL", "POST"] |
                 ["POOL", "MEMBER"] | ["POST", "MEMBER"] |
                 ["POOL", "POST", "MEMBER"]
    }
    |
    {
      "code": "REJECTED",
      [[ "message": <explanatory message> ]]
    }

A requester would use the POOL-POST command to inform a remote server that a
post was added to a pool at the requester.

Responders SHOULD return a "NOT FOUND" code to indicate that it has no record
of the pool, the post, the member, or some, or each.  Requesters SHOULD send a
request matching the "missing" parameter of the "NOT FOUND" response.

Responders MAY respond with a REJECTED code for any reason, and such a response
MAY be accompanied by an explanatory message.  When a POOL-POST request is
rejected, the requester SHOULD retry the POOL-POST request at a future time,
but MAY elect not to after several rejections.

### POOL-POST-DELETE

Request Parameters:

    {
      "pool_id": <pool id on requester>,
      "post_id": <post id on post origin>,
      "origin": <domain of post origin>,
      "username": <member username of pool owner on requester>,
    }

Response Structure:

    { "code": "OK" }
    |
    { "code": "NOT FOUND" }

A requester would use the POOL-POST-DELETE command to request that a remote
server delete its record of an association of a post to a pool which originated
at the requester.

Responders MAY return a "NOT FOUND" code to indicate that it has no record
of the given member-pool-post association.
