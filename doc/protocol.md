# Libertree Protocol

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
character (|). Otherwise, the structures given are to be considered normative
literal JSON data.  Conformant servers MUST issue the strings described in the
case shown (the protocol case sensitive).

Requests are sent through the socket in this format:

    <request command> <data>

For example:

    INTRODUCE { "public_key": "-----BEGIN PGP PUBLIC KEY BLOCK-----\n..." }

### Invalid Requests

Given any data sent by a requester through the connection which does not
conform to this format, responders MUST respond with a "BAD REQUEST" code:

    { "code": "BAD REQUEST" }

Given request data that conforms to the required format, but which contains
a command not specified in this protocol, servers MUST respond with an
"UNKNOWN COMMAND" code:

    { "code": "UNKNOWN COMMAND" }

Given request data that conforms to the required format, contains a valid
command, but whose parameter data is not valid JSON, responders MUST respond
with a "BAD PARAMETER" code.  Servers MAY provide a message element to assist
request debugging.  Response structure:

    {
      "code": "BAD PARAMETER",
      [[ "message": <explanatory text> ]]
    }

Given a request parameter structure which is valid JSON, but which is missing
any required parameter, responders MUST respond with a "MISSING PARAMETER" code
and the name of the missing element.  Unless otherwise specified, a parameter
which is present but blank MUST be considered missing.  Responders MAY provide
a message element to assist request debugging.  Response structure:

    {
      "code": "MISSING PARAMETER",
      "parameter": <field name>,
      [[ "message": <explanatory text> ]]
    }

Given a request parameter structure which has a parameter of the wrong data
type, responders MUST respond with an "WRONG TYPE" error code and the name of
the offending element.  Responders MAY provide a message element to assist
request debugging.  Response structure:

    {
      "code": "WRONG TYPE",
      "parameter": <field name>,
      [[ "message": <explanatory text> ]]
    }

### INTRODUCE

Request Parameters:

    { "public_key": <public key> }

Response Structure:

    { "code": "OK" }
    |
    {
      "code": "OK",
      "challenge": <challenge>
    }

The first request sent through every connection is the INTRODUCE request.

If the responder has no record of a server having the given public key, the
responder MUST store the public key and the requester's IP address.  The
responder responds with an OK code, and MUST consider the requester
authenticated for the remainder of the connection's lifetime.

If the responder has a record of the given public key, the responder generates
a random string which MUST be at least 16 characters long, and encrypts it
using the public key.  It then responds with an OK code together with the
encrypted string.  The requester MUST then issue an AUTHENTICATE request.

The random string used for the challenge SHOULD NOT be the same as one used in
any previous connection, whether with the same requester or a different one.

### AUTHENTICATE

Request Parameters:

    { "response": <challenge response> }

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
lifetime.

### MEMBER

Request Parameters:

    {
      "username": <member username>,
      "avatar_url": <URL to image file>,
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
      "public": <boolean>,
      "text": <post text>,
    }

Response Structure:

    { "code": "OK" }
    |
    {
      "code": "REJECTED",
      [[ "message": <explanatory message> ]]
    }

A requester would use the POST command to share with a remote server a new post
created at the requester.

Responders MAY respond with a REJECTED code for any reason, and such a response
MAY be accompanied by an explanatory message.  When a POST request is rejected,
the requester SHOULD retry the POST request at a future time, but MAY elect
not to after several rejections.

### COMMENT

Request Parameters:

    {
      "id": <comment id on requester>,
      "member_id": <member id of author on requester>,
      "text": <post text>,
    }

Response Structure:

    { "code": "OK" }
    |
    {
      "code": "REJECTED",
      [[ "message": <explanatory message> ]]
    }

A requester would use the COMMENT command to share with a remote server a new
comment created at the requester.

Responders MAY respond with a REJECTED code for any reason, and such a response
MAY be accompanied by an explanatory message.  When a COMMENT request is rejected,
the requester SHOULD retry the COMMENT request at a future time, but MAY elect
not to after several rejections.

### SHARING-REQUEST

Request Parameters:

    {
      "from_username": <username>,
      "to_username": <username>
    }

Response Structure:

    { "code": "OK" }
    |
    { "code": "NOT FOUND" }

If the responder has no record of the to_username, it MUST respond with a "NOT
FOUND" code.

### SHARING-RETRACT

Request Parameters:

    {
      "from_username": <username>,
      "to_username": <username>
    }

Response Structure:

    { "code": "OK" }
    |
    { "code": "NOT FOUND" }

If the responder has no record of the to_username, it MUST respond with a "NOT
FOUND" code.

If the responder has a record of a member with the to_username, it MUST respond
with an "OK" code, regardless of whether or not a matching sharing request was
actually pending.

### SHARING-ACCEPT

Request Parameters:

    {
      "from_username": <username>,
      "to_username": <username>
    }

Response Structure:

    { "code": "OK" }
    |
    { "code": "NOT FOUND" }

If the responder has no record of the to_username, or has no record of a
pending sharing request which matchesit MUST respond with a "NOT FOUND" code.

If the responder has a matching pending sharing request, it MUST respond with
an "OK" code.
