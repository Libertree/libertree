require 'libertree/server'
require_relative 'factories'

class MockServer
  include Libertree::Server

  attr_reader :responses

  def initialize
    @ip_remote = '192.168.0.100'
    @responses = []
  end

  def respond(data)
    @responses << data.to_json
  end

  def close_connection_after_writing
    # noop in testing
  end

  def response
    JSON.parse @responses[-1]
  end

  def has_responded_with?(hash)
    response.should == hash
  end

  def has_responded_with_code?(code)
    has = ( response['code'] == code )
    if ! has
      $stderr.puts "Expected #{code.inspect}, got #{response['code'].inspect}.  Error message: #{response['message'].inspect}"
    end
    has
  end
end

$test_public_key = %{
-----BEGIN PGP PUBLIC KEY BLOCK-----
Version: GnuPG v2.0.17 (GNU/Linux)

mQENBE9qLxoBCADHKbpC26rQ6KdZBp3qDJBWIi7np9Z+XJJOa2H/41eFyCxNYp+N
f3VYiTOkj463AZWGWTIiW83nhNOMTstNexL1/XWwoHdoxfy0G4XiwJ+zXYV5zenL
HipUE/VT1UKdruEv4iG7Dggy0WNmIpHwJuzsLqlLDNxDH79TH09DZiTkgYR7YjT/
7V9+qJvFha9OBQWXYTNEDpB9ixMXMpCDVIx2oFuTSMbnMZNb8+f7DZ0RKxGqVQBW
MIReE6FCgdX3X8qRC17QNoo/X9oIedLoc3yCYxIey2sWHMOH2z+X0GBIVLoP+DJ5
BEIo1qUQGIVdGkKYgVweOVZa8Oo28wmlqhMBABEBAAG0B01lbWJlcjKJATgEEwEC
ACIFAk9qLxoCGwMGCwkIBwMCBhUIAgkKCwQWAgMBAh4BAheAAAoJEPmJjRAbBEuA
k+EIAIvH93ZDoacj2lZ34QDd3Fi98rttdV47ndNpZUkfWbzU0WijEpOIXT0Lv+A2
V0R77h+Wmk2YE9TZK/eYHCsbMWzFi9XF16IV/oUOCroion6D0D4dI28SUY1D58Du
Xxtqcef9TJEIqXFeDideKAPgjJnzqyledjNNCUioZlQG3LVNMy6p+KjPFFz/tSOI
OCz5Au0ihMuUvGGhvURUQYgmwRmZn0QyHPpQIpWrbliw5MmCpc4IuAaimEMt9odD
MC9BTZZzdpKx6tTyUYrBKBrZkX1b3lI3rGKH1a6Mk99sGfO4Gna73yW1kO96LkBp
m9nobUPw7YB8QGKIcaCKMJtpMYi5AQ0ET2ovGgEIAOGaY3mE3MM1Z9wSbGV5uE+p
/1iKpzH+eh/044UWrmxmd/TMFZCCheaF7JRyouWy3SbVwKz2maGv8p1z6HNpHCSE
S1jPSdPBesCA0Uxzs/7zE+/hYfQv8yxF+W6wt02nWuIYX5nS/wqN+OPfBiw8j7gM
MNBj/w0gfDmrn8ZifhPJimDXqAZaNpjK34NUrcjk1tYzMoxyTRDBWtdhXl3xzfPt
BskMmdtSmejCXWtr8Vxd50cjWfsB1L1+Fbh/jAMN604ozT+YWO2Y/AJo0UIKvDmi
kjAZ1p85r9C9ApgHCu+fcKDRpKeI6HgU9Tjh6hO4VTNkgTrvjUUYRb4aVLMJwtsA
EQEAAYkBHwQYAQIACQUCT2ovGgIbDAAKCRD5iY0QGwRLgFYiCACh9oEwCaopiKg/
GiJ3qrEpmv37IogwNra5Y4Lyr5Wrk3HJNxoR/8Q7PgkkQAYgdQWPurzd6kTzhnKm
aLyoht8IEg4mYMDjhy00tivxfjDdERuybhZsdcdJEollGol1cgIWz3Rxs6XykaRl
4I2IMQxOsuD3dF0jajNCj/FK33P0JRzvvDobjQK99uFarJ1ZdJN1mwM0Ppt0X109
ulE1ZL+/cEiF/o/bDUclQ7xL57lq6H4pC8ZAG6Kt2kMWW11Nbnp1tcKnO/knxw4K
lgqFUDKr9P/JZVX5ULIs+WRFX8Xf9cDn6wKwSlzRkEzRGc73erberPq3fpwMfRrB
mpYBtdSR
=s/IG
-----END PGP PUBLIC KEY BLOCK-----
}
