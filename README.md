[![Build Status](https://travis-ci.org/Noeda/bet.svg?branch=master)](https://travis-ci.org/Noeda/bet)

This library contains Haskell bindings to the Betfair API.

<https://developer.betfair.com/default/api-s-and-services/sports-api/sports-overview/>

At the moment, the 'Betting API' and 'Heartbeat API' is implemented.
The 'Accounts API' is not.

See "Network.Betfair".

CAUTION: These are experimental bindings. Because of the financially
dangerous nature of betting, I advice you to have a contingency plan if
something in the library breaks down.

In particular, check the Betfair API documentation page for which version
it is at the moment. This library is written against version 2.0 exactly.

This library enforces limits on the number of calls you can do to Betfair
API in a second, to help you avoid data charges. See
"Network.Betfair.Unsafe".

