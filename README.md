# The Sword Financial Contract Language

Sword is a declarative domain-specific language (DSL) for expressing financial
contracts (derivatives). It distinguishes itself from existing financial
contract languages by being adapted to the blockchain by solving fundamental
issues.

Sword, the language, is inspired by the work of Peyton-Jones et al. (2000)
([ref][spj00]), as well as Bahr et al. (2015) ([ref][bahr15icfp]). Sword, the
implementation, is a work in progress, and the work presented here extends both
efforts.

Several attempts to create a domain-specific language to represent financial
contracts have run into the same two issues: **negative balances** and **poor
liquidity** of the financial contracts, in part caused by high transaction
fees.

 - **Negative balances** can arise since the contract languages for the legacy
   financial sector do not cap the potential negative value of a contract. An
   example of this would be a cash-settled contract-for-difference where party
   A in 90 days owes party B the value of a barrel of oil minus USD $40. Since
   the value of oil in 90 days has no upper boundary, the negative value of
   this contract for party A is not downward restricted. A negative balance is
   not enforceable on the blockchain.

 - The **poor liquidity** of derivatives developed in previous attempts arise
   since the derivative contracts are published with hard-coded parties
   identified through blockchain addresses which cannot be changed. So the
   derivative must be owned in full by a single entity until maturity and
   cannot be traded like derivatives in the legacy financial sector can. This
   also creates the problem of matching parties with each other as they must
   agree on terms and contract size in order to meet. *(This problem is
   equivalent to the double coincidence of wants in economic theory.)*

## Competition

The Findel contract language and execution environment faced the problem of
negative balances, high gas cost, and poor liquidity. Findel's execution
environment was not deployed ([ref][findel-youtube]), and has not been actively
developed on since 2017 ([ref][findel-youtube]).

The Lira contract language, a predecessor to Sword sponsored by eToroX Labs,
solved the problem of negative balances but not the problem of poor liquidity.

Simon Peyton-Jones' financial contract language, and others derived from it,
are not designed with the blockchain in mind. They are designed for a world of
well-identied legal entities functioning as parties and government- sanctioned
enforcement to deal with the potential of negative balances and breach of
contract. A successful financial DSL for the blockchain must abandon these
implicit premises.

## Design

Sword solves the problem of negative balances: Asset quantities are bounded,
and financial contracts are always fully-collateralized. And Sword solves the
problem of low liquidity: Positions in a derivative are expressed as ownership
of tokens in token contracts.

Restating the above example of a financial contract based on the oil price, the
corresponding Sword contract would be: Owners of the B token is, for each token
they own, in 90 days owed the value of oil minus USD $40 up to a maximum of e.g.
USD $20.

**TODO:**
 - Deliberately simple / not Turing-complete:
    - Easy to reason about outcome
    - Hard to express unintended behavior
    - (Possible to formally verify semantics and implementation)

## Nomenclature

 - **Derivative contract:** A Sword contract that contains the names of assets
   and parties, and the logic that pays out the **settlement asset** to parties
   at specified points in time. This contract is expressed in the Sword DSL.

 - **Settlement asset:** An asset (token contract, "*stablecoin*") in which
   parties are paid upon the maturation of the **derivative contract**. These
   are the assets that parties pay for **party tokens** to participate in the
   contract.

   Settlement assets exist prior to creation of a derivative contract.  On
   Ethereum these will be existing ERC20 or ERC1155 tokens. On Tezos these will
   be existing FA1.2 and FA2 tokens.

 - **Party token:** An asset (token contract) that represents positions in a
   **derivative contract**. Party token contracts have a `mint()` and `burn()`
   entrypoint for increasing and decreasing the "shares" of each party.

   A derivative contract will usually have multiple parties, and therefor
   multiple party tokens, associated with it. For that reason, multi-token
   contract standards like ERC1155 on Ethereum and FA2 on Tezos are preferred.

   Party tokens are created and minted for the occasion of representing parties
   in each derivative contract, and they are burned for converting ownership to
   the underlying **settlement asset**.

   Party tokens are either published as part of a derivative contract, by a
   derivative contract or next to a derivative contract. (We currently publish
   party tokens next to, but we aim for a deeper integration.)

## Examples

First some simple examples that cover all the language elements:

 - `transfer(WBTC, A)` transfers 1 wrapped Bitcoin to party `A`.
 - `scale(10, transfer(WBTC, A))` transfers 10 wrapped Bitcoin to party `A`.
 - ```
   scale(
     10,
     both(
       transfer(WBTC, A),
       transfer(WBTC, B)))
   ```
   transfers 10 wrapped Bitcoin to each of parties `A` and `B`.
 - `delay(1 hour, transfer(WBTC, A))` transfers 1 wrapped Bitcoin to party `A` in one hour.
 - ```
   if 2 + 2 == 4 within now
   then transfer(WBTC, A)
   else transfer(WBTC, B)
   ```
   transfers 1 wrapped Bitcoin to party `A` after contract activation.

**TODO:**

 - Examples that involve observables once they have been modified.
 - Examples that provide valid/invalid bounds over observables.
 - Examples that cover more of `Expr`.
 - Examples that describe realistic derivatives.
 - Consider, once some maturity is reached, to release on [readthedocs.io][https://readthedocs.org/].

## Execution model

## Grammar

**Proposals (from Dagger to Sword):**:
 - Remove second `Party` of `transfer`.
 - Remove maximum constant of `scale`.
 - Remove `zero` contract.
 - Rename `translate` into `delay`.
 - Turn `Asset` and `Party` from addresses into placeholders.
 - Turn `Observable` into something nicer, to be determined.

```
Contract ::=
  | 'transfer' '(' Asset    ',' Party    ')'
  | 'scale'    '(' Expr     ',' Contract ')'
  | 'both'     '(' Contract ',' Contract ')'
  | 'delay'    '(' Time     ',' Contract ')'
  | 'if' Expr 'within' Time 'then' Contract 'else' Contract

Asset ::= Ident

Party ::= Ident

Oracle ::= ...

Time ::=
  | 'now'
  | Nat TimeUnit (',' Nat TimeUnit)*

TimeUnit ::=
  | 'second' | 'seconds'
  | 'minute' | 'minutes'
  | 'hour'   | 'hours'
  | 'day'    | 'days'
  | 'week'   | 'weeks'

Expr ::=
  | 'true' | 'false'
  | Const
  | Var

  | Expr '+' Expr
  | Expr '-' Expr
  | Expr '*' Expr
  | Expr '/' Expr
  | 'min' '(' Expr ',' Expr ')'
  | 'max' '(' Expr ',' Expr ')'
  | 'bound' '(' Expr ',' Expr ',' Expr ')'
  
  | Expr '&&' Expr
  | Expr '||' Expr
  | '!' Expr

  | Expr '==' Expr
  | Expr '<'  Expr
  | Expr '>'  Expr
  | Expr '<=' Expr
  | Expr '>=' Expr

  | Var
  | Const
  | Oracle

Nat ::=
  | ['0' - '9']+

Ident ::=
  | ['a'-'z', 'A'-'Z'] ['a'-'z', 'A'-'Z', '0'-'9', '_']*
```

## Deprecated sections not yet merged

### Overview (deprecated)

**This section is outdated and needs to be completely merged into the simple examples above.**

To transfer a unit token from address `p1` to address `p2`, the `transfer(a, p1,
p2)` function is used, where `a` is the token constract address (e.g. eToroUSD).

Transfering an arbitry token amount can be done by using `scale(n, e, c)`, where
`n` is the maximum amount of tokens the contract will lock in escrow, `e` is an
expression resolving in the actual amount of tokens to transfer and `c` is the
ERC-20 token contract. We need to lock tokens in escrow, since the actual token
amount might be evaluated at runtime. The amount of tokens to transfer can never
exceed the maximum amount.

By combining the transfer function
and the scale function above, we can transfer an arbitrary amount of tokens
using `scale(a, e, transfer(a, p1, p2))`.

If a contract should be excecuted at a specific time in the future,
`translate(t,c1)` can be used, where `t` is the time offset.

Notice how the contracts can be composed of simpler contracts by the
constructors `transfer`, `scale` and `translate` (and a few more defined below).

### Examples (deprecated)

**This section is outdated and needs to be completely merged into the simple examples above.**

### Example 1: Binary Option, a Simple Bet

The simplest useful example is a binary option. Two parties are involved in the
contract, and the contract holds a specific amount of tokens which either of the
party will receive in full amount at the maturity of the contract. This contract
is equivalent to a simple bet and the outcome will be determined by some kind of
event whose result is read from an oracle.

This contract will transfer 100 tokens to A if the oracle/data feed shows the
value true at any time within two minutes. If that does not happen, 100 tokens
are transferred to B. Both A and B need to put in 100 tokens for the contract to
be considered activated.

```
scale(
   100,
   100,
   if obs(bool, addressDataFeedBool, 0) within minutes(2) then
      transfer( eToroUSD, B, A)
   else
      transfer( eToroUSD, A, B)
   )
```

The three arguments to `transfer` are addresses: the address of the ERC20 smart
contract of the tokens being transferred, the party that makes this payment, and
the last argument is the party to receive this payment.

The amounts are deposited through the function calls `approve` on the ERC20, and
`activate` on the contract generated by `Swordc` (see section "ABI of the produced
contracts").

The observable `obs` depends on some external information, for example the
outcome of a sports event or something similar.

### Example 2: European put option, an Insurance Against a Drop in the ETH price

Scenario: A owns 1 ETH and A would like an insurance of a drop in the ETH price
below 100 USD three months from now. So A would like a contract whose value plus
the value of an ether is at least 100 USD. This can be achieved by the following
contract:
```
translate(
   days(90),
   scale(
      100,
      max(0, 100 - obs(int, priceFeed, ETHUSD))
      transfer(eToroUSD, B, A )))
```
If the ETH price at the strike time is 10 USD, then this contract will pay out
90 USD, thus guaranteeing A a value of 100 USD at the maturity of the contract.
eToroUSD is the address of an ERC20-complaint token.

Note that the first parameter to the scale function (defining the token amount
to lock in escrow) is important when the second parameter (the token amount) can
depend on external information. Without the tokens in escrow, we would have no guarantee
that the counterparty would have the liquidity needed at excecution time.

### Example 3: Future
The code below describes a future contract, namely a legal agreement to buy or
sell something at a predetermined price at a specified time in the future,
between two parties not knowning each other. The function `both<c1,c2>` is
excecuting both contracts `c1` and `c2`, in this case transfering the specified
currencies and the specified amount to each of the parties.

```
translate(
   seconds(<Time>),
   both(
      scale(
         <Upper limit>,
         <Amount>,
         transfer(
            <Currency>,
            <me>,
            <Counterparty>
         )
      ),
      scale(
         <Upper limit>,
         <Amount to receive>,
         transfer(
            <Currency to receive>,
            <Counterparty>,
            <me>
         )
      )
   )
)
```

### ABI of the Derivative Contract

**TODO:** Also format ABI as code block.

- `constructor()` Runs when the derivative contract is deployed on the blockchain.
  Creates the two PT ERC20 contracts with an initial supply of 0 and some descriptive
  names and symbols. The PTs must have `mint(amount,recipient)` and `burn(amount,address)` functions which are
  only callable by DC, other than `mint` and `burn` they are minimal ERC20 contracts.
- `activate(amount)` transfers SA from caller to DC through `transferFrom(msg.sender,DC,X*amount)`, where `X` is the size
  of each contract. calls `mint(amount,msg.sender)` on both deployed PTs. Starts any timers.
- `mint(amount)` Increases the total supply of both PTs by the same amount. Caller is charged in SA and paid in PTs.
  Requires a successful `transferFrom(msg.sender,DC,X*amount)` call to the SA ERC20 contract with DC as the recipient and `msg.sender` as sender.
  So the `transferFrom` call transfers SA from caller to DC.
  calls `mint(amount,msg.sender)` on both PTs.
- `burn(amount)` Decreases the total supply of both PTs by the same amount. Caller is charged in PTs and paid in SA.
  calls `burn(amount,msg.sender)` on both PTs.
- `pay()` Caller is rewarded in the SA which they are owed if the contract is settled or partly settled.
  Calls `burn(balanceOf(msg.sender),msg.sender)` on both PTs. Calls `transfer(msg.sender, Y)` on SA ERC20 to pay `Y` to
  caller where `Y` is the amount owed.

The Sword (Sword) contract compiles into the Ethereum's ABI and has two
methods: `activate()` and `execute()`.
 - `activate()` collects the margin from the parties' accounts and starts the
    timer. Will only succeed if the parties have allowed the Sword contract to
    withdraw from their balance through the ERC20 contract call `approve`.
 - `execute()` checks whether any subparts of the contracts are ready to be paid
    out to the parties or any margins can be paid back.

`activate()` and `execute()` may change state.

## Installation

git, stack, ghc

## References

 - [Composing contracts: an adventure in financial engineering][spj00]<br>
   by Simon Peyton Jones, Jean-Marc Eber, Julian Seward (2000)
 - [Certified Symbolic Management of Financial Multi-party Contracts][bahr15icfp]<br>
   by Patrick Bahr, Jost Berthold, Martin Elsman (2015)
 - [Automated Execution of Financial Contracts on Blockchains][benjamin17]<br>
   by Benjamin Egelund-Müller, Martin Elsman, Fritz Henglein, Omri Ross (2017)
 - [Findel: Secure Derivative Contracts for Ethereum][findel17]<br>
   by Alex Biryukov, Dmitry Khovratovich, Sergei Tikhomirov (2017)
 - [Secure Execution of Financial Contracts on Ethereum][tfp17]<br>
   by Thorkil Værge, Mads Gram, Omri Ross (2017)

[spj00]: https://www.microsoft.com/en-us/research/publication/composing-contracts-an-adventure-in-financial-engineering/
[bahr15icfp]: https://bahr.io/pubs/entries/bahr15icfp.html
[benjamin17]: https://github.com/sshine/lira/blob/master/docs/ross.pdf
[findel17]: https://ifca.ai/fc17/wtsc/Findel%20-%20Secure%20Derivative%20Contracts%20for%20Ethereum.pdf
[tfp17]: https://www.cs.kent.ac.uk/events/tfp17/cfpapers.html

[findel-github]: https://github.com/cryptolu/findel
[findel-youtube]: https://www.youtube.com/watch?v=D4sa9U2HXMQ
[lira-github]: https://github.com/etoroxlabs/lira
