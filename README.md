# The Sword Financial Contract Language

Sword is a declarative language for expressing fully-collateralized financial contracts. It distinguishes itself from other financial contract languages by being adapted to the blockchain.

**WARNING:** *This is highly experimental software. Use at your own risk.*

## Nomenclature
- **Derivative Contract (DC)** The smart contract containing the conditions of the derivative
- **Party Tokens (PT)** Tokens that represent positions in the derivative contract
- **Settled Asset (SA)** the asset(s) in which the derivative is settled. Typically a stablecoin. Must follow ERC20.

## Example
A bet based on the value of an oracle can be formulated as
```
if obs(bool, <oracle-address>, <oracle-index>) within days(1) then
    transfer(<dai-address>, 1),
else
    transfer(<dai-address>, 2)
```
In this contract, holders of party token 1 will receive 1 DAI unit (1e-18 DAI) per party token of ID 1 that they hold if the oracle returns `true` at any point within 1 day from activation. Otherwise, holders of party token 2 will receive this payout. If you prefer that each position instead pays out 1 DAI, this can be achieved by
```
scale(1e18, 1e18,
    if obs(bool, <oracle-address>, <oracle-index>) within days(1) then
        transfer(<dai-address>, 1),
    else
        transfer(<dai-address>, 2)
)
```
where the 2nd argument to `scale` represents the expression with which the amount of all contained transfers are multiplied, and the 1st argument to `scale` represent a upper bound to this expression.

## Entrypoints
### Entrypoints for the Derivative Logic
- `activate(amount)` transfers settlement asset (SA) from caller to derivative contract (DC) through `transferFrom(caller,DC.address,X*amount)`, where `X` is the size of the collateral of each contract. Mints `amount` of each party token (PT) to caller. Starts any timers. Can only be called once.
- `mint(amount)` Increases the total supply of all party tokens (PT) by the same amount and increases the size of the collateral. Caller is charged in SA and paid in PTs. Requires a successful `transferFrom(caller,DC.address,X*amount)` call to the SA contract with DC as the recipient and `caller` as sender. So the `transferFrom` call transfers SA from caller to DC. This settlement asset amount acts as the collateral of the derivative. mints `amount` of all party tokens and gives these to the caller.
- `burn(amount)` Decreases the total supply of all PTs by the same amount, also decreases the size of the collateral. Caller is charged in PTs and paid in SA. The inverse of `mint`. 
- `pay()` Caller is rewarded in the SA which they are owed if the contract is settled or partly settled **and** caller has a positive PT balance. Burns the PTs that have reached maturity. Calls `transfer(caller, Y)` on SA to pay `Y` to caller where `Y = balance(caller) * Z` is the total amount owed and `Z` is the amount owed to each party according to the contract logic. If caller has a positive balance in multiple party tokens, they get a payout for each one proportional to their balance in this particular token.
### Entrypoints for Transfer of the Party Tokens
The party tokens are handled by the same smart contract as the derivative contract. So they are the same smart contract.
The party token transfer logic follows the ERC1155 standard for which the endpoints are
- `safeTransferFrom(address _from, address _to, uint256 _id, uint256 _value, bytes calldata _data)`
- `safeBatchTransferFrom(address _from, address _to, uint256[] calldata _ids, uint256[] calldata _values, bytes calldata _data)`
- `balanceOf(address _owner, uint256 _id) returns (uint256)`
- `balanceOfBatch(address[] calldata _owners, uint256[] calldata _ids) returns (uint256[])`
- `setApprovalForAll(address _operator, bool _approved)`
- `isApprovedForAll(address _owner, address _operator) returns (bool)`

## Party Token IDs
Each `transfer` as specified in the grammar has an ID as the 2nd argument. This 2nd argument maps directly to the `tokenID` as specified in the ERC1155 standard. Each `ID` in a `transfer` construct must be unique and the IDs must go from 1 to N where N is the number of `transfer` constructs in the contract. All these IDs are created upon activation in an equal amount. More can be created or destroyed later through calls to `mint` and `burn` as specified above.

A special party token with `tokenID = 0` is also created and burned in the same amounts as the other `tokenID`s. This `tokenID = 0` represents positions that get the collateral refunds. If we for example consider the contract below and we imagine that the `datafeed-address` returns a value of `10` for its `datafeed-index`, then the payout evaluates to `10` for `tokenID = 1` and a holder of party token 1 would receive `10 * balance(party-token-1)` when they call `pay`. But the collateral of the contract is `20` as specified in the 1st argument to scale, so the derivative contract would be left with `10` DAI for each unit of its total party token supply. To ensure that all contracts always payout their full collateral when all party token owners have invoked `pay`, `tokenID = 0` is introduced and it is minted and burned along with the other party tokens. And payouts to party token 0 owners also happen when they call `pay`.

```
scale(20, obs(int, <datafeed-address>, <datafeed-index>), transfer(<DAI-address>, 1))
```

With the additional (impplicit) party token ID 0, this contract becomes
```
both(
    scale(20, obs(int, <datafeed-address>, <datafeed-index>), transfer(<DAI-address>, 1)),
    scale(20, 20 - obs(int, <datafeed-address>, <datafeed-index>), transfer(<DAI-address>, 0))
)
```

This way, the existence of `tokenID = 0` ensures that no funds are left on the derivative contract after maturity. At the moment party token 0 are always minted even though they in some cases never will receive a payout. It is the goal of this project to only mint party token 0 when they are needed.

## Description
The derivatives work as fully-collateralized financial contracts where the derivative contract holds the settlement assets until the contract reaches maturity and the funds can be paid out to the involved parties. The parties are identified by the ownership of ERC1155 tokens. The ERC1155 assets are minted by depositing settlement assets (most likely a stablecoin) in the DC and party tokens are then paid to the minter in return. The minter can then sell some or all of their party tokens at an exchange that handles ERC1155 tokens. The minter would call `approve` on the settlement asset smart contract (e.g. DAI) and then call `activate` or `mint` on the DC as described above. The number of tokens in the ERC1155 PT contract is equal to the number of parties in the DSL definition of the derivative contract plus one to receive any collateral that was not paid out during evaluation of the other positions. The total supply of all party tokens will be the same as long as the contract is not partially matured since they are all always minted and burned by the same amount.

## Grammar of Sword

```
contracts:
c ::= scale(nsc,e,c1) | zero | both(c1,c2) |
      transfer(a, n) | translate(t,c1) |
      if e within t1
      then c1 else c2

expressions:
e ::= nsc | obs(ot, f, n) | e1 op e2 | uop e1

time:
t ::= now | u(nsc)

time unit:
u ::= seconds | minutes | hours | days | weeks

operators:
op ::= + | - | x | / | = | if | or | and
uop ::=  not

operator types:
ot ::= int | bool
```
where

* n is a natural number
* nsc is a natural number where scientific notation may be used
* a is a token contract address and represents an asset in the form of an ERC20 contract
* f is the address of a data feed/oracle

## More Examples

All payouts happen when an owner of the party token calls the endpoint `pay()`.

### Basic Constructs
First some simple examples that cover all the language elements. 

 - `transfer(WBTC, 1)` transfers 1 wrapped Bitcoin unit to party `1`
 - `scale(10, 10, transfer(WBTC, 1))` transfers 10 wrapped Bitcoin units to party `1`.
 - ```
   scale(
     10,
     10,
     both(
       transfer(WBTC, 1),
       transfer(WBTC, 2)))
   ```
   transfers 10 wrapped Bitcoin to each of parties `1` and `2`.
 - `translate(hours(1), transfer(WBTC, 1))` transfers 1 wrapped Bitcoin to party `1` in one hour.
 - ```
   if 2 + 2 == 4 within now then
     transfer(WBTC, 1)
   else 
     transfer(WBTC, 2)
   ```
   transfers 1 wrapped Bitcoin to party `1` after contract activation and call to `pay()`.

### European put option, an Insurance Against a Drop in the ETH price

Scenario: A owns 1 ETH and A would like an insurance of a drop in the ETH price
below 600 EUR three months from now. So A would like a contract whose value plus
the value of an ether is at least 600 EUR. This can be achieved by the following
contract:
```
translate(
   days(90),
   scale(
      600,
      max(0, 600 - obs(int, priceFeed, ETHUSD))
      transfer(DAI, A )))
```
If the ETH price at the strike time is 500 EUR, then this contract will pay out
100 EUR, thus guaranteeing A a value of 600 EUR at the maturity of the contract.
DAI is the address of an ERC20-complaint token.

Note that the first parameter to the scale function (defining the token amount
to lock in escrow) is important when the second parameter (the token amount) can
depend on external information. Without the tokens in escrow, we would have no guarantee
that the derivative would have the liquidity needed at excecution time.

## Installation

git, stack, ghc

## Development
Sword, the language, is inspired by the work of Peyton-Jones et al. (2000)
([ref][spj00]), as well as Bahr et al. (2015) ([ref][bahr15icfp]).

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

## Tests
A decent set of integration tests for this compiler is included in [geth_tools](https://github.com/Sword-Smith/geth_tools/).


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
