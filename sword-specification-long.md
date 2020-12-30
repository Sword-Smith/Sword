# Sword Financial Contract Specification

The Sword DSL is a domain-specific language to express financial contracts. It is inspired by existing languages for defining financial derivatives but is tailor-made for the blockchain by being restricted to contracts of non-negative value and by allowing easy trade of the positions in the derivatives.

## Nomenclature
- **Derivative Contract (DC)** The smart contract containing the conditions of the derivative
- **Party Tokens (PT)** Multi asset token contract (ERC1155) representing the positions in the derivative contract
- **Settled Asset (SA)** the asset in which the derivative is settled. Typically a stablecoin following the ERC20 standard.

## Description
The derivatives work as fully-collateralized financial contracts where the DC holds the assets until the contract reaches maturity and the funds can be paid out to the involved parties. The parties are identified by the ownership of ERC1155 assets. The ERC1155 assets are minted by depositing settlement assets (most likely a stablecoin) in the DC and party tokens are then paid to the minter in return. The minter can then sell some or all of their party tokens. The minter would call approve on the settlement asset smart contract (e.g. DAI) and then call `activate` or `mint` on the DC as described below. The number of assets in the ERC1155 PT contract is equal to the number of parties in the DSL definition of the derivative contract plus one to receive any collateral that was not paid out during evaluation of the other positions. The total supply of all assets in the PT ERC1155 contract will be the same as long as the contract is not partially matured yet since they are all always minted and burned by the same amount.

## Entrypoints of the Derivative Contract
  All entrypoints are callable by anyone
- `activate(amount)` transfers SA from caller to DC through `transferFrom(caller,DC.address,X*amount)`, where `X` is the size of the collateral of each contract. Mints `amount` of each PT to caller. Starts any timers. Can only be called once.
- `mint(amount)` Increases the total supply of all PTs by the same amount. Caller is charged in SA and paid in PTs. Requires a successful `transferFrom(caller,DC.address,X*amount)` call to the SA contract with DC as the recipient and `caller` as sender. So the `transferFrom` call transfers SA from caller to DC. mints `amount` of all PTs to caller.
- `burn(amount)` Decreases the total supply of all PTs by the same amount. Caller is charged in PTs and paid in SA. The inverse of `mint`.
- `pay()` Caller is rewarded in the SA which they are owed if the contract is settled or partly settled **and** caller has a positive PT balance. Burns the PTs that have reached maturity. Calls `transfer(caller, Y)` on SA to pay `Y` to caller where `Y = balance(caller) * Z` is the total amount owed and `Z` is the amount owed to each party according to the contract logic. If caller has a positive balance in multiple party tokens, they get a payout for each one proportional to their balance in this particular token.

## Party Token IDs
Each `transfer` as specified in the grammar has an ID as the 2nd argument. This 2nd argument maps directly to the `tokenID` as specified in the ERC1155 standard. Each `ID` in a `transfer` construct must be unique and the IDs must go from 1 to N where N is the number of `transfer` constructs in the specification. All these IDs are created upon activation in an equal amount. More can be created or destroyed later through calls to `mint` and `burn` as specified above.

A special party token with `tokenID = 0` is also created and burned in the same amounts as the other `tokenID`s. This `tokenID = 0` represents positions that get the collateral refunds. If we for example consider the contract below and we imagine that the `datafeed-address` returns a value of `10` for its `datafeed-index`, then the payout evaluates to `10` for `tokenID = 1` and a holder of party token 1 would receive `10 * balance(party-token-1)` when they call `pay`. But the collateral of the contract is `20` as specified in the 1st argument to scale, so the derivative contract would be left with `10` DAI for each unit of its total party token supply. To ensure that all contracts always payout their full collateral when all party token owners have invoked `pay`, `tokenID = 0` is introduced and it is minted and burned along with the other party tokens. And payouts to party token 0 owners also happen when they call `pay`.

```
scale(20, obs(int, <datafeed-address>, <datafeed-index>), transfer(<DAI-address>, 1))
```

## Grammar of Sword

```
contracts:
c ::= scale(n,e,c1) | zero | both(c1,c2) |
      transfer(a, n) | translate(t,c1) |
      if e within t1
      then c1 else c2

expressions:
e ::= b | obs(ot, f, n) | e1 op e2 | uop e1

time:
t ::= now | u(n)

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
* a is a token contract address and represents an asset in the form of an ERC20 contract
* f is the address of a data feed/oracle
* b is a whole number
* obs is an observable depending on external information
