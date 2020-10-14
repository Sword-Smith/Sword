# Sword
By Thorkil Værge
**CONFIDENTIAL -- DO NOT DISTRIBUTE**

The Sword DSL is a domain-specific language to express financial contracts. It is inspired by existing languages for defining financial derivatives but is tailor-made for the blockchain by being restricted to contracts of non-negative value and by allowing effortless trade of the positions in the derivatives.

## Nomenclature
- **Derivative Contract (DC)** The smart contract containing the conditions of the derivative
- **Settled Asset (SA)** the asset in which the derivative is settled. Typically a stablecoin.
- **Party Tokens (PT)** Multi asset token contract (FA2) representing the positions in the derivative contract

## Description
The derivatives work as fully-collateralized financial contracts where the DC holds the assets until the contract reaches maturity and the funds can be paid out to the involved parties. The parties are identified by the ownership of FA2 assets. The FA2 assets are minted by depositing settlement assets (most likely a stablecoin) in the DC and party tokens are then paid to the minter in return. The minter can then sell some or all of their party tokens. The minter would call approve on the settlement asset smart contract (e.g. USDtz) and then call `activate` or `mint` on the DC as described below. The number of assets in the FA2 PT contract is equal to the number of parties in the DSL definition of the derivative contract. Often, only two parties will be involved in the derivative contract. The total supply of all assets in the PT FA2 contract will be the same since they are all always minted and burned by the same amount.

## Entrypoints of the Derivative Contract
  All entrypoints are callable by anyone but the `mint` and `burn` in the PT are only callable by DC.
- `activate(amount)` transfers SA from caller to DC through `transfer(Tezos.sender,DC.address,X*amount)`, where `X` is the size of each contract. calls `mint(Tezos.sender,amount)` on all deployed PTs. Starts any timers. Can only be called once.
- `mint(amount)` Increases the total supply of all PTs by the same amount. Caller is charged in SA and paid in PTs. Requires a successful `transfer(Tezos.sender,DC.address,X*amount)` call to the SA FA2/FA1.2 contract with DC as the recipient and `Tezos.sender` as sender. So the `transfer` call transfers SA from caller to DC. Calls `mint(Tezos.sender,amount)` for all PTs.
- `burn(amount)` Decreases the total supply of all PTs by the same amount. Caller is charged in PTs and paid in SA. Calls `burn(Tezos.sender,amount)` on all PTs.
- `pay()` Caller is rewarded in the SA which they are owed if the contract is settled or partly settled **and** caller has a positive PT balance. Calls `burn(Tezos.sender,get_balance(Tezos.sender))` on all PTs. Calls `transfer(DC.address, Tezos.sender, Y)` on SA to pay `Y` to caller where `Y = get_balance(Tezos.sender) * Z` is the total amount owed and `Z` is the amount owed to each party according to the contract logic.

## Grammar of Sword

```
contracts:
c ::= scale(n,e,c1) | zero | both(c1,c2) |
      transfer(a, p) | translate(t,c1) |
      if e within t1
      then c1 else c2

expressions:
e ::= b | obs(ot, f, t) | e1 op e2 | uop e1

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
* p is a party to the contract, identified as `party_[A-Z]`, where the parties are declared sequentially (`party_A` must be declared before `party_B` is etc.)
* a is a token contract address and represents an asset
* f is the address of a data feed/oracle
* b is a whole number
* obs is an observable depending on external information
