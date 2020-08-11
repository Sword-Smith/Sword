# The Dagger Programming Language

This document describes the Dagger language and its compiler and protocol. The use of
the protocol illustrated through examples of how derivative contracts can be
created.

This protocol centers around a Domain Specific Language (DSL) called
Dagger which is used to specify the derivative contracts and has been adapted
from the contract language of [Bahr et all
(2015)](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.720.1324&rep=rep1&type=pdf)
and [Egelund-Müller et all
(2017)](https://www.researchgate.net/publication/321327355_Automated_Execution_of_Financial_Contracts_on_Blockchains).

Using a non-Turing complete DSL for this purpose has several advantages. In
particular, the language is restricted such that using it to specify unintended
behavior is impossible. Further, as the semantics of the language are formally
verified, contracts specified in the language is guaranteed to behave as
intended and to only have a single interpretation.

To show a possible integration of the language, we provide a graphical frontend
for creating, deploying and monitoring future contracts. Behind the scenes, the
frontend will generate corresponding Dagger code which is subsequently
compiled to EVM and deployed to a local Ethereum testnet. By generating
Dagger, we ensure that the static guarantees of the language applies
regardless of frontend functionality. Additionally, the frontend makes it
possible to view both the generated Dagger code and the compiled EVM code.

The frontend is for demonstration purposes only and relies on being able to act
as a signatory on behalf of the accounts representing both parties agreeing to
the contract. In an actual deployment of the system, a separate signing step by
each individual signatory is required before they can enter into agreement.
Further, the demonstration frontend will always create a contractual agreement
between two predefined parties. Finally, the demostration implementation of the
on-chain contracts will take the amount of tokens held by the contract into
escrow. In an actual implementation of the system, an alternative method would
be used which avoids large amounts of tokens in this manner.

## The Dagger (Dagger)

### Overview
Before we give the full definition of the langauge, let's go through a few of
the functions.

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

### Definition
Below is the full definition of the language written as a context free grammar
definition of the language in which the derivative contracts are written:

```
contracts:
c ::= scale(n,e,c1) | zero | both(c1,c2) |
      transfer(a, p1, p2) | translate(t,c1) |
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
* p is a party to the contract identified by an Ethereum address
* a is a token contract address
* f is the address of a feed
* b is a whole number
* obs is an observable depending on external information

Addresses are written as `0x[0-9a-f]{40}`

## Examples

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
`activate` on the contract generated by `Daggerc` (see section "ABI of the produced
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

## Application Binary Interface (ABI) of the produced contracts

The Dagger (Dagger) contract compiles into the Ethereum's ABI and has two
methods: `activate()` and `execute()`.
 * `activate()` collects the margin from the parties' accounts and starts the
   timer. Will only succeed if the parties have allowed the Dagger contract to
   withdraw from their balance through the ERC20 contract call `approve`.
 * `execute()` checks whether any subparts of the contracts are ready to be paid
   out to the parties or any margins can be paid back.

 `activate()` and `execute()` may change state.

## Online DEMO
A working demo can be found at
[https://sandbox.firmo.network/](https://sandbox.firmo.network/). Below is a
screenshot of the frontend<br> ![Frontend
screenshot](https://raw.githubusercontent.com/Firmo-Network/etlc/master/docs/frontend.png?token=AABFWOQJMDDFRAKTDADEXHC432VAC)

### Example 1: Future

In this example, we will show how two parties not knowning each other can trade
a future on the blockchain.

TODO (steps, screenshots, ...)

## Running the system locally
We now describe how to set up the various components needed to get the
demonstration frontend up and running.

At a high level, the system is comprised of three main components:

1. **Frontend** implements the web interface which is used for visualizing and
   manipulating the contracts.<br>
2. **Backend server** provides the data storage and compilation services for the
   frontend.<br>
3. **Compiler** compiles the Dagger source code to EVM binary code providing
   the contract features described above.

### Dependencies
Before proceeding, make sure that you have the following installed

 * `git`
 * `yarn`
 * `node.js` version [10.xx](https://nodejs.org) (12.x is not supported)
 * A browser with the [Metamask](http://metamask.io) extension installed

### Running and installing
In order to compile and run the demo client you need to go through the following
steps.

#### Starting the server
 1. Clone the etorolang-demo-server repository:  
    `git clone https://github.com/Firmo-Network/etorolang-demo-server`
 1. Change diretory:  
    `cd etorolang-demo-server`
 1. Install dependencies:  
    `yarn install`
 1. Start the server  
    `yarn start`

#### Starting the client
In a separate terminal window do:

 1. Clone the etorolang-demo-server repository:  
    `git clone https://github.com/Firmo-Network/etorolang-demo-client`
 1. Change diretory:  
    `cd etorolang-demo-client`
 1. Install dependencies:  
    `yarn install`
 1. Start the client (will launch a browser)  
    `yarn start`

### Setting up Metamask
Metamask is a convenient way to handle and create wallets and managing your
private keys. Change the endpoint address private keys as follows:

* Connect to a custom RPC endpoint with the address (*Networks -> Custom
  RPC*):<br> `http://127.0.0.1:9545`
* Import the accounts with the private keys below into Metamask (*Accounts ->
  Import Account*):<br>
  `ae6ae8e5ccbfb04590405997ee2d52d2b330726137b875053c36d94e974d162f`<br>
  `0dbbe8e4ae425a6d2687f1a7e3ba17bc98c673636790f1b8ad91193c05875ef1`   

You are now able to create contracts using the interface.

(TODO: ethereum.enable() needs to be called manually, but should be implemented
on the frontend)

### Notes

* Since this is a demo only, all contracts are entered between the two parties
  represented by the accounts belonging to the private keys, as the server
  manages the keys for easy experience (for a more realistic demo, use the
  [online sandbox demo](https://sandbox.firmo.network/)).
* After a contract has expired, you will be able to see the change of the
  balances of the two accounts according to the outcome of the contract.
