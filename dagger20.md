# Dagger 2.0

## Nomenclature
Derivative Contract (DC): The smart contract containing the conditions of the derivative
Settled Asset (SA): the asset in which the derivative is settled
Party Tokens (PT): Token contracts (ERC20) representing the positions in the derivative contract

## ABI of the Derivative Contract
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
