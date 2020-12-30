# Sword Financial Contract Specification

## Nomenclature
Derivative Contract (DC):
The smart contract containing the conditions of the derivative contract

Party Tokens (PT):
Tokens representing the positions in the derivative contract. Token contract
is the derivative contract which can handle multiple tokens in the same
contract. Tokens are implemented as the ERC1155 multitoken standard.

Settled Asset (SA):
The asset in which the derivative is settled, what asset a payout occurs in

## ABI of the Derivative Contract
- `activate(amount)` transfers SA from caller to DC through `transferFrom(caller,DC,X*amount)`, where `X` is the collateral
  of each contract. Starts any timers. mints `amount` party tokens of each kind to the caller. So the caller ends up with
  `amount` of each PT.
- `mint(amount)` Increases the total supply of each PT by the same amount. Caller is charged in SA and paid in PTs.
  Requires a successful `transferFrom(caller,DC,X*amount)` call to the SA ERC20 contract(s) with DC as the recipient
  and caller as sender. So the `transferFrom` call transfers SA from caller to DC and mints PTs to caller.
- `burn(amount)` Decreases the total supply of each PT by `amount`. Caller is charged `amount` in PTs and
  paid `X * amount` in SA where `X` is the size of the collateral for each position. `burn` is the inverse of `mint`
  meaning that a `mint` followed by a `burn` of the same amount has no effect on the callers SA or PT balances.
- `pay()` Caller is rewarded in the SA which they are owed if the contract is settled or partly settled.
  Burns all party tokens that represent positions that have reached maturity. Calls `transfer(caller, Y)` on SA ERC20(s)
  to pay `Y` to caller where `Y` is the amount owed based on the contract specification.
  `Y = evaluatedPositionValue_i * balance_i` for each party token `i`.

Since the derivative contract is also an ERC1155 contract, it also contains the endpoints specified in the
[ERC1155 specification](https://eips.ethereum.org/EIPS/eip-1155): `safeTransferFrom`, `safeBatchTransferFrom`, `balanceOf`,
`balanceOfBatch`, `setApprovalForAll`, `isApprovedForAll`.

## Party token IDs
When specifying a contract, the 2nd argument to `transfer` is the party token ID receiving this payout. These IDs must be
specified consecutively from `1` to `N` where `N` is the number of `transfer` construct (specified payouts) in the
contract definition.

A special token with ID 0 is created for position that pay out all collateral not specified elsewhere in the contract. This
token ID 0 guarantees that the derivative contract cannot end up with a positive settlement asset balance when all token
holders have requested payouts through calls to `pay()`.
