pragma solidity ^0.4.16;

contract Erc20 {

  // Datastructures holding internal state
  mapping (address => uint256) public balanceOf;
  mapping (address => mapping (address => uint256)) allowed; // provider2spender2balance

  // Events
  event TransferEvent(address indexed _from, address indexed _to, uint256 _value);
  event ApprovalEvent(address indexed _owner, address indexed _spender, uint256 _value);

  string public name;
  string public symbol;
  uint8 public decimals;
  uint256 public totalSupply;

  function Erc20(string _name, string _symbol, uint8 _decimals, uint256 _initialSupply){
    name = _name;
    symbol = _symbol;
    decimals = _decimals;
    totalSupply = _initialSupply;
  }

  function transfer(address _to, uint256 _value) returns (bool success){
    // Check if the sufficient balance is present
    if (balanceOf[msg.sender] < _value) return false;

     // check for overflow
    if (balanceOf[_to] + _value < balanceOf[_to]) return false;

    balanceOf[msg.sender] -= _value;
    balanceOf[_to] += _value;
    TransferEvent(msg.sender, _to, _value);

    return true;
  }

  function transferFrom(address _from, address _to, uint256 _value) returns (bool success){
    // Check if the sufficient balance is present
    if (balanceOf[msg.sender] < _value) return false;

    // Check if msg.sender has been approved to transfer from _from
    if (allowed[_from][msg.sender] < _value) throw;

    // Check for overflow
    if (balanceOf[_to] + _value < balanceOf[_to]) return false;

    // Transfer the tokens and subtract from the allowance
    allowed[_from][msg.sender] -= _value;
    balanceOf[_from] -= _value;
    balanceOf[_to] += _value;
    TransferEvent(msg.sender, _to, _value);

    return true;
  }

  function approve(address _spender, uint256 _value) returns (bool success){
    allowed[msg.sender][_spender] = _value;
    ApprovalEvent(msg.sender, _spender, _value);
    return true;
  }
}
