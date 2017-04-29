2pragma solidity ^0.4.9;

contract DataFeedBool1 {
  bool b;
  string public symbol;

  function DataFeedBool1(string _dfSymbol){
    symbol = _dfSymbol;
  }

  function set(bytes32 _key, bool _b){
    b = _b;
  }

  function get(bytes32 _key) constant returns (bool _b){
    return b;
  }
}
