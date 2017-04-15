pragma solidity ^0.4.9;

contract DataFeed {
  uint256 price;

  function set(bytes32 _key, uint256 _price){
    price = _price;
  }

  function get(bytes32 _key) returns (uint256 _price){
    return price;
  }
}
