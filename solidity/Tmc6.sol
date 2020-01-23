/**
 * MIT License
 *
 * Copyright (c) 2019 Thorkil Værge and Mads Gram
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
pragma solidity ^0.4.9;
contract Tmc6 {

    struct Approval {
        address senderAddress;
        uint256 senderAllowance;
    }

    /* This creates an array with all balances */
    mapping (address => uint256) public balanceOf;
    mapping (address => uint256) public totalLockedAmount; // Total amount locked on this account by all contracts
    mapping (address => Approval) public spenderToApproval; // Allow spender to transfer from sender. Different from Tmc3

    string public name;
    string public symbol;
    uint8 public decimals;
    uint256 public tokenSupply; // totalSupply=supply=initialSupply

    event TransferEvent(address indexed from, address indexed to, uint256 value);
    event ApprovalEvent(address indexed _owner, address indexed _spender, uint256 _value);

    /* Initializes contract with initial supply tokens to the creator of the contract */
    function Tmc6(uint256 initialSupply, string tokenName, uint8 decimalUnits, string tokenSymbol) {
        balanceOf[msg.sender] = initialSupply; // Give the creator all initial tokens
        name = tokenName;
        symbol = tokenSymbol;
        decimals = decimalUnits;
        tokenSupply = initialSupply;
    }

    /* Send coins */
    function transfer(address _to, uint256 _value) returns (bool success) {
        if (balanceOf[msg.sender] - totalLockedAmount[msg.sender] < _value) return false; // Check if the sender has enough
        if (balanceOf[_to] + _value < balanceOf[_to]) return false; // Check for overflows
        balanceOf[msg.sender] -= _value;                            // Subtract from the sender
        balanceOf[_to] += _value;                                   // Add the same to the recipient
        TransferEvent(msg.sender, _to, _value);                          // Notify listeners about this event
        return true;
    }

    /* send coins on behalf of other accounts */
    function transferFrom(address _from, address _to, uint256 _value) returns (bool success) {
        bool legalRequest = false;

        if (spenderToApproval[msg.sender].senderAddress == _from){
          legalRequest = spenderToApproval[msg.sender].senderAllowance >= _value;
        }
        if (!legalRequest) return false;
        if (balanceOf[_from] < _value) return false; // DEVRM: Check if the sender has enough. NOT NECESSARY!
        if (balanceOf[_to] + _value < balanceOf[_to]) return false; // Check for overflows

        spenderToApproval[msg.sender].senderAllowance -= _value; // Reduce approved amount from this spender
        totalLockedAmount[_from] -= _value;
        balanceOf[_from] -= _value;
        balanceOf[_to] += _value;
        TransferEvent(msg.sender, _to, _value); // Notify listeners about this event
        return true;
    }

    /* The method does not guarantee that the _value amount is present in the msg.sender balance */
    // DEVFIX: But it should make that check.
    // We need the totalLockedAmount to make that check!
    function approve(address _spender, uint256 _value) returns (bool success) {
        /*
        * If the msg.sender has already allowed to send a value by _spender,
        * this becomes imutable to protect against the msg.sender later can lessen the amount.
        * For Tmc4, a specific DC contract can only record the approval of access to ONE account.
        */
        if (spenderToApproval[_spender].senderAddress != address(0x0)) return false;
        if (spenderToApproval[_spender].senderAddress == msg.sender) return false;
        if (totalLockedAmount[msg.sender] + _value > balanceOf[msg.sender]) return false;
        spenderToApproval[_spender] = Approval(msg.sender, _value);
        totalLockedAmount[msg.sender] += _value;
        ApprovalEvent(msg.sender, _spender, _value);
        return true;
    }

    /* Cancels the approval. Only the approved account (e.g. a deriv. contract) can do this*/
    /* Should releaseApproval also cancel if isContract returns false? */
    function releaseApproval(address _cancelAddress) returns (bool success) {
        if (spenderToApproval[msg.sender].senderAddress == _cancelAddress){
            totalLockedAmount[_cancelAddress] -= spenderToApproval[msg.sender].senderAllowance;
            delete spenderToApproval[msg.sender];
            return true;
        }
        return false;
    }

    function allowance(address _owner, address _spender) constant returns (uint256 remaining) {
        if (spenderToApproval[_spender].senderAddress == _owner){
          return spenderToApproval[_spender].senderAllowance;
        }
        return 0;
    }

    function isContract(address _addr) private returns (bool isContract){
      uint32 size;
      assembly {
        size := extcodesize(_addr)
      }
      if (size > 0) return true;
      return false;
    }
}
