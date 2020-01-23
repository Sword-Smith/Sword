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
contract Tmc3 {
    struct Approval {
        address senderAddress;
        uint256 senderAllowance;
    }

    /* This creates an array with all balances */
    mapping (address => uint256) public balanceOf;
    mapping (address => Approval[]) public spenderToApproval; // Allow spender to transfer from sender
    mapping (address => uint256) public totalLockedAmount;  // Total amount locked on this account by all contracts

    string public name;
    string public symbol;
    uint8 public decimals;
    uint256 public tokenSupply; // totalSupply=supply=initialSupply

    event TransferEvent(address indexed from, address indexed to, uint256 value);
    event ApprovalEvent(address indexed _owner, address indexed _spender, uint256 _value);

    /* Initializes contract with initial supply tokens to the creator of the contract */
    function Tmc3(uint256 initialSupply, string tokenName, uint8 decimalUnits, string tokenSymbol) {
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

    function transferFrom(address _from, address _to, uint256 _value) returns (bool success) {
        bool legalRequest = false;
        uint i;
        for (i = 0; i < spenderToApproval[msg.sender].length; i++) {
            if (spenderToApproval[msg.sender][i].senderAddress == _from){
              legalRequest = spenderToApproval[msg.sender][i].senderAllowance >= _value;
              break;
            }
        }
        if (!legalRequest) return false;

        if (balanceOf[_from] < _value) return false; // DEVRM: Check if the sender has enough. NOT NECESSARY!
        if (balanceOf[_to] + _value < balanceOf[_to]) return false; // Check for overflows
        if (totalLockedAmount[_from] - _value < 0) throw; // DEVRM: NOT NECESSARY! SANITY CHECK.
        totalLockedAmount[_from] -= _value; // Reduce totalLockedAmount (locked amount on _from account)
        spenderToApproval[msg.sender][i].senderAllowance -= _value; // Reduce approved amount from this spender

        balanceOf[_from] -= _value;
        balanceOf[_to] += _value;
        TransferEvent(msg.sender, _to, _value); // Notify listeners about this event
        return true;
    }

    /* The method does not guarantee that the _value amount is present in the msg.sender balance */
    function approve(address _spender, uint256 _value) returns (bool success) {
        /*
        * If the msg.sender has already allowed to send a value by _spender,
        * this becomes imutable to protect against the msg.sender later can lessen the amount.
        */
        // DEVQ: Will spenderToApproval[_spender].length work in spenderToApproval is empty?
        for (uint i = 0; i < spenderToApproval[_spender].length; i++) {
            if (spenderToApproval[_spender][i].senderAddress == msg.sender) return false;
        }

        Approval[] approvals = spenderToApproval[_spender];
        // Check that total locked amount after this action is not greater than balance
        if (_value + totalLockedAmount[msg.sender] > balanceOf[msg.sender]) return false;
        approvals.push(
            Approval({
                senderAddress: msg.sender,
                senderAllowance : _value
            })
        );
        totalLockedAmount[msg.sender] += _value; // Update the total locked amount
        ApprovalEvent(msg.sender, _spender, _value);
        return true;
    }

    /* Cancels the approval. Only the approved account (e.g. a deriv. contracts) can do this*/
    function releaseApproval(address _cancelAddress) returns (bool success) {
        bool present = false;
        for (uint i = 0; i < spenderToApproval[msg.sender].length; i++) {
          if (spenderToApproval[msg.sender][i].senderAddress == _cancelAddress){
              totalLockedAmount[_cancelAddress] -= spenderToApproval[msg.sender][i].senderAllowance;
              delete spenderToApproval[msg.sender][i];
              return true;
          }
        }
        return false;
    }

    function allowance(address _owner, address _spender) constant returns (uint256 remaining) {
        Approval[] approvals = spenderToApproval[_spender];
        for (uint i = 0; i < spenderToApproval[_spender].length; i++ ){
            if ( spenderToApproval[_spender][i].senderAddress == _owner ) return spenderToApproval[_spender][i].senderAllowance;
        }
        return 0;
    }
}
