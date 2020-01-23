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
import "./Tmc4.sol";
import "./DataFeed0.sol";

contract Eur_Option {

    uint deployTime;
    Tmc4 tmc;
    DataFeed0 dataFeed;
    bool contractExecuted; // default value is false
    address to_address;
    address from_address;

    function Eur_Option(address tmc_address, address datafeed_address,
                        address to_address_, address from_address_)
    {
        deployTime = block.timestamp;
        tmc = Tmc4(tmc_address); // Write in the correct address for the TMC
        dataFeed = DataFeed0(datafeed_address); // Write in the correct address for the DF
        to_address = to_address_;
        from_address = from_address_;
        contractExecuted = false;
    }

    function execute()
    {
        if (block.timestamp - deployTime >= 180) // Three minutes
        {
            uint obs_value = dataFeed.get("0x00");
            uint obs_price = (obs_value - 10 >= 0) ? obs_value - 10 : 0; // Strike price is 10
            uint final_price = (obs_price <= 2000) ? obs_price : 2000; // 2000 is maxAmount from scale

            tmc.transferFrom(from_address, to_address, final_price);
            contractExecuted = true;
            remove_contract();
        }
    }

    function remove_contract()
    {
        if (contractExecuted == true) {suicide(msg.sender);}
    }
}
