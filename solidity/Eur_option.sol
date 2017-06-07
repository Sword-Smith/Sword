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
