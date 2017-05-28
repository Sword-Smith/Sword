pragma solidity ^0.4.10;
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
        deployTime = now;
        tmc = Tmc4(tmc_address); // Write in the correct address for the TMC
        dataFeed = DataFeed0(datafeed_address); // Write in the correct address for the DF
        to_address = to_address_;
        from_address = from_address_;
        contractExecuted = false;
    }

    function execute()
    {
        if (contractExecuted){return;}
        if (now - deployTime >= 180) // Three minutes
        {
            uint obs_value = dataFeed.get("0");
            if (obs_value == 0)
            {
                contractExecuted = true;
                return;
            }

            uint obs_price = (obs_value - 10 >= 0) ? obs_value - 10 : 0; // Strike price is 10
            uint final_price = (obs_price <= 2000) ? obs_price : 2000; // 2000 is maxAmount from scale

            tmc.transferFrom(from_address, to_address, final_price);
            contractExecuted = true;
        }
    }
}
