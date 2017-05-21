pragma solidity ^0.4.10;
import "Tmc4.sol";
import "DataFeed0.sol";

contract Eur_Option {

    uint deployTime;
    Tmc4 tmc;
    DataFeed0 dataFeed;
    bool contractExecuted; // default value is false

    function Eur_Option()
    {
        deployTime = now;
        tmc = Tmc4(0xf19a0bf79894b82cda0fc94b238b915d4fa35072); // Write in the correct address for the TMC
        dataFeed = DataFeed0(0xb44c46bc94a2b4d2e1b363216eec996036d44281); // Write in the correct address for the DF
    }

    function execute()
    {
        if (contractExecuted){return;}
        if (now - deployTime >= 36000) // Ten hours
        {
            uint obs_value = dataFeed.get("0");
            if (obs_value == 0)
            {
                contractExecuted = true;
                return;
            }

            uint obs_price = (obs_value - 10 >= 0) ? obs_value - 10 : 0; // Strike price is 10
            uint final_price = (obs_price <= 2000) ? obs_price : 2000; // 2000 is maxAmount from scale

            tmc.transferFrom(0x2174c7f683d87012e8a1e4559be2f38bdc24b164, 0x2174c7f683d87012e8a1e4559be2f38bdc24b160, final_price);
            contractExecuted = true;
        }
    }
}
