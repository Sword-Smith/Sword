web3.eth.defaultAccount = web3.eth.accounts[0];
me = web3.eth.defaultAccount;
other = web3.eth.accounts[1];

// TODO: Write assert

// Set data feed value
console.log("Now setting data feed value to 43");
var df = DataFeed0_.set( 0, 43, {from: me, gas: 3000000});
var t1 = web3.eth.getTransaction(df);
while(t1.blockNumber === null){
    t1 = web3.eth.getTransaction(df);
}

EPAddress = european_option_.address;
console.log("My address is: " + me);
console.log("European option address is: " + EPAddress);

console.log("********* BEFORE EXECUTION OF EUROPEAN OPTION *********");
var balance_A = Tmc4_DKK.balanceOf(me);
console.log("My balance on token contract DKK is: " + balance_A);
balance_A = Tmc4_DKK.balanceOf(other);
console.log("Other balance on token contract DKK is: " + balance_A);

// Call to activate without approval of money
// Confirm failure
var b_A = european_option_.activate({from: me, gas: 3000000});
var t0_A = web3.eth.getTransaction(b_A);
while(t0_A.blockNumber === null){
    t0_A = web3.eth.getTransaction(b_A);
}
// TODO: Assert return value!


// Call execute before successful activation
// Verify no actions took place
var cs  = european_option_.execute({from: me, gas: 3000000});
var tcs = web3.eth.getTransaction(cs);
while(tcs.blockNumber === null){
    tcs = web3.eth.getTransaction(cs);
}

// TODO: Assert return value!

// Call approve with an insufficient amount and call active
// Confirm failure
console.log('Now calling "approve" on the token contract DKK');
b_A = Tmc4_DKK.approve( EPAddress, 10, {from: me, gas: 3000000} );
t0_A = web3.eth.getTransaction(b_A);
while(t0_A.blockNumber === null){
    t0_A = web3.eth.getTransaction(b_A);
}

// Call to activate
b_A = european_option_.activate({from: me, gas: 3000000});
t0_A = web3.eth.getTransaction(b_A);
while(t0_A.blockNumber === null){
    t0_A = web3.eth.getTransaction(b_A);
}

// TODO: Assert return value!

// Approve with sufficient amount and activate
// Confirm successful return value and that money has been moved to DC.
b_A = Tmc4_DKK.approve( EPAddress, 100, {from: me, gas: 3000000} );
t0_A = web3.eth.getTransaction(b_A);
while(t0_A.blockNumber === null){
    t0_A = web3.eth.getTransaction(b_A);
}

// Attempt to execute before activation
// Confirm failure to execute
cs  = european_option_.execute({from: me, gas: 3000000});
tcs = web3.eth.getTransaction(cs);
while(tcs.blockNumber === null){
    tcs = web3.eth.getTransaction(cs);
}
// TODO: Assert return value


// Call to activate
b_A = european_option_.activate({from: me, gas: 3000000});
t0_A = web3.eth.getTransaction(b_A);
while(t0_A.blockNumber === null){
    t0_A = web3.eth.getTransaction(b_A);
}

// TODO: Assert return value
// TODO: Assert balance changes

// Execute the European option contract
cs  = european_option_.execute({from: me, gas: 3000000});
tcs = web3.eth.getTransaction(cs);
while(tcs.blockNumber === null){
    tcs = web3.eth.getTransaction(cs);
}

// TODO: Assert ret val
// TODO: Assert balance changes
// TODO: Assert that superfluous funds is sent back from DC

// Check the balances of my own address and the address to which I have transferred
// and check that the balance of DC is empty.
console.log("********* AFTER EXECUTION OF EUROPEAN OPTION *********");
balance_A = Tmc4_DKK.balanceOf(me);
console.log("My balance on token contract DKK is: " + balance_A);
balance_B = Tmc4_DKK.balanceOf(other);
console.log("Other balance on token contract DKK is: " + balance_B);


// Execute the DC again
// Confirm failure and no balances was altered
cs  = european_option_.execute({from: me, gas: 3000000});
tcs = web3.eth.getTransaction(cs);
while(tcs.blockNumber === null){
    tcs = web3.eth.getTransaction(cs);
}

// TODO: Assert ret val
