web3.eth.defaultAccount = web3.eth.accounts[0];
me = web3.eth.defaultAccount;
other = web3.eth.accounts[1];

function assertEquals(actual, expected, reason) {
    if (actual !== expected) {
        formatError(actual, expected, reason);
    }
}

function assertNotEquals(actual, expected, reason) {
    if (actual === expected) {
        formatError(actual, expected, reason);
    }
}

function formatError(actual, expected, reason) {
        throw "\nActual: " + actual.toString() +
              "\nExpected: " + expected.toString() +
              (reason === "" ? "" : reason);
}

var dataFeedValue = 43;

// Set data feed value
console.log("Now setting data feed value to 43");
var df = DataFeed0_.set( 0, dataFeedValue, {from: me, gas: 3000000});
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

var validApproveAmount = 100;

// Approve with sufficient amount and activate
// Confirm successful return value and that money has been moved to DC.
b_A = Tmc4_DKK.approve( EPAddress, validApproveAmount, {from: me, gas: 3000000} );
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

var valueBeforeActivate = balanceOf(me);

// Call to activate
b_A = european_option_.activate({from: me, gas: 3000000});
t0_A = web3.eth.getTransaction(b_A);
while(t0_A.blockNumber === null){
    t0_A = web3.eth.getTransaction(b_A);
}

// TODO: Assert return value

// Assert that the money has been moved from me to DC
// Check my balance change
assertEquals(
    Tmc4_DKK.balanceOf(me),
    valueBeforeActivate - validApproveAmount,
    "\nReason: Check my balance after activate");

//Check DC balance change
assertEquals(
    Tmc4_DKK.balanceOf(EPAddress),
    validApproveAmount,
    "\nReason: Check DC balance after activate");

// Execute the European option contract
cs  = european_option_.execute({from: me, gas: 3000000});
tcs = web3.eth.getTransaction(cs);
while(tcs.blockNumber === null){
    tcs = web3.eth.getTransaction(cs);
}

// TODO: Assert ret val

// Assert that the right amount was sent back to me after execute
assertEquals(
    Tmc4_DKK.balanceOf(me),
    valueBeforeActivate - dataFeedValue,
    "\nReason: Check my balance after execute");

// Assert that DC has no money left
assertEquals(
    Tmc4_DKK.balanceOf(EPAddress),
    0,
    "\nReason: Check DC balance after execute");

assertEquals(
    Tmc4_DKK.balanceOf(other),
    dataFeedValue,
    "\nReason: Check other balance after execute");

// Execute the DC again
// Confirm failure and no balances was altered
cs  = european_option_.execute({from: me, gas: 3000000});
tcs = web3.eth.getTransaction(cs);
while(tcs.blockNumber === null){
    tcs = web3.eth.getTransaction(cs);
}

// TODO: Assert ret val
