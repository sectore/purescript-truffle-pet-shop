# purescript-truffle-pet-shop

[Ethereum](https://ethereum.org/) based smart contract example by using [Cliquebait](https://github.com/f-o-a-m/cliquebait), [Chanterelle](https://github.com/f-o-a-m/chanterelle), [purescript-web3](https://github.com/f-o-a-m/purescript-web3) and other funny things. It's heavily inspired by original [Truffle's Pet Shop tutorial](http://truffleframework.com/tutorials/pet-shop).

<img src="https://raw.githubusercontent.com/sectore/purescript-truffle-pet-shop/master/ps-screen.png" width="60%" height="60%">


# Installation

- Install dependencies (only once)
```bash
yarn
```

# Run

- Generate PureScript sources of contracts into folder `src/Contracts`
```bash
yarn generator
```

- Use [`cliquebait`](https://github.com/f-o-a-m/cliquebait) to have a running blockchain locally. Replace `METAMASK_ACCOUNT_ADDRESS` with the address of a MetaMask development account you want to use. _Note:_ You might run following command as `sudo`.
```bash
docker run --rm -it -p 8545:8545 -v `pwd`/cliquebait.json:/cliquebait/cliquebait.json -e ACCOUNTS_TO_CREATE=3 -e EXTERNAL_ALLOCS=METAMASK_ACCOUNT_ADDRESS ALLOC_WEI=0xC08DE6FCB28B80000 foamspace/cliquebait:v1.8.11
```

- Deploy contract. With this step contract artifacts ([ABI](https://github.com/ethereum/wiki/wiki/Ethereum-Contract-ABI)) will be created in folder `build/contracts/`. Also the contract address will be stored into `.env` file, which will be injected into Pet Shop application while running it.
```bash
yarn deploy
```

- Build PureScript sources of Pet Shop application
```bash
pulp build
```

- Serve app
```bash
yarn start
```

- Open http://localhost:1234/

# Tests

1. Run [`cliquebait`](https://github.com/f-o-a-m/cliquebait) as described above

2. Run tests
```bash
yarn test
```

# Acknowledge

- [Truffle's Pet Shop tutorial](http://truffleframework.com/tutorials/pet-shop) written in `JS`
- [purescript-web3-tests](https://github.com/f-o-a-m/purescript-web3-tests/) (`Chanterelle` and [purescript-web3](https://github.com/f-o-a-m/purescript-web3/) )
- [parking-dao](https://github.com/f-o-a-m/parking-dao) (`Solidity` app using `Chanterelle`)
- [Purescript Halogen Example](https://github.com/vladciobanu/purescript-halogen-example)
