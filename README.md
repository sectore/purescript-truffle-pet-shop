_...work in progress..._

# purescript-truffle-pet-shop

[Truffle's Pet Shop tutorial](http://truffleframework.com/tutorials/pet-shop) ported to [`PureScript`](http://www.purescript.org/).

However, instead of being lost in space with [`Truffle`](http://truffleframework.com/) it uses a _more functional truffle_ called [`chantrelle`](https://github.com/f-o-a-m/chanterelle).


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
