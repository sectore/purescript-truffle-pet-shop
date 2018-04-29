_...work in progress..._

# purescript-truffle-pet-shop

[Truffle's Pet Shop tutorial](http://truffleframework.com/tutorials/pet-shop) ported to [`PureScript`](http://www.purescript.org/).

However, instead of being lost in space with [`Truffle`](http://truffleframework.com/) it uses a _more functional truffle_ called [`chantrelle`](https://github.com/f-o-a-m/chanterelle).


# Installation

- Install dependencies (only once)

```
yarn
```

# Run

- Generate PureScript sources of contracts into folder `src/Contracts`
```
yarn generator
```

- Make a copy of `cliquebait.template.json` and rename it to `cliquebait.json`. Open that file to replace `METAMASK_ACCOUNT_ADDRESS` with the address of your MetaMask account. With that the value of `"alloc"` will be similar like this:

```
"alloc": {
  "0x627306090abaB3A6e1400e9345bC60c78a8BEf57": {
    "balance": "0x200000000000000000000000000000000000000000000000000000000000000"
  }
}
```

- Run [`cliquebait`](https://github.com/f-o-a-m/cliquebait) to have a running blockchain locally. Replace `ABSOLUTE_PATH_TO_PROJECT` with the absolute path pointing to the location of this project on your machine.
```
sudo docker run --rm -it -p 8545:8545 -v ABSOLUTE_PATH_TO_PROJECT/cliquebait.json:/cliquebait/cliquebait.json foamspace/cliquebait:latest
```

- Deploy contract. With this step contract artifacts ([ABI](https://github.com/ethereum/wiki/wiki/Ethereum-Contract-ABI)) will be created in folder `build/contracts/`. Also the contract address will be stored into `.env` file, which will be injected into Pet Shop application while running it.
```
yarn deploy
```

- Build PureScript sources of Pet Shop application
```
pulp build
```

- Serve app
```
yarn start
```

- Open http://localhost:1234/

# Tests

1. Run [`cliquebait`](https://github.com/f-o-a-m/cliquebait) as described above

2. Run tests
```
yarn test
```

# Acknowledge

- [Truffle's Pet Shop tutorial](http://truffleframework.com/tutorials/pet-shop) written in `JS`
- [purescript-web3-tests](https://github.com/f-o-a-m/purescript-web3-tests/) (`Chanterelle` and [purescript-web3](https://github.com/f-o-a-m/purescript-web3/) )
- [parking-dao](https://github.com/f-o-a-m/parking-dao) (`Solidity` app using `Chanterelle`)
