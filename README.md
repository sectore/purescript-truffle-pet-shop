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

- Run [`cliquebait`](https://github.com/f-o-a-m/cliquebait) to have a running blockchain locally.
```
docker run --rm -it -p 8545:8545 foamspace/cliquebait:latest
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
