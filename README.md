_...work in progress..._

# purescript-truffle-pet-shop

[Truffle's Pet Shop tutorial](http://truffleframework.com/tutorials/pet-shop) ported to [`PureScript`](http://www.purescript.org/).


# Installation

- Install dependencies (only once)

```bash
yarn
```

- Build PS sources
```
pulp build
```

- Migrate data
```
truffle migrate
```

- Check the output of `truffle migrate` and copy the address hash of the Adoption contract from it.
At the output you will find that address by searching for `Adoption:`.
Then create an empty `.env` file to projects root folder and add an `env` variable `ADOPTION_ADDRESS` to it containing the hash. For example:
```
ADOPTION_ADDRESS = "0x30753e4a8aad7f8597332e813735def5dd395028"
```

- Run blockchain locally
```
truffle develop
```

- Serve app
```
yarn start
```
