# cl-naivechain
A simple Blockchain implementation in Common Lisp.
Inspired by https://github.com/lhartikk/naivechain.

### How to Play
```
$ ros install ihuku/cl-naivechain
$ ros exec naivechain 3001 6001
```
`3001` is the port number for API, and `6001` is for P2P.

Open another terminal, then

```
$ ros exec naivechain 3002 6002
```

Now two peers are started.
Let's try talking to them by using API below.

#### To get Blockchain of the first peer:
```
$ curl http://localhost:3001/blocks
```

#### To add the second peer:
```
$ curl http://localhost:3001/add-peer -d "host=localhost" -d "port=6002"
```

#### To get connected peers:
```
$ curl http://localhost:3001/peers
```

#### To mine a new block:
```
$ curl http://localhost:3001/mine-block -d "data=this is a new block data"

Each peer has same chain.
$ curl http://localhost:3001/blocks
$ curl http://localhost:3002/blocks
```
