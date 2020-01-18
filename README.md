# CL-Naivechain
A simple blockchain implementation in Common Lisp.
Inspired by https://github.com/lhartikk/naivechain.

Of cause this code is less than 200 lines!

### How to Play

Install and start a peer:

```
$ ros install ihuku/cl-naivechain
$ ros exec naivechain --api 3001 --p2p 6001
```

Open another terminal. Then

```
$ ros exec naivechain --api 3002 --p2p 6002
```

Now, two peers are started.
Let's open a new terminal and try talking to them by using HTTP API below.

#### To get Blockchain of the first peer:
```
$ curl http://localhost:3001/blocks
```

#### To connect the first peer and the second peer:
```
$ curl http://localhost:3001/add-peer -d "address=localhost" -d "port=6002"
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
