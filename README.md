# Parley

> Par·ley n.  
>   
> To have a discussion, especially with an enemy.

Parley is a web application for storing and retrieving comments. It is an educational
example whose primary goal is to demonstrate the application of beginner Haskell concepts
in a real project.

## Building and running

To build parley, run the following from the root of the project:

```
$ cabal install --only-dependencies
$ cabal configure --enable-tests
$ cabal build
```

Once parley has been built successfully, you may run it thusly:

```
$ cabal run parley
$ # OR
$ ./dist/build/parley/parley
```

...and with command line options

```
$ cabal run parley -- --port=8888
$ # OR
$ ./dist/build/parley/parley --port=8888
```

Finally, if you're using GHCi, you can run the application from there and pass command line arguments as well.

```
λ> :main --port=8888
```

# Troubleshooting

You may need to install the `zlib1g-dev` package on Ubuntu before you can install dependencies.

```
$ sudo apt-get install zlib1g-dev
$ cabal install --only-dependencies
...
```
