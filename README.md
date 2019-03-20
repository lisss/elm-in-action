## TODO
  - [ ] Complete the readme

### Photo Groove sample example

#### Build
```
yarn build:photo
open src/photo-groove/index.html
```
#### Dev
```
yarn dev:photo
```
Open `http://localhost:8000/`


### Tasks from the [elm-study-group](https://github.com/KyivHaskell/elm-study-group/tree/master/resources) resources

#### Build
```
yarn dev:main
```
Open `http://localhost:8000/`


#### For CRUD task
1. [Install Stack](https://docs.haskellstack.org/en/stable/README/)
2. Run [crud.hs](./src/crud/crud.sh) (make sure to `chmod +x` it)
3. The server port is `8002`, change if needed
4. Set up proxy on your client; in case of `elm-live` - add `-x /api --proxyHost http://localhost:8002` options (do nothing if you use `dev:main` script)
