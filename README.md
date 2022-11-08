# Cardano Stats Starter App

## Overview 

This project is composed of a JavaScript focused frotend application, and a
backend application written in Haskell. A redis database server is also 
expected to be running, which the backend application communicates with.

Respository links for:
- [The frontend application](https://github.com/danebulat/cardano-stats-frontend)
- [The backend application](https://github.com/danebulat/cardano-stats-backend) (this repository)

The development stack demonstrated in this project could be adopted for
developing Dapps (decentralised applications). Each stack component is also
replaceable. For example, React JS can replace the Vue JS frontend, a relational
database can replace Redis, etc.

## What does the starter application do?

The application provides a GUI for fetching and caching remote API calls. 
Although the GUI can be used to make requests to abstract APIs (see the 
JavaScript code), Blockfrost is the recommended API provider to use 
out-of-the-box.

Upon making a remote call to Blockfrost and getting a response:
- The response JSON is displayed on the front-end GUI.
- The requested endpoint URL is sent to the backend server via a `POST` request 
  and is cached in our data store - a redis database in this case.

From here, all previous API calls can retrieved from the backend server and 
displayed by clicking the `Fetch` button. We can also send `DELETE` requests 
to the backend server to remove that particular URL from the redis database.

![Screenshot](/doc/screenshot-01.png)

## Tech Stack 

### Backend 

Written in Haskell and utilises the following:

- [Servant](https://hackage.haskell.org/package/servant)<br>
  For writing a type-safe web API.
  
- [QuickCheck](https://hackage.haskell.org/package/QuickCheck)<br>
  For making a test-suite comprising property tests.
  
- [Hedis](https://hackage.haskell.org/package/hedis)<br>
  For interacting with the Redis datastore.

### Datastore 

- [Redis](https://github.com/redis/redis)<br>
  A redis server is expected to be running for the application 
  to fully work.

### Frontend 

Written in JavaScript (HTML5) with the following technologies:

- [Vue 3](https://github.com/vuejs/core)<br>
  For making a front-end single-page application (SPA) that 
  isn't dependent on any particular backend system.
  
- [Vite](https://github.com/vitejs/vite)<br>
  Frontend tooling for rapidly building the app and running a 
  development server.

- [Node JS](https://nodejs.org/en/)<br>
  Package manager and dependency to the above frontend technologies.

## Quick Start

1. Start the Redis service (port 6379):
 
   ```
   # Linux
   systemctl start redis.service
   
   # Mac OS
   brew services start redis
   ```

2. Download, install and start the back-end server (port 8081):

   ```
   git clone https://github.com/danebulat/cardano-stats-backend.git
   cd cardano-stats-backend
   cabal build
   cabal run
   ```

3. **[Optional]** Run the test suite to make sure Redis operations are running correctly:

  ```
  cabal build the-tests
  cabal run the-tests
  ```

4. Download, install and start the front-end server (port 5173):

   ```
   git clone https://github.com/danebulat/cardano-stats-frontend
   cd cardano-stats-frontend
   npm install
   npm run dev
   ```

4. Navigate to `http://localhost:5173/` and start using the app!
