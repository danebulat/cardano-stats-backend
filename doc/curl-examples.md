# Curl Examples 

Performing a `DELETE` request:

```
curl -X DELETE http://localhost:8081/requests/2939 \
  -H "Accept: application/json"
```

Performing a `POST` request:

```
curl -X POST http://localhost:8081/requests \
  -H "Content-Type: application/json" \
  -d '{ "getReqUrl": "http://some.url" }'
```

Performing a `GET` request:

```
curl http://localhost:8081/requests
```
