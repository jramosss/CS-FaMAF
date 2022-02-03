# Tests
## Parte 1 - Categories & Freelancers

#### GET	/api/categories		

Este GET deberia devolver una lista de las categorias disponibles en la base de datos, con el siguiente formato especificado en la consigna.


Return: 200 - [{id: int, name: str}]

$ `curl -X GET http://localhost:8080/api/categories`

---

#### GET	/api/freelancers

Este GET deberia devolver una lista de los freelancers disponibles en la base de datos, con el siguiente formato especificado en la consigna.

Return: 200 - [{id: int, username: str, country_code: str, category_ids: [int], reputation: str, hourly_price: float}]	

$ `curl -X GET http://localhost:8080/api/freelancers`

---

#### GET	/api/freelancers/:id		

Este GET, dado un ID valido, devuelve el freelancer asociado. Caso contrario devuelve un Bad Request, con el siguiente formato especificado en la consigna.


Return: 200 - {id: int, username: str, country_code: str, category_ids: [int], reputation: str, hourly_price: int, total_earnings: int} | 400 - Bad Request If Id is not an Int of the freelancer does not exist.	

$ `curl -X GET http://localhost:8080/api/freelancers/2`

* Observar que si cambiamos el id por uno inexistente devuelve un Bad Request.

---

#### POST	/api/freelancers/

Este POST, se encarga de agregar un nuevo freelancer a la base de datos, con el siguiente formato especificado en la consigna.


Return: 200 - id | 400 - Bad Request

$ `curl -X POST http://localhost:8080/api/freelancers -H "Content-type: application/json" -d '{"username": "MrBurns", "country_code": "ES", "reputation":"Senior", "category_ids": [1], "hourly_price": 40.00}'`

* Observar que si el campo reputation no se indica, se setea por defecto Junior, como especifica la consiga.

## Parte 2: Clientes y trabajos

#### GET /api/clients

Este GET devuelve todos los clientes existentes en la base de datos

Return: 200 - [{id: int, username: str, country_code: str, total_spend: Int}]

$ `curl -X GET http://localhost:8080/api/clients`

---

#### GET /api/clients/id     

Este GET devuelve el freelancer con el id dado si es que existe en la base de datos

Return 200 - {id: int, username: str, country_code: str, total_spend: Int, job_ids: [int]} \ 400 - Bad Request

$ `curl -X GET http://localhost:8080/api/clients/1`

* id valido

$ `curl -X GET http://localhost:8080/api/clients/0` 
* para testear el caso de id 	    					   inexistente, 400 - Bad Request

---

#### POST /api/clients

Este POST crea un nuevo cliente con los parametros dados

Return: 200 - id : int

$ `curl -X POST http://localhost:8080/api/clients -H "Content-type: application/json" -d '{"username": "Shuls","country_code":"AR"}'`

$ `curl -X POST http://localhost:8080/api/clients -H "Content-type: application/json" -d '{"username": "","country_code":"AR"}'`
* Caso campo username nulo, anda de todas formas.

---

#### GET /api/jobs

Este GET devuelve todos los trabajos registrados en la base de datos

Return : 200 - [{id: int, title: str, category_id: int, client_id: int, preferred_expertise: str, preferred_country: str, hourly_price: int}]

$ `curl -X GET http://localhost:8080/api/jobs`

---

#### POST /api/jobs

Este POST crea un nuevo trabajo con los parametros especificados

Return : 200 - id:int \ 400 - Bad Request if: * the client does not exists * any of the categories does not exists

$ `curl -X POST http://localhost:8080/api/jobs -H "Content-type: application/json" -d '{"category_id":1,"client_id":1,"tittle":"Teoria de grafos","preferred_expertise":"Senior" ,"hourly_price":30.00}'`

$ `curl -X POST http://localhost:8080/api/jobs -H "Content-type: application/json" -d '{"category_id":1,"client_id":1,"tittle":"Teoria de grafos","preferred_expertise":"" ,"hourly_price":30.00}'`  
* Sin preferred expertise, Asigna Senior por default.

$ `curl -X POST http://localhost:8080/api/jobs -H "Content-type: application/json" -d '{"category_id":,"client_id":1,"tittle":"Teoria de grafos","preferred_expertise":"Senior" ,"hourly_price":30.00}' //Falta client_id`
* None.get exception.

---
