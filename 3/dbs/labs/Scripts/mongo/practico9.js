use mflix;

//PARTE 1

/*
1. Especificar en la colección users las siguientes reglas de validación: El campo name
(requerido) debe ser un string con un máximo de 30 caracteres, email (requerido) debe
ser un string que matchee con la expresión regular: "^(.*)@(.*)\\.(.{2,4})$" ,
password (requerido) debe ser un string con al menos 50 caracteres.*/

db.runCommand({
    collMod: "users",
    validator : {$jsonSchema : {
        bsonType : 'object',
        required : ['name','email','password'],
        properties : {
            name : {
                bsonType : 'string',
                maxLength : 30,
                description : 'String de maximo 30 caracteres'
            },
            email : {
                bsonType : 'string',
                pattern : "^(.*)@(.*)\\.(.{2,4})$",
                description : 'String que matchee la regex ^(.*)@(.*)\\.(.{2,4})$'
            },
            password : {
                bsonType : 'string',
                minLength : 50,
                description : 'String de minimo 50 letras'
            }
        }
    }   
    },
    validationLevel : 'moderate'
})

//Fails
db.users.insertOne({
    name : 'Chuls'
})

//fails
db.users.insertOne({
    name : 'Chuls',
    email : 'jramos'
})

db.users.insertOne({
    name : 'Chuls',
    email : 'jramos@gmail.com'
})

//Pass
db.users.insertOne({
    name : 'Chuls',
    email : 'jramos@gmail.com',
    password : 'Shulss1223123984723987402983740892379847098237532897753270985798324789573489758973249856893475897349857'
})

/*
2. Obtener metadata de la colección users que garantice que las reglas de validación fueron
correctamente aplicadas.*/

db.getCollectionInfos({name: "users"})

/*
3. Especificar en la colección theaters las siguientes reglas de validación: El campo theaterId
(requerido) debe ser un int y location (requerido) debe ser un object con:
a. un campo address (requerido) que sea un object con campos street1, city, state y
zipcode todos de tipo string y requeridos
b. un campo geo (no requerido) que sea un object con un campo type, con valores
posibles “Point” o null y coordinates que debe ser una lista de 2 doubles
Por último, estas reglas de validación no deben prohibir la inserción o actualización de
documentos que no las cumplan sino que solamente deben advertir.*/

db.runCommand({
    collMod : 'theaters',
    validator : {
        bsonType : 'object',
        required : ['theaterId','location','adress'],
        properties : {
            theaterId : {
                bsonType : 'int'
            },
            location : {
                bsonType : 'object',
                required : ['adress'],
                properties : {
                    adress : {
                        bsonType : 'object',
                        required : ['street1','city','state','zipcode'],
                        properties : {
                            street1 : {bsonType : 'string'},
                            city : {bsonType : 'string'},
                            state : {bsonType : 'string'},
                            zipcode : {bsonType : 'string'},
                        }
                    },
                    geo : {
                        bsonType : 'object',
                        properties : {
                            type : {enum : ['Point',null]},
                            coordinates : {
                                bsonType : "array",
                                minItems : 2,
                                maxItems : 2,
                                items : {bsonType : 'double'}
                            }
                        }
                    }
                }
            }
        }
    },
    validationAction : 'warn'
})

/*
4. Especificar en la colección movies las siguientes reglas de validación: El campo title
(requerido) es de tipo string, year (requerido) int con mínimo en 1900 y máximo en 3000,
y que tanto cast, directors, countries, como genres sean arrays de strings sin duplicados.*/

db.runCommand({
    collMod: "movies",
    validator: {$jsonSchema: {
        bsonType: "object",
        required: ["title", "year"],
        properties: {
            title: {
                bsonType: "string",
            },  
            year: {
                bsonType: "int",
                minimum: NumberInt(1900),
                maximum: NumberInt(3000)
            },  
            cast: {
                bsonType: "array",
                items: {bsonType: "string"},
                uniqueItems: true 
            },  
            directors: {
                bsonType: "array",
                items: {bsonType: "string"},
                uniqueItems: true 
            },  
            countries: {
                bsonType: "array",
                items: {bsonType: "string"},
                uniqueItems: true 
            },  
            genres: {
                bsonType: "array",
                items: {bsonType: "string"},
                uniqueItems: true 
            }   
        }
    }
    }
})

/*
5. Crear una colección userProfiles con las siguientes reglas de validación: Tenga un campo
user_id (requerido) de tipo “objectId”, un campo language (requerido) con alguno de los
siguientes valores [ “English”, “Spanish”, “Portuguese” ] y un campo favorite_genres (no
requerido) que sea un array de strings sin duplicados.
Modelo de relaciones en MongoDB
6. Identificar los distintos tipos de relaciones (One-To-One, One-To-Many) en las
colecciones movies y comments. Determinar si se usó embedding o referenciación en
cada relación y justificar la razón.
*/


/*PARTE 2*/

/*
1. Crear el modelo de datos en mongodb para shop.*/
use shop;

db.createCollection("orders",{
    validator: {$jsonSchema: {
        bsonType: "object",
        required: ["_id", "delivery_name", "delivery_add",
                    "cc_number"],
        properties: {
            order_id:{bsonType: "int"},
            delivery_name: {bsonType: "string", maxLength: 70},
            delivery_add: {bsonType: "string", maxLength: 70},
            cc_number: {bsonType: "string", maxLength: 32},
            cc_name: {bsonType: "string", maxLength: 70},
            cc_expiry: {bsonType: "string", maxLength: 20},
            order_details: {
                bsonType: "array",
                items: {bsonType: "int"},
                uniqueItems: true
            }
        }
    }
    }
})

db.createCollection("order_details",{
    validator: {$jsonSchema: {
        bsonType: "object",
        required: ["_id", "book_id", "order_id", "title", "price"],
        properties: {
            _id: {bsonType: "int"},
            book_id: {bsonType: "string", maxLength: 70},
            order_id:{bsonType: "int"},
            title: {bsonType: "string", maxLength: 32},
            price: {bsonType: "double"},
            quantity: {bsonType: "int"},
            author: {bsonType: "string", maxLength: 70},
        }
    }
    }
})

db.createCollection("books",{
    validator: {$jsonSchema: {
        bsonType: "object",
        required: ["_id", "category_id", "title", "price"],
        properties: {
            _id: {bsonType: "int"},
            category_id: {bsonType: "string", maxLength: 70},
            title: {bsonType: "string", maxLength: 32},
            price: {bsonType: "double"},
            author: {bsonType: "string", maxLength: 70},
            order_id: {
                bsonType: "array",
                items: {bsonType: "int"},
                uniqueItems: true
            }
        }
    }
    }
})

db.createCollection("categories",{
    validator: {$jsonSchema: {
        bsonType: "object",
        required: ["category_id", "category_name"],
        properties: {
            category_id: {bsonType: "int"},
            category_name: {bsonType: "string", maxLength: 70},
            book_id: {
                bsonType: "array",
                items: {bsonType: "int"},
                uniqueItems: true
            }
        }
    }
    }
})


/*
2. Crear el dataset para las colecciones de shop para esta versión inicial.
3. Actualizar el modelo datos según las siguientes necesidades:
a. Modelar información de los usuarios (username, password, email, name, address,
contacts).
b. Modelar información de reviews de libros (title, content, rating) realizados por los
usuarios.c. Se necesita acceder a la cantidad y promedio de rating de reviews por libro.
Dicha información debe poder consultarse de manera rápida cada vez que se
accede a la información de un libro, y se va a acceder de manera frecuente.
4. Crear el dataset para las colecciones de shop para esta nueva versión.
*/



























