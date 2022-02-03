use sample_airbnb;

/*
Sample: 

{
_id: '10006546',
// identificador del alojamiento
listing_url: '...airbnb.com/rooms/10006546', // url del alojamiento
name: 'Ribeira Charming Duplex',
// nombre del alojamiento
summary: 'Fantastic duplex apartment...',
...
property_type: 'House',
// tipo de la propiedad
room_type: 'Entire home/apt', // tipo del lugar
bed_type: 'Real Bed',
// tipo de cama
minimum_nights: '2',
maximum_nights: '30',
cancellation_policy: 'moderate',
...
first_review: ISODate("2016-01-03"), // fecha de la primer reseña
last_review: ISODate("2019-01-20"), // fecha de la última reseña
accommodates: 8, // número de huéspedes que puede alojar el alojamiento
bedrooms: 3,
// número de dormitorios
beds: 5,
// número de camas
number_of_reviews: 51,
// número de reseñas realizadas sobre el alojamiento
bathrooms: Decimal128("1.0"), // número de baños
amenities: [
// servicios que provee el alojamiento'TV',
'Cable TV',
'Wifi',
'Kitchen',
'Paid parking off premises',
'Smoking allowed',
'Pets allowed',
...
],
price: Decimal128("80.00"), // precio
...
host: { // Datos del anfitrión del alojamiento
host_id: '51399391', // identificador del anfitrión
host_url: 'https://www.airbnb.com/users/show/51399391',
host_name: 'Ana&Gonçalo', // nombre del anfitrion
host_is_superhost: false, // indica si es un super anfitrion
host_identity_verified: true, // indica si el anfitrión es verificado
host_listings_count: 3,
host_total_listings_count: 3,
host_verifications: [ // verificaciones realizadas por el anfitrión
'email',
'phone',
...
]
},
address: { // dirección del alojamiento
...
market: 'Porto',
country: 'Portugal', // país donde está localizado el alojamiento
...
},
...
review_scores: {
...
review_scores_rating: 89 // valoración del alojamiento
},
reviews: [ // reseñas sobre el alojamiento
{
_id: '58663741',
date: ISODate("2016-01-03T05:00:00.000Z"),
listing_id: '10006546',
reviewer_id: '51483096',
reviewer_name: 'Cátia',
comments: 'A casa da Ana e do Gonçalo...'
},
...
]
*/

/*
1. Buscar alojamientos en Brazil, Canada, o Australia para 4 huéspedes, con una
valoración de al menos 90 o bien posea más de 5 reseñas. Listar la url, nombre, tipo
de propiedad, país, número de reseñas y valoración del alojamiento. Limitar el
resultado a los 10 primeros alojamientos ordenados por la valoración y número de
reseñas en orden decreciente.*/

db.listingsAndReviews.find({
    "address.country" : {$in : ['Brazil','Canada','Australia']},
    accommodates : {$eq : 4},
    $or : [
        {"review_scores.review_scores_rating" : {$gte : 90}},
        {number_of_reviews : {$gt : 5}}
    ]
})

/*
O su alternativa usango pipelines de agregacion
db.listingsAndReviews.aggregate([
    {
        $match : {
            "address.country" : {$in : ['Brazil','Canada','Australia']},
            accommodates : {$eq : 4},
            $or : [
                {"review_scores.review_scores_rating" : {$gte : 90}},
                {number_of_reviews : {$gt : 5}}
            ]
        }
    }
])*/

/*
2. Buscar alojamientos en los Estados Unidos para 8 huéspedes, con 5 camas, 3
dormitorios y posea al menos una reseña entre noviembre de 2018 y octubre de
2019. Listar el nombre, precio y solo las reseñas dentro del rango de fecha
especificados. Mostrar el resultado ordenados de menor a mayor precio.*/

db.listingsAndReviews.aggregate([
    {
        $match : {
            accommodates : {$eq : 8},
            beds : {$eq : 5},
            bedrooms : {$eq : 3}
        }
    },
    {
        $project : {
            _id : 0,
            name : 1,
            price : 1,
            f : {
                $filter : {
                    input : "$reviews",
                    as : 'review',
                    cond : {
                        $and : [
                            {$gte : ['$$review.date', ISODate('2018-11-01T00:00:00.000Z')]},
                            {$lte : ['$$review.date', ISODate('2019-10-01T00:00:00.000Z')]},
                        ]
                    }
                }
            }
        }
    },
    {$sort : {price : 1}}
])

/*
3. Número de reseñas que se realizan mes a mes en alojamientos de España. Listar
año, mes y el número de reseñas. Mostrar el resultado ordenados por año y mes en
orden decreciente.*/

db.listingsAndReviews.aggregate([
    {
        $unwind : '$reviews'
    },
    {
        $match : {"address.country" : 'España'}
    },
    {
        $group : {
            _id : {$month : '$reviews.date'},
            count : {$sum : 1}
        }
    },
    {
        $project : {
            'reviews.date' : 1
        }
    }
])

/*
4. Top 5 de los super anfitriones con mayor cantidad de alojamientos ofrecidos en los
Estados Unidos. Listar el id y nombre del anfitrión junto con un arreglo de
subdocumentos de id y nombre de sus alojamientos. Mostrar el resultado ordenados
por cantidad de alojamientos ofrecidos y nombre del super anfitrión en orden
alfabético.*/

db.listingsAndReviews.aggregate([
    {
        $match : {
            "address.country" : 'United States',
            "host.host_is_superhost" : true
        }
    },
    {
        $project : {
            _id : 0,
            "host.host_id": 1,
            "host.host_name": 1,          
            
        }
    },
    {$sort : {"host.host_name" : 1,}}
])

/*
5. Para cada anfitrión de alojamientos de Brazil con identidad verificada y que ya
cuenten con facebook y email dentro de la lista de verificaciones, agregar pinterest y
twitter al final de la lista de verificaciones del anfitrión.
6. Crear una vista con información de los 10 alojamientos que recibieron reseñas más
recientemente. Listar el id y nombre del alojamiento junto con la última reseña
recibida.

/*
7. Especificar reglas de validación usando JSON Schema en la colección
listingsAndReviews a los siguientes campos: name, property_type, room_type, beds
y address ( y todos sus campos anidados ). Inferir los tipos y otras restricciones que
considere adecuados para especificar las reglas a partir de los documentos de la
colección. Testear la regla de validación generando dos casos de falla en la regla de
validación y dos casos donde cumple la regla de validación.
*/

db.runCommand({
    collMod : 'listingsAndReviews',
    validator : {$jsonSchema :{
        bsonType : 'object',
        required : []
    }}
})
















