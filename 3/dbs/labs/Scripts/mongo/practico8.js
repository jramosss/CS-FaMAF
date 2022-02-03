use mflix;

/*
1. Cantidad de cines (theaters) por estado.*/

db.theaters.aggregate([
    {
        $group : {
            _id : "$location.adress.state",
            "theaters_per_state" : {$sum : 1}
        }
    }
])

/*
2. Cantidad de estados con al menos dos cines (theaters) registrados.*/

db.theaters.aggregate([
    {
        $group: {
            _id: "$location.address.state",
            theaters_per_state: {$sum: 1}
        }
    },
    {
        $match: {theaters_per_state: {$gte: 2}}
    },
    {$count: "states with at least 2 theaters"}
])


/*
3. Cantidad de películas dirigidas por "Louis Lumière". Se puede responder sin pipeline de
agregación, realizar ambas queries.*/

db.movies.find({directors : 'Louis Lumière'})

db.movies.aggregate([
    {
        $match : {directors : 'Louis Lumière'}
    },
    {
        $count : "Louis movies"
    }
])

/*
4. Cantidad de películas estrenadas en los años 50 (desde 1950 hasta 1959). Se puede
responder sin pipeline de agregación, realizar ambas queries.*/

db.movies.aggregate([
    {
        $match : {
            $and : [{year : {$gte : 1950}},{year : {$lt : 1960}}]
        }
    },
    {
        $count : "50's movies"
    }
])

db.movies.find({$and : [{year : {$gte : 1950}},{year : {$lt : 1960}}]}).count()

/*
5. Listar los 10 géneros con mayor cantidad de películas (tener en cuenta que las películas
pueden tener más de un género). Devolver el género y la cantidad de películas. Hint:
unwind puede ser de utilidad*/

db.movies.aggregate([
    {
        $unwind : "$genres"
    },
    {
        $group : {
            _id : '$genres',
            'n_of_movies' : {$sum : 1}
        }
    },
    {
        $project : {genres : 1,n_of_movies : 1}
    },
    {$sort : {n_of_movies : -1}},
    {$limit : 10}
])

/*
6. Top 10 de usuarios con mayor cantidad de comentarios, mostrando Nombre, Email y
Cantidad de Comentarios.*/

db.comments.aggregate([
    {
        $unwind : '$name'
    },
    {
        $group : {
            _id : '$email',
            name : {$first : '$name'},
            comments_per_user : {$sum : 1}
        }
    },
    {$sort : {comments_per_user : -1}},
    {$limit : 10}
])

/*
7. Ratings de IMDB promedio, mínimo y máximo por año de las películas estrenadas en los
años 80 (desde 1980 hasta 1989), ordenados de mayor a menor por promedio del año.*/

db.movies.aggregate([
    {
        $unwind : '$imdb'
    },
    {
        $match : {
            year : {$gte : 1980,$lt : 1990}
        }
    },
    {
        $group : {
            _id : '$year',
            avg : {$avg : '$imdb.rating'},
            max : {$max : '$imdb.rating'},
            min : {$min : '$imdb.rating'},
        }
    },
    {
        $sort : {'avg' : -1}
    }
])

/*
8. Título, año y cantidad de comentarios de las 10 películas con más comentarios.*/

db.movies.aggregate([
    {
        $group:{
            _id: "$title",
            year: {$first: "$year"},
            num_of_comments: {$first: "$num_mflix_comments"}    //not sure, this field gives me 0 confidence
        }
    },
    {
        $sort: {num_of_comments: -1}
    },
    {$limit: 10}
])


/*
9. Crear una vista con los 5 géneros con mayor cantidad de comentarios, junto con la
cantidad de comentarios.*/

db.movies.aggregate([
    {
        $unwind : '$genres'
    },
    {
        $group : {
            _id : '$genres',
            max_comments : {$max : '$num_mflix_comments'}
        }
    },
    {
        $sort : {'max_comments' : -1}
    }
])

db.createView(
    "Most_commented_genres",
    "movies",
    [
        {
            "$lookup": {
                from: "comments",
                localField: "_id",
                foreignField: "movie_id",
                as: "comments_from_movie"
            }
         },
         {
             "$unwind": "$genres"
         },
         {
             "$group": {
                 "_id": "$genres",
                 "comments_from_genre": {"$sum": {"$size": "$comments_from_movie"}}
              }
          },
          {
              "$addFields": {"genre": "$_id"}
          },
          {
              "$project": {"_id": 0}
          },
          {
              "$sort": {
                  "comments_from_genre": -1
              }
          },
          {
              "$limit": 5
         }
     ]
)


/*
10. Listar los actores (cast) que trabajaron en 2 o más películas dirigidas por "Jules Bass".
Devolver el nombre de estos actores junto con la lista de películas (solo título y año)
dirigidas por “Jules Bass” en las que trabajaron.
a. Hint1: addToSet
b. Hint2: {'name.2': {$exists: true}} permite filtrar arrays con al menos 2
elementos, entender por qué.
c. Hint3: Puede que tu solución no use Hint1 ni Hint2 e igualmente sea correcta
*/

db.movies.aggregate([
    {
        "$match": {
            "directors": {
                "$all": ["Jules Bass"],
            }
        }
    },
    {
        "$unwind": "$cast"
    },
    {
        "$group": {
            "_id": "$cast",
            "movies": {
                "$addToSet": {
                    "title": "$title",
                    "year": "$year"
                }
            }
        }
    },
    {
        "$match": {
            "$expr": {"$gte": [{"$size": "$movies"}, 2]}
        }
    },
    {
        "$project": {
            "actor": "$_id",
            "movies": 1,
            "_id": 0
        }
    }
])













