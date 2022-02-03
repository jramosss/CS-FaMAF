use airbnb;

//db.airbnb.find({})

//Find the first 20 rooms sorted by id with acceptance rate > 50%
db.airbnb.find(
{ $and : [
    {host_acceptance_rate : {$gt : '50%'}},
    {host_acceptance_rate : {$ne : 'N/A'}}
    ]        
}
).limit(20).sort({id : 1});
















