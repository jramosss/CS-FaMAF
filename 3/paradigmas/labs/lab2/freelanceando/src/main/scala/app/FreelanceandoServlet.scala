package app
//En este archivo implementamos todos los endpoints
import org.json4s.{Formats, DefaultFormats, JInt, JString, JNothing, JDouble}
import org.json4s.JsonAST.{JArray,JValue}

import org.json4s.JsonDSL._
import org.scalatra._
import org.scalatra.json._
import models._
import models.database.Database
import scala.collection.mutable.{Map => Dict}
import java.io.IOException
import java.util.concurrent.Executor
import java.security.InvalidParameterException


class FreelanceandoServlet(db : Database) extends ScalatraServlet with 
  JacksonJsonSupport {

  protected implicit val jsonFormats: Formats = DefaultFormats

  final case class BadTypeException(
    private val message: String = "", 
    private val cause: Throwable = None.orNull)
    extends Exception(message, cause) 

  final case class InvalidReputationException(
    private val message: String = "",
    private val cause: Throwable = None.orNull)
    extends Exception(message, cause) 
  
  final case class InvalidCategoryException(
    private val message: String = "",
    private val cause: Throwable = None.orNull)
    extends Exception(message, cause) 

  final case class InvalidClientException(
    private val message: String = "",
    private val cause: Throwable = None.orNull)
    extends Exception(message, cause)
  

  before() {
    contentType = formats("json")
  }

  get("/api/categories") {
    var l = db.categories.all
    var resul : List[(String)] = List()
    var elem : String = "["
     l.foreach(y => elem = elem.concat((s"""{"id": ${y.getId},""" +
                                       s""" "name": "${y.getValue}"},""")))
    elem = elem.slice(0, elem.size - 1)
    elem = elem.concat("]\n")
    elem
  }
  
  get("/api/freelancers") {
    var l: List[models.Freelancer] = List()
    params.size match {
      case 0 => //Caso que no pasen parametros, tomamos todas las instancias
        l = db.freelancers.all
      case _ => //Si hay parametros, filtramos
        l = db.freelancers.filter(params.toMap)
    }
    var resul : List[(String)] = List()
    var elem : String = "["
    l.foreach(y => elem = 
    elem.concat((s"""{"id": ${y.getId}, "username": "${y.getUsername}",""" +
                 s""" "category_ids": ${y.getCategoryids},""" +
                 s""" "country_code": "${y.getCountryCode}",""" +
                 s""" "reputation": "${y.getReputation}",""" +
                 s""" "hourly_price": ${y.getHourlyprice}},""")))
    elem = elem.slice(0,elem.size - 1)
    elem = elem.concat("]\n")
    elem
  }
  
  get("/api/freelancers/:id"){
    try{      
      var id = params("id").toInt
      var elem: String = ""
      var instance = db.freelancers.get(id).get 
      elem = s"""200 - {"id": ${instance.getId}, """ +  
             s""" "username": "${instance.getUsername}", """ +
             s""" "category_ids": ${instance.getCategoryids}, """ +
             s""" "country_code": "${instance.getCountryCode}", """ +
             s""" "reputacion": "${instance.getReputation}", """ +
             s""" "hourly_price": ${instance.getHourlyprice}, """ +
             s""" "total_earnings": ${instance.getTotalEarnings}}\n"""     
      elem
    }
    catch{
      case a : NoSuchElementException  => a.formatted("400 - Bad Request\n")
      case b : NumberFormatException  => b.formatted("400 - Bad Request\n")
    } 
  }


  post("/api/freelancers") {
    /*
    Aca hacemos este matching anidado para que se lanze una excepcion
    si alguno de los argumentos no es del tipo necesario
    */
    var response = "200 - "
    try {
      var fn = new Freelancer
      var param = parseOpt(request.body).get
      (param \ "id") match {
        case JNothing => (param \ "username") match {
          case _:JString => (param \ "country_code") match {
            case _:JString => (param \ "hourly_price") match {
              case _:JDouble => (param \ "category_ids") match {
                case JArray(value) => 
                /*
                En el caso de category_ids, no solo checkeamos el tipo JArray
                sino tambien que cada uno de sus miebros sean de 
                categoria valida
                */
                db.categories.valid_categories(value) match {
                  case 1 => (param \ "reputation") match {
                    case JString(value) => value match {
                        case "Junior" => {
                          db.freelancers.save(fn.fromJson(param)) }
                        case "Senior" => {
                          db.freelancers.save(fn.fromJson(param)) }
                        case "Semi Senior" => {
                          db.freelancers.save(fn.fromJson(param)) }
                        case _ => (throw new InvalidReputationException)
                    } case JNothing => {
                      fn.setReputation("Junior")
                      db.freelancers.save(fn.fromJson(param))
                    }
                  } case _ => (throw new InvalidCategoryException)}
              } case _ => (throw new BadTypeException)
            } case _ => (throw new BadTypeException)
          } case _ => (throw new BadTypeException)
        } case _ => (throw new InvalidParameterException)
      }
      response += s"${fn.getId}\n"
      response
    } catch {
        //Ante cualquiera de estas excepciones, lanzamos un error 400
        case a : NoSuchElementException => {
          a.formatted("400 - Bad Request\n") }
        case b : InvalidReputationException => {
          b.formatted("400 - Bad Request\n") }
        case c : InvalidParameterException => {
          c.formatted("400 - Bad Request\n") }
        case d : BadTypeException => {
          d.formatted("400 - Bad Request\n") }
        case e : InvalidCategoryException => {
          e.formatted("400 - Bad Request\n") }
    }
  }

  get("/api/clients"){
    var list: List[models.Client] = List()
    params.size match {
      case 0 => list :::= db.clients.all
      case _ => list :::= db.clients.filter(params.toMap)
    }  
    var elem = "200 - ["
    list.foreach(c => elem = elem.concat(
      s"{ id: ${c.getId}, username: ${c.getUsername}, " +
      s"country_code: ${c.getCountryCode}, total_spend: ${c.getTotalSpend}"))
    elem = elem.concat("]\n")
    elem
  }
  
  get("/api/clients/:id"){
    try{
      var id = params("id").toInt
      var instance = db.clients.get(id).get
      var response: String = "200 - {"
      response = 
      response.concat(s""" "id": ${instance.getId},""" +
                      s""" "username": "${instance.getUsername}",""" +
                      s""" "country_code": "${instance.getCountryCode}",""" +
                      s""" "total_spend": ${instance.getTotalSpend},""" +
                      s""" "job_ids": ${instance.getJobsIds}""")
      response = response.concat("}\n")

      response
    }
    catch{
      case a: NoSuchElementException => a.formatted("400 - Bad Request\n")
      case b: NumberFormatException => b.formatted("400 - Bad Request\n")
    }
  }

  post("/api/clients"){
    var response: String = ""
    try{
      
      var client = new Client
      var param = parseOpt(request.body).get
      
      (param \ "total_spends") match {
        case JNothing => (param \ "jobs_ids") match {
          case JNothing =>
          case _ => throw new InvalidParameterException
        }
        case _ => throw new InvalidParameterException
      }
      
      (param \ "username") match{
        case JString(value) => (param \ "country_code") match {
          case JString(value) => {
            db.clients.save(client.fromJson(param))
            response = s"200 - ${client.getId}\n"
          } 
          case _ => throw new BadTypeException
        }
        case  _ => throw new BadTypeException  
      }
      
      response
    }
    catch {
      case a: BadTypeException => a.formatted("400 - Bad Request")
      case b: InvalidParameterException =>
                         b.formatted("400 - Bad Request")
    }
  }

  get("/api/jobs"){
    
    var list: List[models.Job] = List()
    params.size match{
      case 0 => list :::= db.jobs.all
      case _ => list :::= db.jobs.filter(params.toMap)
    }
    var response = "200 - ["
    list.foreach(x => response = 
    response.concat
     (s"""{"id": ${x.getId},""" +
      s""" "title": "${x.getTitle}", "category_id": ${x.getCategoryId},""" +
      s""" "client_id": ${x.getClientId},""" +
      s""" "preferred_expertise": "${x.getPreferredExpertise}",""" +
      s""" "preferred_country": "${x.getPreferredCountry}",""" +
      s""" "hourly_price": ${x.getHourlyPrice}}\n"""))
    response = response.slice(0, response.size - 1)
    response = response.concat("]\n")
    
    response
  }

  post("/api/jobs"){
    try{
      var param = parseOpt(request.body).get
      var response = "200 - "
      var job = new Job

      (param \ "category_id") match {
        case JInt(value) => {
          
          if (db.categories.valid_categories(List(value)).==(1)){

            (param \ "client_id") match{
              case JInt(value) => {
                
                if (db.clients.valid_client(value).==(1)){
                  db.jobs.save(job.fromJson(param))
                } else {
                  throw new InvalidClientException
                }
              }
              case _ =>
            }
          } else {
            throw new InvalidCategoryException
          }
        }
        case _ =>
      }
      
      var client = db.clients.get(job.getClientId).get
      client.setNewJob(job.getId)

      response += s"${job.getId}\n"
      response
    }
    catch {
      case a: InvalidClientException => a.formatted("400 - Bad Request")
      case b: InvalidCategoryException => b.formatted("400 - Bad Request")
    }
  }

  post("/api/posts/pay"){
    var param = parseOpt(request.body).get
    var flId: Int = 0
    var jId: Int = 0
    var amount: Double = 0
    var response: String = ""
    
    try{
      (param \ "freelancer_id") match {
        case JInt(value) => flId = value.toInt
        case _ => throw new BadTypeException 
      }

      (param \ "job_id") match {
        case JInt(value) => jId = value.toInt
        case _ => throw new BadTypeException
      }

      (param \ "amount") match {
        case JDouble(value) => amount = value
        case _ => throw new BadTypeException
      }

    var fl: Option[models.Freelancer] = db.freelancers.get(flId)
    var job: Option[models.Job] = db.jobs.get(jId)
    

      fl match {
        case None => response = "400 - Bad Request"
        case _ => job match {
          case None => response = "400 - Bad Request"
          case _ => {
          
            var client = db.clients.get(job.get.getClientId).get
            client.pay(amount, fl.get)
            db.clients.save(client)
            db.freelancers.save(fl.get)
            response = "200"
          }
        }
      }
    response

    }
    catch{
      case a: BadTypeException => a.formatted("BadTypeException")
    }
  }
}
